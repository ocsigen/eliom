(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010
 * Raphaël Proust
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)


(* This syntax extension is made of several steps.
 * This file is the first to be loaded, it modifies the lexer, and the parser.
 * There are two functors that subsequent extensions (type_filter, client_client
 * and client_server) use.
 *
 * The global structure of this module should appear in the comments. *)

module Id =
struct
  let name = "Eliom client-server syntax"
  let version = "alpha"
end



(*** OPTIONS ***)
(* Options are:
 * -pass      The pass executed. May be either "type" for type inference,
 *            "client" for client code or "server" for server code. The matching
 *            subsequent filter have to be loaded.
 * -type_file A filename for the type inferrence intermediary file.
 *)

type pass = Type_pass | Server_pass | Client_pass | Raw_pass
let pass = ref ""
let pass_of_string = function
  | "type" -> Type_pass
  | "server" -> Server_pass
  | "client" -> Client_pass
  | "raw" -> Raw_pass (*this is not documented! It's for debugging. *)
  | _ -> failwith ("Unknown pass \""^ !pass ^"\". Please use -pass argument.")
let get_pass () = pass_of_string !pass

let _ = Camlp4.Options.add "-pass" (Arg.Set_string pass)
          "name of the pass the seed is executing (type, server or client)"

let type_file = ref "syntax_temp_files/server_type_inferrence.mli"
let _ = Camlp4.Options.add "-typefile" (Arg.Set_string type_file)
          "type inferrence file"


open Camlp4.Sig (*for KEYWORD, LIDENT and SYMBOL *)



(*** LEXER ***)
(* In this section, we change the lexer by adding a few keywords. *)

module Make_lexer_filter (Syntax : Camlp4.Sig.Camlp4Syntax) =
struct
  include Syntax

  let merge_locs l ls = List.fold_left (*Syntax.*)Token.Loc.merge ls l

  let rec filter = parser
  (*Here we add {{, {client{, {server{ and {shared{ *)
  | [< '(KEYWORD "{", loc0); next >] ->
    (match next with parser
     | [< '(KEYWORD "{", loc1); nnext >] -> (* {{ *)
       [< '(KEYWORD "{{", merge_locs [loc0] loc1); filter nnext >]

     | [< '(LIDENT ("client"|"server"|"shared" as s), loc1); nnnext >] ->
       (match nnnext with parser
        | [< '(KEYWORD "{", loc2); nnnnext >] -> (* {smthg{ *)
          [< '(KEYWORD ("{"^s^"{"), merge_locs [loc0; loc1] loc2);
             filter nnnnext
          >]

        | [< 'other; nnnnext >] -> (* back *)
          [< '(KEYWORD "{", loc0); '(LIDENT s, loc1); 'other;
             filter nnnnext
          >]
       )

     | [< 'other; nnext >] -> (* back *)
       [< '(KEYWORD "{", loc0); 'other; filter nnext >]
    )

  (*Here we add }}*)
  | [< '(KEYWORD "}", loc0); next >] ->
    (match next with parser
     | [< '(KEYWORD "}", loc1); nnext >] ->
       [< '(KEYWORD "}}", merge_locs [loc0] loc1); filter nnext >]

     | [< 'other; nnext >] -> (* back *)
       [< '(KEYWORD "}", loc0); 'other; filter nnext >]
    )

  (*Here we add \ (we switch from symbol to keyword) *)
  | [< '(SYMBOL "!$",loc); next >] -> [< '(KEYWORD "\\", loc); filter next >]

  | [< 'other; next >] -> [< 'other; filter next >]

  let () =
    Token.Filter.define_filter
      (Gram.get_filter ())
      (fun old_filter stream -> old_filter (filter stream))

end
let () =
  let module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Make_lexer_filter) in
  ()



(*** PARSER ***)
(*Here we produce two seed files. Such files are not to be compiled, but
  processed again. *)

module Make_parser (Syntax : Camlp4.Sig.Camlp4Syntax) =
struct
  include Syntax

  (* This type allow basic level checking (correct nesting of client and server
     pieces of code). *)
  type parsing_level =
    | Base (* for Basic code *)
    | Without_wrapped (* for client code with no support for server values *)
    | With_wrapped (* for client code with support for server values *)
    | Wrapped (* for server values in client code *)

  exception Wrong_nesting_of_client_syntax

  let current_level = ref Base

  (*This is how we generate identifiers*)
  let gen_name =
    let r = ref 0 in
    (fun () ->
       incr r;
       "_eliom__client__server__this__is__a__reserved__name" ^ string_of_int !r
    )

  let gen_num_base _loc = Int64.of_int (Hashtbl.hash (Loc.file_name _loc))
  let gen_num_count = ref Int64.zero
  let gen_num _loc =
    gen_num_count := Int64.succ !gen_num_count;
    Int64.to_string (Int64.add (gen_num_base _loc) !gen_num_count)



  (* Extending syntax *)
  EXTEND Gram
  GLOBAL: str_item expr;

  (* dummies: for level check and level set *)
  dummy_check_level_base:
    [[ -> begin match !current_level with
         | Base -> ()
         | Wrapped | With_wrapped | Without_wrapped ->
             raise Wrong_nesting_of_client_syntax
       end
    ]];
  dummy_set_level_base: [[ -> current_level := Base ]];
  dummy_set_level_w_antiquotations:
    [[ -> begin match !current_level with
         | Base | Wrapped -> current_level := With_wrapped
         | With_wrapped | Without_wrapped ->
             raise Wrong_nesting_of_client_syntax
       end
    ]];
  dummy_set_level_wo_antiquotations:
    [[ -> begin match !current_level with
         | Base -> current_level := Without_wrapped
         | Wrapped | With_wrapped | Without_wrapped ->
             raise Wrong_nesting_of_client_syntax
       end
    ]];
  dummy_set_level_wrapped:
    [[ -> begin match !current_level with
         | With_wrapped -> current_level := Wrapped
         | Wrapped | Base | Without_wrapped ->
             raise Wrong_nesting_of_client_syntax
       end
    ]];

  (* To str_item we add {client{ SELF }}, {server{ SELF }} and
     {shared{ SELF }} *)
  str_item : BEFORE "top"
    [[ KEYWORD "{shared{" ; s = dummy_set_level_wo_antiquotations ;
         es = LIST0 SELF ;
       KEYWORD "}}" ; s = dummy_set_level_base ->
       begin match get_pass () with
         | Type_pass | Server_pass -> Ast.stSem_of_list es
         | Client_pass ->
           <:str_item<
             module Client__str__item__this__is__a__reserved__value =
             struct
               $Ast.stSem_of_list es$
             end
           >>
         | Raw_pass ->
           <:str_item<
             $Ast.stSem_of_list es$ ;;
             module Client__str__item__this__is__a__reserved__value =
             struct
               $Ast.stSem_of_list es$
             end ;;
           >>
       end

     | KEYWORD "{server{" ; s = dummy_check_level_base ;
         es = LIST0 SELF ;
       KEYWORD "}}" ; s = dummy_set_level_base ->
       begin match get_pass () with
         | Type_pass | Server_pass | Raw_pass -> Ast.stSem_of_list es
         | Client_pass -> <:str_item< >>
       end

     | KEYWORD "{client{" ; s = dummy_set_level_wo_antiquotations ;
         es = LIST0 SELF ;
       KEYWORD "}}" ; ss = dummy_set_level_base ->
       begin match get_pass () with
         | Type_pass | Server_pass -> <:str_item< >>
         | Client_pass | Raw_pass ->
           <:str_item<
             module Client__str__item__this__is__a__reserved__value =
             struct
               $Ast.stSem_of_list es$
             end
           >>
       end
    ]];

  (* To expr we add {{ SELF }} and \(SELF)
   * For both cases the SELF part is transformed so that filtering is easy.
   *)
  expr : BEFORE "simple"
    [[ KEYWORD "{{" ; s = dummy_set_level_w_antiquotations ;
         e = SELF ;
       KEYWORD "}}" ; s = dummy_set_level_base ->
       <:expr<
          Client__code__this__is__a__reserved__value
           ($int64:gen_num _loc$, $e$)
       >>

     | KEYWORD "\\" ; KEYWORD "(" ; s = dummy_set_level_wrapped ;
         e = SELF ; (*TODO? tweak LEVEL*)
       KEYWORD ")" ; s = dummy_set_level_w_antiquotations ->
       <:expr<
          (fun
            (Server__expr__this__is__a__reserved__value ($lid:gen_name ()$))
            -> $e$
          )
       >>

(*
     | KEYWORD "\\" ; i = IDENT (*TODO? tweak LEVEL*) ->
       <:expr<
          (fun
            (Server__expr__this__is__a__reserved__value ($lid:gen_name ()$))
            -> $lid:i$
          )
       >>
 *)
    ]];

  END

end
let () =
  let module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Make_parser) in
  ()



(*** COMMON ***)
(*Here we define functions that filters may use.*)

module Pre_make_filter (Filters : Camlp4.Sig.AstFilters) =
struct
(*TODO: extensible wrapper list *)
(*TODO: use options *)
  include Filters

  exception Next
  let wrappers _loc = [

    (let rec aux = function (*node*)
       | <:ctyp< Xhtml5types.$lid:_$ >> -> true

       | <:ctyp< [  $t$ ] >>
       | <:ctyp< [> $t$ ] >>
       | <:ctyp< [< $t$ ] >>
     (*| <:ctyp< [= $t$ ] >> is in the "doc" but doesn't compile*)
       | <:ctyp< _$t$ >> -> aux t

       | <:ctyp< [< $t1$ > $t2$ ] >> -> aux t1 && aux t2

       | _ -> false
     in
     function
       | <:ctyp< ($t$ XHTML5.M.elt) >> ->
           if aux t
           then (<:expr<Eliommod_cli.wrap_node>>,
                 <:expr<Eliommod_cli.unwrap_node>>)
           else raise Next
       | _ -> raise Next
    );

    (function (*channel*)
     | <:ctyp< ($_$ Eliom_comet.Channels.t) >> ->
         (<:expr<Eliom_comet.Channels.wrap>>,
          <:expr<Eliom_client_comet.Channels.unwrap>>)
     | _ -> raise Next
    );

    (function (*buffchan*)
     | <:ctyp< ($_$ Eliom_comet.Buffered_channels.t) >> ->
         (<:expr<Eliom_comet.Buffered_channels.wrap>>,
          <:expr<Eliom_client_comet.Buffered_channels.unwrap>>)
     | _ -> raise Next
    );

    (function (*up_event*)
     | <:ctyp< ($_$ Eliom_react.Up.t) >> ->
         (<:expr<Eliom_react.Up.wrap>>,
          <:expr<Eliom_client_react.Up.unwrap>>)
     | _ -> raise Next
    );

    (function (*down_event*)
     | <:ctyp< ($_$ Eliom_react.Down.t) >> ->
         (<:expr<Eliom_react.Down.wrap>>,
          <:expr<Eliom_client_react.Down.unwrap>>)
     | _ -> raise Next
    );

    (function (*bus*)
     | <:ctyp< ($_$ Eliom_bus.t) >> ->
         (<:expr<Eliom_bus.wrap>>,
          <:expr<Eliom_client_bus.unwrap>>)
     | _ -> raise Next
    );

    (function (*service*)
      | <:ctyp< ($_$,$_$,$_$,$_$,$_$,$_$,$_$,$_$) Eliom_services.service >>
      | <:ctyp< ($_$ Eliom_services.service) >> ->
          (<:expr<Eliom_services.wrap>>,
           <:expr<Eliommod_cli.unwrap>>)
     | _ -> raise Next
    );


    (* magic wrapper *)
    (let rec aux = function (*TODO: complete it*)
       | <:ctyp< float >> | <:ctyp< int >>
       | <:ctyp< string >> | <:ctyp< char >>
       | <:ctyp< bool >>
         -> true
       | <:ctyp< ($t$ list) >> -> aux t
       | _ -> false
     in
     fun t ->
       if aux t
       then (<:expr<Eliommod_cli.wrap>>, <:expr<Eliommod_cli.unwrap>>)
       else raise Next
    );
  ]

  (* Exception raised when an unknown wrapper-keyword is used. *)
  exception No_registered_wrapper_for of Ast.ctyp

  (* associating wrapper-keywords *)
  let find_wrapper_unwrapper _loc t =
    let rec aux = function
      | [] -> raise (No_registered_wrapper_for <:ctyp< $t$ >>)
      | f::fs ->
          try f t
          with Next -> aux fs
    in
    aux (wrappers _loc)

  let find_wrapper _loc n = fst (find_wrapper_unwrapper _loc n)
  let find_unwrapper _loc n = snd (find_wrapper_unwrapper _loc n)

  let patt_nested_tuple_of_patt_list _loc l =
    let rec aux acc = function
      | [] -> acc
      | p::ps -> aux <:patt< $acc$,$p$ >> ps
    in
    match l with
      | [] -> <:patt< () >>
      | p::ps -> aux p ps (*TODO: use a fold*)

  let expr_nested_tuple_of_expr_list _loc l =
    let rec aux acc = function
      | [] -> acc
      | p::ps -> aux <:expr< $acc$,$p$ >> ps
    in
    match l with
      | [] -> <:expr< () >>
      | p::ps -> aux p ps (*TODO: use a fold*)



  class virtual eliom_cli_serv_map =
  object (self)
    inherit (*Filters.*)Ast.map as super

    method virtual server_str_item :
         (*Filters.*)Ast.Loc.t
      -> (*Filters.*)Ast.str_item
      -> (*Filters.*)Ast.str_item
    method virtual client_str_item :
         (*Filters.*)Ast.Loc.t
      -> (*Filters.*)Ast.str_item
      -> (*Filters.*)Ast.str_item
    method virtual client_expr :
         (*Filters.*)Ast.Loc.t
      -> (*Filters.*)Ast.expr
      -> Int64.t
      -> (*Filters.*)Ast.expr
    method virtual server_escaped_expr :
         (*Filters.*)Ast.Loc.t
      -> (*Filters.*)Ast.expr
      -> (*Filters.*)Ast.ident
      -> (*Filters.*)Ast.expr

    method str_item s = match s with
      | <:str_item@_loc<
            module Client__str__item__this__is__a__reserved__value =
            struct
              $ss$
            end
        >> ->
          self#client_str_item _loc ss
      | Ast.StSem (_loc, s1, s2) ->
          let ss1 = self#str_item s1 in
          let ss2 = self#str_item s2 in
          Ast.StSem (_loc, ss1, ss2)
      | Ast.StMod (_loc, i, Ast.MeStr (_, m)) as s ->
          if i = "Client__str__item__this__is__a__reserved__value"
          then self#client_str_item _loc m
          else let ss = super#str_item s in
               self#server_str_item (Ast.loc_of_str_item ss) ss
      | Ast.StMod _
      | Ast.StNil _
      | Ast.StCls _
      | Ast.StClt _
      | Ast.StDir _
      | Ast.StExc _
      | Ast.StExp _
      | Ast.StExt _
      | Ast.StInc _
      | Ast.StRecMod _
      | Ast.StMty _
      | Ast.StOpn _
      | Ast.StTyp _
      | Ast.StVal _
      | Ast.StAnt _ as s ->
          let ss = super#str_item s in
          self#server_str_item (Ast.loc_of_str_item ss) ss

    method expr e = match super#expr e with
      | <:expr@_loc<
            Client__code__this__is__a__reserved__value ($int64:i$, $e$)
        >> ->
          let ee = self#expr e in
          self#client_expr _loc ee (Int64.of_string i)
      | <:expr@_loc<
            (fun (Server__expr__this__is__a__reserved__value ($id:n$))
               -> $e$

            )
        >> ->
          self#server_escaped_expr _loc e n
      | e -> e

  end
end



(*** MLI READER ***)
(*Here we define a set of functions for mli reading.*)

module Make_reader (Filters : Camlp4.Sig.AstFilters) =
struct

  (*TODO: get this module checked*)

  open Filters
  open Filters.Ast

  let load_file f =
    let ic = open_in f in
    try
      let s = Stream.of_channel ic in
      let res =
        Camlp4.Register.CurrentParser.parse_interf
          (Obj.magic (*Ast.*)Loc.ghost)
          s
      in
      close_in ic;
      (Obj.magic (res : Camlp4.PreCast.Ast.sig_item) : Filters.Ast.sig_item)
    with e -> close_in ic; raise e

  let rec string_of_ident = function
    | IdAcc (_,_,i) -> string_of_ident i
    | IdLid (_,s) | IdUid (_,s) | IdAnt (_,s) -> s
    | IdApp (_,i1,i2) ->
        failwith ("What is IdApp? "^string_of_ident i1^" "^ string_of_ident i2)

  let rec find_value_type n = function
    (*
    | <:sig_item< val $lid:s$ : $typ:t$ >>
        -> if string_of_ident n = s then Some t else None
    *)
    | SgSem (_, s1, s2) -> begin match find_value_type n s1 with
        | None -> find_value_type n s2
        | Some _ as topt -> topt
      end
    | SgVal (_, s, t)
        -> if string_of_ident n = s then Some t else None
    | SgExt _
    | SgNil _
    | SgCls _
    | SgClt _
    | SgExc _
    | SgMod _
    | SgMty _
    | SgTyp _
    (* no doc *)
    | SgAnt _ (* Antiquot? *)
    | SgOpn _ (* Open? *)
    | SgRecMod _ (* Rec Module? *)
    | SgDir _ (* ??? *)
    | SgInc _ (* Include? *)
      -> None

  let strip_option_ref = function (*TODO: actually check for option and ref *)
  (*| TyApp (_, _ (*ref*), TyApp (_, _ (*option*), t)) -> t*)
    | <:ctyp< ($t$ option ref) >> -> t
    | _ -> failwith "Unexpected type"

end

