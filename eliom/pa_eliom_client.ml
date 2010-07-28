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


(* It does not seem reasonable to write some camlp4 w/out opening that *)
open Camlp4
open Camlp4.PreCast

let simpl_split c s =
  try
    let p = String.index s c in
    (String.sub s 0 p, String.sub s (p + 1) (String.length s - p - 1))
  with Not_found ->
    (s, "")

(*** Options for parser ***)
(* There are only to options for now :
 * -client <filename>     to indicate the file in which the client code should
 *                        be printed.
 * -pretty                to set pretty printing of client code (instead of AST
 *                        dump). This is provided for syntax extension debugging
 * *)

(* The file the client side is sent to *)
let client_file = ref "client_file.ml"
let _ = Camlp4.Options.add
          "-client"
          (Arg.Set_string client_file)
          "set client code output file name"

(* option for pretty printing of client side. The default behavior is to dump
 * the client code AST. *)
let pretty = ref false
let _ = Camlp4.Options.add
          "-pretty"
          (Arg.Set pretty)
          "pretty print client code instead of dumping it"



(*** Syntax Identity module ***)
module Id : Camlp4.Sig.Id = struct

  let name = "Eliom/Client-Server Symmetric Syntax"
  let version = "beta"

end



(*** Core ***)
module Make (Syntax : Camlp4.Sig.Camlp4Syntax) = struct
  (* Syntax inclusion seems necessary. Although a would be doc might explain
   * why, none is available. *)
  include Syntax


  (*TODO: extensible wrapper list *)
  (* This is the list associating wrapper-keywords to actual wrappers. *)
  let wrappers _loc = [
    ("magic",
     (fun _ -> (<:expr<Eliommod_cli.wrap ~sp>>, <:expr<Eliommod_cli.unwrap>>)));
    ("sp",
     (fun _ -> (<:expr<Eliommod_cli.wrap_sp>>, <:expr<Eliommod_cli.unwrap_sp>>)));
    ("node",
     (fun _ -> (<:expr<Eliommod_cli.wrap_node ~sp>>, <:expr<Eliommod_cli.unwrap_node>>)));
    ("channel",
     (fun _ -> (<:expr<Eliom_comet.Channels.wrap ~sp>>, <:expr<Eliom_client_comet.Channels.unwrap>>)));
    ("buffchan",
     (fun _ -> (<:expr<Eliom_comet.Dlisted_channels.wrap ~sp>>, <:expr<Eliom_client_comet.Dlisted_channels.unwrap>>)));
    ("up_event",
     (fun _ -> (<:expr<Eliom_event.Up.wrap ~sp>>, <:expr<Eliom_client_event.Up.unwrap ~sp>>)));
    ("down_event",
     (fun _ -> (<:expr<Eliom_event.Down.wrap ~sp>>, <:expr<Eliom_client_event.Down.unwrap>>)));
  ]
  (* Exception raised when an unknown wrapper-keyword is used. *)
  exception Not_a_registered_wrapper of string

  (* associating wrapper-keywords *)
  let find_wrapper _loc = function
    | ("", "") -> (List.assoc "magic" (wrappers _loc)) ""
    | (w, o) ->
        try (List.assoc w (wrappers _loc)) o
        with Not_found -> raise (Not_a_registered_wrapper w)



  (* Random int64 and name generation. Collisions are possible but unlikely. *)
  let rnd = Random.State.make [|0x513511d4|]
  let random_int64 () = Random.State.int64 rnd 0x100000000L
  let random_arg_name () =
    Format.sprintf "a%08Lx" (random_int64 ())


  (* Client printers *)
  module Client_code_dumper = Camlp4.Printers.DumpOCamlAst.Make(Syntax)
  module Client_code_printer = Camlp4.Printers.OCaml.Make(Syntax)

  (* A reference to an empty str_item *)
  let client_dump =
    ref (let _loc = Loc.ghost in <:str_item< (* generated file *) >>)
  (* The reference is updated for each piece of client code. *)
  let emit_client _loc ss =
    List.iter
      (fun s -> client_dump := <:str_item< $!client_dump$ ;; $s$ ;; >>)
      ss
  (* The client file is written once *)
  let _ =
    at_exit
      (fun () ->
         (if !pretty (* Depending on !pretty, a different printer is used *)
          then Client_code_printer.print_implem
          else Client_code_dumper.print_implem
         )
           ~output_file:(!client_file)
           !client_dump
      )




  (* Client side code emission. *)
  let register_closure _loc num args expr =
    let rec clo_args_aux acc = function
      | [] -> <:patt< $acc$ >>
      | n::tl ->
         clo_args_aux <:patt< $acc$,$lid:n$ >> tl
    in
    let clo_arg = match args with
      | [] -> <:patt< () >>
      | n::ns -> clo_args_aux <:patt< $lid:n$ >> ns
    in
    <:str_item<
      let _ = Eliommod_cli.register_closure
                $`int:Int64.to_int num$
                (fun $clo_arg$ -> $expr$)
    >>



  (* Server side code emission *)
  let closure_call _loc num args =
    let rec expr_of_args_aux acc = function
      | [] -> (assert false)
      | [e] -> <:expr< $acc$,$e$ >>
      | e::tl ->
         expr_of_args_aux <:expr< $acc$,$e$ >> tl
    in
    let expr_of_args = match args with
      | [] ->  (* placing 'unit' when no arguments are used *)
          <:expr< () >>
      | [e] -> e
      | e1::(_::_ as l) -> expr_of_args_aux <:expr< $e1$ >> l
    in
    <:expr<
         $str:"caml_run_from_table(" ^ string_of_int (Int64.to_int num) ^ ", \'"$
       ^ Eliom_client_types.jsmarshal $expr_of_args$ ^ "\')"
    >>


  (* WITH ANTIQUOTATIONS *)
  (* This is were the magic appears !*)
  let parse_wrapped_expr _loc s = Syntax.AntiquotSyntax.parse_expr _loc s

  type parsing_level =
    | Base
    | Without_wrapped
    | With_wrapped
    | Wrapped

  exception Wrong_nesting_of_client_syntax

  class client_server_ast_map = (* w/ antiquotations *)
  object (self)
    inherit Ast.map as super

    val mutable level = Base
    method set_level l = level <- l
    method get_level = level

    (* Here we accumulate antiquotations *)
    val mutable antis_client = []
    val mutable antis_server = []
    method push_antis x y = antis_client <- x :: antis_client ;
                            antis_server <- y :: antis_server
    method flush_antis =
      let res = (antis_client, antis_server) in
      antis_client <- []; antis_server <- [];
      res

    (* Here we change the expr behavior *)
    method expr e = (* match self#get_level with
      | Base | Without_wrapped | Wrapped -> begin match super#expr e with
        (*| <:expr@_loc< $anti:s$ >> -> raise Wrong_nesting_of_client_syntax*)
          | e -> e
          end
      | With_wrapped ->*) begin match super#expr e with
         | <:expr@_loc< $anti:r$ >> ->
           begin
           (*print_endline ("ANTIQUOT:" ^ r);*)
           self#set_level Wrapped;

           if String.sub r 2 6 = "expr: "
           then
             let s = String.sub r 8 (String.length r - 8) in
             let colon = String.index s ':' in
             let wrapper_name = String.sub s 0 colon in
             let wrapped_expr =
               String.sub s (colon + 1) (String.length s - colon - 1)
             in
             let clos_arg_name = random_arg_name () in
             let (wrapper, unwrapper) =
               find_wrapper _loc (simpl_split ' ' wrapper_name)
             in
             let anti_expr =
               <:expr< ($wrapper$ $parse_wrapped_expr _loc wrapped_expr$) >>
             in
             self#set_level With_wrapped;
             self#push_antis clos_arg_name anti_expr;
             <:expr< ($unwrapper$ $lid:clos_arg_name$) >>
           else failwith "Unknown antiquotation"
           end
         | e -> e
         end

  end

  let mapper = new client_server_ast_map


  (* Extending syntax *)

  EXTEND Gram
    GLOBAL: str_item expr;

    (* dummies: for level check and level set *)
    dummy_check_level_base:
      [[ -> begin match mapper#get_level with
           | Base -> ()
           | Wrapped | With_wrapped | Without_wrapped ->
               raise Wrong_nesting_of_client_syntax
         end
      ]];
    dummy_set_level_base: [[ -> mapper#set_level Base ]];
    dummy_set_level_w_antiquotations:
      [[ -> begin match mapper#get_level with
           | Base -> mapper#set_level With_wrapped
           | Wrapped | With_wrapped | Without_wrapped ->
               raise Wrong_nesting_of_client_syntax
         end
      ]];
    dummy_set_level_wo_antiquotations:
      [[ -> begin match mapper#get_level with
           | Base -> mapper#set_level Without_wrapped
           | Wrapped | With_wrapped | Without_wrapped ->
               raise Wrong_nesting_of_client_syntax
         end
      ]];

    (* To str_item we add
     * * {client{ LIST0 SELF }}
     * * {server{ LIST0 SELF }}
     * * {shared{ LIST0 SELF }}
     *)
    str_item : BEFORE "top"
      [[ "{" ; "shared" ; "{" ; s = dummy_set_level_wo_antiquotations ;
           e = LIST0 SELF ;
         "}" ; "}" ; s = dummy_set_level_base ->
           (emit_client _loc e ;
            List.fold_left
              (fun x y -> <:str_item< $x$ ;; $y$ ;; >>)
              <:str_item< >>
              e
           )
       | "{" ; "server" ; "{" ; s = dummy_check_level_base ;
           e = LIST0 SELF ;
         "}" ; "}" ; s = dummy_set_level_base ->
           (List.fold_left
              (fun x y -> <:str_item< $x$ ;; $y$ ;; >>)
              <:str_item< >>
              e
           )
       | "{" ; "client" ; "{" ; s = dummy_set_level_w_antiquotations ;
           e = LIST0 SELF ;
         "}" ; "}" ; ss = dummy_set_level_base ->
           (emit_client _loc e ;
            <:str_item< >>
           )
      ]];

    (* To expr we add
     * * {{ SELF }} for which the SELF part is mapped
     *)
    expr : BEFORE "simple"
      [[ KEYWORD "{" ; KEYWORD "{" ; s = dummy_set_level_w_antiquotations ;
           e = SELF ;
         KEYWORD "}" ; KEYWORD "}" ; s = dummy_set_level_base ->
           (let num = random_int64 () in
            let mapped_e = mapper#expr e in
            let (client_args, server_args) = mapper#flush_antis in
            emit_client _loc [register_closure _loc num client_args mapped_e] ;
            mapper#set_level Base;
            <:expr< $closure_call _loc num server_args$ >>
           )
      ]];

  END


end

(* Registration side effect *)
let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
