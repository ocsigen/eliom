(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010-2011
 * Raphaël Proust, Grégoire Henry
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

(* Eliom's syntax extension implements five kinds of quotations:

    - a toplevel structure item "{client{ ... }}" for client side code ;
    - a toplevel structure item "{server{ ... }}" (optional) for server side code ;
    - a toplevel structure item "{shared{ ... }}" for code that will be used
      both for the server and the client ;
    - a expression "{{ ... }}" for client side code inside server side expressions ;
    - a escaped expression "%ident" for referencing server value from
      client side code expressions.


   == Compilation of Eliom source generates:

     - a .cmo (or a .cmx) to be loaded by the ocsigen server ;
     - a .js to be executed by the client.

     The {client{... }} sections are ignored on the server side.
     The {server{... }} sections are ignored on the client side.

     '{{ ... }}' are compiled on the client as a function
     parameterized by the values of escaped expressions. On the
     server-side, '{{ ... }}' are compiled as a distant call.  To keep
     the link, each '{{ ... }}' is associated unique integer (see
     gen_closure_num).

     In order to type-check escaped-value with the same type on both
     sides, compilation of Eliom sources infers the static type of
     escaped values on the server-side and adds static type constraint
     on the client-side. Inferred types also permits to specialize
     marshaling (on the server-side) and unmarshalling (on the
     client-side) of escaped values.

   == Compilation of Eliom is implemented in three steps:

     a) infers types of escaped values on the server-side code
     b) generate the source file for the server-side
     c) generate the source file for the client-side

     Each compilation step is based an a specific preprocessor:

     a) pa_eliom_type_inference
     b) pa_eliom_client_server
     c) pa_eliom_client_client

   This module define code shared by the three preprocessors.

*)

(** Helpers for pa_eliom_client_server and pa_eliom_client_client. *)

module type Helpers  = sig

  module Syntax : Camlp4.Sig.Camlp4Syntax
  open Syntax

  (** find infered type for escaped expr *)
  val find_client_value_type: Int64.t -> Ast.ctyp

  (** find infered type for escaped expr *)
  val find_escaped_ident_type: string -> Ast.ctyp

  (** find infered type for injected ident *)
  val find_injected_ident_type: string -> Ast.ctyp

  val is_client_value_type : Ast.ctyp -> Ast.ctyp option

  val raise_syntax_error : Ast.Loc.t -> string -> _

  val is_escaped_indent_string: string -> bool

  val patt_tuple : string list -> Ast.patt
  val expr_tuple : Ast.expr list -> Ast.expr
end

type client_value_context = [ `Server | `Shared ]
let client_value_context_to_string = function
  | `Server -> "server"
  | `Shared -> "shared"

type injection_context = [ `Client | `Shared ]
let injection_context_to_string = function
  | `Client -> "client"
  | `Shared -> "shared"

type escape_inject =
  | Escaped_in_client_value_in of client_value_context
  | Injected_in of injection_context

let id_of_string str =
  Printf.sprintf "%019d" (Hashtbl.hash str)

(** Signature of specific code of a preprocessor. *)

module type Pass = functor (Helpers: Helpers) -> sig

  open Helpers.Syntax

  (** How to handle "{shared{ ... }}" str_item. *)
  val shared_str_items: Ast.Loc.t -> Ast.str_item list -> Ast.str_item

  (** How to handle "{server{ ... }}" str_item and toplevel str_item. *)
  val server_str_items: Ast.Loc.t -> Ast.str_item list -> Ast.str_item

  (** How to handle "{client{ ... }}" str_item. *)
  val client_str_items: Ast.Loc.t -> Ast.str_item list -> Ast.str_item

  val shared_sig_items: Ast.Loc.t -> Ast.sig_item list -> Ast.sig_item
  val client_sig_items: Ast.Loc.t -> Ast.sig_item list -> Ast.sig_item
  val server_sig_items: Ast.Loc.t -> Ast.sig_item list -> Ast.sig_item

  (** How to handle "{{ ... }}" expr. *)
  val client_value_expr: Ast.ctyp option -> client_value_context -> Ast.expr -> Int64.t -> string -> Ast.Loc.t -> Ast.expr

  (** How to handle escaped "%ident" inside "{{ ... }}". *)
  val escape_inject: escape_inject -> Ast.expr -> string -> Ast.expr

  val implem : Ast.Loc.t -> Ast.str_item list -> Ast.str_item list

end

let fst_3 (x, _, _) = x
let snd_3 (_, x, _) = x
let trd_3 (_, _, x) = x

module Register(Id : sig val name: string end)(Pass : Pass) = struct

  module Make(Syntax : Camlp4.Sig.Camlp4Syntax) = struct

    include Syntax

    (* Syntax error exception *)
    module Syntax_error = struct
      type t = string
      exception E of t
      let print fmt msg =
        Format.fprintf fmt "Error: %s" msg
      let to_string msg =
        ignore(Format.flush_str_formatter ());
        print Format.str_formatter msg;
        Format.flush_str_formatter ()
      let raise _loc msg =
        Loc.raise _loc (E msg)
    end

    module Helpers = struct

      (* Anything easier than Camlp4? Create a parser for OCaml which
         shares the Token, AST, etc with those of the [Syntax]
         argument in the above functor [Make], but with an independent
         Grammar, because we want the to parse the .type_mli without
         the grammar modifications in made for the .eliomi files. *)
      module Syntax =
        Camlp4OCamlParser.Make
          (Camlp4OCamlRevisedParser.Make
             (Camlp4.OCamlInitSyntax.Make
                (Syntax.Ast)
                (Camlp4.Struct.Grammar.Static.Make
                   (Camlp4.Struct.Lexer.Make (Syntax.Token)))
                (Syntax.Quotation)))

      let raise_syntax_error _loc msg =
        Syntax_error.raise _loc msg

      (** MLI READER ***)

      (* Here we define a set of functions for mli reading. This is used
         to peek at the type infered by the first pass.*)

      let type_file = ref ""
      let _ =
        Camlp4.Options.add "-type" (Arg.Set_string type_file) "type inference file"

      let get_type_file () = match !type_file with
        | "" -> Filename.chop_extension !Camlp4_config.current_input_file
                ^ ".type_mli"
        | f -> f

      let suppress_underscore =
        let c = ref 0 in
        let uid () = incr c ; !c in
        fun ty ->
          let pfix = Printf.sprintf "__eliom_inferred_type_%d" (uid ()) in
           let map ty = match ty with
             | Ast.TyApp (_, Ast.TyAny _, ty)
             | Ast.TyApp (_, ty, Ast.TyAny _) -> ty
             | Ast.TyQuo (x, var) when var.[0] = '_' ->
               Ast.TyQuo (x, (String.sub var 1 (String.length var - 1)) ^ pfix)
             | ty -> ty in
           (Ast.map_ctyp map)#ctyp ty

      let escaped_ident_prefix = "__eliom__escaped_ident__reserved_name__"
      let escaped_ident_prefix_len = String.length escaped_ident_prefix
      let is_escaped_indent_string id =
        String.length id > escaped_ident_prefix_len &&
        String.sub id 0 escaped_ident_prefix_len = escaped_ident_prefix
      let is_escaped_ident = function
          (* | <:sig_item< val $id$ : $t$ >> -> *)
        | Ast.SgVal (_loc, id, t) ->
            is_escaped_indent_string id
        | si -> false

      let injected_ident_fmt () =
        format_of_string "__eliom__injected_ident__reserved_name__%019d__%d"
      let is_injected_ident = function
          (* | <:sig_item< val $id$ : $t$ >> -> *)
        | Ast.SgVal (_loc, id, t) ->
            (try
               Scanf.sscanf id (injected_ident_fmt ()) (fun _ _ -> true)
             with Scanf.Scan_failure _ ->
               false)
        | si -> false

      let client_value_ident_prefix = "__eliom__client_value__reserved_name__"
      let client_value_ident_prefix_len = String.length client_value_ident_prefix
      let is_client_value_ident = function
          (* | <:sig_item< val $id$ : $t$ >> -> *)
        | Ast.SgVal (_loc, id, t) ->
            String.length id > client_value_ident_prefix_len &&
            String.sub id 0 client_value_ident_prefix_len = client_value_ident_prefix
        | si -> false

      let is_client_value_type = function
        | <:ctyp< $typ$ Eliom_lib.client_value >>
        | <:ctyp< $typ$ Eliom_pervasives.client_value >> ->
            Some typ
        | _ -> None

      let extract_escaped_ident_type = function
          (* | <:sig_item< val $id$ : ($t$ option ref) >> -> *)
        | Ast.SgVal (_loc, id, <:ctyp< ($t$ option ref) >>) ->
            let len = String.length id - escaped_ident_prefix_len in
            int_of_string (String.sub id escaped_ident_prefix_len len),
            suppress_underscore t
        | _ -> failwith "extract_escaped_ident_type"
      let extract_injected_ident_type = function
          (* | <:sig_item< val $id$ : ($t$ option ref) >> -> *)
        | Ast.SgVal (_loc, id, <:ctyp< ($t$ option ref) >>) ->
            Scanf.sscanf id (injected_ident_fmt ()) (fun _filehash n -> n),
            suppress_underscore t
        | _ -> failwith "extract_injected_ident_type"
      let extract_client_value_type = function
          (* | <:sig_item< val $id$ : ($t$ option ref) >> -> *)
        | Ast.SgVal (_, id, <:ctyp< $typ$ option ref>>) ->
            (match is_client_value_type typ with
              | Some t ->
                let len = String.length id - client_value_ident_prefix_len in
                Int64.of_string (String.sub id client_value_ident_prefix_len len),
                suppress_underscore t
              | None ->
                  Printf.ksprintf failwith
                    "extract_client_value_type: Not a client value %S" id)
        | _ -> failwith "extract_client_value_type"

      let load_file f =
        try
          let ic = open_in f in
          let s = Stream.of_channel ic in
          let item = Syntax.parse_interf (Loc.mk f) s in
          let items = Ast.list_of_sig_item item [] in
          close_in ic;
          List.map extract_escaped_ident_type (List.filter is_escaped_ident items),
          List.map extract_injected_ident_type (List.filter is_injected_ident items),
          List.map extract_client_value_type (List.filter is_client_value_ident items)
        with
          | Sys_error _ ->
            Printf.eprintf "Error: File type not found (%s)\n" (get_type_file ());
            exit 1
          | Loc.Exc_located(loc,exn) ->
            Printf.eprintf "%s:\n Exception (%s)\n"
              (Loc.to_string loc) (Printexc.to_string exn);
            exit 1

      let infered_sig = lazy (load_file (get_type_file ()))

      let find_escaped_ident_type id =
        try
          let len = String.length id - escaped_ident_prefix_len in
          let id = int_of_string (String.sub id escaped_ident_prefix_len len) in
          List.assoc id (fst_3 (Lazy.force infered_sig))
        with Not_found ->
          Printf.eprintf "Error: Infered type of escaped ident not found (%s). \
                          You need to regenerate %s.\n"
            id (get_type_file ());
          exit 1

      let find_injected_ident_type id =
        try
          let id = Scanf.sscanf id (injected_ident_fmt ()) (fun _filehash n -> n) in
          List.assoc id (snd_3 (Lazy.force infered_sig))
        with Not_found ->
          Printf.eprintf "Error: Infered type of injected ident not found (%s). \
                          You need to regenerate %s.\n"
            id (get_type_file ());
          exit 1

      let find_client_value_type id =
        try
          List.assoc id (trd_3 (Lazy.force infered_sig))
        with Not_found ->
          Printf.eprintf "Error: Infered type client value not found (%s). \
                          You need to regenerate %s.\n"
            (Int64.to_string id) (get_type_file ());
          exit 1

      (* Convert a list of patterns to a tuple of pattern, one single pattern, or (). *)
      let patt_tuple =
        let _loc = Loc.ghost in
        let patt_of_id id =
          <:patt< $lid:id$ >>
        in function
        | [] -> <:patt< () >>
        | [id] -> patt_of_id id
        | ps -> <:patt< $tup:Ast.paCom_of_list (List.map patt_of_id ps)$ >>

      (* Convert a list of expressions to a tuple, one expression, or (). *)
      let expr_tuple =
        let _loc = Loc.ghost in function
        | [] -> <:expr< () >>
        | [e] -> e
        | es -> <:expr< $tup:Ast.exCom_of_list es$ >>

    end (* End of Helpers *)



    (** Extend LEXER ***)

    (* Add keywords: "{{", "{shared{", "{server{", "{client{" et "}}" *)

    let merge_locs l ls = List.fold_left Token.Loc.merge ls l

    open Camlp4.Sig (* for KEYWORD, LIDENT and SYMBOL *)

    let rec filter = parser
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

      | [< '(KEYWORD "}", loc0); next >] ->
          (match next with parser
          | [< '(KEYWORD "}", loc1); nnext >] ->
              [< '(KEYWORD "}}", merge_locs [loc0] loc1); filter nnext >]

          | [< 'other; nnext >] -> (* back *)
              [< '(KEYWORD "}", loc0); 'other; filter nnext >]
          )

      | [< 'other; next >] ->
          let is_left_delimitor str = List.mem str.[0] ['('; '['; '{'] in
          let ends_with_percent_sign str = str.[String.length str-1] = '%' in
          match other with
            | (* Allow %-sign to for injection directly after left delimitors *)
              SYMBOL str, loc0
              when String.length str > 0 &&
                   is_left_delimitor str &&
                   ends_with_percent_sign str ->
                let left = String.sub str 0 (String.length str - 1) in
                let loc_left = Loc.move `stop (-1) loc0 in
                let loc_right = Loc.move `start (String.length str - 1) loc0 in
                [< '(KEYWORD left, loc_left); '(SYMBOL "%", loc_right); filter next >]
            | _ -> [< 'other; filter next >]

    let () =
      Token.Filter.define_filter
        (Gram.get_filter ())
        (fun old_filter stream -> old_filter (filter stream))



    (** Extend Parser **)

    module Pass = Pass(Helpers)

    (* State of the parser: for checking syntax imbrication. *)
    type parsing_level =
      | Toplevel
      | Toplevel_module_expr
      | Server_item
      | Client_item
      | Shared_item
      | Module_expr
      | Hole_expr of client_value_context
      | Escaped_expr of client_value_context
      | Injected_expr of injection_context
    let level_to_string = function
      | Toplevel -> "toplevel"
      | Toplevel_module_expr -> "toplevel module expr"
      | Server_item -> "server section"
      | Client_item -> "client section"
      | Shared_item -> "shared section"
      | Module_expr -> "module expr"
      | Hole_expr client_value_context ->
          "client value expr in " ^ client_value_context_to_string client_value_context
      | Escaped_expr client_value_context ->
          "escaped expression in " ^ client_value_context_to_string client_value_context
      | Injected_expr injection_context ->
          "injected expression in " ^ injection_context_to_string injection_context
    (* [client_value_context] captures where [client_value_expr]s are allowed. *)
    let client_value_context = function
      | Server_item | Toplevel | Toplevel_module_expr -> `Server
      | Shared_item -> `Shared
      | Client_item | Hole_expr _ | Escaped_expr _ | Injected_expr _ | Module_expr as context ->
          failwith ("client_value_context: " ^ level_to_string context)
    let injection_context_to_parsing_level : injection_context -> parsing_level = function
      | `Client -> Client_item
      | `Shared -> Shared_item
    let current_level = ref Toplevel
    let set_current_level level =
      current_level := level

    (* Identifiers for the closure representing "Hole_expr". *)
    let gen_closure_num_base _loc = Int64.of_int (Hashtbl.hash (Loc.file_name _loc))
    let gen_closure_num_count = ref Int64.zero
    let gen_closure_num _loc =
      gen_closure_num_count := Int64.succ !gen_closure_num_count;
      Int64.add (gen_closure_num_base _loc) !gen_closure_num_count
    let gen_closure_escaped_ident id =
      Helpers.client_value_ident_prefix ^ Int64.to_string id

    (* Globaly unique ident for escaped expression *)
    (* It's used for type inference and as argument name for the
       closure representing the surrounding "Hole_expr". *)
    (* Inside a "Hole_expr", same ident share the global ident. *)
    let escaped_idents = ref []
    let reset_escaped_ident () = escaped_idents := []
    let gen_escaped_expr_ident, gen_escaped_ident =
      let r = ref 0 in
      (fun () ->
        incr r;
        Helpers.escaped_ident_prefix ^ string_of_int !r),
      (fun id ->
        let id = (Ast.map_loc (fun _ -> Loc.ghost))#ident id in
        try List.assoc id !escaped_idents
        with Not_found ->
          incr r; let gen_id = Helpers.escaped_ident_prefix ^ string_of_int !r in
          escaped_idents := (id, gen_id) :: !escaped_idents;
          gen_id)

    let gen_injected_expr_ident, gen_injected_ident =
      let injected_idents = ref [] in
      let r = ref 0 in
      let gen_ident loc =
        let hash = Hashtbl.hash (Loc.file_name loc) in
        incr r;
        Printf.sprintf (Helpers.injected_ident_fmt ()) hash !r
      in
      let gen_injected_ident loc id =
        let id = (Ast.map_loc (fun _ -> Loc.ghost))#ident id in
        try List.assoc id !injected_idents
        with Not_found ->
          let gen_id = gen_ident loc in
          injected_idents := (id, gen_id) :: !injected_idents;
          gen_id
      in
      gen_ident, gen_injected_ident


    (* BBB Before the syntax error was thrown in the productions dummy_set_*. This
       resulted in wrong error locations. The solution is to let the dummy productions
       return an option and raise the syntax error in the enclosing production. *)
    let from_some_or_raise opt loc f fmt =
      match opt with
        | Some x ->
            Printf.ksprintf (fun _ -> f x) fmt
        | None ->
            Printf.ksprintf (Syntax_error.raise loc) fmt

    module E2 = Camlp4.ErrorHandler.Register(Syntax_error)
    DELETE_RULE Gram expr: "{"; TRY [label_expr_list; "}"] END;
    DELETE_RULE Gram expr: "{"; TRY [expr LEVEL "."; "with"]; label_expr_list; "}" END;

    (* Extending syntax *)
    EXTEND Gram
    GLOBAL: str_item sig_item expr module_expr module_binding0 str_items sig_items implem interf;

    (* Dummy rules: for level management and checking. *)
      dummy_set_level_shared:
        [[ ->
             begin match !current_level with
              | Toplevel -> set_current_level Shared_item; Some ()
              | _ -> None
        end
         ]];
      dummy_set_level_server:
        [[ ->  match !current_level with
               | Toplevel -> set_current_level Server_item; Some ()
               | _ -> None
         ]];
      dummy_set_level_client:
        [[ ->
             match !current_level with
              | Toplevel -> set_current_level Client_item; Some ()
              | _ -> None
         ]];
      dummy_set_level_client_value_expr:
        [[ -> reset_escaped_ident ();
             match !current_level with
               | Toplevel | Toplevel_module_expr | Server_item | Shared_item as old ->
                   set_current_level (Hole_expr (client_value_context old));
                   Some old
               | Client_item | Hole_expr _ | Escaped_expr _ | Injected_expr _ | Module_expr ->
                   None
         ]];
      dummy_check_level_escaped_ident:
        [[ -> match !current_level with
              | Hole_expr context ->
                  Some (Escaped_in_client_value_in context)
              | Client_item ->
                  Some (Injected_in `Client)
              | Shared_item ->
                  Some (Injected_in `Shared)
              | _ -> None
         ]];
      dummy_set_level_escaped_expr:
        [[ -> match !current_level with
              | Hole_expr context ->
                  set_current_level (Escaped_expr context);
                  Some (Escaped_in_client_value_in context)
              | Client_item ->
                  set_current_level (Injected_expr `Client);
                  Some (Injected_in `Client)
              | Shared_item ->
                  set_current_level (Injected_expr `Shared);
                  Some (Injected_in `Shared)
              | _ -> None
         ]];
      dummy_set_level_module_expr:
        [[ -> match !current_level with
              | Toplevel ->
                 set_current_level Toplevel_module_expr;
                 Toplevel
              | lvl -> lvl ]];

      str_items: FIRST
        [[ lvl = dummy_set_level_module_expr;
           me = SELF -> set_current_level lvl; me ]];

      sig_items: FIRST
        [[ lvl = dummy_set_level_module_expr; me = SELF ->
           set_current_level lvl; me ]];

      (* Duplicated from camlp4/Camlp4Parsers/Camlp4OCamlRevisedParser.ml *)
      module_expr: BEFORE "top"
        [[ "functor"; "("; i = a_UIDENT; ":"; t = module_type; ")"; "->";
            lvl = dummy_set_level_module_expr;
            me = SELF ->
            set_current_level lvl; <:module_expr< functor ( $i$ : $t$ ) -> $me$ >> ]];

      (* Duplicated from camlp4/Camlp4Parsers/Camlp4OCamlRevisedParser.ml *)
      module_binding0: FIRST
      [ RIGHTA
        [ "("; m = a_UIDENT; ":"; mt = module_type; ")";
          lvl = dummy_set_level_module_expr; mb = SELF ->
            set_current_level lvl; <:module_expr< functor ( $m$ : $mt$ ) -> $mb$ >> ]];

      sig_item: BEFORE "top"
        [ "eliom"
         [ KEYWORD "{shared{" ; opt = dummy_set_level_shared ; es = LIST0 sig_item ; KEYWORD "}}" ->
           from_some_or_raise opt _loc
             (fun () ->
               set_current_level Toplevel;
               Pass.shared_sig_items _loc es)
             "The syntax {shared{ ... }} is only allowed at toplevel"
         | KEYWORD "{server{" ; opt = dummy_set_level_server ; es = LIST0 sig_item ; KEYWORD "}}" ->
             from_some_or_raise opt _loc
               (fun () ->
                  set_current_level Toplevel;
                  Pass.server_sig_items _loc es)
               "The syntax {server{ ... }} is only allowed at toplevel"
         | KEYWORD "{client{" ; opt = dummy_set_level_client ; es = LIST0 sig_item ; KEYWORD "}}" ->
             from_some_or_raise opt _loc
               (fun () ->
                  set_current_level Toplevel;
                  Pass.client_sig_items _loc es)
               "The syntax {client{ ... }} is only allowed at toplevel"
         | si = sig_item LEVEL "top" ->
             if !current_level = Toplevel then
               Pass.server_sig_items _loc [si]
             else
               si
         ]];


      (* To str_item we add {client{ ... }}, {server{ ... }} and {shared{ ... }} *)
      str_item: BEFORE "top"

        [ "eliom"

         [ KEYWORD "{shared{" ; opt = dummy_set_level_shared ; es = LIST0 str_item ; KEYWORD "}}" ->
             from_some_or_raise opt _loc
               (fun () ->
                  set_current_level Toplevel;
                  Pass.shared_str_items _loc es)
               "The syntax {shared{ ... }} is only allowed at toplevel"

         | KEYWORD "{server{" ; opt = dummy_set_level_server ; es = LIST0 str_item ; KEYWORD "}}" ->
             from_some_or_raise opt _loc
               (fun () ->
                  set_current_level Toplevel;
                  Pass.server_str_items _loc es)
               "The syntax {server{ ... }} is only allowed at toplevel"

         | KEYWORD "{client{" ; opt = dummy_set_level_client ; es = LIST0 str_item ; KEYWORD "}}" ->
             from_some_or_raise opt _loc
               (fun () ->
                  set_current_level Toplevel;
                  Pass.client_str_items _loc es)
               "The syntax {client{ ... }} is only allowed at toplevel"

         | si = str_item LEVEL "top" ->

             if !current_level = Toplevel then
               Pass.server_str_items _loc [si]
             else
               si

         ]];

      (* To expr we add {{ ... }} and %IDENT *)

      expr: LEVEL "simple"

        [ [ KEYWORD "{"; lel = TRY [lel = label_expr_list; "}" -> lel] ->
              <:expr< { $lel$ } >>
          | KEYWORD "{"; typ = TRY [ typ = OPT ctyp; KEYWORD "{" -> typ]; opt_lvl = dummy_set_level_client_value_expr ; e = expr; KEYWORD "}}" ->
              from_some_or_raise opt_lvl _loc
                (fun lvl ->
                   set_current_level lvl;
                   let id = gen_closure_num _loc in
                   Pass.client_value_expr typ (client_value_context lvl) e
                     id (gen_closure_escaped_ident id) _loc)
                "The syntax {type{ ... } is not allowed in %s."
                (level_to_string !current_level)
          | KEYWORD "{"; e = TRY [e = expr LEVEL "."; "with" -> e]; lel = label_expr_list; "}" ->
              <:expr< { ($e$) with $lel$ } >>
          | KEYWORD "{{"; opt_lvl = dummy_set_level_client_value_expr ; e = expr; KEYWORD "}}" ->
              from_some_or_raise opt_lvl _loc
                (fun lvl ->
                   set_current_level lvl;
                   let id = gen_closure_num _loc in
                   Pass.client_value_expr None (client_value_context lvl) e
                     id (gen_closure_escaped_ident id) _loc)
                "The syntax {{ ... }} is not allowed in %s."
                (level_to_string !current_level)
           ] ];

      expr: BEFORE "simple"

        [ [ SYMBOL "%" ; id = ident ; opt_context = dummy_check_level_escaped_ident ->
              from_some_or_raise opt_context _loc
                (fun context ->
                     let gen_id =
                       match context with
                         | Escaped_in_client_value_in _ ->
                             gen_escaped_ident id
                         | Injected_in _ ->
                             gen_injected_ident _loc id
                     in
                     Pass.escape_inject context <:expr< $id:id$ >> gen_id)
                "The syntax \"%%ident\" is not allowed in %s."
                (level_to_string !current_level)

          | SYMBOL "%" ; KEYWORD "(" ; opt_context = dummy_set_level_escaped_expr ; e = SELF ; KEYWORD ")" ->
              from_some_or_raise opt_context _loc
                (fun context ->
                   set_current_level
                     (match context with
                        | Escaped_in_client_value_in context -> Hole_expr context
                        | Injected_in context -> injection_context_to_parsing_level context);
                   let gen_id =
                     match context with
                       | Escaped_in_client_value_in _ ->
                           gen_escaped_expr_ident ()
                       | Injected_in _ ->
                           gen_injected_expr_ident _loc
                   in
                   Pass.escape_inject context e gen_id)
                "The syntax \"%%(...)\" is not allowed in %s."
                (level_to_string !current_level)
         ]];

      (* Cf. Camlp4OCamlRevisedParser *)
      implem:
        [[ si = str_item; semi; (sil, stopped) = SELF ->
             let open_pervasives =
               let _loc = Loc.ghost in
               <:str_item< open Eliom_pervasives >>
             in
             (open_pervasives :: Pass.implem _loc (si :: sil), stopped)
         | `EOI -> ([], None)
        ]];

      interf:
        [[ si = sig_item; semi; (sil, stopped) = SELF ->
           let open_pervasives =
             let _loc = Loc.ghost in
             <:sig_item< open Eliom_pervasives >>
           in
           (open_pervasives :: si :: sil, stopped)
         | `EOI -> ([], None) ]];


      END

  end

  (** Register syntax extension *)

  module Id : Camlp4.Sig.Id = struct
    let name = "Eliom source file syntax ("^ Id.name ^")"
    let version = "3.0+alpha"
  end

  module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Make)

end


module Make(Syntax : Camlp4.Sig.Camlp4Syntax) = struct

  include Syntax

  (* Extending syntax *)
  EXTEND Gram
  GLOBAL: implem interf;

  implem: FIRST
    [[ (sil, stopped) = implem LEVEL "top" ->
      ( <:str_item< open Eliom_pervasives >>:: sil , stopped) ]
  | "top" [] ];

  interf: FIRST
    [[ (sil, stopped) = interf LEVEL "top" ->
      ( <:sig_item< open Eliom_pervasives >> :: sil , stopped) ]
  | "top" [] ];

  END
end

module Id : Camlp4.Sig.Id = struct
  let name = "Eliom source file syntax (common)"
  let version = "3.0"
end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Make)
