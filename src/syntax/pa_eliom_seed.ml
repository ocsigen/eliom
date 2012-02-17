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

   == Compilation of Eliom is implemented in three step:

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
  val find_event_handler_type: Int64.t -> Ast.ctyp

  (** find infered type for escaped expr *)
  val find_escaped_ident_type: string -> Ast.ctyp

end

(** Signature of specific code of a preprocessor. *)

module type Pass = functor (Helpers: Helpers) -> sig

  open Helpers.Syntax

  (** How to handle "{shared{ ... }}" str_item. *)
  val shared_str_items: Ast.str_item list -> Ast.str_item

  (** How to handle "{server{ ... }}" str_item and toplevel str_item. *)
  val server_str_items: Ast.str_item list -> Ast.str_item

  (** How to handle "{client{ ... }}" str_item. *)
  val client_str_items: Ast.str_item list -> Ast.str_item

  (** How to handle "{{ ... }}" expr. *)
  val client_expr: Ast.expr -> Int64.t -> string -> Ast.expr

  (** How to handle escaped "%ident" inside "{{ ... }}". *)
  val escaped: Ast.expr -> string -> Ast.expr

end

module Register(Id : sig val name: string end)(Pass : Pass) = struct

  module Make(Syntax : Camlp4.Sig.Camlp4Syntax) = struct

    include Syntax

    module Helpers = struct

      module Syntax = Syntax

      (** Pretty-print for error-message *)

      let printer =
	let module Printer = Camlp4.Printers.OCaml.Make(Syntax) in
	new Printer.printer ()

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

      let escaped_ident_prefix = "__eliom__escaped_expr__reserved_name__"
      let escaped_ident_prefix_len = String.length escaped_ident_prefix
      let is_escaped_ident = function
	  (* | <:sig_item< val $id$ : $t$ >> -> *)
	| Ast.SgVal (_loc, id, t) ->
	    String.length id > escaped_ident_prefix_len &&
	    String.sub id 0 escaped_ident_prefix_len = escaped_ident_prefix
	| si -> false
      let event_handler_ident_prefix = "__eliom__event_handler__reserved_name__"
      let event_handler_ident_prefix_len = String.length event_handler_ident_prefix
      let is_event_handler_ident = function
	  (* | <:sig_item< val $id$ : $t$ >> -> *)
	| Ast.SgVal (_loc, id, t) ->
	    String.length id > event_handler_ident_prefix_len &&
	    String.sub id 0 event_handler_ident_prefix_len = event_handler_ident_prefix
	| si -> false
      let extract_type = function
	  (* | <:sig_item< val $id$ : ($t$ option ref) >> -> *)
	| Ast.SgVal (_loc, id, <:ctyp< ($t$ option ref) >>) ->
	    let len = String.length id - escaped_ident_prefix_len in
	    int_of_string (String.sub id escaped_ident_prefix_len len),
	    suppress_underscore t
	| _ -> assert false
      let extract_event_handler_type = function
	  (* | <:sig_item< val $id$ : ($t$ option ref) >> -> *)
	| Ast.SgVal (_loc, id, <:ctyp< ($t$ Eliom_pervasives.XML.caml_event_handler option ref) >>) ->
	    let len = String.length id - event_handler_ident_prefix_len in
	    Int64.of_string (String.sub id event_handler_ident_prefix_len len),
	    suppress_underscore t
	| _ -> assert false

      let load_file f =
	let ic = open_in f in
	try
	  let s = Stream.of_channel ic in
	  let (items, stopped) = Gram.parse interf (Loc.mk f) s in
	  assert (stopped = None); (* No directive inside the generated ".mli". *)
	  close_in ic;
	  List.map extract_type (List.filter is_escaped_ident items),
	  List.map extract_event_handler_type (List.filter is_event_handler_ident items)
	with e ->
	  Printf.eprintf "Error: File type not found (%s)\n" (get_type_file ());
	  close_in ic;
	  exit 1

      let infered_sig = lazy (load_file (get_type_file ()))

      let find_escaped_ident_type id =
	try
	  let len = String.length id - escaped_ident_prefix_len in
	  let id = int_of_string (String.sub id escaped_ident_prefix_len len) in
	  List.assoc id (fst (Lazy.force infered_sig))
	with Not_found ->
	  Printf.eprintf "Error: Infered type not found (%s).\nYou need to regenerate %s.\n" id (get_type_file ());
	  exit 1

      let find_event_handler_type id =
	try
	  List.assoc id (snd (Lazy.force infered_sig))
	with Not_found ->
	  Printf.eprintf "Error: Infered type not found (%s).\nYou need to regenerate %s.\n" (Int64.to_string id) (get_type_file ());
	  exit 1
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

      | [< 'other; next >] -> [< 'other; filter next >]

    let () =
      Token.Filter.define_filter
	(Gram.get_filter ())
	(fun old_filter stream -> old_filter (filter stream))



    (** Extend Parser **)

    module Pass = Pass(Helpers)

    (* State of the parser: for checking syntax imbrication. *)
    type parsing_level =
      | Toplevel
      | Server_item
      | Client_item
      | Shared_item
      | Module_expr
      | Client_expr
      | Escaped_expr
    let current_level = ref Toplevel

    (* Identifiers for the closure representing "Client_expr". *)
    let gen_closure_num_base _loc = Int64.of_int (Hashtbl.hash (Loc.file_name _loc))
    let gen_closure_num_count = ref Int64.zero
    let gen_closure_num _loc =
      gen_closure_num_count := Int64.succ !gen_closure_num_count;
      Int64.add (gen_closure_num_base _loc) !gen_closure_num_count
    let gen_closure_escaped_ident id =
      Helpers.event_handler_ident_prefix ^ Int64.to_string id

    (* Globaly unique ident for escaped expression *)
    (* It's used for type inference and as argument name for the
       closure representing the surrounding "Client_expr". *)
    (* Inside a "Client_expr", same ident share the global ident. *)
    let escaped_idents = ref []
    let reset_escaped_ident () = escaped_idents := []
    let gen_escaped_ident =
      let r = ref 0 in
      (fun id ->
	let id = (Ast.map_loc (fun _ -> Loc.ghost))#ident id in
	try List.assoc id !escaped_idents
	with Not_found ->
	  incr r; let gen_id = Helpers.escaped_ident_prefix ^ string_of_int !r in
	  escaped_idents := (id, gen_id) :: !escaped_idents;
	  gen_id)

    (* Syntax error exception *)
    module Syntax_error = struct
      type t = string
      exception E of t
      let print fmt msg =
	Format.fprintf fmt "Syntax error: %s" msg
      let to_string msg =
	ignore(Format.flush_str_formatter ());
	print Format.str_formatter msg;
	Format.flush_str_formatter ()
      let raise _loc msg =
	Loc.raise _loc (E msg)
    end
    module E2 = Camlp4.ErrorHandler.Register(Syntax_error)

    (* Extending syntax *)
    EXTEND Gram
    GLOBAL: str_item expr module_expr module_binding0;

    (* Dummy rules: for level management and checking. *)
      dummy_set_level_shared:
	[[ -> begin match !current_level with
              | Toplevel -> current_level := Shared_item
              | _ ->
		  Syntax_error.raise _loc
		    "The syntax {shared{ ... }} is only allowed at toplevel"
	end
	 ]];
      dummy_set_level_server:
	[[ ->  match !current_level with
               | Toplevel -> current_level := Server_item
	       | _ ->
		   Syntax_error.raise _loc
		     "The syntax {server{ ... }} is only allowed at toplevel"
	 ]];
      dummy_set_level_client:
	[[ -> match !current_level with
              | Toplevel -> current_level := Client_item
              | _ ->
		  Syntax_error.raise _loc
		    "The syntax {client{ ... }} is only allowed at toplevel"
	 ]];
      dummy_set_level_client_expr:
	[[ -> match !current_level with
              | Server_item | Toplevel ->
		  reset_escaped_ident ();
		  let old = !current_level in
		  current_level := Client_expr;
		  old
              | _ ->
		  Syntax_error.raise _loc
		    "The syntax {{ ... }} is only allowed inside server specific code"
	 ]];
      dummy_check_level_escaped_ident:
	[[ -> match !current_level with
              | Client_expr -> ()
              | _ ->
		  Syntax_error.raise _loc
		    "The syntax \"%ident\" is only allowed inside code expression {{ ... }}."
	 ]];
      dummy_set_level_escaped_expr:
	[[ -> match !current_level with
              | Server_item | Toplevel -> current_level := Client_expr
              | _ ->
		  Syntax_error.raise _loc
		    "The syntax {{ ... }} is only allowed inside server specific code"
	 ]];
      dummy_set_level_module_expr:
	[[ -> match !current_level with
              | Client_expr -> Client_expr
              | old ->
                 current_level := Module_expr;
                 old ]];

      (* str_items: FIRST *)
	(* [[ lvl = dummy_set_level_module_expr; *)
	   (* me = SELF -> current_level := lvl; me ]]; *)

      (* Duplicated from camlp4/Camlp4Parsers/Camlp4OCamlRevisedParser.ml *)
      module_expr: BEFORE "top"
	[[ "functor"; "("; i = a_UIDENT; ":"; t = module_type; ")"; "->";
            lvl = dummy_set_level_module_expr;
            me = SELF ->
            current_level := lvl; <:module_expr< functor ( $i$ : $t$ ) -> $me$ >> ]];

      (* Duplicated from camlp4/Camlp4Parsers/Camlp4OCamlRevisedParser.ml *)
      module_binding0: FIRST
      [ RIGHTA
        [ "("; m = a_UIDENT; ":"; mt = module_type; ")";
          lvl = dummy_set_level_module_expr; mb = SELF ->
            current_level := lvl; <:module_expr< functor ( $m$ : $mt$ ) -> $mb$ >> ]];


      (* To str_item we add {client{ ... }}, {server{ ... }} and {shared{ ... }} *)
      str_item: BEFORE "top"

	[ "eliom"

	 [ KEYWORD "{shared{" ; dummy_set_level_shared ;
           es = LIST0 str_item ;
	   KEYWORD "}}" ->

	     current_level := Toplevel;
	     Pass.shared_str_items es

         | KEYWORD "{server{" ; dummy_set_level_server ;
             es = LIST0 str_item ;
	   KEYWORD "}}" ->

	     current_level := Toplevel;
	     Pass.server_str_items es

	 | KEYWORD "{client{" ; dummy_set_level_client ;
             es = LIST0 str_item ;
	   KEYWORD "}}" ->

	     current_level := Toplevel;
	     Pass.client_str_items es

	 | si = str_item LEVEL "top" ->

	     if !current_level = Toplevel then
	       Pass.server_str_items [si]
	     else
	       si

	 ]];

      (* To expr we add {{ ... }} and %IDENT *)

      expr: BEFORE "simple"

	[[ KEYWORD "{{" ; lvl = dummy_set_level_client_expr ;
           e = SELF ;
	   KEYWORD "}}" ->

	     current_level := lvl;
	     let id = gen_closure_num _loc in
	     Pass.client_expr e id (gen_closure_escaped_ident id)

      | SYMBOL "%" ; id = ident ; dummy_check_level_escaped_ident ->

	  Pass.escaped <:expr< $id:id$ >> (gen_escaped_ident id)

      | SYMBOL "%" ; KEYWORD "(" ; dummy_set_level_escaped_expr ;
	  e = SELF ;
	  KEYWORD ")" ->

	    current_level := Client_expr;
	    failwith "No %(expr) yet !"

	 ]];

      END

  end

  (** Register syntax extension *)

  module Id : Camlp4.Sig.Id = struct
    let name = "Eliom source file syntax ("^ Id.name ^")"
    let version = "alpha"
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
  let version = "alpha"
end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Make)

