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

(* This prepocessor generates the code to be executed by the navigator. *)

module Id = struct
  let name = "client part"
end

module Client_pass(Helpers : Pa_eliom_seed.Helpers) = struct

  open Helpers.Syntax

  let notyp = ref false
  let _ =
    Camlp4.Options.add "-notype" (Arg.Set notyp) "(not documented)"

  (* Client side code emission. *)
  let register_closure gen_num args orig_expr =
    let _loc = Ast.loc_of_expr orig_expr in
    <:expr<
      Eliom_client.register_closure
        $`int64:gen_num$
        (fun $args$ -> ($orig_expr$ : unit))
    >>

  let arg_ids = ref []
  let push_arg orig_expr gen_id =
    let _loc = Ast.loc_of_expr orig_expr in
    if not (List.mem gen_id !arg_ids) then
      arg_ids := gen_id :: !arg_ids;
    if !notyp then
      <:expr< $lid:gen_id$ >>
    else
      let typ = Helpers.find_escaped_ident_type gen_id in
      <:expr< ($lid:gen_id$ : $typ$) >>


  let flush_args _loc =
    let res = !arg_ids in
    arg_ids := [];
    match res with
    | [] -> <:patt< () >>
    | [id] -> <:patt< $lid:id$ >>
    | _ ->
	let res = List.rev_map (fun id -> <:patt< $lid:id$ >>) res in
	<:patt< $tup:Ast.paCom_of_list res$ >>

  let clos_collection = ref []

  let push_closure_registration orig_expr gen_id =
    let _loc = Ast.loc_of_expr orig_expr in
    clos_collection :=
      <:str_item< let _ = $ register_closure gen_id (flush_args _loc) orig_expr $ >>
      :: !clos_collection

  let flush_closure_registrations () =
      let res = !clos_collection in
      clos_collection := [];
      List.rev res

  (** Syntax extension *)

  let _loc = Ast.Loc.ghost

  let shared_str_items items = Ast.stSem_of_list items
  let client_str_items items = Ast.stSem_of_list items

  let server_str_items items =
    Ast.stSem_of_list (flush_closure_registrations ())

  let client_expr orig_expr gen_num =
    push_closure_registration orig_expr gen_num;
    <:expr< "" >>

  let escaped orig_expr gen_id =
    push_arg orig_expr gen_id

end

module M = Pa_eliom_seed.Register(Id)(Client_pass)
