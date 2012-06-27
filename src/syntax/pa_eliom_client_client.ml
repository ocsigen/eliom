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
    let ty =
      if !notyp then
        <:ctyp< 'a >>
      else
        Helpers.find_event_handler_type gen_num in
    <:expr<
      Eliom_client.register_closure
        $`int64:gen_num$
        (fun $args$ -> (fun (_ev: (Dom_html.event Js.t)) ->
	   let _ev : $ty$ Js.t = Obj.magic _ev in ($orig_expr$ : unit)))
    >>

  let server_arg_ids = ref []
  let push_server_arg orig_expr gen_id =
    let _loc = Ast.loc_of_expr orig_expr in
    if not (List.mem gen_id !server_arg_ids) then
      server_arg_ids := gen_id :: !server_arg_ids


  let flush_server_args _loc =
    let res = !server_arg_ids in
    server_arg_ids := [];
    match res with
    | [] -> <:patt< () >>
    | [id] -> <:patt< $lid:id$ >>
    | _ ->
	let res = List.rev_map (fun id -> <:patt< $lid:id$ >>) res in
	<:patt< $tup:Ast.paCom_of_list res$ >>

  let client_arg_ids = ref []

  let push_client_arg _loc orig_expr gen_id =
    client_arg_ids := (_loc, orig_expr, gen_id) :: !client_arg_ids

  let flush_client_args expr =
    let res = !client_arg_ids in
    client_arg_ids := [];
    List.fold_right
      (fun (_loc, orig_expr, gen_id) sofar ->
         let binding = <:binding< $lid:gen_id$ = $orig_expr$ >> in
         <:expr< let $binding$ in $sofar$ >>)
      res expr


  let clos_collection = ref []

  let push_closure_registration orig_expr gen_id =
    let _loc = Ast.loc_of_expr orig_expr in
    clos_collection :=
      <:str_item< let _ = $ register_closure gen_id (flush_server_args _loc) orig_expr $ >>
      :: !clos_collection

  let flush_closure_registrations () =
      let res = !clos_collection in
      clos_collection := [];
      List.rev res

  (** Syntax extension *)

  let shared_str_items items = Ast.stSem_of_list items
  let client_str_items items = Ast.stSem_of_list items

  let server_str_items items =
    Ast.stSem_of_list (flush_closure_registrations ())

  let client_expr context_level orig_expr gen_num gen_tid =
    if context_level = Pa_eliom_seed.Server_item_context ||
       context_level = Pa_eliom_seed.Shared_item_context
    then
      push_closure_registration orig_expr gen_num;

    let _loc = Ast.loc_of_expr orig_expr in
    match context_level with
      | Pa_eliom_seed.Server_item_context ->
          <:expr< "" >>
      | Pa_eliom_seed.Client_item_context
      | Pa_eliom_seed.Shared_item_context ->
          <:expr< fun _ev -> $flush_client_args orig_expr$ >>

  let escaped context_level orig_expr gen_id =
    if context_level = Pa_eliom_seed.Server_item_context ||
       context_level = Pa_eliom_seed.Shared_item_context
    then
      push_server_arg orig_expr gen_id;
    if context_level = Pa_eliom_seed.Client_item_context ||
       context_level = Pa_eliom_seed.Shared_item_context
    then
      push_client_arg (Ast.loc_of_expr orig_expr) orig_expr gen_id;

    let _loc = Ast.loc_of_expr orig_expr in
    match context_level with
       | Pa_eliom_seed.Server_item_context
       | Pa_eliom_seed.Shared_item_context ->
          if !notyp then
            <:expr< $lid:gen_id$ >>
          else
            let typ = Helpers.find_escaped_ident_type gen_id in
            <:expr< ($lid:gen_id$ : $typ$) >>
      | Pa_eliom_seed.Client_item_context ->
          <:expr< $lid:gen_id$ >>


end

module M = Pa_eliom_seed.Register(Id)(Client_pass)
