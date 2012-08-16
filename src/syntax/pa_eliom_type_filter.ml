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

(* This module generates the file used to infer types (hence wrappers) of server
   escaped values.

   Server-specific and escaped expression will be kept only for
   type-checking. In order to export type of escaped expressions: it
   generates for each escaped expression a toplevel definition that
   looks like:

     let $global_id$ = ref None

   And client-side expressions are replaced by lists of initializers
   (one per escaped expressions):

     $global_id$ := Some $expr$

*)

module Id = struct
  let name = "type-inference"
end

module Type_pass(Helpers : Pa_eliom_seed.Helpers) = struct

  open Helpers.Syntax

  (* accumulator, push and flush for typing expression. *)
  let typing_expr = ref []

  let add_typing_expr orig_expr gen_id =
    let _loc = Ast.loc_of_expr orig_expr in
    typing_expr := <:expr< $lid:gen_id$ := Some $orig_expr$ >> :: !typing_expr

  let flush_typing_expr () =
    let res = !typing_expr in
    typing_expr := [];
    Ast.exSem_of_list (List.rev res)

  (* accumulator, push and flush for typing str_items *)
  let typing_strs = ref []

  let add_typing_str orig_expr gen_id =
    let _loc = Ast.loc_of_expr orig_expr in
    typing_strs := <:str_item< let $lid:gen_id$ = ref None >> :: !typing_strs

  let flush_typing_strs () =
    let res = !typing_strs in
    typing_strs := [];
    Ast.stSem_of_list res

  (** Syntax extension *)

  let client_str_items items =
    Ast.stSem_of_list [
      flush_typing_strs ();
      (let _loc = Loc.ghost in
       <:str_item< let () = begin $flush_typing_expr ()$ end >>);
    ]

  let server_str_items items =
    Ast.stSem_of_list (flush_typing_strs () :: items)

  let shared_str_items = server_str_items

  let hole_expr typ context_level orig_expr gen_id gen_tid loc =
    match context_level with
      | Pa_eliom_seed.Server_item_context
      | Pa_eliom_seed.Shared_item_context ->
          add_typing_str orig_expr gen_tid;
          let typ = match typ with
            | Some typ -> typ
            | None -> let _loc = Loc.ghost in <:ctyp< _ >>
          in
          let _loc = loc in
          <:expr< begin
            $flush_typing_expr ()$;
            $lid:gen_tid$ :=
              Some (Eliom_lib.create_client_value
                      (Eliom_server.Client_value.create ~closure_id:0L ~instance_id:0)
                    : $typ$ Eliom_lib.client_value);
            Eliom_lib.get_option ! $lid:gen_tid$
          end >>
      | Pa_eliom_seed.Client_item_context ->
          let _loc = Loc.ghost in
          <:expr< >>

  let escaped context_level orig_expr gen_id =
    let open Pa_eliom_seed in
    let _loc = Ast.loc_of_expr orig_expr in
    match context_level with
      | Escaped_in_client_item
      | Escaped_in_hole_in Server_item_context
      | Escaped_in_hole_in Shared_item_context ->
          add_typing_str orig_expr gen_id;
          add_typing_expr orig_expr gen_id;
          <:expr< () >>
      | Escaped_in_hole_in Client_item_context ->
          <:expr< () >>

end

module M = Pa_eliom_seed.Register(Id)(Type_pass)
