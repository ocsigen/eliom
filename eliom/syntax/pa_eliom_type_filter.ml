(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010-2011
 * Raphaël Proust, Grégoire Henry
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
    let _loc = Loc.ghost in <:str_item< >>

  let shared_str_items items =
    Ast.stSem_of_list (flush_typing_strs () :: items)

  let server_str_items = shared_str_items

  let client_expr orig_expr gen_id =
    let _loc = Ast.loc_of_expr orig_expr in
    <:expr< begin $flush_typing_expr ()$; "" end >>

  let escaped orig_expr gen_id =
    add_typing_expr orig_expr gen_id;
    add_typing_str orig_expr gen_id;
    let _loc = Ast.loc_of_expr orig_expr in <:expr< () >>

end

module M = Pa_eliom_seed.Register(Id)(Type_pass)
