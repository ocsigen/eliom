(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010-2011
 * Raphaël Proust, Grégoire Henry, Gabriel Radanne
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
open Parsetree
open Asttypes
open Ast_helper

module AC = Ast_convenience
module AM = Ast_mapper

open Ppx_eliom_utils

module Pass = struct

  (* accumulator, push and flush for typing expression
     $gen_id := Some $orig_expr *)
  let push_typing_expr, flush_typing_expr =
    let typing_expr = ref [] in
    let add orig_expr id =
      if List.for_all (function id', _ -> id.txt <> id'.txt) !typing_expr
      then
        let frag_eid = eid id in
        typing_expr :=
          (id,
           [%expr [%e frag_eid] := Some [%e orig_expr]]
             [@metaloc orig_expr.pexp_loc]
          ) :: !typing_expr
    in
    let flush () =
      let res = List.rev (List.map snd !typing_expr) in
      typing_expr := [];
      AC.sequence res
    in
    add, flush

  (* accumulator, push and flush for typing str
     let $id = ref None
  *)
  let push_typing_str_item, flush_typing_str_item =
    let typing_strs = ref [] in
    let add orig_expr id =
      if List.for_all (function id', _ -> id'.txt <> id.txt) !typing_strs
      then
        typing_strs :=
          (id,
           [%stri let [%p Pat.var id] = Pervasives.ref None]
           [@metaloc orig_expr.pexp_loc]
          ) :: !typing_strs
    in
    let flush () =
      let res = List.map snd !typing_strs in
      typing_strs := [];
      res
    in
    add, flush

  (** Syntax extension *)

  let client_str item =
    let loc = item.pstr_loc in
    flush_typing_str_item () @
    [%str let () = [%e flush_typing_expr () ] ] [@metaloc loc]

  let server_str item =
    flush_typing_str_item () @
    [ item ]

  let shared_str = server_str

  let fragment ?typ ~context:_ ~num:_ ~id expr =
    let loc = expr.pexp_loc in
    let frag_eid = eid id in
    push_typing_str_item expr id;
    let typ = match typ with
      | Some typ -> typ
      | None -> Typ.any ~loc ()
    in
    [%expr
      [%e flush_typing_expr () ];
      [%e frag_eid] :=
        Some ( Eliom_service.Syntax_helpers.client_value "" 0 :
                 [%t typ] Eliom_pervasives.client_value);
      match ! [%e frag_eid] with
      | Some x -> x
      | None -> assert false
    ]

  let escape_inject ?ident:_ ~(context:Context.escape_inject) ~id expr =
    push_typing_str_item expr id;
    push_typing_expr expr id;
    match context with
    | `Escaped_value _ -> [%expr assert false]
    | `Injection `Shared -> expr
    | `Injection `Client -> [%expr assert false]

  let prelude loc =
    let txt =
      Printf.sprintf "__eliom__compilation_unit_id__%s" (file_hash loc) in
    let id = Pat.var ~loc { loc ; txt } in
    [%str let [%p id] = () ]

  let postlude _ = []

  let shared_sig _ = []
  let server_sig _ = []
  let client_sig _ = []

end

include Make(Pass)
