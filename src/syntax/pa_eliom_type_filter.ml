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

  (* accumulator, push and flush for typing expression
     <:expr< $gen_id$ := Some $orig_expr$ >> *)
  let push_typing_expr, flush_typing_expr =
    let typing_expr = ref [] in
    let add orig_expr gen_id =
      let _loc = Ast.loc_of_expr orig_expr in
      if List.for_all (function gen_id', _ -> gen_id <> gen_id') !typing_expr then
        typing_expr := (gen_id, <:expr< $lid:gen_id$ := Some $orig_expr$ >>) :: !typing_expr
    in
    let flush () =
      let res = List.rev (List.map snd !typing_expr) in
      typing_expr := [];
      Ast.exSem_of_list res
    in
    add, flush

  (* accumulator, push and flush for typing str_items
     <:str_item< let $gen_id$ = ref None >> *)
  let push_typing_str_item, flush_typing_str_item =
    let typing_strs = ref [] in
    let add orig_expr gen_id =
      let _loc = Ast.loc_of_expr orig_expr in
      if List.for_all (function gen_id', _ -> gen_id' <> gen_id) !typing_strs then
        typing_strs := (gen_id, <:str_item< let $lid:gen_id$ = Pervasives.ref None >>) :: !typing_strs
    in
    let flush () =
      let res = List.map snd !typing_strs in
      typing_strs := [];
      Ast.stSem_of_list res
    in
    add, flush

  (** Syntax extension *)

  let client_str_items _loc items =
    Ast.stSem_of_list [
      flush_typing_str_item ();
      (let _loc = Loc.ghost in
       <:str_item< let () = begin $flush_typing_expr ()$ end >>);
    ]

  let server_str_items _loc items =
    Ast.stSem_of_list (flush_typing_str_item () :: items)

  let shared_str_items = server_str_items

  let client_value_expr typ context_level orig_expr gen_id gen_tid loc =
    push_typing_str_item orig_expr gen_tid;
    let typ = match typ with
      | Some typ -> typ
      | None -> let _loc = Loc.ghost in <:ctyp< _ >>
    in
    let _loc = loc in
    <:expr< begin
      $flush_typing_expr ()$;
      $lid:gen_tid$ := Some (Eliom_service.Syntax_helpers.client_value 0L 0 : $typ$ Eliom_pervasives.client_value);
      match ! $lid:gen_tid$ with | Some x -> x | None -> assert false
    end >>

  let escape_inject context_level orig_expr gen_id =
    let open Pa_eliom_seed in
    push_typing_str_item orig_expr gen_id;
    push_typing_expr orig_expr gen_id;
    match context_level with
      | Escaped_in_client_value_in _ ->
          let _loc = Ast.loc_of_expr orig_expr in
          <:expr< >>
      | Injected_in `Shared ->
          orig_expr
      | Injected_in `Client ->
          let _loc = Ast.loc_of_expr orig_expr in
          <:expr< >>

  let implem loc sil =
    let _loc = Loc.ghost in
    let debug_compilation_unit_name =
      let name = Printf.sprintf "__eliom__compilation_unit_id__%d"
        (Hashtbl.hash (Loc.file_name loc)) in
      <:str_item< let $lid:name$ = () >>
    in
    debug_compilation_unit_name :: sil

  let shared_sig_items _ _ = let _loc = Loc.ghost in <:sig_item< >>
  let server_sig_items _ _ = let _loc = Loc.ghost in <:sig_item< >>
  let client_sig_items _ _ = let _loc = Loc.ghost in <:sig_item< >>

end

module M = Pa_eliom_seed.Register(Id)(Type_pass)
