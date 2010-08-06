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


(* This module generates the file used to infer types (hence wrappers) of server
   escaped values. *)

module Id =
struct
  let name = "Eliom client-server type inferrer"
  let version = "alpha"
end



(*** FILTER: type ***)
module Make_type_filter (Filters : Camlp4.Sig.AstFilters) =
struct
  include Pa_eliom_seed.Pre_make_filter(Filters)
  (*provides Ast*)

  class eliom_cli_serv_typer_map =
  object (self)
    inherit eliom_cli_serv_map as super


    (* accumulator, push and flush for typing expressiond *)
    val mutable typing_expr =
      let _loc = Ast.Loc.ghost in
      <:expr< "" >>

    method add_typing_expr _loc e n =
      let _loc = Ast.Loc.merge (Ast.loc_of_expr typing_expr) _loc in
      typing_expr <- <:expr< $id:n$ := Some $e$; $typing_expr$ >>

    method flush_typing_expr =
      let r = typing_expr in
      let _loc = Ast.Loc.ghost in
      typing_expr <- <:expr< "" >>;
      r


    (* accumulator, push and flush for typing str_items *)
    val mutable typing_strs =
      let _loc = Ast.Loc.ghost in
      <:str_item< >>

    method add_typing_str _loc n =
      let _loc = Ast.Loc.merge (Ast.loc_of_str_item typing_strs) _loc in
      typing_strs <- <:str_item< $typing_strs$ ;; let $id:n$ = ref None ;; >>

    method flush_typing_strs =
      let r = typing_strs in
      let _loc = Ast.Loc.ghost in
      typing_strs <- <:str_item< >>;
      r

    (* simultaneous push for str and expr *)
    method add_typing_expr_and_str _loc e n =
      self#add_typing_expr _loc e n;
      self#add_typing_str _loc n


    method server_str_item _loc s = <:str_item< $s$ >>

    method client_str_item _loc _ = <:str_item< >>

    method client_expr _ _ _ =
      self#flush_typing_expr

    method server_escaped_expr _loc e n =
      self#add_typing_expr_and_str _loc e n;
      let _loc = Ast.Loc.ghost in
      <:expr< () >>

  end

  let mapper = new eliom_cli_serv_typer_map
  let filter s = mapper#str_item s

  let () =
    (*Filters.*)register_str_item_filter
      (fun si ->
         let _loc = Ast.loc_of_str_item si in
         let new_si =
           <:str_item< $list: List.map filter (Ast.list_of_str_item si [])$ >>
         in
         Ast.stSem_of_list [mapper#flush_typing_strs; new_si]
      )

end

open Pa_eliom_seed
let () = match get_pass () with
  | Type_pass ->
      let module M = Camlp4.Register.AstFilter(Id)(Make_type_filter) in ()
  | Server_pass | Client_pass | Raw_pass -> ()
