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

(* This prepocessor generates the module to be loaded by Ocsigen server *)

module Id = struct
  let name = "server part"
end

module Server_pass(Helpers : Pa_eliom_seed.Helpers) = struct

  open Helpers.Syntax

  (* Server side code emission *)
  let closure_call _loc num args =
    <:expr<
         $str:"caml_run_from_table("
          ^ Int64.to_string num
          ^ ", \'" $
       ^ Eliom_client_types.jsmarshal $args$
       ^ "\')"
    >>

  let arg_ids = ref []
  let arg_collection = ref []
  let push_arg orig_expr gen_id =
    if not (List.mem gen_id !arg_ids) then begin
      let _loc = Ast.loc_of_expr orig_expr in
      let typ = Helpers.find_escaped_ident_type gen_id in
      let wrapper = Helpers.find_wrapper _loc typ in
      arg_collection := <:expr< ($wrapper$ $orig_expr$) >> :: !arg_collection;
      arg_ids := gen_id :: !arg_ids
    end

  let flush_args _loc =
    let res = !arg_collection in
    arg_ids := [];
    arg_collection := [];
    match res with
    | [] -> <:expr< () >>
    | [e] -> e
    | _ -> <:expr< $tup:Ast.exCom_of_list (List.rev res)$ >>

  (** Filters *)

  let _loc = Ast.Loc.ghost
  let shared_str_items items = Ast.stSem_of_list items
  let server_str_items items = Ast.stSem_of_list items

  let client_str_items items = <:str_item< >>

  let client_expr orig_expr gen_num =
    let _loc =  Ast.loc_of_expr orig_expr in
    closure_call _loc gen_num (flush_args _loc)

  let escaped orig_expr gen_id =
    push_arg orig_expr gen_id;
    <:expr< >>

end

module M = Pa_eliom_seed.Register(Id)(Server_pass)
