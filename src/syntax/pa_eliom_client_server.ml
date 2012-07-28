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

  let tuple_of_args = function
    | [] -> let _loc = Loc.ghost in <:expr< () >>
    | [e] -> e
    | args -> let _loc = Loc.ghost in <:expr< $tup:Ast.exCom_of_list args$ >>

  let client_value gen_num args loc typ =
    let typ =
      match typ with
        | Some typ -> typ
        | None ->
            match Helpers.find_client_value_type gen_num with
              | Ast.TyQuo _ ->
                  (* TODO BB lighten this - should only apply to holes used in other holes *)
                  Helpers.raise_syntax_error loc
                    "Holes must have closed types"
              | typ -> typ
    in
    <:expr@loc<
        let __eliom_instance_id = Eliom_lib.fresh_ix () in
        Eliom_service.initialization $`int64:gen_num$ __eliom_instance_id
          (Eliom_lib.to_poly $tuple_of_args args$);
        (Eliom_server.Client_value.create $`int64:gen_num$ __eliom_instance_id
           : $typ$ Eliom_server.Client_value.t)
    >>

  let arg_ids = ref []
  let arg_collection = ref []
  let push_arg orig_expr gen_id =
    if not (List.mem gen_id !arg_ids) then begin
      let _loc = Ast.loc_of_expr orig_expr in
      let arg = <:expr< $orig_expr$ >> in
      arg_collection := arg :: !arg_collection;
      arg_ids := gen_id :: !arg_ids
    end

  let flush_args _loc =
    let res = !arg_collection in
    arg_ids := [];
    arg_collection := [];
    List.rev res

  (** Filters *)

  let _loc = Ast.Loc.ghost
  let shared_str_items items = Ast.stSem_of_list items
  let server_str_items items = Ast.stSem_of_list items

  let client_str_items items = <:str_item< >>

  let hole_expr typ context_level orig_expr gen_num _ =
    match context_level with
      | Pa_eliom_seed.Server_item_context
      | Pa_eliom_seed.Shared_item_context ->
          let _loc =  Ast.loc_of_expr orig_expr in
          client_value gen_num (flush_args ()) (Ast.loc_of_expr orig_expr) typ
      | Pa_eliom_seed.Client_item_context ->
          <:expr< >>

  let escaped context_level orig_expr gen_id =
    match context_level with
      | Pa_eliom_seed.Server_item_context
      | Pa_eliom_seed.Shared_item_context ->
          push_arg orig_expr gen_id;
          <:expr< >>
      | Pa_eliom_seed.Client_item_context ->
          <:expr< >>


end

module M = Pa_eliom_seed.Register(Id)(Server_pass)
