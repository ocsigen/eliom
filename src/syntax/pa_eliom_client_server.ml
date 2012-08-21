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
                  Helpers.raise_syntax_error loc
                    "The types of client values must be monomorphic from its usage \
                     or from its type annotation"
              | typ -> typ
    in
    let escaped_value (_, arg) =
      let _loc = Loc.ghost in
      <:expr< Eliom_lib.escaped_value $arg$ >>
    in
    <:expr@loc<
      (Eliom_service.Syntax_helpers.client_value
         $`int64:gen_num$
         $tuple_of_args (List.map escaped_value args)$
       : $typ$ Eliom_lib.client_value)
    >>

  let arg_ids = ref []
  let arg_collection = ref []
  let push_arg orig_expr gen_id =
    if not (List.mem gen_id !arg_ids) then begin
      let _loc = Ast.loc_of_expr orig_expr in
      (* TODO BB Drop Eliom_reference.eref *)
      let arg = <:expr< $orig_expr$ >> in
      arg_collection := (gen_id, arg) :: !arg_collection;
      arg_ids := gen_id :: !arg_ids
    end

  let flush_args () =
    let res = !arg_collection in
    arg_ids := [];
    arg_collection := [];
    List.rev res

  (** Filters *)

  let shared_str_items items = Ast.stSem_of_list items
  let server_str_items items = Ast.stSem_of_list items

  let client_str_items items =
    let aux (gen_id, orig_expr) =
      let value =
        if None = Helpers.(is_eliom_reference_type (find_injected_ident_type gen_id))
        then
          let _loc = Loc.ghost in
          <:expr< fun () -> Lwt.return $orig_expr$ >>
        else
          let _loc = Loc.ghost in
          <:expr<
            fun () ->
              Eliom_reference.get ($orig_expr$ :> _ Eliom_reference.eref)
          >>
      in
      let _loc = Loc.ghost in
      <:str_item<
        let () =
          Eliom_service.Syntax_helpers.injection $str:gen_id$ $value$
      >>
    in
    Ast.stSem_of_list
      (List.map aux (flush_args ()))

  let hole_expr typ context_level orig_expr gen_num _ loc =
    match context_level with
      | Pa_eliom_seed.Server_item_context
      | Pa_eliom_seed.Shared_item_context ->
          client_value gen_num (flush_args ()) loc typ
      | Pa_eliom_seed.Client_item_context ->
          let _loc = Loc.ghost in
          <:expr< >>

  let escaped context_level orig_expr gen_id =
    let open Pa_eliom_seed in
    (match context_level with
       | Escaped_in_hole_in Server_item_context
       | Escaped_in_hole_in Shared_item_context ->
           let _loc = Loc.ghost in
           push_arg orig_expr gen_id
       | Escaped_in_client_item ->
           push_arg orig_expr gen_id
       | Escaped_in_hole_in Client_item_context -> ());
    let _loc = Loc.ghost in
    <:expr< >>



end

module M = Pa_eliom_seed.Register(Id)(Server_pass)
