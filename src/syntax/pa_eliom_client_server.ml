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

  let notyp = ref false
  let _ =
    Camlp4.Options.add "-notype" (Arg.Set notyp) "(not documented)"

  let tuple_of_args = function
    | [] -> let _loc = Loc.ghost in <:expr< () >>
    | [e] -> e
    | args -> let _loc = Loc.ghost in <:expr< $tup:Ast.exCom_of_list args$ >>

  let client_value gen_num args loc typ =
    let typ =
      match typ with
        | Some typ -> typ
        | None ->
            if !notyp then
              let _loc = Loc.ghost in
              <:ctyp< _ >>
            else
              match Helpers.find_client_value_type gen_num with
                | Ast.TyQuo _ ->
                    Helpers.raise_syntax_error loc
                      "The types of client values must be monomorphic from its usage \
                       or from its type annotation"
                | typ -> typ
    in
    let escaped_value (_, arg) =
      let _loc = Loc.ghost in
      <:expr< Eliom_service.Syntax_helpers.escaped_value $arg$ >>
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
      arg_collection := (gen_id, orig_expr) :: !arg_collection;
      arg_ids := gen_id :: !arg_ids
    end

  let flush_args () =
    let res = !arg_collection in
    arg_ids := [];
    arg_collection := [];
    List.rev res

  let push_injected, flush_injected =
    let buffer = ref [] in
    let push orig_expr gen_id =
      if not (List.mem gen_id (List.map fst !buffer)) then begin
        buffer := (gen_id, orig_expr) :: !buffer;
      end
    in
    let flush () =
      let res = !buffer in
      buffer := [];
      List.rev res
    in
    push, flush

  let register_injections injections =
    List.map
      (fun (gen_id, orig_expr) ->
        let _loc = Loc.ghost in
        <:str_item<
          let () =
            Eliom_service.Syntax_helpers.injection $str:gen_id$ (fun () -> $orig_expr$)
        >>)
      injections

  let bind_injected_idents injections =
    let _loc = Loc.ghost in
    let bindings =
      List.map
        (fun (gen_id, orig_expr) ->
           <:patt< $lid:gen_id$ >>,
           orig_expr)
        injections
    in
    <:str_item< let $Ast.binding_of_pel bindings$ >>

  (** Syntax extension *)

  let server_str_items items =
    Ast.stSem_of_list items

  let client_str_items _ =
    Ast.stSem_of_list
      (register_injections (flush_injected ()))

  let shared_str_items items =
    Ast.stSem_of_list
      (bind_injected_idents (flush_injected ()) ::
       items)

  let client_value_expr typ context_level orig_expr gen_num _ loc =
    client_value gen_num (flush_args ()) loc typ

  let escape_inject context_level orig_expr gen_id =
    let open Pa_eliom_seed in
    match context_level with
      | Escaped_in_client_value_in _ ->
          push_arg orig_expr gen_id;
          let _loc = Loc.ghost in
          <:expr< >>
      | Injected_in _ ->
          push_injected orig_expr gen_id;
          let _loc = Loc.ghost in
          <:expr< $lid:gen_id$ >>



end

module M = Pa_eliom_seed.Register(Id)(Server_pass)
