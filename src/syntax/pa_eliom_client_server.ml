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

  let push_escaped_binding, flush_escaped_bindings =
    let arg_ids = ref [] in
    let arg_collection = ref [] in
    let push orig_expr gen_id =
      if not (List.mem gen_id !arg_ids) then begin
        let _loc = Ast.loc_of_expr orig_expr in
        arg_collection := (gen_id, orig_expr) :: !arg_collection;
        arg_ids := gen_id :: !arg_ids
      end
    in
    let flush () =
      let res = List.rev !arg_collection in
      arg_ids := [];
      arg_collection := [];
      let aux (_, arg) =
        let _loc = Ast.loc_of_expr arg in
        <:expr< Eliom_service.Syntax_helpers.escaped_value $arg$ >>
      in
      List.map aux res
    in
    push, flush

  let push_injection, flush_injections =
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

  (* Register every injection of $orig_expr$ as $gen_id$ *)
  let register_injections injections =
    let _loc = Loc.ghost in
    let exprs =
      List.map
        (fun (gen_id, orig_expr) ->
           <:expr< Eliom_service.Syntax_helpers.injection $str:gen_id$ (fun () -> $orig_expr$) >>)
        injections
    in
    <:str_item< let () = $Ast.exSem_of_list exprs$; () >>

  (* For every injection of $orig_expr$ as $gen_id$:
     let $gen_id$ = $orig_expr$ and ... *)
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

  let close_client_value_data_list loc =
    let _loc = Loc.ghost in
    <:str_item<
        let () =
          Eliom_service.Syntax_helpers.close_client_value_data_list
            $str:Loc.file_name loc$ (* BB XXX replace by some ID *)
    >>


  (** Syntax extension *)

  let client_str_items loc _ =
    register_injections (flush_injections ())

  let server_str_items loc items =
    Ast.stSem_of_list
      (items @
       [ close_client_value_data_list loc ])

  let shared_str_items loc items =
    let injections = flush_injections () in
    Ast.stSem_of_list
      (register_injections injections ::
       [ bind_injected_idents injections ] @
       items @
       [ close_client_value_data_list loc ])

  let client_value_expr typ context_level orig_expr gen_num _ loc =
    let typ =
      match typ with
        | Some typ -> typ
        | None ->
            if !notyp then
              let _loc = Loc.ghost in <:ctyp< _ >>
            else
              match Helpers.find_client_value_type gen_num with
                | Ast.TyQuo _ ->
                    Helpers.raise_syntax_error loc
                      "The types of client values must be monomorphic from its usage \
                       or from its type annotation"
                | typ -> typ
    in
    <:expr@loc<
      (Eliom_service.Syntax_helpers.client_value $`int64:gen_num$
         $Helpers.expr_tuple (flush_escaped_bindings ())$
       : $typ$ Eliom_lib.client_value)
    >>

  let escape_inject context_level orig_expr gen_id =
    let open Pa_eliom_seed in
    match context_level with
      | Escaped_in_client_value_in _ ->
          push_escaped_binding orig_expr gen_id;
          let _loc = Loc.ghost in
          <:expr< >>
      | Injected_in _ ->
          push_injection orig_expr gen_id;
          let _loc = Ast.loc_of_expr orig_expr in
          <:expr< $lid:gen_id$ >>
end

module M = Pa_eliom_seed.Register(Id)(Server_pass)
