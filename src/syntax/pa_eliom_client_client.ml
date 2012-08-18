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

(* This prepocessor generates the code to be executed by the navigator. *)

module Id = struct
  let name = "client part"
end

module Client_pass(Helpers : Pa_eliom_seed.Helpers) = struct

  open Helpers.Syntax

  let notyp = ref false
  let _ =
    Camlp4.Options.add "-notype" (Arg.Set notyp) "(not documented)"

  let drop_client_value_ctyp =
    Ast.map_ctyp
      (function
         | <:ctyp< $typ'$ Eliom_lib.client_value >> ->
             typ'
         | typ -> typ)

  let tuple_of_args =
    let _loc = Loc.ghost in function
    | [] -> <:patt< () >>
    | [id] -> <:patt< $lid:id$ >>
    | res ->
        let res = List.map (fun id -> <:patt< $lid:id$ >>) res in
        <:patt< $tup:Ast.paCom_of_list res$ >>

  (* Client side code emission. *)
  let register_closure gen_num args orig_expr =
    let _loc = Ast.loc_of_expr orig_expr in
    let typ =
      if !notyp then
        <:ctyp< _ >>
      else
        Helpers.find_client_value_type gen_num
    in
    <:expr<
      Eliom_client.Client_closure.register $`int64:gen_num$
        (fun $tuple_of_args args$ ->
           ($orig_expr$ : $typ$))
    >>

  let push_server_arg, flush_server_args =
    let server_arg_ids = ref [] in
    let push gen_id =
      if not (List.mem gen_id !server_arg_ids) then
        server_arg_ids := gen_id :: !server_arg_ids
    in
    let flush _loc =
      let res = List.rev !server_arg_ids in
      server_arg_ids := [];
      res
    in
    push, flush


  let push_client_arg, flush_client_args =
    let client_arg_ids = ref [] in
    let push loc orig_expr gen_id =
      if not (List.exists (fun (_, _, gen_id') -> gen_id = gen_id') !client_arg_ids) then
        client_arg_ids := (loc, orig_expr, gen_id) :: !client_arg_ids
    in
    let flush expr =
      let res = List.rev !client_arg_ids in
      client_arg_ids := [];
      res
    in
    push, flush


  let push_closure_registration, flush_closure_registrations =
    let clos_collection = ref [] in
    let push orig_expr gen_id =
      let _loc = Ast.loc_of_expr orig_expr in
      clos_collection :=
        <:str_item<
          let () =
            $register_closure gen_id (flush_server_args _loc) orig_expr$;
            Eliom_client.do_client_value_initializations ~closure_id: $`int64:gen_id$
        >>
        :: !clos_collection
    in
    let flush () =
      let res = List.rev !clos_collection in
      clos_collection := [];
      res
    in
    push, flush

  let push_injected_var, flush_injected_vars =
    let escaped_vars = ref [] in
    let push var =
      if not (List.mem var !escaped_vars) then
        escaped_vars := var :: !escaped_vars
    in
    let flush () =
      let res = List.rev !escaped_vars in
      escaped_vars := [];
      let _loc = Loc.ghost in
      List.map (fun str -> <:expr< $str:str$ >>) res
    in
    push, flush

  (** Syntax extension *)

  let client_str_items items =
    let do_injections =
      let _loc = Loc.ghost in
      let injected_vars = flush_injected_vars () in
      if injected_vars = [] then
        <:str_item< >>
      else
        let names =
          List.fold_right
            (fun hd tl -> <:expr< $hd$ :: $tl$ >>)
            injected_vars <:expr< []>>
        in
        <:str_item<
          let () =
            Eliom_client.do_injections ~names: $names$
        >>
    in
    Ast.stSem_of_list (do_injections :: items)

  let shared_str_items items =
    client_str_items items

  let server_str_items items =
    let closure_registrations = flush_closure_registrations () in
    Ast.stSem_of_list closure_registrations

  let hole_expr typ context_level orig_expr gen_num gen_tid loc =
    if context_level = Pa_eliom_seed.Server_item_context ||
       context_level = Pa_eliom_seed.Shared_item_context
    then
      push_closure_registration orig_expr gen_num;

    let typ =
      match typ with
        | Some typ -> typ
        | None -> let _loc = Loc.ghost in <:ctyp< _ >>
    in

    match context_level with
      | Pa_eliom_seed.Server_item_context ->
          let _loc = Ast.Loc.ghost in
          <:expr< "" >>
      | Pa_eliom_seed.Client_item_context
      | Pa_eliom_seed.Shared_item_context ->
          let bindings =
            List.map
              (fun (_loc, orig_expr, gen_id) ->
                 <:patt< $lid:gen_id$ >>, orig_expr)
              (flush_client_args ())
          in
          <:expr@loc<
            let $Ast.binding_of_pel bindings$ in
            ($orig_expr$ : $typ$)
          >>

  let escaped context_level orig_expr gen_id =
    let open Pa_eliom_seed in
    if context_level = Escaped_in_hole_in Server_item_context ||
       context_level = Escaped_in_hole_in Shared_item_context
    then
      push_server_arg gen_id;
    if context_level = Escaped_in_hole_in Client_item_context ||
       context_level = Escaped_in_hole_in Shared_item_context
    then
      push_client_arg (Ast.loc_of_expr orig_expr) orig_expr gen_id;

    let _loc = Ast.loc_of_expr orig_expr in
    match context_level with
       | Escaped_in_hole_in Server_item_context
       | Escaped_in_hole_in Shared_item_context ->
          if !notyp then
            <:expr< $lid:gen_id$ >>
          else
            let typ =
              let typ = Helpers.find_escaped_ident_type gen_id in
              (* TODO BB Drop Eliom_reference.eref *)
              drop_client_value_ctyp#ctyp typ
            in
            <:expr< ($lid:gen_id$ : $typ$) >>
       | Escaped_in_hole_in Client_item_context ->
          <:expr< $lid:gen_id$ >>
       | Escaped_in_client_item ->
           push_injected_var gen_id;
           let typ = Helpers.find_escaped_ident_type gen_id in
           let typ = (* Replace toplevel [t Eliom_reference.eref] to [t]. *)
             match typ with
               | <:ctyp< ($typ'$ Eliom_reference.Volatile.eref) >>
               | <:ctyp< ($typ'$ Eliom_reference.eref) >> -> typ'
               | typ -> typ
           in
           let typ = (* Replace all [t Eliom_lib.client_value] to [t]. *)
             drop_client_value_ctyp#ctyp typ
           in
           <:expr< (Eliom_client.Injection.get ~name: $str:gen_id$
                    : $typ$) >>



end

module M = Pa_eliom_seed.Register(Id)(Client_pass)
