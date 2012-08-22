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

  (* Replace every type [t Eliom_lib.client_value] by [t]. *)
  let drop_client_value_ctyp =
    let ast_mapper =
      Ast.map_ctyp
        (function
           | <:ctyp< $typ'$ Eliom_lib.client_value >> ->
               typ'
           | typ -> typ)
    in
    fun typ -> ast_mapper#ctyp typ

  (* Replace every escaped identifier [v] with [Eliom_client.Syntax_helpers.get_escaped_value v] *)
  let map_get_escaped_values =
    let mapper =
      Ast.map_expr
        (function
           | <:expr< $lid:str$ >> when Helpers.is_escaped_indent_string str ->
               let _loc = Loc.ghost in
               <:expr< Eliom_client.Syntax_helpers.get_escaped_value $lid:str$ >>
           | expr -> expr)
    in
    fun expr ->
      mapper#expr expr

  (* Convert a list of expressions to a tuple, one expression, or (). *)
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
      Eliom_client.Syntax_helpers.register_client_closure
        $`int64:gen_num$
        (fun $tuple_of_args args$ ->
           ($orig_expr$ : $typ$))
    >>

  let push_escaped_arg, flush_escaped_args =
    let server_arg_ids = ref [] in
    let push gen_id =
      if not (List.mem gen_id !server_arg_ids) then
        server_arg_ids := gen_id :: !server_arg_ids
    in
    let flush () =
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
    let push orig_expr gen_id escaped_vars =
      let _loc = Ast.loc_of_expr orig_expr in
      clos_collection :=
        <:str_item<
          let () =
            $register_closure gen_id escaped_vars orig_expr$
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
    let push gen_id orig_expr =
      if not (List.mem gen_id (List.map fst !escaped_vars)) then
        escaped_vars := (gen_id, orig_expr) :: !escaped_vars
    in
    let flush () =
      let res = List.rev !escaped_vars in
      escaped_vars := [];
      res
    in
    push, flush

  let injection_initializations injected_vars =
    let _loc = Loc.ghost in
    if injected_vars = [] then
      <:str_item< >>
    else
      let names =
        List.fold_right
          (fun (hd, _) tl ->
             <:expr< $str:hd$ :: $tl$ >>)
          injected_vars <:expr< []>>
      in
      <:str_item<
        let () =
          Eliom_client.Syntax_helpers.injection_initializations $names$
      >>


  let injection_bindings injected_vars =
    let _loc = Loc.ghost in
    if injected_vars = [] then
      <:str_item< >>
    else 
      let bindings =
        List.map
          (fun (gen_id, orig_expr) ->
             <:patt< $lid:gen_id$ >>, orig_expr)
          injected_vars
      in
      <:str_item< let $Ast.binding_of_pel bindings$ >>

  (** Syntax extension *)

  let client_str_items items =
    Ast.stSem_of_list
      (injection_initializations (flush_injected_vars ()) ::
       items)

  let server_str_items _ =
    Ast.stSem_of_list
      (flush_closure_registrations ())

  let shared_str_items items =
    Ast.stSem_of_list
      (injection_bindings (flush_injected_vars ()) ::
       flush_closure_registrations () @
       items)

  let client_value_expr typ context_level orig_expr gen_num gen_tid loc =
    push_closure_registration (map_get_escaped_values orig_expr) gen_num (flush_escaped_args ());
    let typ =
      match typ with
        | Some typ -> typ
        | None -> let _loc = Loc.ghost in <:ctyp< _ >>
    in
    match context_level with
      | `Server ->
          let _loc = Ast.Loc.ghost in
          <:expr< >>
      | `Shared ->
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

  let escape_inject context_level orig_expr gen_id =
    let open Pa_eliom_seed in
    match context_level with
       | Escaped_in_client_value_in `Server ->
           push_escaped_arg gen_id;
           let expr =
             let _loc = Loc.ghost in
             <:expr< $lid:gen_id$ >>
           in
           if !notyp then
             expr
           else
             let typ =
               if !notyp then
                 let _loc = Loc.ghost in
                 <:ctyp< _ >>
               else
                 drop_client_value_ctyp
                   (Helpers.find_escaped_ident_type gen_id)
             in
             let _loc = Ast.loc_of_expr orig_expr in
             <:expr< ($expr$ : $typ$) >>
       | Escaped_in_client_value_in `Shared ->
           push_escaped_arg gen_id;
           push_client_arg (Ast.loc_of_expr orig_expr) orig_expr gen_id;
           let _loc = Loc.ghost in
           <:expr< $lid:gen_id$ >>
       | Injected_in `Shared ->
           push_injected_var gen_id orig_expr;
           let _loc = Loc.ghost in
           <:expr< $lid:gen_id$ >>
       | Injected_in `Client ->
           push_injected_var gen_id orig_expr;
           let typ =
             if !notyp then
               let _loc = Loc.ghost in
               <:ctyp< _ >>
             else
               drop_client_value_ctyp
                 (Helpers.find_injected_ident_type gen_id)
           in
           let _loc = Ast.loc_of_expr orig_expr in
           <:expr<
             (Eliom_client.Syntax_helpers.get_injection $str:gen_id$
              : $typ$)
           >>


end

module M = Pa_eliom_seed.Register(Id)(Client_pass)
