(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010-2011
 * Raphaël Proust, Grégoire Henry, Benedikt Becker
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

(* BB {2 map_get_escaped_values / escape_inject generates only $lid:gen_id$}
   The expression $expr$ inside a client_value will be used for for registering
   the client closure (cf. [Client_pass.register_client_closures]), as well as
   for creating a client-only function (cf. [Client_pass.define_client_functions]).
   Only for the former it is necessary to call [Eliom_client.Syntax_helpers.get_escaped_value]
   on the escaped identifier.
   This is done post-hoc by [map_get_escaped_values] in [register_client_closures]. *)

module Id = struct
  let name = "client part"
end

module Client_pass(Helpers : Pa_eliom_seed.Helpers) = struct

  open Helpers.Syntax

  let notyp = ref false
  let _ =
    Camlp4.Options.add "-notype" (Arg.Set notyp) "(not documented)"

  (* {2 Auxiliaries} *)

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
           | <:expr@_loc< $lid:str$ >> when Helpers.is_escaped_indent_string str ->
               <:expr< Eliom_client.Syntax_helpers.get_escaped_value $lid:str$ >>
           | expr -> expr)
    in
    fun expr ->
      mapper#expr expr

  let push_escaped_binding, flush_escaped_bindings =
    let server_arg_ids = ref [] in
    let is_unknown gen_id =
      List.for_all
        (fun (gen_id', _) -> gen_id <> gen_id')
        !server_arg_ids
    in
    let push gen_id expr =
      if is_unknown gen_id then
        server_arg_ids := (gen_id, expr) :: !server_arg_ids
    in
    let flush () =
      let res = List.rev !server_arg_ids in
      server_arg_ids := [];
      res
    in
    push, flush

  let push_client_value_data, flush_client_value_datas =
    let client_value_datas = ref [] in
    let push gen_num gen_id expr args =
      client_value_datas :=
        (gen_num, gen_id, expr, args) :: !client_value_datas
    in
    let flush () =
      let res = List.rev !client_value_datas in
      client_value_datas := [];
      res
    in
    push, flush

  let get_type f x =
    if !notyp then
      let _loc = Loc.ghost in
      <:ctyp< _ >>
    else f x

  let register_client_closures client_value_datas =
    let registrations =
      List.map
        (fun (gen_num, _, expr, args) ->
           let typ = get_type Helpers.find_client_value_type gen_num in
           let _loc = Ast.loc_of_expr expr in
           <:expr<
             Eliom_client.Syntax_helpers.register_client_closure
               $`int64:gen_num$
               (fun $Helpers.patt_tuple args$ ->
                  ($map_get_escaped_values expr$ : $typ$))
           >>)
        client_value_datas
    in
    let _loc = Loc.ghost in
    <:str_item< let () = $Ast.exSem_of_list registrations$; () >>

  let define_client_functions client_value_datas =
    let bindings =
      List.map
        (fun (gen_num, gen_id, expr, args) ->
           let patt =
             let _loc = Loc.ghost in
             <:patt< $lid:gen_id$ >>
           in
           let typ = get_type Helpers.find_client_value_type gen_num in
           let expr =
             let _loc = Loc.ghost in
             <:expr<
               fun $Helpers.patt_tuple args$ ->
                 ($expr$ : $typ$)
             >>
           in
           patt, expr)
        client_value_datas
    in
    let _loc = Loc.ghost in
    <:str_item< let $Ast.binding_of_pel bindings$ >>

  (* For injections *)

  let close_server_section loc =
    let _loc = Loc.ghost in
    <:str_item<
        let () =
          Eliom_client.Syntax_helpers.close_server_section
            $str:Pa_eliom_seed.id_of_string (Loc.file_name loc)$
    >>

  let open_client_section loc =
    let _loc = Loc.ghost in
    <:str_item<
        let () =
          Eliom_client.Syntax_helpers.open_client_section
            $str:Pa_eliom_seed.id_of_string (Loc.file_name loc)$
    >>

  (** Syntax extension *)

  let client_str_items loc items =
    Ast.stSem_of_list
      (open_client_section loc ::
       items)

  let server_str_items loc _ =
    Ast.stSem_of_list
      [ register_client_closures (flush_client_value_datas ());
        close_server_section loc; ]

  let shared_str_items loc items =
    let client_expr_data = flush_client_value_datas () in
    Ast.stSem_of_list
      (open_client_section loc ::
       register_client_closures client_expr_data ::
       define_client_functions client_expr_data ::
       items @
       [ close_server_section loc ])

  let client_value_expr typ context_level orig_expr gen_num gen_id loc =

    let escaped_bindings = flush_escaped_bindings () in

    push_client_value_data gen_num gen_id orig_expr
      (List.map fst escaped_bindings);

    match context_level with
      | `Server ->
          <:expr@loc< >>
      | `Shared ->
          let bindings =
            List.map
              (fun (gen_id, expr) ->
                 let _loc = Loc.ghost in
                 <:patt< $lid:gen_id$ >>, expr)
              escaped_bindings
          in
          let args =
            let _loc = Loc.ghost in
            Helpers.expr_tuple
              (List.map
                 (fun (gen_id, _) ->
                    <:expr< $lid:gen_id$ >>)
                 escaped_bindings)
          in
          <:expr@loc<
            let $Ast.binding_of_pel bindings$ in
            $lid:gen_id$ $args$
          >>

  let escape_inject context_level orig_expr gen_id =
    let open Pa_eliom_seed in
    let _loc = Ast.loc_of_expr orig_expr in
    match context_level with
      | Escaped_in_client_value_in section ->
          (* {section{ ... {{ ... %x ... }} ... }} *)
          let typ =
            drop_client_value_ctyp
              (get_type Helpers.find_escaped_ident_type gen_id)
          in
          push_escaped_binding gen_id orig_expr;
          <:expr< ($lid:gen_id$ : $typ$) >>
      | Injected_in _section ->
          (* {_section{ ... %x ... }} *)
          let typ =
            drop_client_value_ctyp
              (get_type Helpers.find_injected_ident_type gen_id)
          in
          <:expr<
            (Eliom_client.Syntax_helpers.get_injection $str:gen_id$ : $typ$)
          >>

  let implem sil = sil

end

module M = Pa_eliom_seed.Register(Id)(Client_pass)
