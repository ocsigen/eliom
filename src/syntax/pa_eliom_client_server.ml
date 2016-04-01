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
        <:expr< Eliom_syntax.escaped_value $arg$ >>
      in
      List.map aux res
    in
    push, flush

  let push_escaped_binding_nested,
      flush_escaped_bindings_nested =
    let arg_ids = ref [] in
    let arg_collection = ref [] in
    let push orig_expr gen_id =
      if not (List.mem gen_id !arg_ids) then begin
        arg_collection := (gen_id, orig_expr) :: !arg_collection;
        arg_ids := gen_id :: !arg_ids
      end
    and flush () =
      let res = List.rev !arg_collection
      and aux (_, arg) =
        let _loc = Ast.loc_of_expr arg in
        <:expr< Eliom_syntax.escaped_value $arg$ >>
      in
      arg_ids := [];
      arg_collection := [];
      List.map aux res
    in
    push, flush

  let push_injection, flush_injections =
    let module String_set = Set.Make (String) in
    let buffer : (_ * _ * _) list ref = ref [] in
    let gen_ids = ref String_set.empty in
    let push ?ident gen_id orig_expr =
      if not (String_set.mem gen_id !gen_ids) then
        (gen_ids := String_set.add gen_id !gen_ids;
         buffer := (gen_id, orig_expr,ident) :: !buffer)
    in
    let flush_all () =
      let res = List.rev !buffer in
      gen_ids := String_set.empty;
      buffer := [];
      res
    in
    let global_known = ref String_set.empty in
    let flush () =
      let all = flush_all () in
      let novel =
        let is_fresh (gen_id, _,_) =
          not (String_set.mem gen_id !global_known)
        in
        List.filter is_fresh all
      in
      List.iter
        (function gen_id, _, _ ->
           global_known := String_set.add gen_id !global_known)
        novel;
      all
    in
    push, flush

  (* For every injection of $orig_expr$ as $gen_id$:
     let $gen_id$ = $orig_expr$ and ...
     (Necessary for injections in shared section) *)
  let bind_injected_idents injections =
    let _loc = Loc.ghost in
    let bindings =
      List.map
        (fun (gen_id, orig_expr,_) ->
           <:patt< $lid:gen_id$ >>,
           orig_expr)
        injections
    in
    <:str_item< let $Ast.binding_of_pel bindings$ >>

  let close_server_section loc =
    let _loc = Loc.ghost in
    <:str_item<
        let () =
          Eliom_syntax.close_server_section
            $str:Helpers.file_hash loc$
    >>

  let close_client_section loc injections =
    let _loc = Loc.ghost in
    let injection_list =
      List.fold_right
        (fun (gen_id, expr, ident) sofar ->
           let loc1 = Ast.loc_of_expr expr in
           let loc1_expr = Helpers.position loc1 in
           let ident = match ident with
             | None -> <:expr<None>>
             | Some i -> <:expr< Some $str:i$>> in
           let num =
             string_of_int (snd (Helpers.get_injected_ident_info gen_id)) in
           <:expr< ($int:num$, Eliom_lib.to_poly $lid:gen_id$,
                    $loc1_expr$, $ident$) :: $sofar$ >>)
        injections <:expr< [] >>
    in
    <:str_item<
        let () =
          Eliom_syntax.close_client_section
            $str:Helpers.file_hash loc$
            $injection_list$
    >>


  (** Syntax extension *)

  let client_str_items loc _ =
    let all_injections = flush_injections () in
    Ast.stSem_of_list
      [bind_injected_idents all_injections;
       close_client_section loc all_injections]

  let server_str_items loc items =
    Ast.stSem_of_list
      (items @
       [ close_server_section loc ])

  let shared_str_items loc items =
    let all_injections = flush_injections () in
    Ast.stSem_of_list
      (bind_injected_idents all_injections ::
       items @
       [ close_server_section loc;
         close_client_section loc all_injections ])

  let client_value_expr typ context_level orig_expr gen_id _ loc =
    let typ =
      match typ with
      | Some typ -> typ
      | None ->
        if !notyp then
          let _loc = Loc.ghost in <:ctyp< _ >>
        else
          match Helpers.find_client_value_type gen_id with
          | Ast.TyQuo _ ->
            Helpers.raise_syntax_error loc
              "The types of client values must be monomorphic from its usage \
               or from its type annotation"
          | typ -> typ
    in
    let _loc = Ast.loc_of_expr orig_expr
    and l =
      match context_level with
      | `Shared_expr _ ->
        flush_escaped_bindings_nested ()
      | _ ->
        flush_escaped_bindings ()
    in
    <:expr@loc<
      (Eliom_syntax.client_value
         ~pos:($Helpers.position _loc$)
         $str:gen_id$ $Helpers.expr_tuple l$
       : $typ$ Eliom_client_value.t) >> ;;

  let shared_value_expr typ _ orig_expr gen_id _ loc =
    let typ =
      match typ with
      | Some typ -> typ
      | None ->
        if !notyp then
          let _loc = Loc.ghost in <:ctyp< _ >>
        else
          match Helpers.find_client_value_type gen_id with
          | Ast.TyQuo _ ->
            Helpers.raise_syntax_error loc
              "The types of shared values must be monomorphic from its usage \
               or from its type annotation"
          | typ -> typ
    in
    let _loc = Ast.loc_of_expr orig_expr in
    <:expr@loc<
    Eliom_shared.Value.create
      $orig_expr$
      (Eliom_syntax.client_value
        ~pos:($Helpers.position _loc$)
        $str:gen_id$
        $Helpers.expr_tuple (flush_escaped_bindings ())$
      : $typ$ Eliom_client_value.t)
    >>

  let escape_inject context_level ?ident orig_expr gen_id =
    let open Pa_eliom_seed in
    match context_level with
      | Escaped_in_client_value_in (`Shared_expr _) ->
          push_escaped_binding_nested orig_expr gen_id;
          let _loc = Loc.ghost in
          <:expr< >>
      | Escaped_in_shared_value_in _ ->
          push_escaped_binding orig_expr gen_id;
          orig_expr
      | Escaped_in_client_value_in _ ->
          push_escaped_binding orig_expr gen_id;
          let _loc = Loc.ghost in
          <:expr< >>
      | Injected_in _ ->
          push_injection ?ident gen_id orig_expr;
          let _loc = Ast.loc_of_expr orig_expr in
          <:expr< $lid:gen_id$ >>

  let implem loc sil =
    let _loc = Loc.ghost in
    let set_global b =
      <:str_item< let () = Eliom_syntax.set_global $`bool:b$ >>
    in
    set_global true :: sil @ [ set_global false ]

  let shared_sig_items _ items = Ast.sgSem_of_list items
  let server_sig_items _ items = Ast.sgSem_of_list items
  let client_sig_items _ items = Ast.sgSem_of_list []

end

module M = Pa_eliom_seed.Register(Id)(Server_pass)
