(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010-2011
 * Raphaël Proust, Grégoire Henry, Gabriel Radanne
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

open Parsetree
open Asttypes
open Ast_helper

module AC = Ast_convenience
module AM = Ast_mapper

open Ppx_eliom_utils

module Pass = struct

  let push_escaped_binding, flush_escaped_bindings =
    let args = ref [] in
    let push orig_expr id =
      if List.for_all (function id', _ -> id.txt <> id'.txt) !args then
        args := (id, orig_expr) :: !args;
    in
    let flush () =
      let res = List.rev !args in
      args := [];
      let aux (_, arg) =
        [%expr Eliom_service.Syntax_helpers.escaped_value [%e arg ] ]
        [@metaloc arg.pexp_loc]
      in
      List.map aux res
    in
    push, flush

  module SSet = Set.Make (String)

  let push_injection, flush_injections =
    let buffer : (_ * _ * _) list ref = ref [] in
    let gen_ids = ref SSet.empty in
    let push ?ident id orig_expr =
      if not (SSet.mem id !gen_ids) then
        (gen_ids := SSet.add id !gen_ids;
         buffer := (id, orig_expr,ident) :: !buffer)
    in
    let flush_all () =
      let res = List.rev !buffer in
      gen_ids := SSet.empty;
      buffer := [];
      res
    in
    let global_known = ref SSet.empty in
    let flush () =
      let all = flush_all () in
      let novel =
        let is_fresh (gen_id, _,_) =
          not (SSet.mem gen_id !global_known)
        in
        List.filter is_fresh all
      in
      List.iter
        (function gen_id, _, _ ->
           global_known := SSet.add gen_id !global_known)
        novel;
      all
    in
    push, flush

  (* For every injection of $orig_expr$ as $gen_id$:
     let $gen_id$ = $orig_expr$ and ...
     (Necessary for injections in shared section) *)
  let bind_injected_idents injections =
    let bindings =
      List.map
        (fun (txt, expr,_) ->
           let loc = expr.pexp_loc in
           Vb.mk ~loc (Pat.var ~loc {txt;loc}) expr)
        injections
    in
    Str.value Nonrecursive bindings

  let close_server_section loc =
    let s = id_of_loc loc in
    [%stri
      let () =
        Eliom_service.Syntax_helpers.close_server_section
          [%e Exp.constant (Const_string (s,None)) ]
    ] [@metaloc loc]

  let close_client_section loc injections =
    let injection_list =
      List.fold_right
        (fun (txt, expr, ident) sofar ->
           let loc = expr.pexp_loc in
           let loc_expr = position loc in
           let frag_eid = eid {txt;loc} in
           let ident = match ident with
             | None -> [%expr None]
             | Some i -> [%expr Some [%e AC.str i ]] in
           [%expr
             ([%e AC.str txt],
              (fun () -> Eliom_lib.to_poly [%e frag_eid ]),
              [%e loc_expr], [%e ident ]) :: [%e sofar ]
           ])
        injections
        [%expr []]
    in
    let s = id_of_loc loc in
    [%stri
      let () =
        Eliom_service.Syntax_helpers.close_client_section
          [%e AC.str s ]
          [%e injection_list ]
    ][@metaloc loc]


  (** Syntax extension *)

  let client_str item =
    let all_injections = flush_injections () in
    let loc = item.pstr_loc in
    [ bind_injected_idents all_injections;
      close_client_section loc all_injections
    ]

  let server_str item = [
    item ;
    close_server_section item.pstr_loc
  ]

  let shared_str item =
    let all_injections = flush_injections () in
    let loc = item.pstr_loc in
    [ bind_injected_idents all_injections ;
      item ;
      close_server_section loc ;
      close_client_section loc all_injections ;
    ]

  let fragment ?typ ~context:_ ~num ~id expr =
    let typ =
      match typ with
      | Some typ -> typ
      | None ->
        match Mli.find_fragment id with
        | { ptyp_desc = Ptyp_var _ } ->
          let loc = expr.pexp_loc in
          Typ.extension ~loc @@ AM.extension_of_error @@ Location.errorf ~loc
            "The types of client values must be monomorphic from its usage \
             or from its type annotation"
        | typ -> typ
    in
    let loc = expr.pexp_loc in
    let e = format_args @@ flush_escaped_bindings () in
    [%expr
      (Eliom_service.Syntax_helpers.client_value
         ~pos:([%e position loc ])
         [%e Exp.constant (Const_int64 num) ]
         [%e e ]
       : [%t typ ] Eliom_pervasives.client_value)
    ][@metaloc loc]

  let escape_inject ?ident ~(context:Context.escape_inject) ~id expr =
    match context with
    | `Escaped_value _ ->
      push_escaped_binding expr id;
      [%expr assert false ]
    | `Injection _ ->
      push_injection ?ident id.txt expr;
      eid id

  let set_global ~loc b =
    let b = Exp.construct ~loc
        {loc ; txt = Longident.Lident (if b then "true" else "false")} None
    in
    [%stri let () = Eliom_service.Syntax_helpers.set_global [%e b ] ]

  let prelude loc = [ set_global ~loc true ]
  let postlude loc = [ set_global ~loc false ]

  let shared_sig item = [item]
  let server_sig item = [item]
  let client_sig _    = []

end

include Make(Pass)
