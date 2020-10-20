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

open Migrate_parsetree
open Ast_408
open Parsetree
open Asttypes
open Ast_helper

module AC = Ast_convenience_408

open Ppx_eliom_utils

module Pass = struct

  let one_char_location loc =
    {loc
    with Location.loc_end =
           {loc.Location.loc_start
            with pos_cnum = loc.Location.loc_start.Lexing.pos_cnum + 1}}

  let push_escaped_binding, flush_escaped_bindings =
    let args = ref [] in
    let push loc orig_expr id =
      if List.for_all (function _, id', _ -> id.txt <> id'.txt) !args then
        args := (loc, id, orig_expr) :: !args;
    in
    let flush () =
      let res = List.rev !args in
      args := [];
      let aux (loc, id, arg) =
        [%expr Eliom_syntax.escaped_value
            [%e [%expr ((fun x -> x) [%e arg ])]
                [@metaloc one_char_location loc]]]
        [@metaloc loc]
      in
      List.map aux res
    in
    push, flush

  module SSet = Set.Make (String)

  let push_injection, flush_injections =
    let buffer : (_ * _ * _ * _) list ref = ref [] in
    let gen_ids = ref SSet.empty in
    let push loc ?ident id orig_expr =
      if not (SSet.mem id !gen_ids) then
        (gen_ids := SSet.add id !gen_ids;
         buffer := (loc, id, orig_expr,ident) :: !buffer)
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
        let is_fresh (_, gen_id, _,_) =
          not (SSet.mem gen_id !global_known)
        in
        List.filter is_fresh all
      in
      List.iter
        (function _, gen_id, _, _ ->
           global_known := SSet.add gen_id !global_known)
        novel;
      all
    in
    push, flush

  (* For every injection of $orig_expr$ as $gen_id$:
     let $gen_id$ = $orig_expr$ and ...
     (Necessary for injections in shared section) *)
  let bind_injected_idents injections =
    assert (injections <> []);
    let bindings =
      List.map
        (fun (_, txt, expr,_) ->
           let loc = expr.pexp_loc in
           Vb.mk ~loc (Pat.var ~loc {txt;loc}) expr)
        injections
    in
    Str.value Nonrecursive bindings

  let close_server_section loc =
    [%stri
      let () =
        Eliom_syntax.close_server_section
          [%e eid @@ id_file_hash loc]
    ] [@metaloc loc]

  let may_close_server_section ~no_fragment loc =
    if no_fragment
    then []
    else [close_server_section loc]


  let close_client_section loc injections =
    assert (injections <> []) ;
    let injection_list =
      List.fold_right
        (fun (loc0, txt, expr, ident) sofar ->
           let loc = expr.pexp_loc in
           let loc_expr = position loc in
           let frag_eid = eid {txt;loc} in
           let ident = match ident with
             | None -> [%expr None]
             | Some i -> [%expr Some [%e AC.str i ]] in
           let (_, num) = Mli.get_injected_ident_info txt in
           [%expr
             ([%e AC.int num],
              Eliom_lib.to_poly [%e
                                    [%expr (fun x -> x) [%e frag_eid ]]
                                    [@metaloc one_char_location loc0]
                ],
              [%e loc_expr], [%e ident ]) :: [%e sofar ]
           ])
        injections
        [%expr []]
    in
    [%stri
      let () =
        Eliom_syntax.close_client_section
          [%e eid @@ id_file_hash loc ]
          [%e injection_list ]
    ][@metaloc loc]


  (** Syntax extension *)

  let client_str item =
    let all_injections = flush_injections () in
    let loc = item.pstr_loc in
    match all_injections with
    | [] -> []
    | l  ->
      bind_injected_idents l ::
      [ close_client_section loc all_injections ]

  let server_str no_fragment item =
    let loc = item.pstr_loc in
    item ::
    may_close_server_section ~no_fragment loc

  let shared_str no_fragment item =
    let all_injections = flush_injections () in
    let loc = item.pstr_loc in
    let cl =
      item ::
      may_close_server_section ~no_fragment loc
    in
    match all_injections with
    | [] -> cl
    | l ->
      bind_injected_idents l ::
      cl @
      [ close_client_section loc all_injections ]

  let fragment ~loc ?typ ~context:_ ~num ~id _ =
    let typ =
      match typ with
      | Some typ -> typ
      | None -> [%type: _]
    in
    let e = format_args @@ flush_escaped_bindings () in
    [%expr
        (fun x -> (x : _ Eliom_client_value.t :> _ Eliom_client_value.t))
        [%e
            [%expr
                ( (Eliom_syntax.client_value
                     ~pos:([%e position loc ])
                     [%e AC.str num ]
                     [%e e ])
                  : [%t typ ] Eliom_client_value.t)
            ][@metaloc one_char_location loc]
        ]
    ][@metaloc loc]

  let escape_inject ~loc ?ident ~(context:Context.escape_inject) ~id expr =
    match context with
    | `Escaped_value _ ->
      push_escaped_binding loc expr id;
      [%expr assert false ]
    | `Injection _ ->
      push_injection loc ?ident id.txt expr;
      eid id

  let set_global ~loc b =
    let b = Exp.construct ~loc
        {loc ; txt = Longident.Lident (if b then "true" else "false")} None
    in
    [%stri let () = Eliom_syntax.set_global [%e b ] ]

  let prelude loc = [ set_global ~loc true ]
  let postlude loc = [ set_global ~loc false ]

  let shared_sig item = [item]
  let server_sig item = [item]
  let client_sig _    = []

end

include Make(Pass)

let () =
  Migrate_parsetree.Driver.register ~name:"ppx_eliom_server" ~args:driver_args
    Migrate_parsetree.Versions.ocaml_408 mapper
