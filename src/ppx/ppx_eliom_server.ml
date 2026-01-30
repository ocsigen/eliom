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

open Ppxlib
open Ast_helper
open Ppx_eliom_utils

module Pass = struct
  let push_nongen_str_item, flush_nongen_str_item =
    let typing_strs = ref [] in
    let add ~fragment ~unsafe loc id =
      let typ =
        if fragment then [%type: _ Eliom_client_value.t] else [%type: _]
      in
      typing_strs :=
        (if unsafe || Mli.exists ()
         then [%stri let [%p Pat.var id] = fun y -> (y : [%t typ] :> [%t typ])]
         else
           [%stri
           let [%p Pat.var id] =
             let x = Stdlib.ref None in
             fun y ->
               if false then x := Some y;
               (y : [%t typ] :> [%t typ])])
        :: !typing_strs
    in
    let flush loc =
      let res = !typing_strs in
      typing_strs := [];
      Str.open_ ~loc (Opn.mk ~loc (Mod.structure ~loc res))
    in
    add, flush

  let one_char_location loc =
    { loc with
      Location.loc_end =
        { loc.Location.loc_start with
          pos_cnum = loc.Location.loc_start.Lexing.pos_cnum + 1 } }

  let push_escaped_binding, flush_escaped_bindings =
    let args = ref [] in
    let push loc orig_expr id ~unsafe =
      if List.for_all (function _, id', _, _ -> id.txt <> id'.txt) !args
      then args := (loc, id, orig_expr, unsafe) :: !args
    in
    let flush () =
      let res = List.rev !args in
      args := [];
      let aux (loc, id, arg, unsafe) =
        push_nongen_str_item ~fragment:false ~unsafe loc id;
        [%expr
          Eliom_syntax.escaped_value
            [%e
              let loc = one_char_location loc in
              [%expr [%e eid id] [%e arg]]]]
      in
      List.map aux res
    in
    push, flush

  module SSet = Set.Make (String)

  let push_injection, flush_injections =
    let buffer : (_ * _ * _ * _ * _) list ref = ref [] in
    let gen_ids = ref SSet.empty in
    let push loc ?ident id ~unsafe orig_expr =
      if not (SSet.mem id !gen_ids)
      then (
        gen_ids := SSet.add id !gen_ids;
        buffer := (loc, id, orig_expr, ident, unsafe) :: !buffer)
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
        let is_fresh (_, gen_id, _, _, _) =
          not (SSet.mem gen_id !global_known)
        in
        List.filter is_fresh all
      in
      List.iter
        (function
          | _, gen_id, _, _, _ -> global_known := SSet.add gen_id !global_known)
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
        (fun (_, txt, expr, _, _) ->
           let loc = expr.pexp_loc in
           Vb.mk ~loc (Pat.var ~loc {txt; loc}) expr)
        injections
    in
    Str.value Nonrecursive bindings

  let close_server_section loc =
    [%stri
    let () = Eliom_syntax.close_server_section [%e eid @@ id_file_hash loc]]

  let may_close_server_section ~no_fragment loc =
    if no_fragment then [] else [close_server_section loc]

  let close_client_section loc injections =
    assert (injections <> []);
    let injection_list =
      List.fold_right
        (fun (loc0, txt, expr, ident, unsafe) sofar ->
           let loc = expr.pexp_loc in
           let loc_expr = position loc in
           let frag_eid = eid {txt; loc} in
           let ident =
             match ident with
             | None -> [%expr None]
             | Some i -> [%expr Some [%e str i]]
           in
           let _, num = Mli.get_injected_ident_info txt in
           let f_id = {txt = txt ^ "_f"; loc} in
           push_nongen_str_item ~fragment:false ~unsafe loc f_id;
           [%expr
             ( [%e int num]
             , Eliom_lib.to_poly
                 [%e
                   let loc = one_char_location loc0 in
                   [%expr [%e eid f_id] [%e frag_eid]]]
             , [%e loc_expr]
             , [%e ident] )
             :: [%e sofar]])
        injections [%expr []]
    in
    [%stri
    let () =
      Eliom_syntax.close_client_section
        [%e eid @@ id_file_hash loc]
        [%e injection_list]]

  (** Syntax extension *)

  let client_str item =
    let all_injections = flush_injections () in
    let loc = item.pstr_loc in
    let str =
      match all_injections with
      | [] -> []
      | l -> [bind_injected_idents l; close_client_section loc all_injections]
    in
    flush_nongen_str_item loc :: str

  let server_str no_fragment item =
    let loc = item.pstr_loc in
    flush_nongen_str_item loc :: item
    :: may_close_server_section ~no_fragment loc

  let shared_str no_fragment item =
    let all_injections = flush_injections () in
    let loc = item.pstr_loc in
    let cl =
      item
      :: may_close_server_section
           ~no_fragment:(no_fragment || all_injections <> [])
           loc
    in
    let str =
      match all_injections with
      | [] -> cl
      | l ->
          (bind_injected_idents l :: cl)
          @ [close_client_section loc all_injections]
    in
    flush_nongen_str_item loc :: str

  let fragment ~loc ?typ ~context:_ ~num ~id ~unsafe _ =
    let typ = match typ with Some typ -> typ | None -> [%type: _] in
    let e = format_args @@ flush_escaped_bindings () in
    push_nongen_str_item ~fragment:true ~unsafe loc id;
    [%expr
      [%e eid id]
        [%e
          let loc = one_char_location loc in
          [%expr
            (Eliom_syntax.client_value ~pos:[%e position loc] [%e str num]
               [%e e]
             : [%t typ] Eliom_client_value.t)]]]

  let escape_inject
        ~loc
        ?ident
        ~(context : Context.escape_inject)
        ~id
        ~unsafe
        expr
    =
    match context with
    | `Escaped_value _ ->
        push_escaped_binding loc expr id ~unsafe;
        [%expr assert false]
    | `Injection _ ->
        push_injection loc ?ident id.txt ~unsafe expr;
        eid id

  let set_global ~loc b =
    let b =
      Exp.construct ~loc
        {loc; txt = Longident.Lident (if b then "true" else "false")}
        None
    in
    [%stri let () = Eliom_syntax.set_global [%e b]]

  let prelude loc = [set_global ~loc true]
  let postlude loc = [set_global ~loc false]
  let shared_sig item = [item]
  let server_sig item = [item]
  let client_sig _ = []
end

include Make (Pass)

let () =
  Ppxlib.Driver.register_transformation ~impl:mapper#structure
    ~intf:mapper#signature "ppx_eliom_server"
