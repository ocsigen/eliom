open Ppxlib
open Ast_helper

open Ppx_eliom_utils

let attribute_of_warning loc s =
  Attr.mk
    {loc; txt = "ocaml.ppwarning" }
    (PStr ([Str.eval ~loc (Exp.constant (Const.string s))]))

module Pass = struct

  (** {2 Auxiliaries} *)

  (* Replace every escaped identifier [v] with
     [Eliom_client_core.Syntax_helpers.get_escaped_value v] *)
  let map_get_escaped_values expr =
    (object
       inherit Ppxlib.Ast_traverse.map as super
       method! expression e =
         match e.pexp_desc with
         | Pexp_ident {txt}
              when Mli.is_escaped_ident @@ Longident.last_exn txt ->
            let loc = e.pexp_loc in
            [%expr Eliom_client_core.Syntax_helpers.get_escaped_value [%e e] ]
         | _ -> super#expression e
     end)#expression expr

  let push_escaped_binding, flush_escaped_bindings =
    let server_arg_ids = ref [] in
    let push gen_id (expr : expression) get_type =
      match
        List.find_opt (fun (gen_id', _, _) -> gen_id.txt = gen_id'.txt)
          !server_arg_ids
      with
      | Some (_, _, typ) -> typ
      | None ->
         let typ = get_type () in
         server_arg_ids := (gen_id, expr, typ) :: !server_arg_ids;
         typ
    in
    let flush () =
      let res = List.rev !server_arg_ids in
      server_arg_ids := [];
      res
    in
    push, flush

  let mark_injection, flush_injection =
    let has_injection = ref false in
    let mark () = has_injection := true in
    let flush () =
      let x = !has_injection in
      has_injection := false ;
      x
    in
    mark, flush

  let push_client_value_data, flush_client_value_datas =
    let client_value_datas = ref [] in
    let push loc gen_num gen_id expr (args : string Location.loc list) =
      client_value_datas :=
        (loc, gen_num, gen_id, expr, args) :: !client_value_datas
    in
    let flush () =
      let res = List.rev !client_value_datas in
      client_value_datas := [];
      res
    in
    push, flush

  let find_escaped_ident loc id =
    if Mli.exists () then Mli.find_escaped_ident id
    else if Cmo.exists () then Cmo.find_escaped_ident loc
    else [%type: _]

  let find_injected_ident loc id =
    if Mli.exists () then Mli.find_injected_ident id
    else if Cmo.exists () then Cmo.find_injected_ident loc
    else [%type: _]

  let find_fragment loc id =
    if Mli.exists () then Mli.find_fragment id
    else if Cmo.exists () then Cmo.find_fragment loc
    else [%type: _]

  let register_client_closures client_value_datas =
    let registrations =
      List.map
        (fun (loc, num, id, expr, args) ->
           let typ = find_fragment loc id in
           let args = List.map Pat.var args in
           let loc = expr.pexp_loc in
           [%expr
             Eliom_client_core.Syntax_helpers.register_client_closure
               [%e str num]
               (fun [%p pat_args args] ->
                  ([%e map_get_escaped_values expr] : [%t typ]))
           ]
        )
        client_value_datas
    in
    match registrations with
    | [] -> []
    | _ -> [Str.eval (sequence registrations)]

  (* We hoist the body of client fragments to enforce the correct scoping:
     Identifiers declared earlier in the client section should not be
     visible inside the client fragment (unless via escaped value). *)
  let define_client_functions ~loc client_value_datas =
    match client_value_datas with
    | [] ->
      []
    | _ ->
      let bindings =
        List.map
          (fun (loc, _num, id, expr, args) ->
             let patt = Pat.var id in
             let typ = find_fragment loc id in
             let args = List.map Pat.var args in
             let expr =
               [%expr
                 fun [%p pat_args args] -> ([%e expr] : [%t typ])
               ]
             in
             Vb.mk ~loc patt expr)
          client_value_datas
      in
      [Str.value ~loc Nonrecursive bindings]

  (* For injections *)

  let close_server_section loc =
    [%stri
      let () =
        Eliom_client_core.Syntax_helpers.close_server_section
          [%e eid @@ id_file_hash loc]
    ]

  let may_close_server_section ~no_fragment item =
    if no_fragment
    then []
    else [close_server_section item.pstr_loc]


  let open_client_section loc =
    [%stri
      let () =
        Eliom_client_core.Syntax_helpers.open_client_section
          [%e eid @@ id_file_hash loc]
    ]

  let may_open_client_section loc =
    if flush_injection ()
    then [ open_client_section loc ]
    else []

  (** Syntax extension *)

  let client_str item =
    let loc = item.pstr_loc in
    may_open_client_section loc @
    [ item ]

  let server_str no_fragment item =
    register_client_closures (flush_client_value_datas ()) @
    may_close_server_section ~no_fragment item

  let shared_str no_fragment item =
    let loc = item.pstr_loc in
    let client_expr_data = flush_client_value_datas () in
    let op = may_open_client_section loc in
    op @
    register_client_closures client_expr_data @
    define_client_functions loc client_expr_data @
    [ item ] @
    may_close_server_section ~no_fragment:(no_fragment || op <> []) item

  let fragment ~loc ?typ ~context ~num ~id ~unsafe expr =

    let frag_eid = eid id in
    let escaped_bindings = flush_escaped_bindings () in

    begin match typ with
      | Some _ -> ()
      | None when not (Mli.exists () || Cmo.exists ()) -> ()
      | None ->
        match find_fragment loc id with
        | { ptyp_desc = Ptyp_var _ } when not unsafe ->
          Location.raise_errorf ~loc
            "The types of client values must be monomorphic from its usage \
             or from its type annotation"
        | _ -> ()
    end;

    push_client_value_data loc num id expr
      (List.map (fun (gen_id, _, _) -> gen_id) escaped_bindings);

    match context, escaped_bindings with
    | `Server, _ ->
      (* We are in a server fragment, this code should always be discarded. *)
      Exp.extension @@ Location.Error.to_extension @@
      Location.Error.make ~loc ~sub:[] "Eliom: ICE"
    | `Shared, [] ->
      [%expr [%e frag_eid] ()]
    | `Shared, _ ->
      let bindings =
        List.map
          (fun (gen_id, expr, _) ->
             Vb.mk ~loc:expr.pexp_loc (Pat.var gen_id) expr )
          escaped_bindings
      in
      let args =
        format_args @@ List.map
          (fun (id, _, _) -> eid id)
          escaped_bindings
      in
      Exp.let_ ~loc
        Nonrecursive
        bindings
        [%expr [%e frag_eid] [%e args]]


  let check_no_variable =
    (object
       inherit Ppxlib.Ast_traverse.map as super
       method! core_type typ =
         match typ with
         | {ptyp_desc = Ptyp_var _ ; ptyp_loc = loc} ->
            let attr =
              attribute_of_warning loc
                "The type of this injected value contains a type variable \
                 that could be wrongly inferred."
            in
            { typ with ptyp_attributes = attr :: typ.ptyp_attributes;
                       ptyp_loc = loc }
         | _ -> super#core_type typ
     end)#core_type

  let escape_inject
        ~loc:loc0 ?ident ~(context:Context.escape_inject) ~id ~unsafe expr =
    let loc = expr.pexp_loc in
    let frag_eid = eid id in

    let assert_no_variables t =
      if unsafe then t else check_no_variable t in

    match context with

    (* [%%server [%client ~%( ... ) ] ] *)
    | `Escaped_value _section ->
       let typ =
         push_escaped_binding id expr (fun () ->
             let typ = find_escaped_ident loc0 id in
             let typ = assert_no_variables typ in
             typ)
       in
       [%expr ([%e frag_eid] : [%t typ]) ]

    (* [%%server ... %x ... ] *)
    | `Injection _section ->
      mark_injection () ;
      let typ = find_injected_ident loc0 id in
      let typ = assert_no_variables typ in
      let ident = match ident with
        | None   -> [%expr None]
        | Some i -> [%expr Some [%e str i]]
      in
      let (u, d) = Mli.get_injected_ident_info id.txt in
      let es = str ~loc:id.loc (Printf.sprintf "%s%d" u d) in
      [%expr
        (Eliom_client_core.Syntax_helpers.get_injection
           ?ident:([%e ident])
           ~pos:([%e position loc])
           [%e es]
         : [%t typ])
      ]

  let shared_sig item = [item]
  let server_sig _    = []
  let client_sig item = [item]

  let prelude _ = []
  let postlude _ = []

end

include Make(Pass)

let () =
  Ppxlib.Driver.register_transformation
    ~impl:mapper#structure
    ~intf:mapper#signature
    "ppx_eliom_client"
