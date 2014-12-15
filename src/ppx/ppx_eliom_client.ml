open Parsetree
open Asttypes
open Ast_helper

module C = Ast_convenience

module E = Ppx_eliom_seed


module Pass = struct

  (** {2 Auxiliaries} *)

  (* Replace every escaped identifier [v] with [Eliom_client.Syntax_helpers.get_escaped_value v] *)
  let map_get_escaped_values =
    let mapper =
      {Ast_mapper.default_mapper with
       expr = (fun mapper e ->
         match e.pexp_desc with
         | Pexp_ident str when E.is_escaped_ident_string str ->
           [%expr Eliom_client.Syntax_helpers.get_escaped_value [%e e] ]
         | _ -> e
       );
      }
    in
    fun expr -> mapper.expr mapper expr

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

  let register_client_closures client_value_datas =
    let registrations =
      List.map
        (fun (gen_num, _, expr, args) ->
           let typ = get_type Helpers.find_client_value_type gen_num in
           [%expr
             Eliom_client.Syntax_helpers.register_client_closure
               [%e Exp.constant @@ Const_int64 gen_num]
               (fun [%p Pat.tuple args] ->
                  ([%e map_get_escaped_values expr] : [%t typ]))
           ] [@metaloc expr.pexp_loc]
        )
        client_value_datas
    in
    [%str
      let () =
        [%e Ast.exSem_of_list registrations] ;
        ()
    ]

  let define_client_functions client_value_datas =
    let bindings =
      List.map
        (fun (gen_num, gen_id, expr, args) ->
           let patt = Pat.var gen_id in
           let typ = get_type Helpers.find_client_value_type gen_num in
           let expr =
             let _loc = Loc.ghost in
             [%expr
               fun [%p Pat.tuple args] -> ([%e expr] : [%t typ])
             ]
           in
           patt, expr)
        client_value_datas
    in
    Str.value Nonrecursive bindings

  (* For injections *)

  let close_server_section loc =
    [%stri
      let () =
        Eliom_client.Syntax_helpers.close_server_section
          [%e C.str @@ Pa_eliom_seed.id_of_string !Location.input_name]
    ][@metaloc loc]

  let open_client_section loc =
    [%stri
      let () =
        Eliom_client.Syntax_helpers.open_client_section
          [%e C.str @@ Pa_eliom_seed.id_of_string !Location.input_name]
    ][@metaloc loc]

  (** Syntax extension *)

  let client_str_items loc items =
    [ open_client_section loc ;
      item ;
    ]

  let server_str_items loc _ =
    [ register_client_closures (flush_client_value_datas ());
      close_server_section loc;
    ]

  let shared_str_items loc items =
    let client_expr_data = flush_client_value_datas () in
    [ open_client_section loc ;
      register_client_closures client_expr_data ;
      define_client_functions client_expr_data ;
      item ;
      close_server_section loc ;
    ]



  let client_value_expr ?typ ~context ~id orig_expr =

    let ident = E.gen_closure_escaped_ident id in

    let escaped_bindings = flush_escaped_bindings () in

    push_client_value_data gen_num gen_id orig_expr
      (List.map fst escaped_bindings);

    match context_level with
    | `Server -> None
    | `Shared ->
      let bindings =
        List.map
          (fun (gen_id, expr) -> (Pat.var gen_id , expr) )
          escaped_bindings
      in
      let args =
        Helpers.expr_tuple
          (List.map
             (fun (gen_id, _) -> Exp.ident gen_id)
             escaped_bindings
          )
      in
      let new_e =
        Exp.let_
          Nonrecursive
          bindings
          (Exp.apply gen_id args)
      in Some new_e


  let escape_inject ?ident ~context ~id orig_expr =
    let loc = orig_expr.pexp_loc in

    let eid = Exp.ident id in

    let assert_no_variables typ =
      let f = function
        | Ast.TyQuo _ as typ ->
            Printf.eprintf
              "%s: %s\n"
              (Loc.to_string _loc)
              ": Warning. The type of an injected value contains a type variable that could be wrongly infered (to be fixed in Eliom).";
            typ
        | typ -> typ
      in
      ignore ((Ast.map_ctyp f)#ctyp typ)
    in

    match context_level with

    (* {section{ ... {{ ... %x ... }} ... }} *)
    | Escaped_in_client_value_in section ->
      let typ =
        get_type Helpers.find_escaped_ident_type id
      in
      assert_no_variables typ;
      push_escaped_binding gen_id orig_expr;
      [%expr ([%e eid] : [%t typ]) ]


    (* {_section{ ... %x ... }} *)
    | Injected_in _section ->
      let typ =
        get_type Helpers.find_injected_ident_type id
      in
      assert_no_variables typ;
      let ident = match ident with
        | None   -> [%expr None]
        | Some i -> [%expr Some [%e C.evar i]]
      in
      let str_pos =
        Exp.str @@
        Format.asprintf "%a" Location.print loc
      in

      [%expr
        (Eliom_client.Syntax_helpers.get_injection
           ?ident:([%e ident])
           ~pos:([%e loc])
           [%e C.str id]
         : [%t typ])
      ]

  let implem _ sil = sil

  let shared_sig_items items = items
  let server_sig_items items = []
  let client_sig_items items = items

end
