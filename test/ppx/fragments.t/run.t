Client values. The server creates a reference to a closure registered by the
client, with the values sent from the server. The type pass binds each client
value and each sent value to a reference whose type is inferred on the server.
Without inferred types, the client uses `_`.

  $ ppx_eliom_server --impl fragments.eliom
  let __eliom__compilation_unit_id__16vYpk = "16vYpk"
  let () = Eliom.Syntax.set_global true
  open
    struct
      let _eliom_fragment_16vYpk1 =
        let x = Stdlib.ref None in
        fun y ->
          if false then x := (Some y);
          (y : _ Eliom.Client_value.t  :> _ Eliom.Client_value.t)
    end
  let simple () =
    _eliom_fragment_16vYpk1
      (Eliom.Syntax.client_value
         ~pos:({
                 Lexing.pos_fname = "fragments.eliom";
                 Lexing.pos_lnum = 2;
                 Lexing.pos_bol = 45;
                 Lexing.pos_cnum = 68
               },
                {
                  Lexing.pos_fname = "fragments.eliom";
                  Lexing.pos_lnum = 2;
                  Lexing.pos_bol = 45;
                  Lexing.pos_cnum = 69
                }) "16vYpk1" () : _ Eliom.Client_value.t)
  open
    struct
      let _eliom_fragment_16vYpk2 =
        let x = Stdlib.ref None in
        fun y ->
          if false then x := (Some y);
          (y : _ Eliom.Client_value.t  :> _ Eliom.Client_value.t)
    end
  let annotated () =
    _eliom_fragment_16vYpk2
      (Eliom.Syntax.client_value
         ~pos:({
                 Lexing.pos_fname = "fragments.eliom";
                 Lexing.pos_lnum = 5;
                 Lexing.pos_bol = 125;
                 Lexing.pos_cnum = 151
               },
                {
                  Lexing.pos_fname = "fragments.eliom";
                  Lexing.pos_lnum = 5;
                  Lexing.pos_bol = 125;
                  Lexing.pos_cnum = 152
                }) "16vYpk2" () : unit Eliom.Client_value.t)
  open
    struct
      let _eliom_fragment_16vYpk3 =
        let x = Stdlib.ref None in
        fun y ->
          if false then x := (Some y);
          (y : _ Eliom.Client_value.t  :> _ Eliom.Client_value.t)
      let _eliom_escaped_ident_2 =
        let x = Stdlib.ref None in
        fun y -> if false then x := (Some y); (y : _  :> _)
      let _eliom_escaped_ident_1 =
        let x = Stdlib.ref None in
        fun y -> if false then x := (Some y); (y : _  :> _)
    end
  let escaped x y =
    _eliom_fragment_16vYpk3
      (Eliom.Syntax.client_value
         ~pos:({
                 Lexing.pos_fname = "fragments.eliom";
                 Lexing.pos_lnum = 9;
                 Lexing.pos_bol = 287;
                 Lexing.pos_cnum = 312
               },
                {
                  Lexing.pos_fname = "fragments.eliom";
                  Lexing.pos_lnum = 9;
                  Lexing.pos_bol = 287;
                  Lexing.pos_cnum = 313
                }) "16vYpk3"
         ((Eliom.Syntax.escaped_value (_eliom_escaped_ident_1 x)),
           (Eliom.Syntax.escaped_value (_eliom_escaped_ident_2 y))) : _
                                                                      Eliom.Client_value.t)
  open
    struct
      let _eliom_fragment_16vYpk4 =
        let x = Stdlib.ref None in
        fun y ->
          if false then x := (Some y);
          (y : _ Eliom.Client_value.t  :> _ Eliom.Client_value.t)
      let _eliom_escaped_ident_3 =
        let x = Stdlib.ref None in
        fun y -> if false then x := (Some y); (y : _  :> _)
    end
  let escaped_expr x =
    _eliom_fragment_16vYpk4
      (Eliom.Syntax.client_value
         ~pos:({
                 Lexing.pos_fname = "fragments.eliom";
                 Lexing.pos_lnum = 12;
                 Lexing.pos_bol = 397;
                 Lexing.pos_cnum = 425
               },
                {
                  Lexing.pos_fname = "fragments.eliom";
                  Lexing.pos_lnum = 12;
                  Lexing.pos_bol = 397;
                  Lexing.pos_cnum = 426
                }) "16vYpk4"
         (Eliom.Syntax.escaped_value (_eliom_escaped_ident_3 (x + 1))) : 
      _ Eliom.Client_value.t)
  open
    struct
      let _eliom_fragment_16vYpk5 =
        let x = Stdlib.ref None in
        fun y ->
          if false then x := (Some y);
          (y : _ Eliom.Client_value.t  :> _ Eliom.Client_value.t)
      let _eliom_escaped_ident_4 =
        let x = Stdlib.ref None in
        fun y -> if false then x := (Some y); (y : _  :> _)
    end
  let from_shared x =
    _eliom_fragment_16vYpk5
      (Eliom.Syntax.client_value
         ~pos:({
                 Lexing.pos_fname = "fragments.eliom";
                 Lexing.pos_lnum = 16;
                 Lexing.pos_bol = 543;
                 Lexing.pos_cnum = 570
               },
                {
                  Lexing.pos_fname = "fragments.eliom";
                  Lexing.pos_lnum = 16;
                  Lexing.pos_bol = 543;
                  Lexing.pos_cnum = 571
                }) "16vYpk5"
         (Eliom.Syntax.escaped_value (_eliom_escaped_ident_4 x)) : _
                                                                     Eliom.Client_value.t)
  open
    struct
      let _eliom_fragment_16vYpk6 y =
        (y : _ Eliom.Client_value.t  :> _ Eliom.Client_value.t)
    end
  let unsafe () =
    _eliom_fragment_16vYpk6
      (Eliom.Syntax.client_value
         ~pos:({
                 Lexing.pos_fname = "fragments.eliom";
                 Lexing.pos_lnum = 19;
                 Lexing.pos_bol = 636;
                 Lexing.pos_cnum = 659
               },
                {
                  Lexing.pos_fname = "fragments.eliom";
                  Lexing.pos_lnum = 19;
                  Lexing.pos_bol = 636;
                  Lexing.pos_cnum = 660
                }) "16vYpk6" () : _ Eliom.Client_value.t)
  open
    struct
      let _eliom_fragment_16vYpk7 =
        let x = Stdlib.ref None in
        fun y ->
          if false then x := (Some y);
          (y : _ Eliom.Client_value.t  :> _ Eliom.Client_value.t)
      let _eliom_escaped_ident_5 =
        let x = Stdlib.ref None in
        fun y -> if false then x := (Some y); (y : _  :> _)
    end
  let shared_value x =
    Eliom.Shared.Value.create (x + 1)
      (_eliom_fragment_16vYpk7
         (Eliom.Syntax.client_value
            ~pos:({
                    Lexing.pos_fname = "fragments.eliom";
                    Lexing.pos_lnum = 22;
                    Lexing.pos_bol = 745;
                    Lexing.pos_cnum = 773
                  },
                   {
                     Lexing.pos_fname = "fragments.eliom";
                     Lexing.pos_lnum = 22;
                     Lexing.pos_bol = 745;
                     Lexing.pos_cnum = 774
                   }) "16vYpk7"
            (Eliom.Syntax.escaped_value (_eliom_escaped_ident_5 x)) : _
                                                                      Eliom.Client_value.t))
  open
    struct
      let _eliom_fragment_16vYpk8 =
        let x = Stdlib.ref None in
        fun y ->
          if false then x := (Some y);
          (y : _ Eliom.Client_value.t  :> _ Eliom.Client_value.t)
    end
  let at_toplevel =
    _eliom_fragment_16vYpk8
      (Eliom.Syntax.client_value
         ~pos:({
                 Lexing.pos_fname = "fragments.eliom";
                 Lexing.pos_lnum = 26;
                 Lexing.pos_bol = 896;
                 Lexing.pos_cnum = 921
               },
                {
                  Lexing.pos_fname = "fragments.eliom";
                  Lexing.pos_lnum = 26;
                  Lexing.pos_bol = 896;
                  Lexing.pos_cnum = 922
                }) "16vYpk8" () : _ Eliom.Client_value.t)
  let () =
    Eliom.Syntax.close_server_section __eliom__compilation_unit_id__16vYpk
  let () = Eliom.Syntax.set_global false

  $ ppx_eliom_types --impl fragments.eliom
  let __eliom__compilation_unit_id__16vYpk = "16vYpk"
  let _eliom_fragment_16vYpk1 = Stdlib.ref None
  let simple () =
    ();
    _eliom_fragment_16vYpk1 :=
      (Some (Eliom.Syntax.client_value "" 0 : _ Eliom.Client_value.t));
    (Stdlib.Option.get (!_eliom_fragment_16vYpk1) : _ Eliom.Client_value.t)
  let _eliom_fragment_16vYpk2 = Stdlib.ref None
  let annotated () =
    ();
    _eliom_fragment_16vYpk2 :=
      (Some (Eliom.Syntax.client_value "" 0 : unit Eliom.Client_value.t));
    (Stdlib.Option.get (!_eliom_fragment_16vYpk2) : _ Eliom.Client_value.t)
  let _eliom_fragment_16vYpk3 = Stdlib.ref None
  let _eliom_escaped_ident_2 = Stdlib.ref None
  let _eliom_escaped_ident_1 = Stdlib.ref None
  let escaped x y =
    (_eliom_escaped_ident_1 := (Some x); _eliom_escaped_ident_2 := (Some y));
    _eliom_fragment_16vYpk3 :=
      (Some (Eliom.Syntax.client_value "" 0 : _ Eliom.Client_value.t));
    (Stdlib.Option.get (!_eliom_fragment_16vYpk3) : _ Eliom.Client_value.t)
  let _eliom_fragment_16vYpk4 = Stdlib.ref None
  let _eliom_escaped_ident_3 = Stdlib.ref None
  let escaped_expr x =
    _eliom_escaped_ident_3 := (Some (x + 1));
    _eliom_fragment_16vYpk4 :=
      (Some (Eliom.Syntax.client_value "" 0 : _ Eliom.Client_value.t));
    (Stdlib.Option.get (!_eliom_fragment_16vYpk4) : _ Eliom.Client_value.t)
  let _eliom_fragment_16vYpk5 = Stdlib.ref None
  let _eliom_escaped_ident_4 = Stdlib.ref None
  let () = ()
  let from_shared x =
    _eliom_escaped_ident_4 := (Some x);
    _eliom_fragment_16vYpk5 :=
      (Some (Eliom.Syntax.client_value "" 0 : _ Eliom.Client_value.t));
    (Stdlib.Option.get (!_eliom_fragment_16vYpk5) : _ Eliom.Client_value.t)
  let _eliom_fragment_16vYpk6 = Stdlib.ref None
  let unsafe () =
    ();
    _eliom_fragment_16vYpk6 :=
      (Some (Eliom.Syntax.client_value "" 0 : _ Eliom.Client_value.t));
    (Stdlib.Option.get (!_eliom_fragment_16vYpk6) : _ Eliom.Client_value.t)
  let _eliom_fragment_16vYpk7 = Stdlib.ref None
  let _eliom_escaped_ident_5 = Stdlib.ref None
  let shared_value x =
    Eliom.Shared.Value.create (x + 1)
      (_eliom_escaped_ident_5 := (Some x);
       _eliom_fragment_16vYpk7 :=
         (Some (Eliom.Syntax.client_value "" 0 : _ Eliom.Client_value.t));
       (Stdlib.Option.get (!_eliom_fragment_16vYpk7) : _ Eliom.Client_value.t))
  let _eliom_fragment_16vYpk8 = Stdlib.ref None
  let at_toplevel =
    ();
    _eliom_fragment_16vYpk8 :=
      (Some (Eliom.Syntax.client_value "" 0 : _ Eliom.Client_value.t));
    (Stdlib.Option.get (!_eliom_fragment_16vYpk8) : _ Eliom.Client_value.t)

  $ ppx_eliom_client --impl fragments.eliom
  let __eliom__compilation_unit_id__16vYpk = "16vYpk"
  ;;Eliom.Client_core.Syntax_helpers.register_client_closure "16vYpk1"
      (fun () : _-> 1 + 1)
  ;;Eliom.Client_core.Syntax_helpers.register_client_closure "16vYpk2"
      (fun () : _-> print_endline "x")
  ;;Eliom.Client_core.Syntax_helpers.register_client_closure "16vYpk3"
      (fun (_eliom_escaped_ident_1, _eliom_escaped_ident_2) : _->
         ((Eliom.Client_core.Syntax_helpers.get_escaped_value
             _eliom_escaped_ident_1 : _)
            +
            (Eliom.Client_core.Syntax_helpers.get_escaped_value
               _eliom_escaped_ident_2 : _))
           +
           (Eliom.Client_core.Syntax_helpers.get_escaped_value
              _eliom_escaped_ident_1 : _))
  ;;Eliom.Client_core.Syntax_helpers.register_client_closure "16vYpk4"
      (fun _eliom_escaped_ident_3 : _->
         (Eliom.Client_core.Syntax_helpers.get_escaped_value
            _eliom_escaped_ident_3 : _)
           * 2)
  ;;Eliom.Client_core.Syntax_helpers.register_client_closure "16vYpk5"
      (fun _eliom_escaped_ident_4 : _->
         (Eliom.Client_core.Syntax_helpers.get_escaped_value
            _eliom_escaped_ident_4 : _)
           + 1)
  let _eliom_fragment_16vYpk5 _eliom_escaped_ident_4 : _=
    (_eliom_escaped_ident_4 : _) + 1
  let from_shared x =
    let _eliom_escaped_ident_4 = x in
    _eliom_fragment_16vYpk5 _eliom_escaped_ident_4
  ;;Eliom.Client_core.Syntax_helpers.register_client_closure "16vYpk6"
      (fun () : _-> fun x -> x)
  ;;Eliom.Client_core.Syntax_helpers.register_client_closure "16vYpk7"
      (fun _eliom_escaped_ident_5 : _->
         (Eliom.Client_core.Syntax_helpers.get_escaped_value
            _eliom_escaped_ident_5 : _)
           + 1)
  ;;Eliom.Client_core.Syntax_helpers.register_client_closure "16vYpk8"
      (fun () : _-> 3)
  let () =
    Eliom.Client_core.Syntax_helpers.close_server_section
      __eliom__compilation_unit_id__16vYpk
