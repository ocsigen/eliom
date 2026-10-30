Injections of server values in client code. The server sends the values of each
client section with the module; the client reads them back in the same order.

  $ ppx_eliom_server --impl injections.eliom
  let __eliom__compilation_unit_id__xxLZGv = "xxLZGv"
  let () = Eliom.Syntax.set_global true
  open struct  end
  let x = 1
  open struct  end
  let y = 2
  open
    struct
      let _eliom_injected_ident_xxLZGv1_f =
        let x = Stdlib.ref None in
        fun y -> if false then x := (Some y); (y : _  :> _)
      let _eliom_injected_ident_xxLZGv2_f =
        let x = Stdlib.ref None in
        fun y -> if false then x := (Some y); (y : _  :> _)
    end
  let _eliom_injected_ident_xxLZGv1 = x
  and _eliom_injected_ident_xxLZGv2 = y
  let () =
    Eliom.Syntax.close_client_section __eliom__compilation_unit_id__xxLZGv
      [(1,
         (Eliom.Syntax.to_poly
            (_eliom_injected_ident_xxLZGv1_f _eliom_injected_ident_xxLZGv1)),
         ({
            Lexing.pos_fname = "injections.eliom";
            Lexing.pos_lnum = 6;
            Lexing.pos_bol = 152;
            Lexing.pos_cnum = 179
          },
           {
             Lexing.pos_fname = "injections.eliom";
             Lexing.pos_lnum = 6;
             Lexing.pos_bol = 152;
             Lexing.pos_cnum = 180
           }), (Some "x"));
      (2,
        (Eliom.Syntax.to_poly
           (_eliom_injected_ident_xxLZGv2_f _eliom_injected_ident_xxLZGv2)),
        ({
           Lexing.pos_fname = "injections.eliom";
           Lexing.pos_lnum = 6;
           Lexing.pos_bol = 152;
           Lexing.pos_cnum = 185
         },
          {
            Lexing.pos_fname = "injections.eliom";
            Lexing.pos_lnum = 6;
            Lexing.pos_bol = 152;
            Lexing.pos_cnum = 186
          }), (Some "y"))]
  open
    struct
      let _eliom_injected_ident_xxLZGv3_f =
        let x = Stdlib.ref None in
        fun y -> if false then x := (Some y); (y : _  :> _)
    end
  let _eliom_injected_ident_xxLZGv3 = x * 2
  let () =
    Eliom.Syntax.close_client_section __eliom__compilation_unit_id__xxLZGv
      [(3,
         (Eliom.Syntax.to_poly
            (_eliom_injected_ident_xxLZGv3_f _eliom_injected_ident_xxLZGv3)),
         ({
            Lexing.pos_fname = "injections.eliom";
            Lexing.pos_lnum = 9;
            Lexing.pos_bol = 231;
            Lexing.pos_cnum = 251
          },
           {
             Lexing.pos_fname = "injections.eliom";
             Lexing.pos_lnum = 9;
             Lexing.pos_bol = 231;
             Lexing.pos_cnum = 258
           }), None)]
  open
    struct
      let _eliom_injected_ident_xxLZGv4_f =
        let x = Stdlib.ref None in
        fun y -> if false then x := (Some y); (y : _  :> _)
    end
  let _eliom_injected_ident_xxLZGv4 = Stdlib.max_int
  let () =
    Eliom.Syntax.close_client_section __eliom__compilation_unit_id__xxLZGv
      [(4,
         (Eliom.Syntax.to_poly
            (_eliom_injected_ident_xxLZGv4_f _eliom_injected_ident_xxLZGv4)),
         ({
            Lexing.pos_fname = "injections.eliom";
            Lexing.pos_lnum = 12;
            Lexing.pos_bol = 322;
            Lexing.pos_cnum = 347
          },
           {
             Lexing.pos_fname = "injections.eliom";
             Lexing.pos_lnum = 12;
             Lexing.pos_bol = 322;
             Lexing.pos_cnum = 361
           }), (Some "Stdlib_max_int"))]
  open
    struct
      let _eliom_injected_ident_xxLZGv5_f =
        let x = Stdlib.ref None in
        fun y -> if false then x := (Some y); (y : _  :> _)
    end
  let _eliom_injected_ident_xxLZGv5 = x
  let from_shared = _eliom_injected_ident_xxLZGv5 + 1
  let () =
    Eliom.Syntax.close_client_section __eliom__compilation_unit_id__xxLZGv
      [(5,
         (Eliom.Syntax.to_poly
            (_eliom_injected_ident_xxLZGv5_f _eliom_injected_ident_xxLZGv5)),
         ({
            Lexing.pos_fname = "injections.eliom";
            Lexing.pos_lnum = 15;
            Lexing.pos_bol = 424;
            Lexing.pos_cnum = 451
          },
           {
             Lexing.pos_fname = "injections.eliom";
             Lexing.pos_lnum = 15;
             Lexing.pos_bol = 424;
             Lexing.pos_cnum = 452
           }), (Some "x"))]
  let () = Eliom.Syntax.set_global false

  $ ppx_eliom_types --impl injections.eliom
  let __eliom__compilation_unit_id__xxLZGv = "xxLZGv"
  let x = 1
  let y = 2
  let _eliom_injected_ident_xxLZGv2 = Stdlib.ref None
  let _eliom_injected_ident_xxLZGv1 = Stdlib.ref None
  let () =
    _eliom_injected_ident_xxLZGv1 := (Some x);
    _eliom_injected_ident_xxLZGv2 := (Some y)
  let _eliom_injected_ident_xxLZGv3 = Stdlib.ref None
  let () = _eliom_injected_ident_xxLZGv3 := (Some (x * 2))
  let _eliom_injected_ident_xxLZGv4 = Stdlib.ref None
  let () = _eliom_injected_ident_xxLZGv4 := (Some Stdlib.max_int)
  let _eliom_injected_ident_xxLZGv5 = Stdlib.ref None
  let () = _eliom_injected_ident_xxLZGv5 := (Some x)
  let from_shared = x + 1

  $ ppx_eliom_client --impl injections.eliom
  let __eliom__compilation_unit_id__xxLZGv = "xxLZGv"
  let () =
    Eliom.Client_core.Syntax_helpers.open_client_section
      __eliom__compilation_unit_id__xxLZGv
  let from_client =
    ((Eliom.Client_core.Syntax_helpers.get_injection ?ident:(Some "x")
        ~pos:({
                Lexing.pos_fname = "injections.eliom";
                Lexing.pos_lnum = 6;
                Lexing.pos_bol = 152;
                Lexing.pos_cnum = 179
              },
               {
                 Lexing.pos_fname = "injections.eliom";
                 Lexing.pos_lnum = 6;
                 Lexing.pos_bol = 152;
                 Lexing.pos_cnum = 180
               }) "xxLZGv1" : _)
       +
       (Eliom.Client_core.Syntax_helpers.get_injection ?ident:(Some "y")
          ~pos:({
                  Lexing.pos_fname = "injections.eliom";
                  Lexing.pos_lnum = 6;
                  Lexing.pos_bol = 152;
                  Lexing.pos_cnum = 185
                },
                 {
                   Lexing.pos_fname = "injections.eliom";
                   Lexing.pos_lnum = 6;
                   Lexing.pos_bol = 152;
                   Lexing.pos_cnum = 186
                 }) "xxLZGv2" : _))
      +
      (Eliom.Client_core.Syntax_helpers.get_injection ?ident:(Some "x")
         ~pos:({
                 Lexing.pos_fname = "injections.eliom";
                 Lexing.pos_lnum = 6;
                 Lexing.pos_bol = 152;
                 Lexing.pos_cnum = 191
               },
                {
                  Lexing.pos_fname = "injections.eliom";
                  Lexing.pos_lnum = 6;
                  Lexing.pos_bol = 152;
                  Lexing.pos_cnum = 192
                }) "xxLZGv1" : _)
  let () =
    Eliom.Client_core.Syntax_helpers.open_client_section
      __eliom__compilation_unit_id__xxLZGv
  let expr =
    (Eliom.Client_core.Syntax_helpers.get_injection ?ident:None
       ~pos:({
               Lexing.pos_fname = "injections.eliom";
               Lexing.pos_lnum = 9;
               Lexing.pos_bol = 231;
               Lexing.pos_cnum = 251
             },
              {
                Lexing.pos_fname = "injections.eliom";
                Lexing.pos_lnum = 9;
                Lexing.pos_bol = 231;
                Lexing.pos_cnum = 258
              }) "xxLZGv3" : _)
  let () =
    Eliom.Client_core.Syntax_helpers.open_client_section
      __eliom__compilation_unit_id__xxLZGv
  let qualified =
    (Eliom.Client_core.Syntax_helpers.get_injection
       ?ident:(Some "Stdlib_max_int")
       ~pos:({
               Lexing.pos_fname = "injections.eliom";
               Lexing.pos_lnum = 12;
               Lexing.pos_bol = 322;
               Lexing.pos_cnum = 347
             },
              {
                Lexing.pos_fname = "injections.eliom";
                Lexing.pos_lnum = 12;
                Lexing.pos_bol = 322;
                Lexing.pos_cnum = 361
              }) "xxLZGv4" : _)
  let () =
    Eliom.Client_core.Syntax_helpers.open_client_section
      __eliom__compilation_unit_id__xxLZGv
  let from_shared =
    (Eliom.Client_core.Syntax_helpers.get_injection ?ident:(Some "x")
       ~pos:({
               Lexing.pos_fname = "injections.eliom";
               Lexing.pos_lnum = 15;
               Lexing.pos_bol = 424;
               Lexing.pos_cnum = 451
             },
              {
                Lexing.pos_fname = "injections.eliom";
                Lexing.pos_lnum = 15;
                Lexing.pos_bol = 424;
                Lexing.pos_cnum = 452
              }) "xxLZGv5" : _)
      + 1
