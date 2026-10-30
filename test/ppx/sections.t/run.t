Toplevel code goes to the server by default. Each pass keeps the code of its
sides: the server keeps the server and shared code, the client keeps the client
and shared code, and the type pass, which infers the types of what the server
sends to the client, keeps the server and shared code.

  $ ppx_eliom_server --impl sections.eliom
  let __eliom__compilation_unit_id__n26i2n = "n26i2n"
  let () = Eliom.Syntax.set_global true
  open struct  end
  let default = 0
  open struct  end
  open struct  end
  let in_shared = 2
  open struct  end
  let in_server = 3
  open struct  end
  open struct  end
  let let_shared = 5
  open struct  end
  let let_server = 6
  open struct  end
  open struct  end
  let after_shared_start = 8
  open struct  end
  let after_server_start = 9
  open struct  end
  let included = 10
  open struct  end
  let () = Eliom.Syntax.set_global false

  $ ppx_eliom_types --impl sections.eliom
  let __eliom__compilation_unit_id__n26i2n = "n26i2n"
  let default = 0
  let () = ()
  let () = ()
  let in_shared = 2
  let in_server = 3
  let () = ()
  let () = ()
  let let_shared = 5
  let let_server = 6
  let () = ()
  let () = ()
  let after_shared_start = 8
  let after_server_start = 9
  let () = ()
  let included = 10
  let () = ()

  $ ppx_eliom_client --impl sections.eliom
  let __eliom__compilation_unit_id__n26i2n = "n26i2n"
  let in_client = 1
  let in_shared = 2
  let let_client = 4
  let let_shared = 5
  let after_client_start = 7
  let after_shared_start = 8
  let included = 10
  module M = struct let in_module = 11 end

To compile Eliom itself, `-internal` refers to its modules without the `Eliom`
prefix:

  $ ppx_eliom_server -internal --impl internal.eliom
  let __eliom__compilation_unit_id__56fLrz = "56fLrz"
  let () = Syntax.set_global true
  open struct  end
  let x = 1
  open
    struct
      let _eliom_injected_ident_56fLrz1_f =
        let x = Stdlib.ref None in
        fun y -> if false then x := (Some y); (y : _  :> _)
    end
  let _eliom_injected_ident_56fLrz1 = x
  let () =
    Syntax.close_client_section __eliom__compilation_unit_id__56fLrz
      [(1,
         (Syntax.to_poly
            (_eliom_injected_ident_56fLrz1_f _eliom_injected_ident_56fLrz1)),
         ({
            Lexing.pos_fname = "internal.eliom";
            Lexing.pos_lnum = 2;
            Lexing.pos_bol = 17;
            Lexing.pos_cnum = 34
          },
           {
             Lexing.pos_fname = "internal.eliom";
             Lexing.pos_lnum = 2;
             Lexing.pos_bol = 17;
             Lexing.pos_cnum = 35
           }), (Some "x"))]
  let () = Syntax.set_global false

  $ ppx_eliom_client -internal --impl internal.eliom
  let __eliom__compilation_unit_id__56fLrz = "56fLrz"
  let () =
    Client_core.Syntax_helpers.open_client_section
      __eliom__compilation_unit_id__56fLrz
  let y =
    (Client_core.Syntax_helpers.get_injection ?ident:(Some "x")
       ~pos:({
               Lexing.pos_fname = "internal.eliom";
               Lexing.pos_lnum = 2;
               Lexing.pos_bol = 17;
               Lexing.pos_cnum = 34
             },
              {
                Lexing.pos_fname = "internal.eliom";
                Lexing.pos_lnum = 2;
                Lexing.pos_bol = 17;
                Lexing.pos_cnum = 35
              }) "56fLrz1" : _)
