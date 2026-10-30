Errors in the use of the Eliom syntax.

Sections are only allowed at toplevel:

  $ ppx_eliom_server --impl nested_section.eliom
  File "nested_section.eliom", line 2, characters 2-22:
  2 |   [%%client let x = 1]
        ^^^^^^^^^^^^^^^^^^^^
  Error: Sections are only allowed at toplevel.
  [1]
  $ ppx_eliom_client --impl nested_section.eliom
  File "nested_section.eliom", line 2, characters 2-22:
  2 |   [%%client let x = 1]
        ^^^^^^^^^^^^^^^^^^^^
  Error: Sections are only allowed at toplevel.
  [1]

The `.start` forms take no argument:

  $ ppx_eliom_server --impl start_argument.eliom
  let __eliom__compilation_unit_id__J85YE2 = "J85YE2"
  let () = Eliom.Syntax.set_global true
  [%%ocaml.error "The %%client.start extension doesn't accept arguments."]
  let () = Eliom.Syntax.set_global false
  $ ppx_eliom_client --impl start_argument.eliom
  let __eliom__compilation_unit_id__J85YE2 = "J85YE2"
  [%%ocaml.error "The %%client.start extension doesn't accept arguments."]

Injections are only allowed in client code:

  $ ppx_eliom_server --impl injection_in_server.eliom
  File "injection_in_server.eliom", line 1, characters 17-20:
  1 | let%server f x = ~%x
                       ^^^
  Error: The syntax ~% ... is not allowed inside server code.
  [1]
  $ ppx_eliom_client --impl injection_in_server.eliom
  File "injection_in_server.eliom", line 1, characters 17-20:
  1 | let%server f x = ~%x
                       ^^^
  Error: The syntax ~% ... is not allowed inside server code.
  [1]

Injections cannot be nested:

  $ ppx_eliom_server --impl nested_injection.eliom
  File "nested_injection.eliom", line 2, characters 17-22:
  2 | let%client y = ~%(~%x)
                       ^^^^^
  Error: The syntax ~% ... can not be nested.
  [1]
  $ ppx_eliom_client --impl nested_injection.eliom
  File "nested_injection.eliom", line 2, characters 17-22:
  2 | let%client y = ~%(~%x)
                       ^^^^^
  Error: The syntax ~% ... can not be nested.
  [1]

The following errors are in client code, which only the client pass checks.
Client code cannot create client values:

  $ ppx_eliom_client --impl fragment_in_client.eliom
  let __eliom__compilation_unit_id__f262wn = "f262wn"
  let c =
    [%ocaml.error
      "The syntax [%client ...] is not allowed inside client code."]
  let s =
    [%ocaml.error
      "The syntax [%shared ...] is not allowed inside client code."]

Client values cannot be nested:

  $ ppx_eliom_client --impl nested_fragment.eliom
  let __eliom__compilation_unit_id__Ps3760 = "Ps3760"
  ;;Eliom.Client_core.Syntax_helpers.register_client_closure "Ps37601"
      (fun () : _->
         [%ocaml.error "The syntax [%client ...] can not be nested."])
  let () =
    Eliom.Client_core.Syntax_helpers.close_server_section
      __eliom__compilation_unit_id__Ps3760
  ;;Eliom.Client_core.Syntax_helpers.register_client_closure "Ps37602"
      (fun () : _->
         [%ocaml.error "The syntax [%client.unsafe ...] can not be nested."])
  let () =
    Eliom.Client_core.Syntax_helpers.close_server_section
      __eliom__compilation_unit_id__Ps3760

In an interface, a section takes a signature, `[%%client: ...]`. Without the
colon, the error says that the section is not at toplevel, which is misleading:

  $ ppx_eliom_server --intf section_without_colon.eliomi
  File "section_without_colon.eliomi", lines 1-2, characters 0-12:
  1 | [%%client
  2 | val x : int]
  Error: Sections are only allowed at toplevel.
  [1]
  $ ppx_eliom_client --intf section_without_colon.eliomi
  File "section_without_colon.eliomi", lines 1-2, characters 0-12:
  1 | [%%client
  2 | val x : int]
  Error: Sections are only allowed at toplevel.
  [1]

An empty section in an interface is not reported, unlike in an implementation
(see error_empty_section.t):

  $ ppx_eliom_server --intf empty_section.eliomi
  val x : int
  $ ppx_eliom_client --intf empty_section.eliomi
