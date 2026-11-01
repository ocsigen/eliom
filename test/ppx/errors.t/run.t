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

  $ ppx_eliom_client --impl nested_fragment.eliom | grep -Eo 'The syntax [^"]*'
  The syntax [%client ...] can not be nested.
  The syntax [%client.unsafe ...] can not be nested.

In an interface, a section takes a signature, `[%%client: ...]`. The error says
so when the colon is missing:

  $ ppx_eliom_server --intf section_without_colon.eliomi
  [%%ocaml.error
    "In an interface, a section takes a signature: [%%client: ...]."]
  $ ppx_eliom_client --intf section_without_colon.eliomi
  [%%ocaml.error
    "In an interface, a section takes a signature: [%%client: ...]."]

An empty section without colon is reported as in an implementation:

  $ ppx_eliom_server --intf empty_section_without_colon.eliomi
  [%%ocaml.error "Empty [%%client] extension. Did you mean [%%client.start] ?"]
  val x : int
  $ ppx_eliom_client --intf empty_section_without_colon.eliomi
  [%%ocaml.error "Empty [%%client] extension. Did you mean [%%client.start] ?"]

An empty signature section, `[%%shared:]`, is not reported, unlike an empty
section in an implementation (see error_empty_section.t):

  $ ppx_eliom_server --intf empty_section.eliomi
  val x : int
  $ ppx_eliom_client --intf empty_section.eliomi
