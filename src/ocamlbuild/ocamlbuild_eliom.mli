
(** The main dispatcher

    It calls {!Ocamlbuild_js_of_ocaml.dispatcher} first, with the same
    parameters, and then initialize the plugin for eliom.

    The dispatcher should be used with {!Ocamlbuild_plugin.dispatch} as:
    [Ocamlbuild_plugin.dispatch Ocamlbuild_eliom.dispatcher]
    or if you use oasis it would look like:
    [Ocamlbuild_plugin.dispatch
       (fun hook ->
         dispatch_default hook;
         Ocamlbuild_js_of_ocaml.dispatcher
           ~oasis_executables:["src/yourprogram.byte"]
           hook;
       )
    ]

    [?oasis_executables] is the paths of the executables
    (having the .byte extension) you want to compile
    as a javascript executable. The former executables are still compiled.

    Side note: {!Ocamlbuild_plugin.dispatch} should be used only once as
    it record only one function for an ocamlbuild module.

    [?runtime] allows to use a custom version of the eliom runtime.
    Typically used with
    [expand_module ["src"] "Eliom_runtime"
      ["server.cmo"; "server.cmi" ; "client.cmo"; "client.cmi"]]
*)
val dispatcher :
  ?runtime:Ocamlbuild_plugin.Pathname.t list ->
  ?oasis_executables:Ocamlbuild_plugin.Pathname.t list ->
  Ocamlbuild_plugin.hook ->
  unit
