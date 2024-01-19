(** The paths to each eliom directories *)
module type ELIOM = sig
  val server_dir : Ocamlbuild_plugin.Pathname.t
  val type_dir : Ocamlbuild_plugin.Pathname.t
  val client_dir : Ocamlbuild_plugin.Pathname.t
end

module Make (_ : ELIOM) : sig
  val dispatcher :
     ?oasis_executables:Ocamlbuild_plugin.Pathname.t list
    -> Ocamlbuild_plugin.hook
    -> unit
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
  *)
end

(**/**)

module type INTERNALS = sig
  val with_eliom_ppx : ([< `Client | `Server] -> string) option
  (** Internally built client and server ppx executables are invoked explicitly
      in order to pass the right -type arguments. Other syntax extensions are
      handled by {!with_package} *)

  val with_package : string -> string
end

module MakeIntern (_ : INTERNALS) (_ : ELIOM) : sig
  val dispatcher :
     ?oasis_executables:Ocamlbuild_plugin.Pathname.t list
    -> Ocamlbuild_plugin.hook
    -> unit
end
