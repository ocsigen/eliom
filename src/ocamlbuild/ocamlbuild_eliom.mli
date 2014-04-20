module Make (Eliom : Ocamlbuild_eliom_core.ELIOM) : sig
  val dispatcher : Ocamlbuild_plugin.hook -> unit

  val dispatcher_with_oasis_support :
    executables:string list ->
    Ocamlbuild_plugin.hook ->
    unit
end
