module Make (Eliom : Ocamlbuild_eliom_core.ELIOM) : sig
  val dispatcher : Ocamlbuild_plugin.hook -> unit
end
