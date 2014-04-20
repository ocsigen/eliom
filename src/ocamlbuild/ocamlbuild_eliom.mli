module type ELIOM = sig
  val server_dir : string
  val type_dir : string
  val client_dir : string
end

module Make (Eliom : ELIOM) : sig
  val dispatcher_without_js_of_ocaml_support : Ocamlbuild_plugin.hook -> unit

  val dispatcher : Ocamlbuild_plugin.hook -> unit

  val dispatcher_with_oasis_support :
    executables:string list ->
    Ocamlbuild_plugin.hook ->
    unit
end
