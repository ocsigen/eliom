module type ELIOM = sig
  val server_dir : string
  val type_dir : string
  val client_dir : string
end

module Make (Eliom : ELIOM) : sig
  val dispatcher : Ocamlbuild_plugin.hook -> unit
end
