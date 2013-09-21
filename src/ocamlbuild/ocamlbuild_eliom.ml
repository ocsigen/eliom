open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

module Make (Eliom : Ocamlbuild_eliom_core.ELIOM) = struct
  module M = Ocamlbuild_eliom_core.Make(Eliom)

  let dispatcher hook =
    Ocamlbuild_js_of_ocaml.dispatcher hook;
    M.dispatcher hook
end
