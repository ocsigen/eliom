open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

module Conf = struct
  let server_dir = "server"
  let client_dir = "client"
  let type_dir = "type_dir"
end

module Eliom_plugin = Ocamlbuild_eliom.Make (Conf)

let _ = dispatch Eliom_plugin.dispatcher

let _ =
  Options.make_links := false;
  Options.plugin := false;
  Options.use_ocamlfind := true;
  Ocamlbuild_unix_plugin.setup ();
  Ocamlbuild_pack.Main.main ()
