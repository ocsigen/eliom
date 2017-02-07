open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

let _ = dispatch Ocamlbuild_eliom.dispatcher

let _ =
  Options.make_links:=false;
  Options.plugin := false;
  Options.use_ocamlfind := true;
  Ocamlbuild_unix_plugin.setup ();
  Ocamlbuild_pack.Main.main ()
