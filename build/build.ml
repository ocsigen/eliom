open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

let _ = dispatch (fun x ->
  Ocamlbuild_eliom.dispatcher x;
  match x with
  | After_rules ->
    Doc.init ();

    let link source dest =
      rule (Printf.sprintf "%s -> %s" source dest) ~dep:source ~prod:dest
        (fun env _ -> Cmd (S [A"ln"; A"-f";P (env source); P (env dest)])) in

    (* link executable aliases *)
    let link_exec f t =
      link (Printf.sprintf "src/tools/%s.byte" f)   (Printf.sprintf "src/tools/%s.byte" t);
      link (Printf.sprintf "src/tools/%s.native" f) (Printf.sprintf "src/tools/%s.native" t);
    in
    List.iter (link_exec "eliomc") [ "eliomopt";"eliomcp";"js_of_eliom"];
    link_exec "distillery" "eliom-distillery";
  | _ -> ())

let _ =
  Options.make_links:=false;
  Options.plugin := false;
  Options.use_ocamlfind := true;
  Pack.Log.classic_display := true;
  Pack.Log.level := 3;
  Ocamlbuild_unix_plugin.setup ();
  Ocamlbuild_pack.Main.main ()
