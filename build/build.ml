open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

module Conf = struct
  let server_dir = "server"
  let client_dir = "client"
  let type_dir = "type_dir"
end
module Intern = struct
  let with_package = function
    | "eliom.syntax.type" -> "pkg_pa_eliom_type_filter"
    | "eliom.syntax.client" -> "pkg_pa_eliom_client_client"
    | "eliom.syntax.server" -> "pkg_pa_eliom_client_server"
    | "eliom.syntax.predef"
    | "eliom.client"
    | "eliom.server" -> (* do noting in this case *) "pkg_dummy"
    | _ -> assert false
end

module Eliom_plugin = Ocamlbuild_eliom.MakeIntern(Intern)(Conf)


let _ = dispatch (fun x ->
  Eliom_plugin.dispatcher x;
  match x with
  | After_rules ->
    Doc.init ();
    Util.init ();

    (* add syntax extension *)
    let add_syntax ?needs name path =
      let _S l = match needs with
        | None -> S l
        | Some p -> S (A "-ppopt":: P p ::l) in
      (* hack : not dep when "compile" to avoid the extension syntax to be link with binaries *)
      (* the dep with ocamldep make sure the extension syntax is compiled before *)
      flag ["ocaml";"compile";"pkg_"^name]  (_S [A "-ppopt" ;P (path ^ name -.- "cmo") ]);
      flag_and_dep ["ocaml";"ocamldep";"pkg_"^name] (_S [A "-ppopt" ;P (path ^ name -.- "cmo") ]);
      flag_and_dep ["ocaml";"infer_interface";"pkg_"^name] (_S [A "-ppopt" ;P (path ^ name -.- "cmo") ]);
      flag_and_dep ["doc";"pkg_"^name] (_S [A "-ppopt" ; A"-printer"; A"-ppopt";A "o";
                                            A "-ppopt" ; A"-parser"; A"-ppopt";A "o";
                                            A "-ppopt" ;P (path ^ name -.- "cmo") ]) in

    add_syntax "pa_include" "src/syntax/";
    pflag ["compile"] "sig_inc" (fun d -> S[A"-ppopt";A"-sig-inc";A"-ppopt";A d]);
    pflag ["ocamldep"] "sig_inc" (fun d -> S[A"-ppopt";A"-sig-inc";A"-ppopt";A d]);
    pflag ["infer_interface"] "sig_inc" (fun d -> S[A"-ppopt";A"-sig-inc";A"-ppopt";A d]);
    pflag ["ocaml";"doc"] "sig_inc" (fun d -> S[A"-ppopt";A"-sig-inc";A"-ppopt";A d;]);

    add_syntax "pa_eliom_seed" "src/syntax/";
    let needs = "src/syntax/pa_eliom_seed.cmo" in
    add_syntax ~needs "pa_eliom_client_client" "src/syntax/";
    add_syntax ~needs "pa_eliom_client_server" "src/syntax/";
    add_syntax ~needs "pa_eliom_type_filter" "src/syntax/";

    (* copy rules *)
    Util.copy_rule_with_header "client.ml -> .ml"
      "%(path)/%(file).client.ml" ("%(path)/" ^ Conf.client_dir ^ "/%(file:<*>).ml");
    Util.copy_rule_with_header "client.mli -> .mli"
      "%(path)/%(file).client.mli" ("%(path)/" ^ Conf.client_dir ^ "/%(file:<*>).mli");
    Util.copy_rule_with_header "common -> client.ml"
      "%(path)/common/%(file).ml" ("%(path)/" ^ Conf.client_dir ^ "/%(file:<*>).ml");
    Util.copy_rule_with_header "common -> client.mli"
      "%(path)/common/%(file).mli" ("%(path)/" ^ Conf.client_dir ^ "/%(file:<*>).mli");
    Util.copy_rule_with_header "server.ml -> .ml"
      "%(path)/%(file).server.ml" ("%(path)/" ^ Conf.server_dir ^ "/%(file:<*>).ml");
    Util.copy_rule_with_header "server.mli -> .mli"
      "%(path)/%(file).server.mli" ("%(path)/" ^ Conf.server_dir ^ "/%(file:<*>).mli");
    Util.copy_rule_with_header "common -> server.ml"
      "%(path)/common/%(file).ml" ("%(path)/" ^ Conf.server_dir ^ "/%(file:<*>).ml");
    Util.copy_rule_with_header "common -> server.mli"
      "%(path)/common/%(file).mli" ("%(path)/" ^ Conf.server_dir ^ "/%(file:<*>).mli");

    (* link executable aliases *)
    let link_exec f t =
      Util.link (Printf.sprintf "src/tools/%s.byte" f)   (Printf.sprintf "src/tools/%s.byte" t);
      Util.link (Printf.sprintf "src/tools/%s.native" f) (Printf.sprintf "src/tools/%s.native" t);
    in
    List.iter (link_exec "eliomc") [ "eliomopt";"eliomcp";"js_of_eliom"];
    link_exec "distillery" "eliom-distillery";
  | _ -> ())

let _ =
  Options.make_links:=false;
  Options.plugin := false;
  Options.use_ocamlfind := true;
  Ocamlbuild_unix_plugin.setup ();
  Ocamlbuild_pack.Main.main ()
