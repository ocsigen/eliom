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

    let link source dest =
      rule (Printf.sprintf "%s -> %s" source dest) ~dep:source ~prod:dest
        (fun env _ -> Cmd (S [A"ln"; A"-f";P (env source); P (env dest)])) in

    (* add I pflag *)
    pflag [ "ocaml"; "compile"] "I" (fun x -> S[A"-I"; A x]);
    pflag [ "ocaml"; "infer_interface"] "I" (fun x -> S[A"-I"; A x]);
    pflag [ "ocaml"; "doc"] "I" (fun x -> S[A"-I"; A x]);

    let ppopt = A "-ppopt" in
    let rec for_camlp4 = function
      | [] -> []
      | S l::t -> S (for_camlp4 l) :: for_camlp4 t
      | (T _ as h) :: t -> h :: for_camlp4 t
      | P s :: t -> ppopt :: Quote (A s) :: for_camlp4 t
      | h::t -> ppopt :: h :: for_camlp4 t
    in

    (* add syntax extension *)
    let add_syntax ?needs name path =
      let bytes_dep = Findlib.(link_flags_byte [query "bytes"]) in
      let _S l = match needs with
        | None -> S (for_camlp4 [bytes_dep] @ l)
        | Some p -> S (for_camlp4 [bytes_dep] @ ppopt :: P p ::l) in
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
  Ocamlbuild_unix_plugin.setup ();
  Ocamlbuild_pack.Main.main ()
