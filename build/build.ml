open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

let best = if Sys.command "command -v ocamlopt > /dev/null" = 0 then "native" else "byte"

module Conf = struct
  let server_dir = "server"
  let client_dir = "client"
  let type_dir = "type_dir"
end

module Intern = struct

  let with_eliom_ppx = Some begin function
    | `Client -> "src/ppx/ppx_eliom_client_ex." ^ best
    | `Server -> "src/ppx/ppx_eliom_server_ex." ^ best
  end

  let with_package = function
    | "eliom.ppx.type" -> "pkg_ppx_eliom_types"
    | "eliom.ppx.client"
    | "eliom.ppx.server"
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

    (* add syntax extension *)
    let add_syntax name path =
      let bytes_dep = Findlib.(link_flags_byte [query "bytes"]) in
      (* hack : not dep when "compile" to avoid the extension syntax to be link with binaries *)
      (* the dep with ocamldep make sure the extension syntax is compiled before *)
      flag ["ocaml";"compile";"pkg_"^name] (S [A "-ppx" ;P (path ^ name ^ "_ex." ^ best) ]);
      flag_and_dep ["ocaml";"ocamldep";"pkg_"^name] (S [A "-ppx" ;P (path ^ name ^ "_ex." ^ best ^ " -as-ppx") ]);
      flag_and_dep ["ocaml";"infer_interface";"pkg_"^name] (S [A "-ppx" ;P (path ^ name ^ "_ex." ^ best) ]);
      flag_and_dep ["doc";"pkg_"^name] (S [A "-ppx" ;P (path ^ name ^ "_ex." ^ best) ]) in

    add_syntax "ppx_eliom_utils" "src/ppx/";
    add_syntax "ppx_eliom_types" "src/ppx/";

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
