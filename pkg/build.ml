#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;
#use "filelist.ml"
let exts_syntax = [".cmo";".cmx";".cma";".cmxa";".cmxs";".a"]

let _ =
  list_to_file "src/lib/client/client.mllib" client_mllib;
  list_to_file "src/lib/server/server.mllib" server_mllib;
  list_to_file "src/lib/server/server.mldylib" server_mllib;
  list_to_file "src/lib/server/extensions/extensions.mllib" server_ext_mllib;
  list_to_file "src/lib/server/extensions/extensions.mldylib" server_ext_mllib;
  list_to_file "doc/client/api.odocl" client_api;
  list_to_file "doc/server/api.odocl" server_api

let spf = Printf.sprintf

let builder = `Other ("ocamlbuild -use-ocamlfind -plugin-tag \"package(js_of_ocaml.ocamlbuild)\"",
                      "_build")

let () =
  Pkg.describe "eliom" ~builder ([
    (* META *)
    Pkg.lib "pkg/META";

    (* MANPAGE *)
    Pkg.man ~dst:"man1/eliomc.1" "pkg/man/eliomc.1";
    Pkg.man ~dst:"man1/eliomcp.1" "pkg/man/eliomc.1";
    Pkg.man ~dst:"man1/eliomopt.1" "pkg/man/eliomc.1";
    Pkg.man ~dst:"man1/eliomdep.1" "pkg/man/eliomc.1";
    Pkg.man ~dst:"man1/js_of_eliom.1" "pkg/man/eliomc.1";
    Pkg.man ~dst:"man1/man1/eliom-distillery.1" "pkg/man/eliom-distillery.1";

    (* TOOLS *)
    Pkg.bin ~auto:true "src/tools/eliomc";
    Pkg.bin ~auto:true "src/tools/eliomcp";
    Pkg.bin ~auto:true "src/tools/eliomdep";
    Pkg.bin ~auto:true "src/tools/eliomopt";
    Pkg.bin ~auto:true "src/tools/js_of_eliom";
    Pkg.bin ~auto:true ~dst:"eliom-distillery" "src/tools/distillery";

    (* SYNTAXES *)
    Pkg.lib ~exts:exts_syntax ~dst:"syntax/pa_eliom_seed" "src/syntax/pa_eliom_seed";
    Pkg.lib ~exts:exts_syntax ~dst:"syntax/pa_eliom_client_client" "src/syntax/pa_eliom_client_client";
    Pkg.lib ~exts:exts_syntax ~dst:"syntax/pa_eliom_client_server" "src/syntax/pa_eliom_client_server";
    Pkg.lib ~exts:exts_syntax ~dst:"syntax/pa_eliom_type_filter" "src/syntax/pa_eliom_type_filter";

    Pkg.lib ~exts:Exts.module_library ~dst:"ocamlbuild/ocamlbuild_eliom" "src/ocamlbuild/ocamlbuild_eliom";

  ] @ (
    (* CLIENT LIBS *)
    Pkg.lib ~dst:"client/client" ~exts:[".cma"] "src/lib/client/client" ::
    Pkg.lib ~dst:"client/eliom_client_main.cmo" "src/lib/client/eliom_client_main.cmo" ::
    Pkg.lib ~dst:"client/eliom_client.js" "src/lib/client/eliom_client.js" ::
    List.map (fun x -> Pkg.lib ~dst:(spf "client/%s" x) (spf "src/lib/client/%s" x)) client_extra
  ) @ (
    (* SERVER LIBS *)
    Pkg.lib ~dst:"server/server" ~exts:Exts.library "src/lib/server/server" ::
    List.map (fun x -> Pkg.lib ~dst:(spf "server/%s" x) (spf "src/lib/server/%s" x)) server_extra
  ) @ (
    (* SERVER EXTENSIONS *)
    Pkg.lib ~dst:"server/extensions/extensions" ~exts:Exts.library "src/lib/server/extensions/extensions" ::
    List.map (fun x -> Pkg.lib ~dst:(spf "server/extensions/%s" x) (spf "src/lib/server/extensions/%s" x)) server_ext_extra
  ) @ [
    (* MISC *)

    Pkg.doc "README";
    Pkg.doc "CHANGES";
    Pkg.etc "pkg/etc/mime.types"
  ] @ (
    List.flatten (
      List.map (fun (name,files) ->
          List.map (fun file ->
              Pkg.lib ~dst:(spf "templates/%s/%s" name file) (spf "%s/%s/%s" templates_dir name file)
            ) files) templates_files )
  ))