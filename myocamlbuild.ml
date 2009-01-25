(* Ocsigen
 * http://www.ocsigen.org
 * Ocamlbuild plugin
 * Copyright (C) 2009 Stéphane Glondu
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, with linking
 * exception; either version 2.1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

open Ocamlbuild_plugin

let packages = ["unix"; "threads"; "ssl"; "findlib"; "dynlink"; "camlp4";
                "pcre"; "netstring"; "netsys"; "zip";
                "lwt"; "lwt.extra"; "lwt.ssl"]


(* Use ocamlfind/ocamlducefind wrapper. The wrapper will call
   ocamlducefind if a "-duce" flag is provided (and remove it), and
   will call ocamlfind otherwise. It must be compiled before the build
   process. *)
let ocamlfind x = S[A"./myocamlfind.byte"; A x]


let _ = dispatch begin function

  | Before_options ->
      Options.make_links := false;
      (* Override default commands by ocamlfind ones *)
      Options.ocamlc := ocamlfind "ocamlc";
      Options.ocamlopt := ocamlfind "ocamlopt";
      Options.ocamldep := ocamlfind "ocamldep";
      Options.ocamldoc := ocamlfind "ocamldoc";
      Options.ocamlmktop := ocamlfind "ocamlmktop";

  | After_rules ->
      (* Link packages in executables *)
      flag ["ocaml"; "link"; "program"] (A"-linkpkg");

      (* For each ocamlfind package one inject the -package option
         when compiling, computing dependencies, generating
         documentation and linking. *)
      List.iter begin fun pkg ->
        let spec = S[A"-package"; A pkg] and tag = "pkg_"^pkg in
        flag ["ocaml"; "compile"; tag] spec;
        flag ["ocaml"; "ocamldep"; tag] spec;
        flag ["ocaml"; "doc"; tag] spec;
        flag ["ocaml"; "link"; tag] spec;
      end packages;

      (* We don't use the built-in thread tag because of findlib *)
      flag ["ocaml"; "compile"] (A"-thread");
      flag ["ocaml"; "link"] (A"-thread");

      (* Compilation contexts *)
      let std = ["baselib"; "server"; "http"] in
      let xhtml = "xmlp4/ohl-xhtml" in
      let xml = "xmlp4/newocaml" in
      let xmlp4 = "xmlp4" in
      Pathname.define_context "baselib" ["baselib"; xml];
      Pathname.define_context "xmlp4" [xmlp4; xhtml; "http"];
      Pathname.define_context "server" (xml::std);
      Pathname.define_context "http" (xmlp4::xhtml::std);
      Pathname.define_context "extensions" ("extensions"::std);

      let cma = "server/server.cma" in
      let cmxa = "server/server.cmxa" in
      let server_main_cmo = "server/server_main.cmo" in
      let server_main_cmx = "server/server_main.cmx" in
      let cma_tag = ["file:"^cma] in
      let cmxa_tag = ["file:"^cmxa] in

      (* Handle C stubs *)
      let stubs = "server/libocsigen.a" in
      let cclib = [A"-cclib"; A"-locsigen"] in
      flag cma_tag (S (A"-dllib"::A"-locsigen"::cclib));
      flag cmxa_tag (S cclib);

      (* Additional dependencies for server.cma (aka ocsigen.cma) *)
      dep cma_tag ["extensions/localFiles.cmo"];
      dep cmxa_tag ["extensions/localFiles.cmx"];

      (* We want a strict control on what is linked in, so we don't
         rely on ocamlbuild's automatic rules here *)

      let ocsigen_byte = "server/ocsigen.byte" in
      begin rule ocsigen_byte
        ~deps:[stubs; cma; server_main_cmo]
        ~prod:ocsigen_byte
        (fun env build ->
           (* We don't want to use the findlib package here *)
           let camlp4 = [A"-I"; A"+camlp4"; A"camlp4lib.cma"] in
           (* Camlp4OCamlRevisedParser.cmo and Camlp4OCamlParser.cmo
              were also linked in the Makefiles... were they
              needed? *)
           Cmd (S[!Options.ocamlc;
                  T (tags_of_pathname ocsigen_byte ++ "link");
                  A"-I"; P"server";
                  A"-o"; P ocsigen_byte;
                  S camlp4;
                  P cma;
                  P server_main_cmo]));
      end;

      let ocsigen_native = "server/ocsigen.native" in
      begin rule ocsigen_native
        ~deps:[stubs; cmxa; server_main_cmx]
        ~prod:ocsigen_native
        (fun env build ->
           (* Same remarks as above apply *)
           let camlp4 = [A"-I"; A"+camlp4"; A"camlp4lib.cmxa"] in
           Cmd (S[!Options.ocamlopt;
                  T (tags_of_pathname ocsigen_native ++ "link");
                  A"-I"; P"server";
                  A"-o"; P ocsigen_native;
                  S camlp4;
                  P cmxa;
                  P server_main_cmx]));
      end;

      let ocsigen_top = "server/ocsigen.top" in
      begin rule ocsigen_top
        ~deps:[stubs; cma]
        ~prod:ocsigen_top
        (fun env build ->
           (* Same remarks as above apply *)
           let camlp4 = [A"-I"; A"+camlp4"; A"camlp4lib.cma"] in
           Cmd (S[!Options.ocamlmktop;
                  T (tags_of_pathname ocsigen_native ++ "link");
                  A"-I"; P"server";
                  A"-o"; P ocsigen_top;
                  S camlp4;
                  P cma]));
      end;

  | _ -> ()

end
