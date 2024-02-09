open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

(* Doc should move in its own ocamlbuild plugin *)

(* Compile the wiki version of the Ocamldoc.
   Thanks to Till Varoquaux on usenet:
   http://www.digipedia.pl/usenet/thread/14273/231/ *)

let ocamldoc_wiki tags deps docout docdir =
  let tags = tags -- "extension:html" in
  Ocamlbuild_pack.Ocaml_tools.ocamldoc_l_dir tags deps docout docdir

let ocamldoc_man tags deps docout docdir =
  let tags = tags (* -- "extension:html" *) in
  Ocamlbuild_pack.Ocaml_tools.ocamldoc_l_dir tags deps docout docdir

let init_wikidoc () =
  try
    let wikidoc_dir =
      let base =
        Ocamlbuild_pack.My_unix.run_and_read "ocamlfind query wikidoc"
      in
      String.sub base 0 (String.length base - 1)
    in
    Ocamlbuild_pack.Rule.rule
      "ocamldoc: document ocaml project odocl & *odoc -> wikidocdir"
      ~insert:`top ~prod:"%.wikidocdir/index.wiki"
      ~stamp:"%.wikidocdir/wiki.stamp" ~dep:"%.odocl"
      (Ocamlbuild_pack.Ocaml_tools.document_ocaml_project
         ~ocamldoc:ocamldoc_wiki "%.odocl" "%.wikidocdir/index.wiki"
         "%.wikidocdir");
    flag ["wikidoc"]
    & S [A "-colorize-code"; A "-i"; A wikidoc_dir; A "-g"; A "odoc_wiki.cma"];
    pflag ["wikidoc"] "subproject" (fun sub ->
      S [A "-passopt"; A "-subproject"; A sub])
  with Failure e -> ()
(* Silently fail if the package wikidoc isn't available *)

let init_mandoc () =
  Ocamlbuild_pack.Rule.rule
    "ocamldoc: document ocaml project odocl & *odoc -> mandocdir" ~insert:`top
    ~prod:"%.mandocdir/man.%(ext)" ~stamp:"%.mandocdir/man.%(ext).stamp"
    ~dep:"%.odocl"
    (Ocamlbuild_pack.Ocaml_tools.document_ocaml_project ~ocamldoc:ocamldoc_man
       "%.odocl" "%.mandocdir/man.%(ext)" "%.mandocdir");
  pflag ["apiref"] "man_ext" (fun ext ->
    S [A "-man-mini"; A "-man-section"; A ext; A "-man-suffix"; A ext])

let init () =
  init_wikidoc ();
  init_mandoc ();
  (* ocamldoc intro *)
  pflag_and_dep ["doc"] "with_intro" (fun f -> S [A "-intro"; P f])
