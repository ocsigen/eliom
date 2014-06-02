open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

module type ELIOM = sig
  val server_dir : Ocamlbuild_plugin.Pathname.t
  val type_dir : Ocamlbuild_plugin.Pathname.t
  val client_dir : Ocamlbuild_plugin.Pathname.t
end

module Make (Eliom : ELIOM) = struct
  let copy_with_header src prod =
    let contents = Pathname.read src in
    let header = Printf.sprintf "# 1 %S\n\n" src in
    (* we need an empty new line after the line directive
      because of camlp4 https://github.com/ocaml/camlp4/issues/39 *)
    Pack.Shell.mkdir_p (Filename.dirname prod);
    Echo ([header; contents], prod)

  let copy_rule_with_header f name ?(deps=[]) src prod =
    rule name ~deps:(src :: deps) ~prod
      (fun env _ ->
         let prod = env prod in
         let src = env src in
         f env (Pathname.dirname prod) (Pathname.basename prod) src prod;
         copy_with_header src prod
      )

  let flag_infer ~file ~name ~path =
    let type_inferred =
      Pathname.concat
        (Pathname.concat path Eliom.type_dir)
        (Pathname.update_extension "inferred.mli" name)
    in
    let file_tag = "file:" ^ file in
    let tags =
      [["ocaml"; "ocamldep"; file_tag];
       ["ocaml"; "compile"; file_tag];
       ["ocaml"; "infer_interface"; file_tag];
      ]
    in
    let f tags =
      flag tags (S [A "-ppopt"; A "-type"; A "-ppopt"; P type_inferred])
    in
    List.iter f tags;
    flag ["ocaml"; "doc"; file_tag] (S [A "-ppopt"; A "-notype"])

  let tag_file_inside_rule file tags =
    tag_file file tags;
    (* Workaround. See: http://caml.inria.fr/mantis/view.php?id=6186 *)
    Pack.Param_tags.partial_init (Tags.of_list tags)

  let get_syntaxes src =
    ["thread" ; "syntax(camlp4o)"]
    @ Tags.elements (tags_of_pathname src)

  let copy_rule_server =
    copy_rule_with_header
      (fun env dir name src file ->
         let path = env "%(path)" in
         tag_file_inside_rule file
           ( "pkg_pa_eliom_seed"
             :: "pkg_pa_eliom_client_server"
             :: get_syntaxes src
           );
         flag_infer ~file ~name ~path;
         Pathname.define_context dir [path];
         Pathname.define_context path [dir];
      )

  let copy_rule_client =
    copy_rule_with_header
      (fun env dir name src file ->
         let path = env "%(path)" in
         tag_file_inside_rule file
           ( "pkg_pa_eliom_seed"
             :: "pkg_pa_eliom_client_client"
             :: get_syntaxes src
           );
         flag_infer ~file ~name ~path;
         Pathname.define_context dir [path];
      )

  let copy_rule_type =
    copy_rule_with_header
      (fun env dir name src file ->
         let path = env "%(path)" in
         let server_dir = Pathname.concat path Eliom.server_dir in
         let server_file = Pathname.concat server_dir name in
         tag_file_inside_rule file
           ( "pkg_pa_eliom_seed"
             :: "pkg_pa_eliom_type_filter"
             :: get_syntaxes src
             @ Tags.elements (tags_of_pathname server_file)
           );
         Pathname.define_context dir [path; server_dir];
      )

  let init = function
    | After_rules ->
        copy_rule_server "*.eliom -> **/_server/*.ml"
          ~deps:["%(path)/" ^ Eliom.type_dir ^ "/%(file).inferred.mli"]
          "%(path)/%(file).eliom"
          ("%(path)/" ^ Eliom.server_dir ^ "/%(file:<*>).ml");
        copy_rule_server "*.eliomi -> **/_server/*.mli"
          "%(path)/%(file).eliomi"
          ("%(path)/" ^ Eliom.server_dir ^ "/%(file:<*>).mli");
        copy_rule_type "*.eliom -> **/_type/*.ml"
          "%(path)/%(file).eliom"
          ("%(path)/" ^ Eliom.type_dir ^ "/%(file:<*>).ml");
        copy_rule_client "*.eliom -> **/_client/*.ml"
          ~deps:["%(path)/" ^ Eliom.type_dir ^ "/%(file).inferred.mli"]
          "%(path)/%(file).eliom"
          ("%(path)/" ^ Eliom.client_dir ^ "/%(file:<*>).ml");
        copy_rule_client "*.eliomi -> **/_client/*.mli"
          "%(path)/%(file).eliomi"
          ("%(path)/" ^ Eliom.client_dir ^ "/%(file:<*>).mli");

        copy_rule_server "*.eliom -> _server/*.ml"
          ~deps:[Eliom.type_dir ^ "/%(file).inferred.mli"]
          "%(file).eliom" (Eliom.server_dir ^ "/%(file:<*>).ml");
        copy_rule_server "*.eliomi -> _server/*.mli"
          "%(file).eliomi" (Eliom.server_dir ^ "/%(file:<*>).mli");
        copy_rule_type "*.eliom -> _type/*.ml"
          "%(file).eliom" (Eliom.type_dir ^ "/%(file:<*>).ml");
        copy_rule_client "*.eliom -> _client/*.ml"
          ~deps:[Eliom.type_dir ^ "/%(file).inferred.mli"]
          "%(file).eliom" (Eliom.client_dir ^ "/%(file:<*>).ml");
        copy_rule_client "*.eliomi -> _client/*.mli"
          "%(file).eliomi" (Eliom.client_dir ^ "/%(file:<*>).mli");
    | _ -> ()

  let dispatcher ?oasis_executables hook =
    Ocamlbuild_js_of_ocaml.dispatcher ?oasis_executables hook;
    init hook
end

let link source dest =
  rule (Printf.sprintf "%s -> %s" source dest) ~dep:source ~prod:dest
    (fun env _ ->
       Cmd (S [A"ln"; A"-f";P (env source); P (env dest)]))

(* Compile the wiki version of the Ocamldoc.

   Thanks to Till Varoquaux on usenet:
   http://www.digipedia.pl/usenet/thread/14273/231/

*)

let ocamldoc_wiki tags deps docout docdir =
  let tags = tags -- "extension:html" in
  Ocamlbuild_pack.Ocaml_tools.ocamldoc_l_dir tags deps docout docdir

let ocamldoc_man tags deps docout docdir =
  let tags = tags (* -- "extension:html" *) in
  Ocamlbuild_pack.Ocaml_tools.ocamldoc_l_dir tags deps docout docdir



let init_wikidoc () =
  try
    let wikidoc_dir =
      let base = Ocamlbuild_pack.My_unix.run_and_read "ocamlfind query wikidoc" in
      String.sub base 0 (String.length base - 1)
    in

    Ocamlbuild_pack.Rule.rule
      "ocamldoc: document ocaml project odocl & *odoc -> wikidocdir"
      ~insert:`top
      ~prod:"%.wikidocdir/index.wiki"
      ~stamp:"%.wikidocdir/wiki.stamp"
      ~dep:"%.odocl"
      (Ocamlbuild_pack.Ocaml_tools.document_ocaml_project
         ~ocamldoc:ocamldoc_wiki
         "%.odocl" "%.wikidocdir/index.wiki" "%.wikidocdir");
    flag ["wikidoc"] & S[A"-i";A wikidoc_dir;A"-g";A"odoc_wiki.cma"]

  with Failure e -> () (* Silently fail if the package wikidoc isn't available *)

let init_mandoc () =
  Ocamlbuild_pack.Rule.rule
    "ocamldoc: document ocaml project odocl & *odoc -> mandocdir"
    ~insert:`top
    ~prod:"%.mandocdir/man.%(ext)"
    ~stamp:"%.mandocdir/man.%(ext).stamp"
      ~dep:"%.odocl"
      (Ocamlbuild_pack.Ocaml_tools.document_ocaml_project
         ~ocamldoc:ocamldoc_man
         "%.odocl" "%.mandocdir/man.%(ext)" "%.mandocdir");
    pflag ["apiref"] "man_ext" (fun ext -> S[A"-man-mini";A "-man-section"; A ext; A"-man-suffix";A ext])


let subst vars s =
  let buf = Buffer.create (String.length s) in
  let start = ref 0 in
  let last = ref 0 in
  let len = String.length s in
  while (!last < len - 4) do
    if not (s.[!last] = '%' && s.[!last + 1] = '%') then incr last else
      begin
        let start_subst = !last in
        let last_id = ref (!last + 2) in
        let stop = ref false in
        while (!last_id < len - 1 && not !stop) do
          if not (s.[!last_id] = '%' && s.[!last_id + 1] = '%') then begin
            if s.[!last_id] <> ' ' then (incr last_id) else
              (stop := true; last := !last_id)
          end else begin
            let id_start = start_subst + 2 in
            let id = String.sub s (id_start) (!last_id - id_start) in
            try
              let subst = List.assoc id vars in
              Buffer.add_substring buf s !start (start_subst - !start);
              Buffer.add_string buf subst;
              stop := true;
              start := !last_id + 2;
              last := !last_id + 2;
            with Not_found ->
              stop := true;
              last := !last_id
          end
        done
      end
  done;
  Buffer.add_substring buf s !start (len - !start);
  Buffer.contents buf

let subst_rule file args =
  rule file ~dep:(file^"p") ~prod:file
    (fun env build ->
       let ifile = env (file^"p") in
       let ic = open_in ifile in
       let ilen = in_channel_length ic in
       let content = String.create ilen in
       really_input ic content 0 ilen;
       let res = subst args content in
       Echo( [ res ], env file)
    )


module Conf = struct
  let server_dir = "server"
  let client_dir = "client"
  let type_dir = "type_dir"
end

module Eliom = Make(Conf)

let more_dispatch = function
  | After_rules ->

    Options.make_links:=false;

    init_wikidoc ();
    init_mandoc ();
    (* ocamldoc intro *)
    pflag_and_dep ["doc"] "with_intro" (fun f -> S [A "-intro"; P f]);

    pflag [ "ocaml"; "compile"] "I" (fun x -> S[A"-I"; A x]);
    pflag [ "ocaml"; "infer_interface"] "I" (fun x -> S[A"-I"; A x]);
    pflag [ "ocaml"; "doc"] "I" (fun x -> S[A"-I"; A x]);

    let open Eliom in
    (* add syntax extension *)
    let add_syntax name path =
      (* hack : not dep when "compile" to avoid the extension syntax to be link with binaries *)
      (* the dep with ocamldep make sure the extension syntax is compiled before *)
      flag ["ocaml";"compile";"pkg_"^name]  (S [A "-ppopt" ;P (path ^ name -.- "cmo") ]);
      flag_and_dep ["ocaml";"ocamldep";"pkg_"^name] (S [A "-ppopt" ;P (path ^ name -.- "cmo") ]);
      flag_and_dep ["ocaml";"infer_interface";"pkg_"^name] (S [A "-ppopt" ;P (path ^ name -.- "cmo") ]);
      flag_and_dep ["doc";"pkg_"^name] (S [A "-ppopt" ; A"-printer"; A"-ppopt";A "o";
                                           A "-ppopt" ; A"-parser"; A"-ppopt";A "o";
                                           A "-ppopt" ;P (path ^ name -.- "cmo") ])
    in

    add_syntax "pa_include" "src/syntax/";
    pflag ["compile"] "sig_inc" (fun d -> S[A"-ppopt";A"-sig-inc";A"-ppopt";A d]);
    pflag ["ocamldep"] "sig_inc" (fun d -> S[A"-ppopt";A"-sig-inc";A"-ppopt";A d]);
    pflag ["infer_interface"] "sig_inc" (fun d -> S[A"-ppopt";A"-sig-inc";A"-ppopt";A d]);
    pflag ["ocaml";"doc"] "sig_inc" (fun d -> S[A"-ppopt";A"-sig-inc";A"-ppopt";A d;]);

    add_syntax "pa_eliom_seed" "src/syntax/";
    add_syntax "pa_eliom_client_client" "src/syntax/";
    add_syntax "pa_eliom_client_server" "src/syntax/";
    add_syntax "pa_eliom_type_filter" "src/syntax/";

    let do_nothing _ _ _ _ _ = () in
    copy_rule_with_header do_nothing "client.ml -> .ml"
      "%(path)/%(file).client.ml" ("%(path)/" ^ Conf.client_dir ^ "/%(file:<*>).ml");
    copy_rule_with_header do_nothing "client.mli -> .mli"
      "%(path)/%(file).client.mli" ("%(path)/" ^ Conf.client_dir ^ "/%(file:<*>).mli");
    copy_rule_with_header do_nothing "common -> client.ml"
      "%(path)/common/%(file).ml" ("%(path)/" ^ Conf.client_dir ^ "/%(file:<*>).ml");
    copy_rule_with_header do_nothing "common -> client.mli"
      "%(path)/common/%(file).mli" ("%(path)/" ^ Conf.client_dir ^ "/%(file:<*>).mli");
    copy_rule_with_header do_nothing "server.ml -> .ml"
      "%(path)/%(file).server.ml" ("%(path)/" ^ Conf.server_dir ^ "/%(file:<*>).ml");
    copy_rule_with_header do_nothing "server.mli -> .mli"
      "%(path)/%(file).server.mli" ("%(path)/" ^ Conf.server_dir ^ "/%(file:<*>).mli");
    copy_rule_with_header do_nothing "common -> server.ml"
      "%(path)/common/%(file).ml" ("%(path)/" ^ Conf.server_dir ^ "/%(file:<*>).ml");
    copy_rule_with_header do_nothing "common -> server.mli"
      "%(path)/common/%(file).mli" ("%(path)/" ^ Conf.server_dir ^ "/%(file:<*>).mli");

    let link_exec f t =
      link (Printf.sprintf "src/tools/%s.byte" f)   (Printf.sprintf "src/tools/%s.byte" t);
      link (Printf.sprintf "src/tools/%s.native" f) (Printf.sprintf "src/tools/%s.native" t);
    in
    List.iter (link_exec "eliomc") [ "eliomopt";"eliomcp";"js_of_eliom"];
    link_exec "distillery" "eliom-distillery";

    (* let datadir = try Sys.getenv "DATADIR" with Not_found -> *)
    (*   failwith "DATADIR must be exported in your enviroment" in *)

    (* subst_rule "src/tools/config.ml" ["DATADIR",datadir]; *)

  | _ -> ()

let _ = dispatch (fun x ->
    Eliom.dispatcher x;
    more_dispatch x
  )
