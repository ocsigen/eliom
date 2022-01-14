open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

module type ELIOM = sig
  val server_dir : Ocamlbuild_plugin.Pathname.t
  val type_dir : Ocamlbuild_plugin.Pathname.t
  val client_dir : Ocamlbuild_plugin.Pathname.t
end

module type INTERNALS = sig
  val with_eliom_ppx : ([< `Client | `Server] -> string) option
  val with_package : string -> string
end
module MakeIntern (I : INTERNALS)(Eliom : ELIOM) = struct

  (* WARNING: if you change this, also change inferred_type_prefix in
     ppx/ppx_eliom_utils.ml and tools/eliomc.ml *)
  let inferred_type_prefix = "eliom_inferred_type_"

  let sed_rule name ~dep ~prod scripts =
    rule name ~dep ~prod
      (fun env _build ->
        let dep = env dep and prod = env prod in
        let script_args = List.map (fun script -> S[A"-e"; A script]) scripts in
        Cmd (S[A"sed"; S script_args; P dep; Sh">"; Px prod]))

  let copy_with_header src prod =
    let contents = Pathname.read src in
    (* we need an empty line to keep the comments : weird camlp4 *)
    let header = "# 0 \"" ^ src ^ "\"\n\n" in
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

  let syntaxes_p4 = [I.with_package "eliom.syntax.predef"]

  let no_extra_syntaxes = "no_extra_syntaxes"

  let eliom_ppx = "eliom_ppx"

  let use_ppx src =
    Tags.mem eliom_ppx (tags_of_pathname src)

  let tag_file_inside_rule file tags =
    tag_file file tags;
    Pack.Param_tags.partial_init "Eliom plugin" (Tags.of_list tags)

  let use_all_syntaxes src =
    if Filename.check_suffix src ".eliomi" then
      false
    else
      not (Tags.mem no_extra_syntaxes (tags_of_pathname src))

  let get_eliom_syntax_ppx = function
    | `Client ->
      "eliom.ppx.client"
    | `Server ->
      "eliom.ppx.server"
    | `Type ->
      "eliom.ppx.type"

  let get_syntaxes_p4 _ _eliom_syntax src =
    let s = if use_all_syntaxes src then syntaxes_p4 else [] in
    let s = if s = [] then [] else "thread" :: "syntax(camlp4o)" :: s in
    s @ Tags.elements (tags_of_pathname src)

  let get_syntaxes_ppx with_eliom_syntax eliom_syntax _src =
    if with_eliom_syntax then
      [I.with_package (get_eliom_syntax_ppx eliom_syntax)]
    else
      []

  let get_syntaxes with_eliom_syntax eliom_syntax src =
    (if use_ppx src then get_syntaxes_ppx else get_syntaxes_p4)
      with_eliom_syntax eliom_syntax src

  (* A variant of flag_and_dep which recurse into Quote. *)
  let dflag tags x =
    let rec aux = function
      | Quote x -> aux x
      | S xs -> List.iter aux xs
      | P path -> dep tags [path]
      | N | A _ | Sh _ | V _ | T _ | Px _ -> ()
    in
    aux x; flag tags x

  let flag_infer ~file ~name ~path eliom_syntax =
    let type_inferred =
      Pathname.concat
        (Pathname.concat path Eliom.type_dir)
        (Pathname.update_extension "inferred_gen.mli" name)
    in
    let ppflags, ppflags_notype =
      if use_ppx file then
        match I.with_eliom_ppx with
        | None ->
          let pkg = get_eliom_syntax_ppx eliom_syntax in
          (S [A"-ppxopt"; A (pkg ^ ",-type," ^ type_inferred)],
           S [A"-ppxopt"; A (pkg ^ ",-notype")])
        | Some f ->
          let ppx = f eliom_syntax in
          (S [A"-ppx"; Quote (S [P ppx; A"-type"; P type_inferred])],
           S [A"-ppx"; Quote (S [P ppx; A"-notype"])])
      else
        (S [A "-ppopt"; A "-type"; A "-ppopt"; P type_inferred],
         S [A "-ppopt"; A "-notype"])
    in
    let file_tag = "file:" ^ file in
    dflag ["ocaml"; "ocamldep";        file_tag] ppflags;
    dflag ["ocaml"; "compile";         file_tag] ppflags;
    dflag ["ocaml"; "infer_interface"; file_tag] ppflags;
    dflag ["ocaml"; "doc";             file_tag] ppflags_notype

  let ocamlfind_query pkg =
    let cmd = Printf.sprintf "ocamlfind query %s" (Filename.quote pkg) in
    Ocamlbuild_pack.My_unix.run_and_open cmd input_line

  let copy_rule_server ?(eliom=true) =
    copy_rule_with_header
      (fun env dir name src file ->
         let path = env "%(path)" in
         tag_file_inside_rule file
           ( I.with_package "eliom.server"
             :: get_syntaxes eliom `Server src
           );
         if eliom then flag_infer ~file ~name ~path `Server;
         dflag ["ocaml"; "compile"; "file:" ^ file]
           (S [A "-I";
               A (ocamlfind_query "js_of_ocaml")]);
         Pathname.define_context dir [path];
         Pathname.define_context path [dir];
      )

  let copy_rule_client ?(eliom=true) =
    copy_rule_with_header
      (fun env dir name src file ->
         let path = env "%(path)" in
         tag_file_inside_rule file
           ( I.with_package "eliom.client"
             :: get_syntaxes eliom `Client src
           );
         if eliom then flag_infer ~file ~name ~path `Client;
         Pathname.define_context dir [path];
      )

  let copy_rule_type =
    copy_rule_with_header
      (fun env dir name src file ->
         let path = env "%(path)" in
         let server_dir = Pathname.concat path Eliom.server_dir in
         let server_file = Pathname.concat server_dir name in
         tag_file_inside_rule file
           ( I.with_package "eliom.server"
             :: get_syntaxes true `Type src
             @ Tags.elements (tags_of_pathname server_file)
           );
         Pathname.define_context dir [path; server_dir];
      )

  let init = function
    | After_rules ->
        mark_tag_used no_extra_syntaxes;
        mark_tag_used eliom_ppx;

        sed_rule ".inferred.mli -> .inferred_gen.mli"
          ~dep:"%(path)/%(file).inferred.mli"
          ~prod:"%(path)/%(file).inferred_gen.mli"
          ["s$/[1-9][0-9]*$$g";
           "s/_\\[\\([<>]\\)/[\\1/g";
           Printf.sprintf "s/'\\(_[a-z0-9_]*\\)/'%s\\1/g" inferred_type_prefix];

        (* eliom files *)
        copy_rule_server "*.eliom -> **/_server/*.ml"
          ~deps:["%(path)/" ^ Eliom.type_dir ^ "/%(file).inferred_gen.mli"]
          "%(path)/%(file).eliom"
          ("%(path)/" ^ Eliom.server_dir ^ "/%(file:<*>).ml");
        copy_rule_server "*.eliomi -> **/_server/*.mli"
          "%(path)/%(file).eliomi"
          ("%(path)/" ^ Eliom.server_dir ^ "/%(file:<*>).mli");
        copy_rule_type "*.eliom -> **/_type/*.ml"
          "%(path)/%(file).eliom"
          ("%(path)/" ^ Eliom.type_dir ^ "/%(file:<*>).ml");
        copy_rule_client "*.eliom -> **/_client/*.ml"
          ~deps:["%(path)/" ^ Eliom.type_dir ^ "/%(file).inferred_gen.mli"]
          "%(path)/%(file).eliom"
          ("%(path)/" ^ Eliom.client_dir ^ "/%(file:<*>).ml");
        copy_rule_client "*.eliomi -> **/_client/*.mli"
          "%(path)/%(file).eliomi"
          ("%(path)/" ^ Eliom.client_dir ^ "/%(file:<*>).mli");

        copy_rule_server "*.eliom -> _server/*.ml"
          ~deps:[Eliom.type_dir ^ "/%(file).inferred_gen.mli"]
          "%(file).eliom" (Eliom.server_dir ^ "/%(file:<*>).ml");
        copy_rule_server "*.eliomi -> _server/*.mli"
          "%(file).eliomi" (Eliom.server_dir ^ "/%(file:<*>).mli");
        copy_rule_type "*.eliom -> _type/*.ml"
          "%(file).eliom" (Eliom.type_dir ^ "/%(file:<*>).ml");
        copy_rule_client "*.eliom -> _client/*.ml"
          ~deps:[Eliom.type_dir ^ "/%(file).inferred_gen.mli"]
          "%(file).eliom" (Eliom.client_dir ^ "/%(file:<*>).ml");
        copy_rule_client "*.eliomi -> _client/*.mli"
          "%(file).eliomi" (Eliom.client_dir ^ "/%(file:<*>).mli");

        (* copy {shared,client,server}.ml rules *)
        copy_rule_client ~eliom:false "client.ml -> .ml"
          "%(path)/%(file).client.ml" ("%(path)/" ^ Eliom.client_dir ^ "/%(file:<*>).ml");
        copy_rule_client ~eliom:false "client.mli -> .mli"
          "%(path)/%(file).client.mli" ("%(path)/" ^ Eliom.client_dir ^ "/%(file:<*>).mli");
        copy_rule_client ~eliom:false "shared.ml -> client.ml"
          "%(path)/%(file).shared.ml" ("%(path)/" ^ Eliom.client_dir ^ "/%(file:<*>).ml");
        copy_rule_client ~eliom:false "shared -> client.mli"
          "%(path)/%(file).shared.mli" ("%(path)/" ^ Eliom.client_dir ^ "/%(file:<*>).mli");
        copy_rule_server ~eliom:false "server.ml -> .ml"
          "%(path)/%(file).server.ml" ("%(path)/" ^ Eliom.server_dir ^ "/%(file:<*>).ml");
        copy_rule_server ~eliom:false "server.mli -> .mli"
          "%(path)/%(file).server.mli" ("%(path)/" ^ Eliom.server_dir ^ "/%(file:<*>).mli");
        copy_rule_server ~eliom:false "shared.ml -> server.ml"
          "%(path)/%(file).shared.ml" ("%(path)/" ^ Eliom.server_dir ^ "/%(file:<*>).ml");
        copy_rule_server ~eliom:false "shared.ml -> server.mli"
          "%(path)/%(file).shared.mli" ("%(path)/" ^ Eliom.server_dir ^ "/%(file:<*>).mli");

        (* Include C stubs in client.cma *)
        flag ["link"; "eliomstubs"]
          (S[A"-dllib"; A"-leliom_stubs"; A"-cclib"; A"-leliom_stubs"]);
        dep ["link"; "eliomstubs"] ["src/lib/client/libeliom_stubs.a"]
    | _ -> ()

  let dispatcher ?oasis_executables hook =
    Ocamlbuild_js_of_ocaml.dispatcher ?oasis_executables hook;
    init hook
end

module Make(Eliom : ELIOM) = MakeIntern
  (struct
    let with_eliom_ppx = None
    let with_package = Printf.sprintf "package(%s)"
  end)
  (Eliom)
