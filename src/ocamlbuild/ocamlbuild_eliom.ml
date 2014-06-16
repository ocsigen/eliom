open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

module type ELIOM = sig
  val server_dir : Ocamlbuild_plugin.Pathname.t
  val type_dir : Ocamlbuild_plugin.Pathname.t
  val client_dir : Ocamlbuild_plugin.Pathname.t
end

module type INTERNALS = sig
  val with_package : string -> string
end
module MakeIntern (I : INTERNALS)(Eliom : ELIOM) = struct

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

  let syntaxes = [I.with_package "eliom.syntax.predef"]

  let no_extra_syntaxes = "no_extra_syntaxes"

  let tag_file_inside_rule file tags =
    tag_file file tags;
    (* Workaround. See: http://caml.inria.fr/mantis/view.php?id=6186 *)
#if ocaml_version < (4, 01)
    Pack.Param_tags.init ()
#else
    Pack.Param_tags.partial_init (Tags.of_list tags)
#endif

  let use_all_syntaxes src =
    if Filename.check_suffix src ".eliomi" then
      false
    else
      not (Tags.mem no_extra_syntaxes (tags_of_pathname src))

  let get_syntaxes with_eliom_syntax eliom_syntax src =
    let s = if use_all_syntaxes src then syntaxes else [] in
    let s = if with_eliom_syntax then I.with_package eliom_syntax :: s else s in
    let s = if s = [] then [] else "thread" :: "syntax(camlp4o)" :: s in
    s @ Tags.elements (tags_of_pathname src)

  let copy_rule_server ?(eliom=true) =
    copy_rule_with_header
      (fun env dir name src file ->
         let path = env "%(path)" in
         tag_file_inside_rule file
           ( I.with_package "eliom.server"
             :: get_syntaxes eliom "eliom.syntax.server" src
           );
         if eliom then flag_infer ~file ~name ~path;
         Pathname.define_context dir [path];
         Pathname.define_context path [dir];
      )

  let copy_rule_client ?(eliom=true) =
    copy_rule_with_header
      (fun env dir name src file ->
         let path = env "%(path)" in
         tag_file_inside_rule file
           ( I.with_package "eliom.client"
             :: get_syntaxes eliom "eliom.syntax.client" src
           );
         if eliom then flag_infer ~file ~name ~path;
         Pathname.define_context dir [path];
      )

  let copy_rule_type =
    copy_rule_with_header
      (fun env dir name src file ->
         let path = env "%(path)" in
         let server_dir = Pathname.concat path Eliom.server_dir in
         let server_file = Pathname.concat server_dir name in
         tag_file_inside_rule file
           ( get_syntaxes true "eliom.syntax.type" src
             @ Tags.elements (tags_of_pathname server_file)
           );
         Pathname.define_context dir [path; server_dir];
      )

  let init = function
    | After_rules ->
        (* eliom files *)
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

    | _ -> ()

  let dispatcher ?oasis_executables hook =
    Ocamlbuild_js_of_ocaml.dispatcher ?oasis_executables hook;
    init hook
end

module Make(Eliom : ELIOM) = MakeIntern(struct let with_package = Printf.sprintf "package(%s)" end)(Eliom)
