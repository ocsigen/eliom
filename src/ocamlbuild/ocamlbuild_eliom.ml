open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

let init () =
  let module Eliom_rules = struct
open Pack ;;

(* Copy of the rules in ocamlbuild ocaml_specific.ml.
   We only copy the one that involves .ml and .mli files.
*)

rule "eliom: eliomi -> cmi"
    ~prod:"%.cmi"
    ~deps:["%.eliomi"; "%.eliomi.depends"]
    (Ocaml_compiler.compile_ocaml_interf "%.eliomi" "%.cmi") ;;

rule "eliom: mlpack & cmo* & cmi -> cmo"
    ~prod:"%.cmo"
    ~deps:["%.eliomi"; "%.cmi"; "%.mlpack"]
    ~doc:"If foo.mlpack contains a list of capitalized module names, \
          the target foo.cmo will produce a packed module containing \
          those modules as submodules. You can also have a foo.eliomi file \
          to restrict the interface of the resulting module.

\
          Warning: to produce a native foo.cmx out of a foo.mlpack, you must \
          manually tag the included compilation units with for-pack(foo). \
          See the documentation of the corresponding rules for more details.

\
          The modules named in the .mlpack \
          will be dynamic dependencies of the compilation action. \
          You cannot give the .mlpack the same name as one of the module \
          it contains, as this would create a circular dependency."
    (Ocaml_compiler.byte_pack_mlpack "%.mlpack" "%.cmo");;

rule "eliom: eliom & cmi -> d.cmo"
    ~prod:"%.d.cmo"
    ~deps:["%.eliomi"(* This one is inserted to force this rule to be skiped when
                        a .eliom is provided without a .eliomi *); "%.eliom"; "%.eliom.depends"; "%.cmi"]
    ~doc:"The foo.d.cmo target compiles foo.eliom with the 'debug' tag enabled (-g).\
          See also foo.d.byte.

\
          For technical reason, .d.cmx and .d.native are not yet supported, \
          so you should explicitly add the 'debug' tag \
          to native targets (both compilation and linking)."
    (Ocaml_compiler.byte_compile_ocaml_implem ~tag:"debug" "%.eliom" "%.d.cmo");;

rule "eliom: eliom & cmi -> cmo"
    ~prod:"%.cmo"
    ~deps:["%.eliomi"(* This one is inserted to force this rule to be skiped when
                        a .eliom is provided without a .eliomi *); "%.eliom"; "%.eliom.depends"; "%.cmi"]
    (Ocaml_compiler.byte_compile_ocaml_implem "%.eliom" "%.cmo");;

rule "eliom: eliom & cmi -> cmx & o"
    ~prods:["%.cmx"; "%" -.- !Options.ext_obj]
    ~deps:["%.eliom"; "%.eliom.depends"; "%.cmi"]
    (Ocaml_compiler.native_compile_ocaml_implem "%.eliom");;

rule "eliom: eliom -> d.cmo & cmi"
    ~prods:["%.d.cmo"]
    ~deps:["%.eliom"; "%.eliom.depends"; "%.cmi"]
    (Ocaml_compiler.byte_compile_ocaml_implem ~tag:"debug" "%.eliom" "%.d.cmo");;

rule "eliom: eliom -> cmo & cmi"
    ~prods:["%.cmo"; "%.cmi"]
    ~deps:["%.eliom"; "%.eliom.depends"]
    ~doc:"This rule allows to produce a .cmi from a .eliom file \
          when the corresponding .eliomi is missing.

\
          Note: you are strongly encourage to have a .eliomi file \
          for each of your .eliom module, as it is a good development \
          practice which also simplifies the way build systems work, \
          as it avoids producing .cmi files as a silent side-effect of \
          another compilation action."
    (Ocaml_compiler.byte_compile_ocaml_implem "%.eliom" "%.cmo");;

rule "eliom dependencies"
    ~prod:"%.eliom.depends"
    ~dep:"%.eliom"
    ~doc:"OCamlbuild will use ocamldep to approximate dependencies \
          of a source file. The ocamldep tool being purely syntactic, \
          it only computes an over-approximation of the dependencies.

\
          If you manipulate a module Foo that is in fact a submodule Bar.Foo \
          (after 'open Bar'), ocamldep may believe that your module depends \
          on foo.eliom -- when such a file also exists in your project. This can \
          lead to spurious circular dependencies. In that case, you can use \
          OCamlbuild_plugin.non_dependency in your myocamlbuild.eliom \
          to manually remove the spurious dependency. See the plugins API."
    (Ocaml_tools.ocamldep_command "%.eliom" "%.eliom.depends");;

rule "eliom dependencies eliomi"
    ~prod:"%.eliomi.depends"
    ~dep:"%.eliomi"
    (Ocaml_tools.ocamldep_command "%.eliomi" "%.eliomi.depends");;

rule "eliom: eliomi -> odoc"
    ~prod:"%.odoc"
    ~deps:["%.eliomi"; "%.eliomi.depends"]
    ~doc:".odoc are intermediate files storing the result of ocamldoc processing \
          on a source file. See the various .docdir/... targets for ocamldoc."
    (Ocaml_tools.document_ocaml_interf "%.eliomi" "%.odoc");;

rule "eliom: eliom -> odoc"
    ~prod:"%.odoc"
    ~deps:["%.eliom"; "%.eliom.depends"]
    (Ocaml_tools.document_ocaml_implem "%.eliom" "%.odoc");;

rule "eliom: eliom & eliom.depends & *cmi -> .inferred.eliomi"
    ~prod:"%.inferred.eliomi"
    ~deps:["%.eliom"; "%.eliom.depends"]
    ~doc:"The target foo.inferred.eliomi will produce a .eliomi that exposes all the \
          declarations in foo.eliom, as obtained by direct invocation of `ocamlc -i`."
    (Ocaml_tools.infer_interface "%.eliom" "%.inferred.eliomi");;

let compile_tags = [
  ["ocaml"; "byte"; "compile"];
  ["ocaml"; "native"; "compile"];
  ["ocaml"; "infer_interface"];
] in
let link_tags = [
  ["ocaml"; "byte"; "link"];
  ["ocaml"; "native"; "link"];
] in
let other_tags = [
  ["ocaml"; "ocamldep"];
  ["ocaml"; "doc"];
] in

List.iter (fun tags ->
  pflag tags "server-package" (fun pkg -> S [A "-server-package"; A pkg]);
  pflag tags "client-package" (fun pkg -> S [A "-client-package"; A pkg]);
) (compile_tags @ link_tags) ;
List.iter (fun tags ->
  pflag tags "server-I" (fun x -> S[A"-server-I"; A x]);
  pflag tags "server-I" (fun x -> S[A"-server-I"; A x]);
  pflag tags "server-I" (fun x -> S[A"-server-I"; A x]);
) (compile_tags @ link_tags);
List.iter (fun tags ->
  flag ("client"::tags) (S [A "-passopt" ; A "-mode"; A "-passopt" ; A "client"]);
  flag ("server"::tags) (S [A "-passopt" ; A "-mode"; A "-passopt" ; A "server"]);
  flag ("eliom"::tags) (S [A "-passopt" ; A "-mode"; A "-passopt" ; A "eliom"]);
) (compile_tags @ link_tags @ other_tags) ;;

(* pflag [ "ocaml"; "compile"] "server-I" (fun x -> S[A"-server-I"; A x]);; *)
(* pflag [ "ocaml"; "infer_interface"] "server-I" (fun x -> S[A"-server-I"; A x]);; *)
(* pflag [ "ocaml"; "doc"] "server-I" (fun x -> S[A"-server-I"; A x]);; *)

(* pflag [ "ocaml"; "compile"] "client-I" (fun x -> S[A"-client-I"; A x]);; *)
(* pflag [ "ocaml"; "infer_interface"] "client-I" (fun x -> S[A"-client-I"; A x]);; *)
(* pflag [ "ocaml"; "doc"] "client-I" (fun x -> S[A"-client-I"; A x]);; *)


  end in ()

module type ELIOM = sig
  val server_dir : Ocamlbuild_plugin.Pathname.t
  val client_dir : Ocamlbuild_plugin.Pathname.t
end

module type INTERNALS = sig
  val with_eliom_ppx : ([< `Client | `Server] -> string) option
  val with_package : string -> string
end
module MakeIntern (I : INTERNALS)(Eliom : ELIOM) = struct


  let copy_rule f name ?(deps=[]) src prod =
    rule name ~deps:(src :: deps) ~prod
      (fun env _ ->
         let prod = env prod in
         let src = env src in
         f env (Pathname.dirname prod) (Pathname.basename prod) src prod;
         Pack.Shell.mkdir_p (Filename.dirname prod);
         cp src prod
      )

  let syntaxes_p4 = [I.with_package "eliom.syntax.predef"]

  let no_extra_syntaxes = "no_extra_syntaxes"

  let tag_file_inside_rule file tags =
    tag_file file tags;
    Pack.Param_tags.partial_init "Eliom plugin" (Tags.of_list tags)

  let use_all_syntaxes src =
    if Filename.check_suffix src ".eliomi" then
      false
    else
      not (Tags.mem no_extra_syntaxes (tags_of_pathname src))

  let copy_rule_server =
    copy_rule
      (fun env dir name src file ->
         let path = env "%(path)" in
         tag_file_inside_rule file [
           I.with_package "eliom.server" ;
           Printf.sprintf "server-I(%s)" Eliom.server_dir ;
         ];
         Pathname.define_context dir [path];
         Pathname.define_context path [dir];
      )

  let copy_rule_client =
    copy_rule
      (fun env dir name src file ->
         let path = env "%(path)" in
         tag_file_inside_rule file [
           I.with_package "eliom.client" ;
           Printf.sprintf "client-I(%s)" Eliom.client_dir ;
         ];
         Pathname.define_context dir [path];
      )




  let init = function
    | After_rules ->
        mark_tag_used no_extra_syntaxes;

        sed_rule ".inferred.mli -> .inferred_gen.mli"
          ~dep:"%(path)/%(file).inferred.mli"
          ~prod:"%(path)/%(file).inferred_gen.mli"
          ["s/_\\[\\([<>]\\)/[\\1/g";
           Printf.sprintf "s/'\\(_[a-z0-9_]*\\)/'%s\\1/g" inferred_type_prefix];

        (* eliom files *)
        init () ;

        (* copy {shared,client,server}.ml rules *)
        (* copy_rule_client "client.ml -> .ml" *)
        (*   "%(path)/%(file).client.ml" ("%(path)/" ^ Eliom.client_dir ^ "/%(file:<*>).ml"); *)
        (* copy_rule_client "client.mli -> .mli" *)
        (*   "%(path)/%(file).client.mli" ("%(path)/" ^ Eliom.client_dir ^ "/%(file:<*>).mli"); *)

        (* copy_rule_client "shared.ml -> client.ml" *)
        (*   "%(path)/%(file).shared.ml" ("%(path)/" ^ Eliom.client_dir ^ "/%(file:<*>).ml"); *)
        (* copy_rule_client "shared -> client.mli" *)
        (*   "%(path)/%(file).shared.mli" ("%(path)/" ^ Eliom.client_dir ^ "/%(file:<*>).mli"); *)

        (* copy_rule_server "server.ml -> .ml" *)
        (*   "%(path)/%(file).server.ml" ("%(path)/" ^ Eliom.server_dir ^ "/%(file:<*>).ml"); *)
        (* copy_rule_server "server.mli -> .mli" *)
        (*   "%(path)/%(file).server.mli" ("%(path)/" ^ Eliom.server_dir ^ "/%(file:<*>).mli"); *)

        (* copy_rule_server "shared.ml -> server.ml" *)
        (*   "%(path)/%(file).shared.ml" ("%(path)/" ^ Eliom.server_dir ^ "/%(file:<*>).ml"); *)
        (* copy_rule_server "shared.ml -> server.mli" *)
        (*   "%(path)/%(file).shared.mli" ("%(path)/" ^ Eliom.server_dir ^ "/%(file:<*>).mli"); *)

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
