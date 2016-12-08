open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

let copy_rule name src prod =
  rule name ~dep:src ~prod
      (fun env _ ->
         let prod = env prod in
         let src = env src in
         (* f env (Pathname.dirname prod) (Pathname.basename prod) src prod; *)
         Pack.Shell.mkdir_p (Filename.dirname prod);
         cp src prod
      )

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

rule "eliom: {server,client}.cmi -> cmi"
    ~prod:"%(name:<*> and not <*.client> and not <*.server>).cmi"
    ~deps:["%(name).client.cmi";"%(name).server.cmi"]
    (fun _ _ -> Nop);;

rule "eliom: {server,client}.cmi -> cmi | in subdir"
    ~prod:"%(name:<**/*> and not <**/*.client> and not <**/*.server>).cmi"
    ~deps:["%(name).client.cmi";"%(name).server.cmi"]
    (fun _ _ -> Nop);;

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
  pflag tags "server-I" (fun x -> S[A"-server-I"; P x]);
  pflag tags "client-I" (fun x -> S[A"-client-I"; P x]);
  pflag tags "I" (fun x -> S[A"-I"; P x]);
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


copy_rule "shared.ml -> client.ml"
  "%(path)/%(file).shared.ml" "%(path)/%(file).client.ml";;
copy_rule "shared.mli -> client.mli"
  "%(path)/%(file).shared.mli" "%(path)/%(file).client.mli";;

copy_rule "shared.mli -> server.ml"
  "%(path)/%(file).shared.ml" "%(path)/%(file).server.ml";;
copy_rule "shared.mli -> server.mli"
  "%(path)/%(file).shared.mli" "%(path)/%(file).server.mli";;

  end in ()

let init = function
  | After_rules -> init () ;
  | _ -> ()

let dispatcher ?oasis_executables hook =
  Ocamlbuild_js_of_ocaml.dispatcher ?oasis_executables hook;
  init hook
