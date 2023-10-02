(lang dune 3.11)

(name eliom)
(maintainers dev@ocsigen.org)
(authors dev@ocsigen.org)
(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")

(package
 (name eliom)
 (synopsis "Client/server Web framework")
 (description "Eliom is a framework for implementing client/server Web applications.
It introduces new concepts to simplify the implementation of common behaviors, and uses advanced static typing features of OCaml to check many properties of the Web application at compile-time.
Eliom allows implementing the whole application as a single program that includes both the client and the server code.
We use a syntax extension to distinguish between the two sides.
The client-side code is compiled to JS using Ocsigen Js_of_ocaml.
")
 (homepage http://ocsigen.org/eliom/)
 (source (github ocsigen/eliom))
 (depends
  (ocaml (>= 4.08.0))
  ocamlfind
  ppx_deriving
  (ppxlib (>= 0.15.0))
  (js_of_ocaml-compiler (>= 3.6.0))
  (js_of_ocaml (>= 3.6.0))
  (js_of_ocaml-lwt (>= 3.6.0))
  (js_of_ocaml-ocamlbuild :build)
  (js_of_ocaml-ppx (>= 3.6.0))
  (js_of_ocaml-ppx_deriving_json (>= 3.6.0))
  (js_of_ocaml-tyxml (>= 3.6.0))
  lwt_log
  (lwt_ppx (>= 1.2.3))
  (tyxml (and (>= 4.4.0) (< 5.0.0)))
  (ocsigenserver (and (>= 5.1.0) (< 6.0.0)))
  (ipaddr (>= 2.1))
  (reactiveData (>= 0.2.1))
  base-bytes
  (ocsipersist (and (>= 1.0) (< 2.0)))
  ppx_optcomp))

(generate_opam_files)

