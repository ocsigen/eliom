(lang dune 3.18)

(name eliom)
(version 11.1.1)
(maintainers dev@ocsigen.org)
(authors dev@ocsigen.org)
(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")

(generate_opam_files true)

(package
 (name eliom)
 (synopsis "Advanced client/server Web and mobile framework")
 (description "Eliom is a framework for implementing Web sites and client/server Web and mobile applications.
It uses advanced concepts to simplify the implementation of common behaviors (e.g. scoped sessions, continuation based Web programming ...).
It uses advanced static typing features of OCaml to check many properties of the Web application at compile-time (html, page parameters ...).
Eliom allows implementing the whole application as a single program that includes both the client and the server code. For example, you can implement event handlers (onclick ...) directly in OCaml, and you can call a server-side OCaml function from the client. Pages are generated either on the server or the client.
These client-side features remain compatible with traditional Web programming (links, forms, URLs, bookmarks, sessions ...).
It is possible to generate mobile applications for Android and iOS with the exact same code as your Web application.
The client-side code is compiled to JS using Ocsigen Js_of_ocaml or to Wasm using Wasm_of_ocaml.
")
 (homepage https://ocsigen.org/eliom/)
 (source (github ocsigen/eliom))
 (depends
  (ocaml (and (>= 4.12) (< 5.3)))
  ocamlfind
  ppx_deriving
  (ppxlib (>= 0.15))
  (js_of_ocaml-compiler (and (>= 5.5) (< 6.0)))
  (js_of_ocaml (>= 5.5))
  (js_of_ocaml-lwt (>= 5.5))
  (js_of_ocaml-ocamlbuild :build)
  (js_of_ocaml-ppx (>= 5.5))
  (js_of_ocaml-ppx_deriving_json (>= 5.5))
  (js_of_ocaml-tyxml (>= 5.5))
  lwt_log
  (tyxml (and (>= 4.6) (< 4.7)))
  (ocsigenserver (and (>= 6.0) (< 7.0)))
  lwt_log
  (ipaddr (>= 2.1))
  (reactiveData (>= 0.2.1))
  base-bytes
  (ocsipersist (and (>= 2.0) (< 3.0)))
  ppx_optcomp
  (xml-light (>= "2.5"))))
