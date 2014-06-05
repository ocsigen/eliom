opam pin eliom .
opam pin tyxml https://github.com/ocsigen/tyxml.git
opam pin ocsigenserver https://github.com/ocsigen/ocsigenserver.git
opam pin js_of_ocaml https://github.com/ocsigen/js_of_ocaml.git
opam install --deps-only eliom
opam install --verbose eliom
opam remove --verbose eliom
