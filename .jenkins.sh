opam pin eliom .
opam pin ocsigenserver https://github.com/ocsigen/ocsigenserver.git
opam pin tyxml https://github.com/ocsigen/tyxml.git#new_wrap
opam pin js_of_ocaml https://github.com/ocsigen/js_of_ocaml.git#new_wrap
opam pin reactiveData https://github.com/hhugo/reactiveData.git
opam install --deps-only eliom
opam install --verbose eliom
opam remove --verbose eliom
