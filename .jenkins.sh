opam pin eliom .
opam pin ocsigenserver https://github.com/ocsigen/ocsigenserver.git
opam install --deps-only eliom
opam install --verbose eliom
opam remove --verbose eliom
