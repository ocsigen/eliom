opam pin add --no-action eliom .
opam pin add --no-action ocsigenserver 'https://github.com/ocsigen/ocsigenserver.git'
opam pin add --no-action js_of_ocaml 'https://github.com/ocsigen/js_of_ocaml.git'
opam pin add --no-action reactiveData 'https://github.com/ocsigen/reactiveData.git'

opam install --deps-only eliom
opam install --verbose eliom

do_build_doc () {
  make wikidoc
  cp -Rf doc/manual-wiki/*.wiki ${MANUAL_SRC_DIR}
  mkdir -p ${API_DIR}/server ${API_DIR}/client ${API_DIR}/ocamlbuild ${API_DIR}/ppx
  cp -Rf _build/src/lib/server/api.wikidocdir/*.wiki ${API_DIR}/server/
  cp -Rf _build/src/lib/client/api.wikidocdir/*.wiki ${API_DIR}/client/
  cp -Rf _build/src/ocamlbuild/api.wikidocdir/*.wiki ${API_DIR}/ocamlbuild/
  cp -Rf _build/src/ppx/api.wikidocdir/*.wiki ${API_DIR}/ppx/
  cp -Rf doc/index.wiki ${API_DIR}/
}

do_remove () {
  opam remove --verbose eliom
}
