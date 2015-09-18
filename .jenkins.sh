opam pin add --no-action eliom .
opam pin add --no-action reactiveData 'https://github.com/hhugo/reactiveData.git#master'
opam pin add --no-action tyxml 'https://github.com/ocsigen/tyxml.git#master'
opam pin add --no-action ocsigenserver 'https://github.com/ocsigen/ocsigenserver.git#master'
opam pin add --no-action js_of_ocaml 'https://github.com/ocsigen/js_of_ocaml.git#master'
opam install --deps-only eliom
opam install --verbose eliom

do_build_doc () {
  make wikidoc
  cp -Rf doc/manual-wiki/*.wiki ${MANUAL_SRC_DIR}
  mkdir -p ${API_DIR}/server ${API_DIR}/client
  cp -Rf _build/src/lib/server/api.wikidocdir/*.wiki ${API_DIR}/server/
  cp -Rf _build/src/lib/client/api.wikidocdir/*.wiki ${API_DIR}/client/
  cp -Rf doc/index.wiki ${API_DIR}/
}

do_remove () {
  opam remove --verbose eliom
}
