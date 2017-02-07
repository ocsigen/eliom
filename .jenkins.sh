opam pin add --no-action eliom .
opam install --deps-only eliom
opam install --verbose eliom

do_build_doc () {
  make wikidoc
  cp -Rf doc/manual-wiki/*.wiki ${MANUAL_SRC_DIR}
  mkdir -p ${API_DIR}/server ${API_DIR}/client ${API_DIR}/ocamlbuild $
  cp -Rf _build/src/lib/server/api.wikidocdir/*.wiki ${API_DIR}/server/
  cp -Rf _build/src/lib/client/api.wikidocdir/*.wiki ${API_DIR}/client/
  cp -Rf _build/src/ocamlbuild/api.wikidocdir/*.wiki ${API_DIR}/ocamlbuild/
  cp -Rf doc/index.wiki ${API_DIR}/
}

do_remove () {
  opam remove --verbose eliom
}
