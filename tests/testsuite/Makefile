OCB=ocamlbuild -use-ocamlfind \
               -plugin-tag "package(eliom.ocamlbuild,js_of_ocaml.ocamlbuild)"

client:
	$(OCB) testsuite_client.js

byte: client
	$(OCB) testsuite.cma \
         server/eliom_testsuite_global.cma \
         server/eliom_testsuite_site.cma
opt: client
	$(OCB) testsuite.cmxs \
         server/eliom_testsuite_global.cmxs \
         server/eliom_testsuite_site.cmxs
build-%:
	$(OCB) $*

clean:
	$(OCB) -clean
distclean: clean
