### Building
BEST=$(if $(shell command -v ocamlopt 2> /dev/null),native,byte)
BUILDER=_build/build/build.$(BEST)

.PHONY: all
all:
	dune build

### Doc
.PHONY: wikidoc
API_DIR=_build/doc/dev/api
wikidoc:
	bash build/gen_wikidoc.sh all

doccp: wikidoc
	cp -Rf doc/index.wiki $(API_DIR)/

$(BUILDER): $(wildcard build/*.ml)
	ocamlbuild -no-plugin -I src/ocamlbuild -no-links -use-ocamlfind build/build.$(BEST) 1> /dev/null
	ocaml pkg/build.ml manpage=false native=true native-dynlink=true
builder: $(BUILDER)

### Cleaning ###
.PHONY: clean distclean
clean:
	dune clean

distclean: clean
	-find ./ -name "*\#*" | xargs rm -f

### Installation ####
.PHONY: install uninstall reinstall


install uninstall: eliom.install
ifneq ($(PREFIX),)
	opam-installer --$@ --prefix $(PREFIX) eliom.install
else
	@echo you must provide a prefix with : make PREFIX=myprefix $@
endif

reinstall:
	${MAKE} uninstall
	${MAKE} install
