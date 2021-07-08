### Building
BEST=$(if $(shell command -v ocamlopt 2> /dev/null),native,byte)
BUILDER=_build/build/build.$(BEST)

.PHONY: all
all:
	make gen-dune
	dune build

.PHONY: gen-dune
gen-dune:
	ocaml src/tools/gen_dune.ml --client src/lib > src/lib/client/dune.client
	ocaml src/tools/gen_dune.ml --server src/lib > src/lib/server/dune.server

### Doc
.PHONY: doc wikidoc doc man alldoc
DOCS_DIR=src/lib/client src/lib/server src/ocamlbuild src/ppx
DOCS_HTML=$(addsuffix /api.docdir/index.html,$(DOCS_DIR))
DOCS_WIKI=$(addsuffix /api.wikidocdir/index.wiki,$(DOCS_DIR))
DOCS_MAN= src/lib/client/api.mandocdir/man.3oc \
          src/lib/server/api.mandocdir/man.3os \
          src/ocamlbuild/api.mandocdir/man.3o \
          src/ppx/api.mandocdir/man.3o
API_DIR=_build/doc/dev/api
doc: $(BUILDER)
	$(BUILDER) $(DOCS_HTML)
wikidoc: $(BUILDER)
	$(BUILDER) $(DOCS_WIKI)
man: $(BUILDER)
	$(BUILDER) $(DOCS_MAN)
alldoc: man wikidoc doc

doccp: alldoc
	mkdir -p $(API_DIR)/server $(API_DIR)/client $(API_DIR)/ocamlbuild $(API_DIR)/ppx
	cp -Rf _build/src/lib/server/api.wikidocdir/*.wiki $(API_DIR)/server/
	cp -Rf _build/src/lib/client/api.wikidocdir/*.wiki $(API_DIR)/client/
	cp -Rf _build/src/ocamlbuild/api.wikidocdir/*.wiki $(API_DIR)/ocamlbuild/
	cp -Rf _build/src/ppx/api.wikidocdir/*.wiki $(API_DIR)/ppx/
	cp -Rf doc/index.wiki $(API_DIR)/

$(BUILDER): $(wildcard build/*.ml)
	ocamlbuild -no-plugin -I src/ocamlbuild -no-links -use-ocamlfind build/build.$(BEST) 1> /dev/null
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
