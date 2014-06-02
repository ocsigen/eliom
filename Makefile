### Building
OCB=ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)"
BUILD=pkg/build.ml

.PHONY: all byte opt
all:
	$(BUILD) native=true native-dynlink=true
byte:
	$(BUILD) native=false native-dynlink=false
opt:
	$(BUILD) native=true native-dynlink=true

### Doc
.PHONY: doc wikidoc doc man alldoc
DOCS_DIR=src/lib/client src/lib/server src/ocamlbuild
DOCS_HTML=$(addsuffix /api.docdir/index.html,$(DOCS_DIR))
DOCS_WIKI=$(addsuffix /api.wikidocdir/index.wiki,$(DOCS_DIR))
DOCS_MAN= src/lib/client/api.mandocdir/man.3oc \
          src/lib/server/api.mandocdir/man.3os \
          src/ocamlbuild/api.mandocdir/man.3o
doc:
	$(OCB) $(DOCS_HTML)
wikidoc:
	$(OCB) $(DOCS_WIKI)
man:
	$(OCB) $(DOCS_MAN)
alldoc: man wikidoc doc

### Testing ###

.PHONY: run.local run.opt.local links

run.local: tests.byte fifo tests/eliom.conf
	OCAMLPATH=tests/:$(OCAMLPATH) ocsigenserver -c tests/eliom.conf

run.opt.local: tests.opt fifo tests/eliom.conf
	OCAMLPATH=tests/:$(OCAMLPATH) ocsigenserver.opt -c tests/eliom.conf

tests.byte: byte links
	${MAKE} -C tests byte

tests.opt: opt links
	${MAKE} -C tests opt

links:
	-mkdir -p local/var/run
	-mkdir -p local/var/log
	-mkdir -p local/var/lib
	-mkdir -p local/etc
	-mkdir -p local/lib
	cd local/lib ; \
	ln -sf ../../_build/src/syntax .  ; \
	ln -sf ../../_build/src/lib/server . ; \
	ln -sf ../../_build/src/lib/client . ; \
	cd ../..

fifo:
	[ -p local/var/run/eliom_command ] || \
	 { mkfifo local/var/run/eliom_command; \
	   chmod 660 local/var/run/eliom_command; }


### Cleaning ###
.PHONY: clean clean.local distclean
clean: clean.local
	$(OCB) -clean
	${MAKE} -C tests clean

clean.local:
	-rm -f eliom-*.tar.gz

distclean: clean clean.local
	${MAKE} -C tests distclean
	-find ./ -name "*\#*" | xargs rm -f
	-rm -f local/lib/syntax
	-rm -f local/lib/client
	-rm -f local/lib/server

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
