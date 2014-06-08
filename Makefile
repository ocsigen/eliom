### Building
BUILDER=_build/build/build.native
BUILD=ocaml pkg/build.ml

.PHONY: all byte opt builder
all: $(BUILDER)
	$(BUILD) manpage=false native=true native-dynlink=true
byte: $(BUILDER)
	$(BUILD) manpage=false native=false native-dynlink=false
opt: $(BUILDER)
	$(BUILD) manpage=false native=true native-dynlink=true

$(BUILDER): $(wildcard build/*.ml)
	ocamlbuild -no-plugin -I src/ocamlbuild -no-links -use-ocamlfind build/build.native 1> /dev/null
builder: $(BUILDER)
### Doc
.PHONY: doc wikidoc doc man alldoc
DOCS_DIR=src/lib/client src/lib/server src/ocamlbuild
DOCS_HTML=$(addsuffix /api.docdir/index.html,$(DOCS_DIR))
DOCS_WIKI=$(addsuffix /api.wikidocdir/index.wiki,$(DOCS_DIR))
DOCS_MAN= src/lib/client/api.mandocdir/man.3oc \
          src/lib/server/api.mandocdir/man.3os \
          src/ocamlbuild/api.mandocdir/man.3o
doc: $(BUILDER)
	$(BUILDER) $(DOCS_HTML)
wikidoc: $(BUILDER)
	$(BUILDER) $(DOCS_WIKI)
man: $(BUILDER)
	$(BUILDER) $(DOCS_MAN)
alldoc: man wikidoc doc

### Testing ###

.PHONY: run.local run.opt.local links

run.local: tests.byte fifo tests/eliom.conf
	OCAMLPATH=tests/:$(OCAMLPATH) ocsigenserver -c tests/eliom.conf

run.opt.local: tests.opt fifo tests/eliom.conf
	OCAMLPATH=tests/:$(OCAMLPATH) ocsigenserver.opt -c tests/eliom.conf

tests.byte: links
	${MAKE} -C tests byte

tests.opt: links
	${MAKE} -C tests opt

links:
	-mkdir -p local/var/run
	-mkdir -p local/var/log
	-mkdir -p local/var/lib

fifo:
	[ -p local/var/run/eliom_command ] || \
	 { mkfifo local/var/run/eliom_command; \
	   chmod 660 local/var/run/eliom_command; }


### Cleaning ###
.PHONY: clean clean.local distclean
clean: clean.local
	ocamlbuild -quiet -no-plugin -clean
	${MAKE} -C tests clean

clean.local:
	-rm -f eliom-*.tar.gz

distclean: clean clean.local
	${MAKE} -C tests distclean
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
