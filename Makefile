### Building
.PHONY: all
all:
	dune build

### Doc
.PHONY: wikidoc doccp
API_DIR=_build/doc/dev/api
wikidoc:
	bash build/gen_wikidoc.sh all

doccp: wikidoc
	cp -Rf doc/index.wiki $(API_DIR)/

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
