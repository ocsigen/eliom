### Building
.PHONY: all
all:
	dune build

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
