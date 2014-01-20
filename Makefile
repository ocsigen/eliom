include Makefile.config

### Building

.PHONY: all byte opt doc

all:
	${MAKE} -C src all

byte:
	${MAKE} -C src byte

opt:
	${MAKE} -C src opt

odoc:
	$(MAKE) -C src odoc

doc:
	$(MAKE) -C doc doc

### Testing ###

.PHONY: run.local run.opt.local top

run.local: tests.byte fifo
	ocsigenserver -c local/etc/${PROJECTNAME}.conf

tests.byte: byte
	${MAKE} -C tests byte

run.opt.local: tests.opt fifo
	ocsigenserver.opt -c local/etc/${PROJECTNAME}.conf

tests.opt: opt
	${MAKE} -C tests opt

fifo:
	[ -p local/var/run/${PROJECTNAME}_command ] || \
	 { mkfifo local/var/run/${PROJECTNAME}_command; \
	   chmod 660 local/var/run/${PROJECTNAME}_command; }


### Cleaning ###

clean: clean.local
	${MAKE} -C src clean
	${MAKE} -C tests clean

clean.local:
	-rm -f $(PROJECTNAME)-*.tar.gz

distclean: clean.local
	${MAKE} -C src distclean
	${MAKE} -C tests distclean
	-${MAKE} -C doc clean
	-rm Makefile.config
	-rm -f *~ \#* .\#*

### Installation ####

.PHONY: install uninstall reinstall

install:
	$(MAKE) -C src install
	@echo
	@echo "## Run \"make doc\" and \"make install.doc\" to build and install the ocamldoc."
install.byte:
	$(MAKE) -C src install.byte
install.opt:
	$(MAKE) -C src install.opt

uninstall:
	-$(MAKE) -C src uninstall

reinstall:
	$(MAKE) -C src reinstall
reinstall.byte:
	$(MAKE) -C src reinstall.byte
reinstall.opt:
	$(MAKE) -C src reinstall.opt

install.doc:
	${MAKE} -C doc install

###

.PHONY: dist

VERSION := $(shell head -n 1 VERSION)
dist:
	DARCS_REPO=$(PWD) darcs dist -d $(PROJECTNAME)-$(VERSION)

###

.PHONY: depend
depend:
	${MAKE} -C src syntax.depend
	${MAKE} -C src/syntax
	${MAKE} -C src tools.depend
	${MAKE} -C src/tools
	${MAKE} -C src files/META.${PROJECTNAME}
	${MAKE} -C src server.depend client.depend
	${MAKE} -C tests depend
