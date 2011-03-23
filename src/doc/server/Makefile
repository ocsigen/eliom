include ../../Makefile.config
include ../../src/server/Makefile.filelist

PACKAGE :=  ${SERVERPACKAGE}

ifeq "${OCAMLDUCE}" "NO"
OCAMLDOC := ${OCAMLFIND} ocamldoc
else
OCAMLDOC := ${OCAMLDUCEFIND} ocamldoc -package ocamlduce
endif

LIBS    := -I ../../src/server/ -I ../../src/server/extensions $(addprefix -package ,${PACKAGE})

doc: odoc

odoc:
	mkdir -p api-html
	$(OCAMLDOC) ${LIBS} -d api-html -intro indexdoc -html $(addprefix ../../src/server/,$(DOC) $(PLUGINS_DOC))

install:
	${INSTALL} -d -m 644 $(TEMPROOT)$(DOCDIR)/server
	$(INSTALL) -m 644 api-html/* $(TEMPROOT)$(DOCDIR)/server

clean:
	-rm -f api-html/*
	-rm -f *~ \#* .\#*
