include ../../Makefile.config
include ../../src/client/Makefile.filelist

PACKAGE :=  ${CLIENT_PACKAGE}
OCAMLDOC := ${OCAMLFIND} ocamldoc

LIBS    := -I ../../src/client/ $(addprefix -package ,${PACKAGE})

doc: odoc

odoc:
	mkdir -p api-html
	$(OCAMLDOC) ${LIBS} -d api-html -intro indexdoc -html $(addprefix ../../src/client/,$(DOC) $(PLUGINS_DOC))

install:
	${INSTALL} -d -m 644 $(TEMPROOT)$(DOCDIR)/client
	$(INSTALL) -m 644 api-html/* $(TEMPROOT)$(DOCDIR)/client

clean:
	-rm -f api-html/*
	-rm -f *~ \#* .\#*
