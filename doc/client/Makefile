include ../../Makefile.config
include ../../src/client/Makefile.filelist

OCAMLDOC := ${OCAMLFIND} ocamldoc

ODOC := $(addprefix ../../src/client/,$(DOC:.mli=.odoc))

odoc:
	${MAKE} -C ../../src/client odoc

doc: odoc
	mkdir -p api-html
	$(OCAMLDOC) ${LIBS} -d api-html -intro indexdoc $(addprefix -load ,${ODOC}) -html

install:
	${INSTALL} -d -m 755 $(TEMPROOT)$(DOCDIR)/client
	$(INSTALL) -m 644 api-html/* $(TEMPROOT)$(DOCDIR)/client

clean:
	-rm -f api-html/*
	-rm -f *~ \#* .\#*
	-rm -rf ${TMP}
