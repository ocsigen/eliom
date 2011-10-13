include ../../Makefile.config
include ../../src/server/Makefile.filelist

OCAMLDOC := ${OCAMLFIND} ocamldoc

ODOC := $(addprefix ../../src/server/,$(DOC:.mli=.odoc))

odoc:
	${MAKE} -C ../../src/server odoc

doc: odoc
	mkdir -p api-html
	$(OCAMLDOC) ${LIBS} -d api-html -intro indexdoc $(addprefix -load ,${ODOC}) -html

install:
	${INSTALL} -d -m 755 $(TEMPROOT)$(DOCDIR)/server
	$(INSTALL) -m 644 api-html/* $(TEMPROOT)$(DOCDIR)/server

clean:
	-rm -f api-html/*
	-rm -f *~ \#* .\#*
