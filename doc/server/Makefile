include ../../Makefile.config
include ../../src/server/Makefile.filelist

OCAMLDOC := ${OCAMLFIND} ocamldoc

ODOC := $(addprefix ../../src/server/,$(DOC:.mli=.odoc))

odoc:
	${MAKE} -C ../../src/server odoc

doc: odoc
	mkdir -p api-html
	mkdir -p api-man
	$(OCAMLDOC) ${LIBS} -d api-html -intro indexdoc $(addprefix -load ,${ODOC}) -html
	$(OCAMLDOC) ${LIBS} -d api-man -man-mini $(addprefix -load ,${ODOC}) -man -man-suffix server.3o

install:
	${INSTALL} -d -m 755 $(TEMPROOT)$(DOCDIR)/server
	$(INSTALL) -m 644 api-html/* $(TEMPROOT)$(DOCDIR)/server
	$(INSTALL) -m 755 -d $(TEMPROOT)$(MANDIR)/man3
	$(INSTALL) -m 755 api-man/* $(TEMPROOT)$(MANDIR)/man3

clean:
	-rm -f api-html/*
	-rm -f api-man/*
	-rm -f *~ \#* .\#*
