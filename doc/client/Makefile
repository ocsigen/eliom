include ../../Makefile.config
include ../../src/client/Makefile.filelist
include ../../src/clientserver/Makefile.filelist
include ../../src/client2/Makefile.filelist

OCAMLDOC := ${OCAMLFIND} ocamldoc

ODOC := $(addprefix ../../src/client/,$(DOC:.mli=.odoc)) \
        $(addprefix ../../src/clientserver/,$(CCDOC:.mli=.odoc)) \
        $(addprefix ../../src/client2/,$(CCCDOC:.mli=.odoc))


doc: odoc api-html/index.html api-man/Eliom_lib.client.3o
wikidoc: odoc api-wiki/index.wiki

odoc:
	${MAKE} -C ../../src/client odoc

api-html/index.html: indexdoc ${ODOC}
	mkdir -p api-html
	$(OCAMLDOC) ${LIBS} -d api-html -intro indexdoc $(addprefix -load ,${ODOC}) -html

api-man/Eliom_lib.client.3o: ${ODOC}
	mkdir -p api-man
	$(OCAMLDOC) ${LIBS} -d api-man -man-mini $(addprefix -load ,${ODOC}) -man \
	  -man-section 3o -man-suffix client.3o

api-wiki/index.wiki: indexdoc ${ODOC}
	mkdir -p api-wiki
	ODOC_WIKI_SUBPROJECT=client $(OCAMLDOC) -d api-wiki -intro indexdoc \
	   -i $(shell ocamlfind query wikidoc) -g odoc_wiki.cma \
	   -colorize-code $(addprefix -load ,${ODOC})

install:
	${INSTALL} -d -m 755 $(TEMPROOT)$(DOCDIR)/client
	$(INSTALL) -m 644 api-html/* $(TEMPROOT)$(DOCDIR)/client
	$(INSTALL) -m 755 -d $(TEMPROOT)$(MANDIR)/man3
	$(INSTALL) -m 755 api-man/* $(TEMPROOT)$(MANDIR)/man3

clean:
	-rm -f ${ODOC}
	-rm -f api-html/*
	-rm -f api-man/*
	-rm -f api-wiki/*
	-rm -f *~ \#* .\#*
	-rm -rf ${TMP}
