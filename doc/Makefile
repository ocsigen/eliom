include ../Makefile.config
include ../Makefile.filelist

export OCAMLPATH := ../deriving/tmp:${OCAMLPATH}

CAMLDOC = ocamlducefind ocamldoc $(LIB)
INSTALL = install

LIB = -package netstring,lwt,ssl,netstring,ocamlduce,react,text,deriving,calendar,js_of_ocaml $(LIBDIRS) -I ../xmlp4/xhtml

DOCPREF=../

doc: odoc

odoc:
	mkdir -p api-html
	$(CAMLDOC) -d api-html -intro indexdoc -html $(DOC)


DOCUPDIR=$(shell  dirname $(TEMPROOT)$(DOCDIR))
install:
	[ -d $(DOCUPDIR) ] || (mkdir -p $(DOCUPDIR); chmod a+rx $(DOCUPDIR))
	mkdir -p $(TEMPROOT)$(DOCDIR)
	$(INSTALL) -m 644 api-html/* $(TEMPROOT)$(DOCDIR)
	chmod a+rx $(TEMPROOT)$(DOCDIR)
	chmod a+r $(TEMPROOT)$(DOCDIR)/*

clean:
	-rm -f *.cm[ioxa] *.o *.a *.cmx[sa] *~
	-rm -f api-html/*

