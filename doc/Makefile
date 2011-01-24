include ../Makefile.config
include Makefile.filelist

export OCAMLPATH := ../deriving/tmp:${OCAMLPATH}

CAMLDOC = ocamlducefind ocamldoc $(LIB)

LIB = -package netstring,lwt,ssl,netstring,ocamlduce,react,text,deriving,calendar $(LIBDIRS) -I ../xmlp4/xhtml

DOCPREF=../

doc: odoc

odoc:
	mkdir -p api-html
	$(CAMLDOC) -d api-html -intro indexdoc -html $(DOC)

clean:
	-rm -f *.cm[ioxa] *.o *.a *.cmx[sa] *~
	-rm -f api-html/*

