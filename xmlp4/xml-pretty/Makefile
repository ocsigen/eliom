include ../../Makefile.config

#FILES = xhtmlpretty.ml xhtmlcompact.ml \
#	xhtmlpretty_streams.ml xhtmlcompact_streams.ml \
#	xhtml5pretty.ml xhtml5compact.ml \
#	xhtml5pretty_streams.ml xhtml5compact_streams.ml

FILES = xhtml_format.ml xhtml_mkcompact.ml xhtml_mkpretty.ml xhtmlpretty.ml xhtmlcompact.ml \
	xhtml5pretty.ml xhtml5compact.ml

FILES_STREAM = xhtml_mkpretty_streams.ml xhtml_mkcompact_streams.ml \
	xhtmlpretty_streams.ml xhtmlcompact_streams.ml \
	xhtml5pretty_streams.ml xhtml5compact_streams.ml


CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB) -package camlp4 -I ..
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB) -package camlp4 -I ..
CAMLDEP = $(OCAMLFIND) ocamldep $(LIB) -package camlp4
LIB = -I ../../baselib

OBJS = $(FILES:.ml=.cmo)
CMIS = $(FILES:.ml=.cmi) $(FILES_STREAM:.ml=.cmi)
OBJSOPT = $(FILES:.ml=.cmx)


byte: ../xhtmlpretty.cma $(FILES_STREAM:.ml=.cmo)
	cp $(CMIS)  $(FILES_STREAM:.ml=.cmo)  .. 

opt: ../xhtmlpretty.cmxa $(FILES_STREAM:.ml=.cmx)
	cp $(CMIS)  $(FILES_STREAM:.ml=.cmx) $(FILES_STREAM:.ml=.o) ..

../xhtmlpretty.cma: $(CMIS) $(OBJS)
	$(CAMLC) -a -o $@ $(OBJS)

../xhtmlpretty.cmxa: $(CMIS) $(OBJSOPT)
	$(CAMLOPT) -a -o $@ $(OBJSOPT)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx


.ml.cmo:
	$(CAMLC) $(PP) -c $<
.mli.cmi:
	$(CAMLC) $(PP) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<



clean:
#	touch oldocaml/.depend
#	touch newocaml/.depend
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *~ *.annot


depend:
#	touch oldocaml/.depend
#	touch newocaml/.depend
	$(CAMLDEP) $(PP) $(FILES) > .depend



-include .depend
