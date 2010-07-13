include ../../Makefile.config

FILES = basicTypes.ml xhtmlsyntax.ml xhtmlparser.ml 
CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB) 
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB) 
CAMLDEP = $(OCAMLFIND) ocamldep
LIB = -I `$(CAMLP4) -where` -I +camlp4/Camlp4Parsers $(LIBDIRS2)
PP = -pp "camlp4rf -- -loc loc"
PPLEXER = -pp "camlp4of -- -loc loc"


predep: pp/simplexmlparser.mli pp/xhtmltypes.ml pp/xhtml5types.ml

byte: xhtmlsyntax.cma ../simplexmlparser.cmo 
	cp -f xhtmlsyntax.cma $(FILES:.ml=.cmi) xmllexer.cmo ..

opt: xhtmlsyntax.cmxa ../simplexmlparser.cmx 
	cp -f xhtmlsyntax.cmxa $(FILES:.ml=.cmi) xmllexer.cmx xmllexer.o $(FILES:.ml=.o) ..


../simplexmlparser.cmi: simplexmlparser.mli
	$(CAMLC) -c simplexmlparser.mli  -pp camlp4r
	mv simplexmlparser.cmi $@

../simplexmlparser.cmo: ../simplexmlparser.cmi simplexmlparser.ml
	$(CAMLC) -c simplexmlparser.ml -o $@ $(PP)


../simplexmlparser.cmx: ../simplexmlparser.cmi simplexmlparser.ml
	$(CAMLOPT) -c simplexmlparser.ml -o $@ $(PP)

xhtmlsyntax.cma: basicTypes.cmo xmllexer.cmo camllexer.cmo xhtmlparser.cmo xhtmlsyntax.cmo
	$(CAMLC) -a -o $@ xmllexer.cmo camllexer.cmo xhtmlparser.cmo xhtmlsyntax.cmo

xhtmlsyntax.cmxa: basicTypes.cmx xmllexer.cmx camllexer.cmx xhtmlparser.cmx xhtmlsyntax.cmx
	$(CAMLOPT) -a -o $@ xmllexer.cmx camllexer.cmx xhtmlparser.cmx xhtmlsyntax.cmx



pp/simplexmlparser.mli: simplexmlparser.mli
	camlp4r $^ -printer o > $@

pp/xhtmltypes.ml: ../xhtml/xhtmltypes.ml
	camlp4r $^ -printer o > $@

pp/xhtml5types.ml: ../xhtml/xhtml5types.ml
	camlp4r $^ -printer o > $@

xmllexer.cmo: xmllexer.ml
	$(CAMLC) $(PPLEXER) -c $<

xmllexer.cmx: xmllexer.ml
	$(CAMLOPT) $(PPLEXER) -c $<

xmllexer.ml: xmllexer.mll
	$(CAMLLEX) $<


camllexer.cmo: camllexer.ml
	$(CAMLC) $(PPLEXER) -c $<

camllexer.cmx: camllexer.ml
	$(CAMLOPT) $(PPLEXER) -c $<

camllexer.ml: camllexer.mll
	$(CAMLLEX) $<


# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(PP) -c $<
.mli.cmi:
	$(CAMLC) $(PP) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<


# Clean up
clean:
	-rm -f xmllexer.ml camllexer.ml *.cm[ioax] *.cmxa *~ *.annot *.a *.o


# Dependencies
depend: *.mli *.ml
	-rm -f xmllexer.ml camllexer.ml
# Fails because some files are in revised syntax
#	$(CAMLDEP) $(PP) $(LIBDIRS2) *.mli *.ml > .depend

FORCE:

-include .depend
