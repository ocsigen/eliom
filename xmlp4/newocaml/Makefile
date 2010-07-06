include ../../Makefile.config


FILESR = xmllexer.ml camllexer.ml
FILES = basicTypes.ml xhtmltypes.ml xhtml5types.ml simplexmlparser.ml xhtmlsyntax.ml xhtmlparser.ml 

CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB) 
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB) 
CAMLDEP = $(OCAMLFIND) ocamldep
LIB = -I `$(CAMLP4) -where` -I +camlp4/Camlp4Parsers $(LIBDIRS2)
PP = -pp "camlp4rf -- -loc loc"
PPLEXER = -pp "camlp4of -- -loc loc"

OBJS = $(FILESR:.ml=.cmo) $(FILES:.ml=.cmo)
OBJSOPT = $(FILESR:.ml=.cmx) $(FILES:.ml=.cmx)

CMA = xhtmlsyntax.cma
CMXA = xhtmlsyntax.cmxa

pp/simplexmlparser.mli:
	camlp4r simplexmlparser.mli -printer o > pp/simplexmlparser.mli

pp/xhtmltypes.ml:
	camlp4r xhtmltypes.ml -printer o > pp/xhtmltypes.ml
	camlp4r xhtml5types.ml -printer o > pp/xhtml5types.ml

byte: ohl-xhtml-byte $(CMA) $(OBJS) pp/simplexmlparser.mli pp/xhtmltypes.ml
	ln -sf newocaml/xhtmltypes.cmi newocaml/xhtml5types.cmi newocaml/xhtmltypes.cmo newocaml/xhtml5types.cmo newocaml/basicTypes.cmi newocaml/basicTypes.cmo newocaml/simplexmlparser.cmi newocaml/simplexmlparser.cmo newocaml/xmllexer.cmi newocaml/xmllexer.cmo newocaml/camllexer.cmi newocaml/camllexer.cmo newocaml/xhtmlsyntax.cma ..


opt: ohl-xhtml-opt $(CMXA) $(OBJSOPT)
	ln -sf newocaml/xhtmltypes.cmi newocaml/xhtml5types.cmi newocaml/xhtmltypes.cmx newocaml/xhtmltypes.o newocaml/xhtml5types.cmx newocaml/xhtml5types.o newocaml/basicTypes.cmi newocaml/basicTypes.cmx newocaml/basicTypes.o newocaml/simplexmlparser.cmi newocaml/simplexmlparser.cmx newocaml/simplexmlparser.o newocaml/xmllexer.cmi newocaml/xmllexer.cmx newocaml/xmllexer.o newocaml/camllexer.cmi newocaml/camllexer.cmx newocaml/camllexer.o newocaml/xhtmlsyntax.cmxa newocaml/xhtmlsyntax.a ..

.PHONY: depend ohl-xhtml-byte ohl-xhtml-opt

ohl-xhtml-byte:
	-rm -f ../xhtmltypes.cmi ../xhtml5types.cmi ../basicTypes.cmi ../simplexmlparser.cmi ../xmllexer.cmi
	$(MAKE) -C ../ohl-xhtml byte
	cp ../ohl-xhtml/xhtml.cma ../ohl-xhtml/xHTML.cmi ../ohl-xhtml/xHTML5.cmi ../ohl-xhtml/xML.cmi ..

ohl-xhtml-opt:
	-rm -f ../xhtmltypes.cmi ../xhtml5types.cmi ../basicTypes.cmi ../simplexmlparser.cmi ../xmllexer.cmi
	$(MAKE) -C ../ohl-xhtml opt
	cp ../ohl-xhtml/xhtml.cmxa ../ohl-xhtml/xhtml.a ../ohl-xhtml/xHTML.cmi  ../ohl-xhtml/xHTML5.cmi ../ohl-xhtml/xML.cmi ..

$(CMA): basicTypes.cmo xmllexer.cmo camllexer.cmo xhtmlparser.cmo xhtmlsyntax.cmo
	$(CAMLC) -a -o $(CMA) xmllexer.cmo camllexer.cmo xhtmlparser.cmo xhtmlsyntax.cmo

$(CMXA): basicTypes.cmx xmllexer.cmx camllexer.cmx xhtmlparser.cmx xhtmlsyntax.cmx
	$(CAMLOPT) -a -o $(CMXA) xmllexer.cmx camllexer.cmx xhtmlparser.cmx xhtmlsyntax.cmx


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
	$(MAKE) -C ../ohl-xhtml clean


# Dependencies
depend: *.mli *.ml
	-rm -f xmllexer.ml camllexer.ml
	$(CAMLDEP) $(PP) $(LIBDIRS2) *.mli *.ml > .depend

FORCE:

-include .depend

