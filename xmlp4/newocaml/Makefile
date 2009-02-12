include ../../Makefile.config


FILESR = xmllexer.ml 
FILES = basicTypes.ml xhtmltypes.ml simplexmlparser.ml xhtmlsyntax.ml xhtmlparser.ml 

CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB) 
# -package camlp4
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB) 
# -package camlp4
CAMLDEP = $(OCAMLFIND) ocamldep
LIB = -I `$(CAMLP4) -where` -I +camlp4/Camlp4Parsers $(LIBDIRS2)
PP = -pp "camlp4rf -- -loc loc"
PPLEXER = -pp "camlp4of -- -loc loc"
# PP = -syntax camlp4rf -ppopt "-loc loc"
# PPLEXER = -syntax camlp4of -ppopt "-loc loc"

OBJS = $(FILESR:.ml=.cmo) $(FILES:.ml=.cmo)
OBJSOPT = $(FILESR:.ml=.cmx) $(FILES:.ml=.cmx)

CMA = xhtmlsyntax.cma
CMXA = xhtmlsyntax.cmxa

byte: ohl-xhtml-byte $(CMA) $(OBJS)
	ln -sf newocaml/xhtmltypes.cmi newocaml/xhtmltypes.cmo newocaml/basicTypes.cmi newocaml/basicTypes.cmo newocaml/simplexmlparser.cmi newocaml/simplexmlparser.cmo newocaml/xmllexer.cmi newocaml/xmllexer.cmo newocaml/xhtmlsyntax.cma ..

opt: ohl-xhtml-opt $(CMXA) $(OBJSOPT)
	ln -sf newocaml/xhtmltypes.cmi newocaml/xhtmltypes.cmx newocaml/xhtmltypes.o newocaml/basicTypes.cmi newocaml/basicTypes.cmx newocaml/basicTypes.o newocaml/simplexmlparser.cmi newocaml/simplexmlparser.cmx newocaml/simplexmlparser.o newocaml/xmllexer.cmi newocaml/xmllexer.cmx newocaml/xmllexer.o newocaml/xhtmlsyntax.cmxa newocaml/xhtmlsyntax.a ..

.PHONY: depend ohl-xhtml-byte ohl-xhtml-opt

ohl-xhtml-byte:
	-rm -f ../xhtmltypes.cmi ../basicTypes.cmi ../simplexmlparser.cmi ../xmllexer.cmi
	$(MAKE) -C ../ohl-xhtml byte
	cp ../ohl-xhtml/xhtml.cma ../ohl-xhtml/xHTML.cmi ../ohl-xhtml/xML.cmi ..

ohl-xhtml-opt:
	-rm -f ../xhtmltypes.cmi ../basicTypes.cmi ../simplexmlparser.cmi ../xmllexer.cmi
	$(MAKE) -C ../ohl-xhtml opt
	cp ../ohl-xhtml/xhtml.cmxa ../ohl-xhtml/xhtml.a ../ohl-xhtml/xHTML.cmi ../ohl-xhtml/xML.cmi ..

$(CMA): basicTypes.cmo xmllexer.cmo xhtmlparser.cmo xhtmlsyntax.cmo
	$(CAMLC) -a -o $(CMA) xmllexer.cmo xhtmlparser.cmo xhtmlsyntax.cmo

$(CMXA): basicTypes.cmx xmllexer.cmx xhtmlparser.cmx xhtmlsyntax.cmx
	$(CAMLOPT) -a -o $(CMXA) xmllexer.cmx xhtmlparser.cmx xhtmlsyntax.cmx

xmllexer.cmo: xmllexer.ml
	$(CAMLC) $(PPLEXER) -c $<

xmllexer.cmx: xmllexer.ml
	$(CAMLOPT) $(PPLEXER) -c $<

xmllexer.ml: xmllexer.mll
	$(CAMLLEX) $<

xhtmltypes.ml:
	camlp4 -parser OCaml -printer OCamlr ../oldocaml/xhtmltypes.ml > \
		xhtmltypes.ml

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(PP) -c $<
.mli.cmi:
	$(CAMLC) $(PP) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<


# doc
doc:
	mkdir doc
	ocamldoc -sort -html -d doc tdpe.ml # tdpe.mli


# Clean up
clean:
	-rm -f xhtmltypes.ml xmllexer.ml *.cm[ioax] *.cmxa *~ *.annot *.a *.o
	$(MAKE) -C ../ohl-xhtml clean


# Dependencies
depend: *.mli *.ml
#	-rm -f xhtmltypes.ml
#	camlp4 -parser OCaml -printer OCamlr ../oldocaml/xhtmltypes.ml > \
#		xhtmltypes.ml
	-rm -f xmllexer.ml
	$(CAMLDEP) $(PP) $(LIBDIRS2) *.mli *.ml > .depend

FORCE:

-include .depend

