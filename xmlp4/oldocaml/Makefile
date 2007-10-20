include ../../Makefile.config

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

FILESR = xmllexer.ml 
FILES = xhtmltypes.ml xmlsyntax.ml xhtmlsyntax.ml xmlparser.ml simplexmlparser.ml xhtmlparser.ml xhtmlpp.ml

CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB) -package camlp4
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB) -package camlp4
CAMLDEP = $(OCAMLFIND) ocamldep -package camlp4
LIB = -I `$(CAMLP4) -where` $(LIBDIRS2)
PP = -syntax camlp4o -ppopt "pa_extend.cmo" -ppopt "q_MLast.cmo" -ppopt "-locloc"
PPLEXER = -syntax camlp4r -ppopt "-locloc"

SRCLIBREP = ../../lib

OBJS = $(FILESR:.ml=.cmo) $(FILES:.ml=.cmo)
OBJSOPT = $(FILESR:.ml=.cmx) $(FILES:.ml=.cmx)

CMA = xhtmlsyntax.cma
CMXA = xhtmlsyntax.cmxa

byte: depend ohl-xhtml-byte $(CMA) $(OBJS) essai
	ln -sf oldocaml/xhtmltypes.cmi oldocaml/xhtmltypes.cmo oldocaml/xmlparser.cmi oldocaml/simplexmlparser.cmi oldocaml/simplexmlparser.cmo oldocaml/xmlparser.cmo oldocaml/xmlsyntax.cmo oldocaml/xmllexer.cmo oldocaml/xhtmlpp.cmo oldocaml/xhtmlpp.cmi oldocaml/xhtmlsyntax.cma ..
#	cp -p xhtmltypes.cmi xhtmltypes.cmo xmlparser.cmi simplexmlparser.cmi simplexmlparser.cmo xmlparser.cmo xmlsyntax.cmo xmllexer.cmo xhtmlpp.cmo xhtmlpp.cmi xhtmlsyntax.cma ..

opt: ohl-xhtml-opt $(CMXA) $(OBJSOPT)
	ln -sf oldocaml/xhtmltypes.cmi oldocaml/xhtmltypes.cmx oldocaml/xhtmltypes.o oldocaml/xmlparser.cmi oldocaml/simplexmlparser.cmi oldocaml/simplexmlparser.cmx oldocaml/simplexmlparser.o oldocaml/xmlparser.cmx oldocaml/xmlparser.o oldocaml/xmlsyntax.cmx oldocaml/xmlsyntax.o oldocaml/xmllexer.cmx oldocaml/xmllexer.o oldocaml/xhtmlpp.cmx oldocaml/xhtmlpp.o oldocaml/xhtmlpp.cmi oldocaml/xhtmlsyntax.cmxa oldocaml/xhtmlsyntax.a ..


#.PHONY: depend
.PHONY: ohl-xhtml

ohl-xhtml-byte:
	-rm -f ../xhtmltypes.cmi ../basicTypes.cmi ../simplexmlparser.cmi ../xmllexer.cmi
	$(MAKE) -C ../ohl-xhtml byte
	ln -sf ohl-xhtml/xhtml.cma ohl-xhtml/xHTML.cmi ohl-xhtml/xML.cmi ..

ohl-xhtml-opt:
	-rm -f ../xhtmltypes.cmi ../basicTypes.cmi ../simplexmlparser.cmi ../xmllexer.cmi
	$(MAKE) -C ../ohl-xhtml opt
	ln -sf ohl-xhtml/xhtml.cmxa ohl-xhtml/xhtml.a ohl-xhtml/xHTML.cmi ohl-xhtml/xML.cmi ..

essai: $(OBJS)
	$(MAKE) -C exemple

$(CMA): xmllexer.cmo xhtmlparser.cmo xhtmlsyntax.cmo
	$(CAMLC) -a -o $(CMA) xmllexer.cmo xhtmlparser.cmo xhtmlsyntax.cmo

$(CMXA): xmllexer.cmx xhtmlparser.cmx xhtmlsyntax.cmx
	$(CAMLOPT) -a -o $(CMXA) xmllexer.cmx xhtmlparser.cmx xhtmlsyntax.cmx

xmllexer.cmo: xmllexer.ml
	$(CAMLC) $(PPLEXER) -c $<

xmllexer.cmx: xmllexer.ml
	$(CAMLOPT) $(PPLEXER) -c $<


# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(PP) -c $<
.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<


# doc
doc:
	mkdir doc
	ocamldoc -sort -html -d doc tdpe.ml # tdpe.mli


# Clean up
clean:
	-rm -f *.cm[ioax] *.cmxa *~ *.annot
	$(MAKE) -C exemple clean
	$(MAKE) -C ../ohl-xhtml clean


# Dependencies
depend: *.mli $(FILES)
	$(CAMLDEP) $(PP) $(LIBDIRS2) *.mli $(FILES) > depend

FORCE:

include depend

