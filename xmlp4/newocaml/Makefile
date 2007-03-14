include ../../Makefile.config

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

FILESR = xmllexer.ml 
FILES = basicTypes.ml xhtmltypes.ml simplexmlparser.ml xhtmlsyntax.ml xhtmlparser.ml 

CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) ocamlopt $(DBG) $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
LIB = -I `$(CAMLP4) -where` -I +camlp4/Camlp4Parsers -I ../../lib
PP = -pp "camlp4rf -- -loc loc"
PPLEXER = -pp "camlp4of -- -loc loc"

SRCLIBREP = ../../lib

OBJS = $(FILESR:.ml=.cmo) $(FILES:.ml=.cmo)

CMA = xhtmlsyntax.cma

all: ohl-xhtml $(CMA) $(OBJS)
	ln -sf newocaml/xhtmlsyntax.cma ..
	ln -sf newocaml/xhtmltypes.cma ..
	ln -sf newocaml/xhtmltypes.cmi ..
	ln -sf newocaml/simplexmlparser.cmi ..
#	cp xhtmltypes.cmi xhtmltypes.cmo xmlparser.cmi simplexmlparser.cmi simplexmlparser.cmo xmlparser.cmo xmlsyntax.cmo xmllexer.cmo xhtmlpp.cmo xhtmlpp.cmi xhtmlsyntax.cma $(SRCLIBREP)
	cp xhtmltypes.cmi xhtmltypes.cmo basicTypes.cmi basicTypes.cmo simplexmlparser.cmi simplexmlparser.cmo xmllexer.cmi xmllexer.cmo xhtmlsyntax.cma $(SRCLIBREP)

.PHONY: depend
.PHONY: ohl-xhtml

ohl-xhtml:
	$(MAKE) -C ../ohl-xhtml all
	cp ../ohl-xhtml/xhtml.cma ../ohl-xhtml/xHTML.cmi ../ohl-xhtml/xML.cmi $(SRCLIBREP)

$(CMA): basicTypes.cmo xmllexer.cmo xhtmlparser.cmo xhtmlsyntax.cmo
	$(CAMLC) -a -o $(CMA) xmllexer.cmo xhtmlparser.cmo xhtmlsyntax.cmo

xmllexer.cmo: xmllexer.ml
	$(CAMLC) $(PPLEXER) -c $<

xmllexer.ml: xmllexer.mll
	$(CAMLLEX) $<


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
	-rm -f xmllexer.ml *.cm[ioax] *~ *.annot
	$(MAKE) -C ../ohl-xhtml clean


# Dependencies
depend:
	rm -f xhtmltypes.ml
	camlp4 -parser OCaml -printer OCamlr ../oldocaml/xhtmltypes.ml > \
		xhtmltypes.ml
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend

