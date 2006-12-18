include ../../Makefile.config

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

FILESR = xmllexer.ml 
FILES = xhtmltypes.ml xmlsyntax.ml xhtmlsyntax.ml xmlparser.ml simplexmlparser.ml xhtmlparser.ml xhtmlpp.ml

CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) ocamlopt $(DBG) $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
LIB = -I `$(CAMLP4) -where` -I ../../lib
PP = -pp "camlp4o pa_extend.cmo q_MLast.cmo -- -loc loc"
PPLEXER = -pp "camlp4r -- -loc loc"

SRCLIBREP = ../../lib

OBJS = $(FILESR:.ml=.cmo) $(FILES:.ml=.cmo)

CMA = xhtmlsyntax.cma

all: ohl-xhtml $(CMA) $(OBJS) essai
	ln -sf oldocaml/xhtmlsyntax.cma ..
	ln -sf oldocaml/xhtmltypes.cma ..
	ln -sf oldocaml/xhtmltypes.cmi ..
	cp xhtmltypes.cmi xhtmltypes.cmo xmlparser.cmi simplexmlparser.cmi simplexmlparser.cmo xmlparser.cmo xmlsyntax.cmo xmllexer.cmo xhtmlpp.cmo xhtmlpp.cmi xhtmlsyntax.cma $(SRCLIBREP)


.PHONY: depend
.PHONY: ohl-xhtml

ohl-xhtml:
	$(MAKE) -C ../ohl-xhtml all
	cp ../ohl-xhtml/xhtml.cma ../ohl-xhtml/xHTML.cmi ../ohl-xhtml/xML.cmi $(SRCLIBREP)

essai: $(OBJS)
	$(MAKE) -C exemple

$(CMA): xmllexer.cmo xhtmlparser.cmo xhtmlsyntax.cmo
	$(CAMLC) -a -o $(CMA) xmllexer.cmo xhtmlparser.cmo xhtmlsyntax.cmo

xmllexer.cmo: xmllexer.ml
	$(CAMLC) $(PPLEXER) -c $<


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
	-rm -f *.cm[ioax] *~ *.annot
	$(MAKE) -C exemple clean
	$(MAKE) -C ../ohl-xhtml clean


# Dependencies
depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend

