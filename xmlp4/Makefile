FILESR = xmllexer.ml 
FILES = xmlparser.ml xhtmlpp.ml

CAMLC = ocamlc $(LIB)
CAMLOPT = ocamlopt $(LIB)
CAMLDEP = ocamldep
CAMLP4 = camlp4
LIB = -I `$(CAMLP4) -where`
PP = -pp "camlp4o pa_extend.cmo q_MLast.cmo"
PPLEXER = -pp "camlp4r"

SRCLIBREP = ../lib

OBJS = $(FILESR:.ml=.cmo) $(FILES:.ml=.cmo)

all: $(OBJS) essai
	cp xmlparser.cmo xmllexer.cmo xhtmlpp.cmo xhtmlpp.cmi $(SRCLIBREP)


.PHONY: depend

essai: $(OBJS)
	make -C exemple

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
	-rm -f *.cm[iox] *~
	make -C exemple clean
	touch .depend


# Dependencies
depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend



