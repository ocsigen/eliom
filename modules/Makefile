FILES=tutorial.ml profiling.ml

CAMLC = ocamlc $(LIB)
CAMLOPT = ocamlopt $(LIB)
CAMLDEP = ocamldep
LIB = -I ../lib
PP = -pp "camlp4o ../lib/xmllexer.cmo ../lib/xmlparser.cmo"

OBJS = $(FILES:.ml=.cmo)

all: depend $(OBJS)
	cp *.cmo *.cmi ../lib

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: depend

.ml.cmo:
	$(CAMLC) $(PP) -c $<
.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

clean:
	-rm -f *.cm[iox] *~ $(NAME)

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


