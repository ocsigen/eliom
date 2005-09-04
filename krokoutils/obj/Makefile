
FILES= krokobj.ml

CAMLC = ocamlc -g $(LIB)
CAMLOPT = ocamlopt $(LIB)
CAMLDOC = ocamldoc $(LIB)
CAMLDEP = ocamldep
LIB = -I ../../lib -I +dbi
PP = -pp "camlp4o ../../lib/xmllexer.cmo ../../lib/xmlparser.cmo"

CMAO = 

OBJS = $(FILES:.ml=.cmo)

all: depend $(OBJS) doc

install: all
	cp *.cm? ../../lib

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

doc:
	$(CAMLDOC) $(PP) -d doc -html krokobj.ml

clean:
	-rm -f *.cm[iox] *~ $(NAME)

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


