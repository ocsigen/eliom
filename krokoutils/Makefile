
FILES= db_create.ml krokopersist.ml krokache.ml krokodata.ml krokopages.ml krokosavable.ml krokoboxes.ml krokoxample_util.ml krokoxample3.ml krokoxample1.ml krokoxample2.ml

CAMLC = ocamlfind ocamlc -g $(LIB)
CAMLOPT = ocamlfind ocamlopt $(LIB)
CAMLDOC = ocamlfind ocamldoc $(LIB)
CAMLDEP = ocamlfind ocamldep
LIB = -package dbi.postgresql -I ../lib
PP = -pp "camlp4o ../lib/xmllexer.cmo ../lib/xmlparser.cmo"

OBJS = $(FILES:.ml=.cmo)

all: depend $(OBJS) doc

install: all
	cp *.cm? ../lib

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
	$(CAMLDOC) -d doc -html krokache.mli

clean:
	-rm -f *.cm[iox] *~ $(NAME)

depend:
	$(CAMLDEP) $(PP) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


