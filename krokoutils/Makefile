
FILES= db_create.ml krokopersist.ml krokache.ml krokodata.ml krokopages.ml krokosavable.ml krokoboxes.ml krokoxample.ml

CAMLC = ocamlc -g $(LIB)
CAMLOPT = ocamlopt $(LIB)
CAMLDOC = ocamldoc $(LIB)
CAMLDEP = ocamldep
LIB = -I +postgres -I +dbi -I +pcre -I ../lib
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
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


