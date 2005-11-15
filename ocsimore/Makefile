
FILES= db_create.ml ocsipersist.ml ocsicache.ml ocsidata.ml ocsipages.ml ocsisav.ml ocsiboxes.ml ocsexample_util.ml ocsexample3.ml ocsexample1.ml ocsexample2.ml

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
	$(CAMLDOC) -d doc -html ocsicache.mli

clean:
	-rm -f *.cm[iox] *~ $(NAME)

depend:
	$(CAMLDEP) $(PP) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


