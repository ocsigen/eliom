
FILES= db_create.ml persistant.ml cache.ml

CAMLC = ocamlc -g $(LIB)
CAMLOPT = ocamlopt $(LIB)
CAMLDOC = ocamldoc $(LIB)
CAMLDEP = ocamldep
LIB = -I +postgres -I +dbi -I +pcre
PP = 

CMAO = pcre.cma postgres.cma dbi.cma dbi_postgres.cmo

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
	$(CAMLDOC) -d doc -html cache.ml persistant.ml db_create.ml

clean:
	-rm -f *.cm[iox] *~ $(NAME)

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


