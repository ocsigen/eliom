include ../Makefile.config

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

FILES= db_create.ml ocsipersist.ml ocsicache.ml ocsidata.ml ocsipages.ml ocsisav.ml ocsiboxes.ml

OTHERFILES= ocsexample_util.ml ocsexample3.ml ocsexample1.ml ocsexample2.ml

CAMLC = $(OCAMLFIND) ocamlc $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) ocamlopt $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
LIB = -package dbi.postgresql -I ../lib
PP = -pp "camlp4o ../lib/xhtmlsyntax.cma -- -loc loc"

OBJS = $(FILES:.ml=.cmo)
CMI = $(FILES:.ml=.cmi)
OTHEROBJS = $(OTHERFILES:.ml=.cmo)

CMA = ocsimore.cma

all: $(CMA) $(OTHEROBJS)
	cp *.cma $(OTHEROBJS) ../lib

$(CMA): $(OBJS)
	$(CAMLC) -a -o $(CMA) $(OBJS)



.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

doc:
	$(CAMLDOC) -d doc -html ocsicache.mli

clean:
	-rm -f *.cm[iox] *~ $(NAME) *.annot

depend:
	$(CAMLDEP) $(PP) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


