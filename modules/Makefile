include ../Makefile.config

FILES=tutorial.ml monitoring.ml essais.ml

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

CAMLC = $(OCAMLFIND) ocamlc $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) ocamlopt $(DBG) $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
LIB = -I ../lib
PP = -pp "camlp4o ../lib/xhtmlsyntax.cma -loc loc"

OBJS = $(FILES:.ml=.cmo)

all: $(OBJS)
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
	-rm -f *.cm[ioax] *~ $(NAME) *.annot

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


