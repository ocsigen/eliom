include ../Makefile.config

FILES=$(wildcard *.ml)

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

ifeq "$(OCAMLDUCE)" "YES"
DUCEMOD=exampleduce.cmo
DUCEINSTALL=ocamlduce/exampleduce.cmo
else
DUCEMOD=
DUCEINSTALL=
endif


CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) ocamlopt $(DBG) $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
LIB = -I ../lib
PP = -pp "camlp4o ../lib/xhtmlsyntax.cma -loc loc"

OBJS = $(FILES:.ml=.cmo)

all: $(OBJS) $(DUCEMOD)
	cp *.cmo *.cmi $(DUCEINSTALL) ../lib

exampleduce.cmo:
	$(OCAMLDUCEFIND) ocamlc $(DBG) $(LIB) -c ocamlduce/exampleduce.ml

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
	-rm -f *.cm[ioax] *~ $(NAME) *.annot ocamlduce/*.cm[ioax] ocamlduce/*~ ocamlduce/*.annot

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


