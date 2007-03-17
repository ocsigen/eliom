include ../Makefile.config

FILES=$(wildcard *.ml)

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

ifeq "$(OCAMLDUCE)" "YES"
DUCEMODBYTE=exampleduce.cmo
DUCEINSTALLBYTE=ocamlduce/exampleduce.cmo
DUCEMODOPT=exampleduce.cmxs
DUCEINSTALLOPT=ocamlduce/exampleduce.cmxs
else
DUCEMODBYTE=
DUCEINSTALLBYTE=
DUCEMODOPT=
DUCEINSTALLOPT=
endif


CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
LIB = $(LIBDIRS)
PP = -pp "camlp4o ../xmlp4/xhtmlsyntax.cma -loc loc"

OBJS = $(FILES:.ml=.cmo)
OBJSOPT = $(FILES:.ml=.cmxs)

byte: $(OBJS) $(DUCEMODBYTE)

opt: $(OBJSOPT) $(DUCEMODOPT)

exampleduce.cmo:
	$(OCAMLDUCEFIND) ocamlc $(DBG) $(LIB) -c ocamlduce/exampleduce.ml

exampleduce.cmx:
	$(OCAMLDUCEFIND) ocamlopt $(DBG) $(LIB) -c ocamlduce/exampleduce.ml

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs

.PHONY: depend

.ml.cmo:
	$(CAMLC) $(PP) -c $<
.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<
.cmx.cmxs:
	$(CAMLOPT) -shared -linkall -o $@ $<

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *~ $(NAME) *.annot ocamlduce/*.cm[ioax] ocamlduce/*.cmxa ocamlduce/*.cmxs ocamlduce/*~ ocamlduce/*.annot

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


