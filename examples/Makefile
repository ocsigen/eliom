include ../Makefile.config

FILES=$(wildcard *.ml)

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

ifeq "$(OCAMLDUCE)" "YES"
DUCEMODBYTE=exampleduce.cmo
DUCEMODOPT=exampleduce.cmxs
else
DUCEMODBYTE=
DUCEMODOPT=
endif


LIB = $(LIBDIRS) -package netstring
CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
PP = -pp "camlp4o ../xmlp4/xhtmlsyntax.cma -loc loc"

OBJS = $(FILES:.ml=.cmo)
OBJSOPT = $(FILES:.ml=.cmxs)

byte: $(OBJS) $(DUCEMODBYTE) nurpawiki.byte

opt: $(OBJSOPT) $(DUCEMODOPT) nurpawiki.opt

exampleduce.cmo:
	$(OCAMLDUCEFIND) ocamlc $(DBG) $(LIB) -c ocamlduce/exampleduce.ml
	cp ocamlduce/exampleduce.cmo .

exampleduce.cmx:
	$(OCAMLDUCEFIND) ocamlopt $(DBG) $(LIB) -c ocamlduce/exampleduce.ml
	cp ocamlduce/exampleduce.cmx .

nurpawiki.byte:
	make -C nurpawiki byte

nurpawiki.opt:
	make -C nurpawiki opt

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
	make -C nurpawiki clean
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *~ $(NAME) *.annot ocamlduce/*.cm[ioax] ocamlduce/*.cmxa ocamlduce/*.cmxs ocamlduce/*~ ocamlduce/*.annot

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


