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


LIB = $(LIBDIRS) -package netstring,camlp4
CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep $(LIB)
PP = -syntax camlp4o -ppopt "../xmlp4/xhtmlsyntax.cma" -ppopt "-loc loc"

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
	$(MAKE) -C nurpawiki byte

nurpawiki.opt:
	$(MAKE) -C nurpawiki opt

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs

.PHONY: depend


.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	-rm ../modules/ocsipersist.cmx
	$(CAMLOPT) $(PP) -c $<
	touch ../modules/ocsipersist.cmx

.cmx.cmxs:
	$(CAMLOPT) -shared -linkall -o $@ $<

clean:
	$(MAKE) -C nurpawiki clean
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *~ $(NAME) *.annot ocamlduce/*.cm[ioax] ocamlduce/*.cmxa ocamlduce/*.cmxs ocamlduce/*~ ocamlduce/*.annot

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


