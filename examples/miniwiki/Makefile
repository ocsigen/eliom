include ../../Makefile.config

FILES=$(wildcard *.ml)

LIB = -package pcre $(LIBDIRS2)
CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep

OBJS = $(FILES:.ml=.cmo)
OBJSOPT = $(FILES:.ml=.cmxs)

byte: $(OBJS)
	cp $(OBJS) ..

opt: $(OBJSOPT)
	cp $(OBJSOPT) ..

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs

.PHONY: doc

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

.cmx.cmxs:
	$(CAMLOPT) -shared -linkall -o $@ $<

clean:
	-rm -f *.cm[ioxa] *.cmxa *.cmxs *.o *~ $(NAME)

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

-include .depend
