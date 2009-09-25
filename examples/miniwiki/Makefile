include ../../Makefile.config

FILES=$(wildcard *.ml)

LIB = -package lwt.unix,pcre $(LIBDIRS2)
CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep

ifeq "$(NATDYNLINK)" "YES"
CMXS=$(FILES:.ml=.cmxs)
CMXSCMD=cp $(OBJSOPT) ..
else
CMXS=
CMXSCMD=
endif


OBJS=$(FILES:.ml=.cmo)
OBJSOPT=$(CMXS)

byte: $(OBJS)
	cp $(OBJS) ..

opt: $(OBJSOPT)
	$(CMXSCMD)


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
