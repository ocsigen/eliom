include ../../Makefile.config

PP = -pp camlp4of
LIBS = -I +camlp4 -I +camlp4/Camlp4Printers -I +camlp4/Camlp4Parsers -I +camlp4/Camlp4Filters

OCAMLDEP = $(OCAMLFIND) ocamldep $(LIBS) $(PP)
OCAMLC = $(OCAMLFIND) $(CAMLCNAME) $(LIBS) $(PP)

FILES:= pa_eliom_seed.ml \
	pa_eliom_type_filter.ml \
	pa_eliom_client_client.ml \
	pa_eliom_client_server.ml

all: $(FILES:.ml=.cmo)

%.cmo: %.ml
	$(OCAMLC) -c $<

clean:
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *~ doc/*  *.annot

depend:
	$(OCAMLDEP) $(FILES) > .depend

-include .depend
