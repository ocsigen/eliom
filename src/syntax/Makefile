include ../../Makefile.config

PACKAGE  := camlp4.quotations.o,camlp4.extend
LIBS := ${addprefix -package ,${PACKAGE}}

OCAMLDEP := $(OCAMLFIND) ocamldep -syntax camlp4o
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG} -syntax camlp4o

FILES:= pa_eliom_seed.ml          \
	pa_eliom_type_filter.ml   \
	pa_eliom_client_client.ml \
	pa_eliom_client_server.ml \
	pa_include.ml             \

all: $(FILES:.ml=.cmo)

install:
	${INSTALL} -m 755 -d ${SYNTAXDIR}
	${INSTALL} -m 644 ${FILES:.ml=.cmi} ${FILES:.ml=.cmo} ${SYNTAXDIR}

uninstall:
	-rm -fr ${SYNTAXDIR}

%.cmo: %.ml
	$(OCAMLC) ${LIBS} -c $<

clean:
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *~ doc/*  *.annot

distclean: clean
	-rm -f *~ \#* .\#*

depend:
	$(OCAMLDEP) ${LIBS} $(FILES) > .depend

-include .depend
