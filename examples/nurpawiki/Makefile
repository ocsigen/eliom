
FILES=$(wildcard *.ml)

CAMLC = ocamlc -g -I ../../baselib -I ../../xmlp4 -I ../../modules -I ../../lwt ocsigenmod.cmo ocsigen.cmo

#ocamlfind ocamlc -g $(LIB)
CAMLDEP = ocamlfind ocamldep

OCSIGENREP = `ocamlfind query ocsigen`

#OCSIGENREP = ../ocsigen/lib
 # ^ pour l'instant
LIB = -package str -I $(OCSIGENREP)
PP = -pp "camlp4o $(OCSIGENREP)/xhtmlsyntax.cma"

OBJS = $(FILES:.ml=.cmo)

CMA = nurpawiki.cma

all: $(CMA) install

$(CMA): $(OBJS)
	$(CAMLC) -a -o $(CMA) $(OBJS)

install:
	chmod a+r $(CMA)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<

clean:
	-rm -f *.cm[ioxa] *~ $(NAME)

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

