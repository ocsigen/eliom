
FILES= db_create.ml ocsipersist.ml ocsicache.ml ocsidata.ml ocsipages.ml ocsisav.ml ocsiboxes.ml

OTHERFILES= ocsexample_util.ml ocsexample3.ml ocsexample1.ml ocsexample2.ml

OCAMLFIND = ocamlfind
CAMLC = ocamlfind ocamlc -g $(LIB)
CAMLOPT = ocamlfind ocamlopt $(LIB)
CAMLDOC = ocamlfind ocamldoc $(LIB)
CAMLDEP = ocamlfind ocamldep
LIB = -package dbi.postgresql -I ../lib
PP = -pp "camlp4o ../lib/xmllexer.cmo ../lib/xhtmlparser.cmo ../lib/xhtmlsyntax.cmo"

OBJS = $(FILES:.ml=.cmo)
CMI = $(FILES:.ml=.cmi)
OTHEROBJS = $(OTHERFILES:.ml=.cmo)

CMA = ocsimore.cma

all: depend $(CMA) $(OTHEROBJS)
	cp *.cma $(OTHEROBJS) ../lib

$(CMA): $(OBJS)
	$(CAMLC) -a -o $(CMA) $(OBJS)



.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

doc:
	$(CAMLDOC) -d doc -html ocsicache.mli

clean:
	-rm -f *.cm[iox] *~ $(NAME)

.PHONY: install
install:
	$(OCAMLFIND) install ocsigen $(CMA) $(CMI) $(OTHEROBJS) ../modules/tutorial.cmo ../modules/profiling.cmo ../server/ocsigen.cmi ../xmlp4/ohl-xhtml/xHTML.cmi ../xmlp4/ohl-xhtml/xML.cmi ../xmlp4/ohl-xhtml/xhtml.cma ../xmlp4/xhtmltypes.cmi ../xmlp4/xmllexer.cmo ../xmlp4/xhtmlparser.cmo ../xmlp4/xhtmlsyntax.cmo META

.PHONY: uninstall
uninstall:
	$(OCAMLFIND) remove ocsigen


depend:
	$(CAMLDEP) $(PP) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


