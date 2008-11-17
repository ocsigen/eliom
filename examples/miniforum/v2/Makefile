# Write here all the findlib packages you need, for example:
# PACKAGES= ,extlib,netstring

# Write here all your .ml files, in dependency order (default: all)
FILES=services.ml mylib.ml forum0.ml forum1.ml forum2.ml

CAMLC = ocamlfind ocamlc -g -thread $(LIB)
CAMLOPT = ocamlfind ocamlopt -thread $(LIB)
CAMLDOC = ocamlfind ocamldoc $(LIB)
CAMLDEP = ocamlfind ocamldep
OCSIGENREP = `ocamlfind query ocsigen`
#OCSIGENREP = ../ocsigen/lib
LIB = -package lwt,ocsigen$(PACKAGES) -I $(OCSIGENREP)
# If you use the syntax extension:
# PP = -pp "camlp4o $(OCSIGENREP)/xhtmlsyntax.cma"
# otherwise
PP = 

OBJS = $(FILES:.ml=.cmo)

CMA = site0.cma site1.cma site2.cma

all: depend $(CMA) install

#$(CMA): $(OBJS)
#	$(CAMLC) -a -o $(CMA) $(OBJS)

site0.cma: mylib.cmo forum.cmo 
	$(CAMLC) -a -o $@ $^

site1.cma: mylib.cmo forum.cmo 
	$(CAMLC) -a -o $@ $^

site2.cma: services.cmo mylib.cmo forum.cmo 
	$(CAMLC) -a -o $@ $^


install:
	chmod a+r $(CMA)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

doc:
#	$(CAMLDOC) -d doc -html db.mli

clean:
	-rm -f *.cm[ioxa] *~ $(NAME)

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

-include .depend


