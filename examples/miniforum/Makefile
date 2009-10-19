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
LIB = -package lwt.unix,ocsigen$(PACKAGES) -I $(OCSIGENREP)
# If you use the syntax extension:
# PP = -pp "camlp4o $(OCSIGENREP)/xhtmlsyntax.cma"
# otherwise
PP = 

OBJS = $(FILES:.ml=.cmo)

CMA = v0/site0.cma v1/site1.cma v2/site2.cma

.PHONY: $(CMA)

all: depend $(CMA) install

#$(CMA): $(OBJS)
#	$(CAMLC) -a -o $(CMA) $(OBJS)

v0/site0.cma: 
	$(MAKE) -C v0 site0.cma

v1/site1.cma:  
	$(MAKE) -C v1 site1.cma

v2/site2.cma: 
	$(MAKE) -C v2 site2.cma

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
	$(MAKE) -C v0 clean
	$(MAKE) -C v1 clean
	$(MAKE) -C v2 clean

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend
	$(MAKE) -C v0 depend
	$(MAKE) -C v1 depend
	$(MAKE) -C v2 depend


FORCE:

-include .depend


