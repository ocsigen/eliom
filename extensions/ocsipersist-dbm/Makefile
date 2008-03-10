include ../../Makefile.config

FILES= ocsipersist.ml
OBJDBM= ocsipersist-dbm.cma

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

ifeq "$(PROFILING)" "YES"
CAMLCNAME= ocamlcp -p a
else
CAMLCNAME= ocamlc
endif

LIB = -thread -package netstring $(LIBDIRS2)
CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep $(LIBDIRS2)
PP = 
PP2 = 
# -pp "$(CAMLP4O) ../lib/xhtmlsyntax.cma -- -loc loc"


OBJS = $(FILES:.ml=.cmo) ocsidbm $(OBJDBM)

OBJSOPT = $(FILES:.ml=.cmx) ocsidbm.opt $(OBJDBM:.cma=.cmxs)


byte: ocsipersist.mli $(OBJS)

opt: ocsipersist.mli $(OBJSOPT)
	[ -f ../ocsipersist.cmx ] || \
	cp ocsipersist.cmx ..


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

ocsipersist-dbm.cma: ocsipersist.cmo
	$(CAMLC) -a -o ocsipersist-dbm.cma ocsipersist.cmo

ocsipersist-dbm.cmxs: ocsipersist.cmx
	$(CAMLOPT) -shared -linkall -o ocsipersist-dbm.cmxs ocsipersist.cmx

ocsidbm: ocsidbm.cmo
	$(CAMLC) -package ssl -o $@ unix.cma ssl.cma dbm.cma lwt.cma $<

ocsidbm.opt: ocsidbm.cmx
	$(CAMLOPT) -package ssl -o $@ unix.cmxa ssl.cmxa dbm.cmxa lwt.cmxa $<

ocsipersist.mli:
	cp -f ../ocsipersist.mli .

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

#doc:
#	$(CAMLDOC) -d doc -html eliom.mli ocsigen.mli ocsigen_extensions.mli

.PHONY: clean
clean:
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.cmx *.o *.a *~ doc/*  *.annot ocsidbm ocsidbm.opt ocsipersist.mli

depend: ocsipersist.mli
	$(CAMLDEP) $(PP2) $(FILES:.ml=.mli) $(FILES) > .depend


FORCE:

-include .depend


