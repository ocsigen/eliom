include ../../Makefile.config

FILES= ocsipersist.ml
OBJDBM= ocsipersist-dbm.cma


ifeq "$(NATDYNLINK)" "YES"
CMXS=$(OBJDBM:.cma=.cmxs)
else
CMXS=
endif


LIB = -thread -package lwt.unix,netstring,dbm $(LIBDIRS2)
CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep $(LIBDIRS2)
PP = 
PP2 = 
# -pp "$(CAMLP4O) ../lib/xhtmlsyntax.cma -- -loc loc"


OBJS = $(FILES:.ml=.cmo) ocsidbm $(OBJDBM)

OBJSOPT = $(FILES:.ml=.cmx) ocsidbm.opt $(CMXS) $(OBJDBM:.cma=.cmxa)


byte: ocsipersist.mli $(OBJS)

opt: ocsipersist.mli $(OBJSOPT)
	[ -f ../ocsipersist.cmx ] || \
	cp ocsipersist.cmx ..
	[ -f ../ocsipersist.o ] || \
	cp ocsipersist.o ..


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

ocsipersist-dbm.cma: ocsipersist.cmo
	$(CAMLC) -a -o ocsipersist-dbm.cma ocsipersist.cmo
	cp -f ocsipersist-dbm.cma ..

ocsipersist-dbm.cmxa: ocsipersist.cmx
	$(CAMLOPT) -a -o ocsipersist-dbm.cmxa ocsipersist.cmx
	cp -f ocsipersist-dbm.cmxa ..

ocsipersist-dbm.cmxs: ocsipersist.cmx
	$(CAMLOPT) -shared -linkall -o ocsipersist-dbm.cmxs ocsipersist.cmx
	cp -f ocsipersist-dbm.cmxs ..

ocsidbm: ocsidbm.cmo
	$(CAMLC) -linkpkg -o $@ $<

ocsidbm.opt: ocsidbm.cmx
	$(CAMLOPT) -linkpkg -o $@ $<

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
