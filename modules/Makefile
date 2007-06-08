include ../Makefile.config

FILES= ocsidbm.ml ocsipersist.ml \
       eliommod.ml eliom.ml eliomboxes.ml \
       ocsigenmod.ml ocsigen.ml ocsigenboxes.ml staticmod.ml \
       extensiontemplate.ml

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

ifeq "$(OCAMLDUCE)" "YES"
DUCEFILES= xhtml1_strict.ml ocsigenduce.ml eliomduce.ml
# rss2.ml ocsigenrss.ml

  ifeq "$(BYTECODE)" "YES"
  DUCEMODBYTE=ocsigenduce.cma eliomduce.cma
# ocsigenrss.cma
  else
  DUCEMODBYTE=
  endif

  ifeq "$(NATIVECODE)" "YES"
  DUCEMODOPT=ocsigenduce.cmxs eliomduce.cmxs
# ocsigenrss.cmxs
  else
  DUCEMODOPT=
  endif

OCAMLFIND=$(OCAMLDUCEFIND)
else
DUCEFILES=
DUCEMODBYTE=
DUCEMODOPT=
endif


ifeq "$(NEWOCAMLNET)" "YES"
NETSYS=netsys
NETSYSCMA=netsys.cma
NETSYSCMXA=netsys.cmxa
else
NETSYS=
NETSYSCMA=
NETSYSCMXA=
endif

# LIB=$(LIBDIRS)

LIB = -thread -package netstring $(LIBDIRS)
CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
PP = 
PP2 = 
# -pp "$(CAMLP4O) ../lib/xhtmlsyntax.cma -- -loc loc"


# cma and cmo to be linked in the server:

OBJS = $(FILES:.ml=.cmo) $(DUCEFILES:.ml=.cmo)

OBJSOPT = $(FILES:.ml=.cmx) $(DUCEFILES:.ml=.cmx)


byte: $(OBJS) ocsidbm eliom.cma ocsigenmod.cma staticmod.cmo $(DUCEMODBYTE)

opt: $(OBJSX) ocsidbm.opt eliom.cmxs ocsigenmod.cmxs staticmod.cmxs $(DUCEMODOPT)


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

ocsidbm: ocsidbm.cmo
	$(CAMLC) -package ssl -o $@ unix.cma ssl.cma dbm.cma lwt.cma $<

ocsidbm.opt: ocsidbm.cmx
	$(CAMLOPT) -package ssl -o $@ unix.cmxa ssl.cmxa dbm.cmxa lwt.cmxa $<

eliom.cma: ocsipersist.cmo eliommod.cmo eliom.cmo eliomboxes.cmo
	$(CAMLC) -a -o eliom.cma ocsipersist.cmo eliommod.cmo eliom.cmo eliomboxes.cmo

eliom.cmxs: eliommod.cmx eliom.cmx eliomboxes.cmx
	$(CAMLOPT) -shared -linkall -o eliom.cmxs eliommod.cmx eliom.cmx \
	eliomboxes.cmx

ocsigenmod.cma: ocsigenmod.cmo ocsigen.cmo ocsigenboxes.cmo
	$(CAMLC) -a -o ocsigenmod.cma ocsigenmod.cmo ocsigen.cmo \
	ocsigenboxes.cmo

ocsigenmod.cmxs: ocsigenmod.cmx ocsigen.cmx ocsigenboxes.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenmod.cmxs ocsigenmod.cmx \
	ocsigen.cmx ocsigenboxes.cmx

staticmod.cmxs: staticmod.cmx
	$(CAMLOPT) -shared -linkall -o staticmod.cmxs staticmod.cmx

ocsigenduce.cma: xhtml1_strict.cmo ocsigenduce.cmo
	$(CAMLC) -a -o ocsigenduce.cma xhtml1_strict.cmo ocsigenduce.cmo 

ocsigenduce.cmxs: xhtml1_strict.cmx ocsigenduce.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenduce.cmxs xhtml1_strict.cmx ocsigenduce.cmx 

eliomduce.cma: xhtml1_strict.cmo eliomduce.cmo
	$(CAMLC) -a -o eliomduce.cma xhtml1_strict.cmo eliomduce.cmo 

eliomduce.cmxs: xhtml1_strict.cmx eliomduce.cmx
	$(CAMLOPT) -shared -linkall -o eliomduce.cmxs xhtml1_strict.cmx eliomduce.cmx 

ocsigenrss.cma: rss2.cmo ocsigenrss.cmo
	$(CAMLC) -a -o ocsigenrss.cma $^

ocsigenrss.cmxs: rss2.cmx ocsigenrss.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenrss.cmxs $^

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

#doc:
#	$(CAMLDOC) -d doc -html eliom.mli ocsigen.mli extensions.mli

.PHONY: clean
clean:
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *~ doc/* ocsidbm ocsidbm.opt *.annot

depend:
	$(CAMLDEP) $(PP2) $(FILES:.ml=.mli) $(FILES) $(DUCEFILES:.ml=.mli) $(DUCEFILES) > .depend


FORCE:

include .depend


