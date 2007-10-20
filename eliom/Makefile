
include ../Makefile.config

# /!\ Simple expanded variable, because of camlzip below
FILES:= eliommod.ml eliomsessions.ml \
       eliomparameters.ml eliomservices.ml \
       eliommkforms.ml eliommkreg.ml eliompredefmod.ml \
       eliomtools.ml \
       ocsigenmod.ml ocsigen.ml ocsigenboxes.ml


ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

ifeq "$(PROFILING)" "YES"
CAMLCNAME2= $(CAMLCPNAME)
else
CAMLCNAME2= $(CAMLCNAME)
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

LIB = -thread -package netstring,cryptokit $(LIBDIRS)
CAMLC = $(OCAMLFIND) $(CAMLCNAME2) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep $(LIBDIRS)
PP = 
PP2 = 
# -pp "$(CAMLP4O) ../lib/xhtmlsyntax.cma -- -loc loc"


OBJS = $(OBJSQLITE) $(FILES:.ml=.cmo) $(DUCEFILES:.ml=.cmo) $(OBJDBM)

OBJSOPT = $(OBJSQLITE:.cma=.cmxs) $(FILES:.ml=.cmx) $(DUCEFILES:.ml=.cmx) $(OBJDBM:.cma=.cmxs)


byte: $(FILESSQLITE:.ml=.cmo) $(OBJS) \
      eliom.cma ocsigenmod.cma $(DUCEMODBYTE)

opt: $(FILESSQLITE:.ml=.cmx) $(OBJSOPT) \
     eliom.cmxs ocsigenmod.cmxs $(DUCEMODOPT)


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

eliomsessiondata.cmx: eliomsessiondata.ml
	-rm ../extensions/ocsipersist.cmx
	$(CAMLOPT) $(PP) -c $<
	-touch ../extensions/ocsipersist.cmx
# we remove ocsipersist.cmx, 
# otherwise eliomsessiondata.cmx will will compiled against
# that precise version of ocsipersist.cmx

eliommod.cmx: eliommod.ml
	-rm ../extensions/ocsipersist.cmx
	$(CAMLOPT) $(PP) -c $<
	-touch ../extensions/ocsipersist.cmx
# idem

eliom.cma: eliommod.cmo eliomsessions.cmo \
	eliomparameters.cmo eliomservices.cmo \
	eliommkforms.cmo eliommkreg.cmo eliompredefmod.cmo \
	eliomtools.cmo
	$(CAMLC) -a -o eliom.cma \
	eliommod.cmo eliomsessions.cmo eliomparameters.cmo eliomservices.cmo \
	eliommkforms.cmo eliommkreg.cmo eliompredefmod.cmo \
	eliomtools.cmo

eliom.cmxs: eliommod.cmx eliomsessions.cmx \
	eliomparameters.cmx eliomservices.cmx \
	eliommkforms.cmx eliommkreg.cmx eliompredefmod.cmx \
	eliomtools.cmx
	$(CAMLOPT) -shared -linkall -o eliom.cmxs \
	eliommod.cmx eliomsessions.cmx eliomparameters.cmx eliomservices.cmx \
	eliommkforms.cmx eliommkreg.cmx eliompredefmod.cmx \
	eliomtools.cmx

ocsigenmod.cma: ocsigenmod.cmo ocsigen.cmo ocsigenboxes.cmo
	$(CAMLC) -a -o ocsigenmod.cma ocsigenmod.cmo ocsigen.cmo \
	ocsigenboxes.cmo

ocsigenmod.cmxs: ocsigenmod.cmx ocsigen.cmx ocsigenboxes.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenmod.cmxs ocsigenmod.cmx \
	ocsigen.cmx ocsigenboxes.cmx

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
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *~ doc/*  *.annot

depend:
	$(CAMLDEP) $(PP2) $(FILESSQLITE:.ml=.mli) $(FILESSQLITE) $(FILES:.ml=.mli) $(FILES) $(DUCEFILES:.ml=.mli) $(DUCEFILES) > .depend


FORCE:

-include .depend


