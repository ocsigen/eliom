include ../Makefile.config

FILESA= eliommod.ml eliom.ml eliomboxes.ml \
       ocsigenmod.ml ocsigen.ml ocsigenboxes.ml \
       staticmod.ml cgimod.ml \
       extensiontemplate.ml

ifeq "$(OCSIPERSISTSQLITE)" "YES"
FILES= ocsipersist.ml $(FILESA)
OBJSQLITE= ocsipersist-sqlite.cma
PACKSQLITE=,sqlite3
else
FILES= $(FILESA)
endif

ifeq "$(OCSIPERSISTDBM)" "YES"
OBJDBMBYTE= dbm-byte
OBJDBMOPT= dbm-opt
else
endif

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

LIB = -thread -package netstring$(PACKSQLITE) $(LIBDIRS)
CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
PP = 
PP2 = 
# -pp "$(CAMLP4O) ../lib/xhtmlsyntax.cma -- -loc loc"


OBJS = $(FILES:.ml=.cmo) $(DUCEFILES:.ml=.cmo) $(OBJDBM) $(OBJSQLITE)

OBJSOPT = $(FILES:.ml=.cmx) $(DUCEFILES:.ml=.cmx) $(OBJDBM:.cma=.cmxs) $(OBJSQLITE:.cma=.cmxs)


byte: $(OBJS) eliom.cma ocsigenmod.cma staticmod.cmo cgimod.cmo \
      $(DUCEMODBYTE) $(OBJDBMBYTE)

opt: $(OBJSX) eliom.cmxs ocsigenmod.cmxs staticmod.cmxs cgimod.cmxs \
     $(DUCEMODOPT) $(OBJDBMOPT)


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend dbm-byte dbm-opt

dbm-byte:
	make -C ocsipersist-dbm byte

dbm-opt:
	make -C ocsipersist-dbm opt

eliom.cma: eliommod.cmo eliom.cmo eliomboxes.cmo
	$(CAMLC) -a -o eliom.cma eliommod.cmo eliom.cmo eliomboxes.cmo

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

cgimod.cmxs: cgimod.cmx
	$(CAMLOPT) -shared -linkall -o cgimod.cmxs cgimod.cmx

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

ocsipersist-sqlite.cma: ocsipersist.cmo
	$(CAMLC) -a -o ocsipersist-sqlite.cma ocsipersist.cmo

ocsipersist-sqlite.cmxs: ocsipersist.cmx
	$(CAMLOPT) -shared -linkall -o ocsipersist-sqlite.cmxs ocsipersist.cmx


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
	make -C ocsipersist-dbm clean

depend:
	$(CAMLDEP) $(PP2) $(FILES:.ml=.mli) $(FILES) $(DUCEFILES:.ml=.mli) $(DUCEFILES) > .depend
	make -C ocsipersist-dbm depend


FORCE:

include .depend


