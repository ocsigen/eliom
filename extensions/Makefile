
include ../Makefile.config

# /!\ Simple expanded variable, because of camlzip below
FILES:= staticmod.ml cgimod.ml redirectmod.ml extensiontemplate.ml

ifeq "$(OCSIPERSISTSQLITE)" "YES"
FILESSQLITE= ocsipersist.ml
OBJSQLITE= ocsipersist-sqlite.cma
PACKSQLITE=,sqlite3
else

endif


ifeq "$(CAMLZIP)" "YES"
FILES:= $(FILES) deflatemod.ml
PACKCAMLZIP=,$(CAMLZIPNAME)
else
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
CAMLCNAME2= $(CAMLCPNAME)
else
CAMLCNAME2= $(CAMLCNAME)
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

LIB = -thread -package netstring$(PACKCAMLZIP)$(PACKSQLITE) $(LIBDIRS)
CAMLC = $(OCAMLFIND) $(CAMLCNAME2) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep $(LIBDIRS)
PP = 
PP2 = 
# -pp "$(CAMLP4O) ../lib/xhtmlsyntax.cma -- -loc loc"


OBJS = $(OBJSQLITE) $(FILES:.ml=.cmo) $(DUCEFILES:.ml=.cmo) $(OBJDBM)

OBJSOPT = $(OBJSQLITE:.cma=.cmxs) $(FILES:.ml=.cmx) $(DUCEFILES:.ml=.cmx) $(OBJDBM:.cma=.cmxs)


byte: $(FILESSQLITE:.ml=.cmo) $(OBJDBMBYTE) $(OBJS) \
      staticmod.cmo cgimod.cmo \
      $(DUCEMODBYTE)

opt: $(FILESSQLITE:.ml=.cmx) $(OBJDBMOPT) $(OBJSOPT) \
     staticmod.cmxs cgimod.cmxs \
     $(DUCEMODOPT)


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend dbm-byte dbm-opt

dbm-byte:
	$(MAKE) -C ocsipersist-dbm byte

dbm-opt:
	$(MAKE) -C ocsipersist-dbm opt

staticmod.cmxs: staticmod.cmx
	$(CAMLOPT) -shared -linkall -o staticmod.cmxs staticmod.cmx

cgimod.cmxs: cgimod.cmx
	$(CAMLOPT) -shared -linkall -o cgimod.cmxs cgimod.cmx

deflatemod.cmxs: deflatemod.cmx
	$(CAMLOPT) -shared -linkall -o deflatemod.cmxs deflatemod.cmx

ocsipersist.cmxs: ocsipersist.cmx
	$(CAMLOPT) -shared -linkall -o ocsipersist.cmxs ocsipersist.cmx

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
#	$(CAMLDOC) -d doc -html ocsigen.mli extensions.mli

.PHONY: clean
clean:
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *~ doc/*  *.annot
	$(MAKE) -C ocsipersist-dbm clean

depend:
	$(CAMLDEP) $(PP2) $(FILESSQLITE:.ml=.mli) $(FILESSQLITE) $(FILES:.ml=.mli) $(FILES) $(DUCEFILES:.ml=.mli) $(DUCEFILES) > .depend
	$(MAKE) -C ocsipersist-dbm depend


FORCE:

-include .depend


