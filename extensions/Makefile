
include ../Makefile.config

# /!\ Simple expanded variable, because of camlzip below
FILES:= ocsigen_LocalFiles.ml staticmod.ml cgimod.ml redirectmod.ml	\
	revproxy.ml extensiontemplate.ml accesscontrol.ml staticmod.ml	\
	cgimod.ml userconf.ml outputfilter.ml authbasic.ml		\
	rewritemod.ml extendconfiguration.ml polytables.ml

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

ifeq "$(NEWOCAMLNET)" "YES"
NETSYS=netsys
NETSYSCMA=netsys.cma
NETSYSCMXA=netsys.cmxa
else
NETSYS=
NETSYSCMA=
NETSYSCMXA=
endif

ifeq "$(NATDYNLINK)" "YES"
CMXS1=$(OBJSQLITE:.cma=.cmxs)
CMXS2=$(FILES:.ml=.cmxs) $(DUCEFILES:.ml=.cmxs)
else
CMXS1=$(OBJSQLITE:.cma=.cmxa)
CMXS2=$(FILES:.ml=.cmx) $(DUCEFILES:.ml=.cmx)
endif

# LIB=$(LIBDIRS)

LIB = -thread -package lwt,netstring$(PACKCAMLZIP)$(PACKSQLITE) $(LIBDIRS)
CAMLC = $(OCAMLFIND) $(CAMLCNAME2) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep $(LIBDIRS)
PP = 
PP2 = 
# -pp "$(CAMLP4O) ../lib/xhtmlsyntax.cma -- -loc loc"


OBJS = $(OBJSQLITE) $(FILES:.ml=.cmo) $(DUCEFILES:.ml=.cmo)

OBJSOPT = $(CMXS1) $(OBJSQLITE:.cma=.cmxa) \
          $(CMXS2)


byte: $(OBJDBMBYTE) $(OBJS)

opt: $(OBJDBMOPT) $(OBJSOPT)


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs .cmxa

.PHONY: doc depend dbm-byte dbm-opt

dbm-byte:
	$(MAKE) -C ocsipersist-dbm byte

dbm-opt:
	$(MAKE) -C ocsipersist-dbm opt

ocsipersist-sqlite.cma: ocsipersist.cmo
	$(CAMLC) -a -o ocsipersist-sqlite.cma ocsipersist.cmo

ocsipersist-sqlite.cmxa: ocsipersist.cmx
	$(CAMLOPT) -a -o ocsipersist-sqlite.cmxa ocsipersist.cmx

ocsipersist-sqlite.cmxs: ocsipersist.cmx
	$(CAMLOPT) -shared -linkall -o ocsipersist-sqlite.cmxs ocsipersist.cmx


.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

.cmx.cmxs:
	$(CAMLOPT) -shared -linkall -o $@ $<

#doc:
#	$(CAMLDOC) -d doc -html ocsigen.mli ocsigen_extensions.mli

$(FILES:.ml=.cmxs): $(FILES:.ml=.cmx)
$(DUCEFILES:.ml=.cmxs): $(DUCEFILES:.ml=.cmx)

.PHONY: clean
clean:
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.cmx *.a *~ doc/*  *.annot
	$(MAKE) -C ocsipersist-dbm clean

depend:
	$(CAMLDEP) $(PP2) $(FILESSQLITE:.ml=.mli) $(FILESSQLITE) $(FILES:.ml=.mli) $(FILES) $(DUCEFILES:.ml=.mli) $(DUCEFILES) > .depend
	$(MAKE) -C ocsipersist-dbm depend


FORCE:

-include .depend


