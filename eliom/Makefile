
include ../Makefile.config

# /!\ Simple expanded variable, because of camlzip below
FILES:= error_pages.ml eliom_common.ml eliommod_sessiongroups.ml \
	eliommod_gc.ml eliommod_persess.ml \
	eliommod_sessadmin.ml eliommod_cookies.ml  \
	eliommod_naservices.ml eliommod_sersess.ml eliommod_sessexpl.ml \
	eliommod_datasess.ml eliommod_pagegen.ml \
	eliommod_services.ml eliommod_sessiongroups.ml eliommod_timeouts.ml \
	eliommod.ml eliom_sessions.ml \
       eliom_parameters.ml eliom_services.ml \
       eliom_mkforms.ml eliom_mkreg.ml eliom_predefmod.ml \
       eliom_tools.ml
#       ocsigenmod.ml ocsigen.ml ocsigenboxes.ml


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
DUCEFILES= xhtmltypes_duce.ml eliom_duce.ml eliom_duce_tools.ml
#DUCEFILES= xhtmltypes_duce.ml ocsigenduce.ml eliom_duce.ml
# rss2.ml ocsigenrss.ml

  ifeq "$(BYTECODE)" "YES"
  DUCEMODBYTE= eliom_duce.cma
#  DUCEMODBYTE=ocsigenduce.cma eliom_duce.cma
# ocsigenrss.cma
  else
  DUCEMODBYTE=
  endif

  ifeq "$(NATIVECODE)" "YES"
  DUCEMODOPT= eliom_duce.cmxs
#  DUCEMODOPT=ocsigenduce.cmxs eliom_duce.cmxs
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
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) -dlcode $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep $(LIBDIRS)
PP = 
PP2 = 
# -pp "$(CAMLP4O) ../lib/xhtmlsyntax.cma -- -loc loc"


OBJS = $(OBJSQLITE) $(FILES:.ml=.cmo) $(DUCEFILES:.ml=.cmo) $(OBJDBM)

OBJSOPT = $(OBJSQLITE:.cma=.cmxs) $(FILES:.ml=.cmx) $(DUCEFILES:.ml=.cmx) $(OBJDBM:.cma=.cmxs)


byte: $(FILESSQLITE:.ml=.cmo) $(OBJS) \
      eliom.cma $(DUCEMODBYTE)

opt: $(FILESSQLITE:.ml=.cmx) $(OBJSOPT) \
     eliom.cmxs $(DUCEMODOPT)


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

eliom_sessions.cmx: eliom_sessions.ml
#	-rm ../extensions/ocsipersist.cmx
	$(CAMLOPT) $(PP) -c $<
#	-touch ../extensions/ocsipersist.cmx
# we remove ocsipersist.cmx, 
# otherwise eliomsessiondata.cmx will will compiled against
# that precise version of ocsipersist.cmx

eliommod.cmx: eliommod.ml
#	-rm ../extensions/ocsipersist.cmx
	$(CAMLOPT) $(PP) -c $<
#	-touch ../extensions/ocsipersist.cmx
# idem

eliom.cma: error_pages.cmo \
	eliommod_sessiongroups.cmo eliom_common.cmo eliommod_cookies.cmo \
	eliommod_sersess.cmo eliommod_datasess.cmo eliommod_persess.cmo \
	eliommod_gc.cmo \
	eliommod_services.cmo eliommod_naservices.cmo \
	eliommod_sessexpl.cmo eliommod_sessadmin.cmo \
	eliommod_timeouts.cmo \
	eliommod_pagegen.cmo \
	eliommod.cmo eliom_sessions.cmo \
	eliom_parameters.cmo eliom_services.cmo \
	eliom_mkforms.cmo eliom_mkreg.cmo eliom_predefmod.cmo \
	eliom_tools.cmo
	$(CAMLC) -a -o eliom.cma \
	error_pages.cmo \
	eliommod_sessiongroups.cmo eliom_common.cmo eliommod_cookies.cmo \
	eliommod_sersess.cmo eliommod_datasess.cmo eliommod_persess.cmo \
	eliommod_gc.cmo \
	eliommod_services.cmo eliommod_naservices.cmo \
	eliommod_sessexpl.cmo eliommod_sessadmin.cmo \
	eliommod_timeouts.cmo \
	eliommod_pagegen.cmo \
	eliommod.cmo eliom_sessions.cmo \
	eliom_parameters.cmo eliom_services.cmo \
	eliom_mkforms.cmo eliom_mkreg.cmo eliom_predefmod.cmo \
	eliom_tools.cmo

eliom.cmxs: error_pages.cmx \
	eliommod_sessiongroups.cmx eliom_common.cmx \
	eliommod_gc.cmx eliommod_persess.cmx \
	eliommod_sessadmin.cmx eliommod_cookies.cmx  \
	eliommod_naservices.cmx eliommod_sersess.cmx eliommod_sessexpl.cmx \
	eliommod_datasess.cmx eliommod_pagegen.cmx \
	eliommod_services.cmx eliommod_timeouts.cmx \
	eliommod.cmx eliom_sessions.cmx \
	eliom_parameters.cmx eliom_services.cmx \
	eliom_mkforms.cmx eliom_mkreg.cmx eliom_predefmod.cmx \
	eliom_tools.cmx
	$(CAMLOPT) -shared -linkall -o eliom.cmxs \
	error_pages.cmx \
	eliommod_sessiongroups.cmx eliom_common.cmx \
	eliommod_gc.cmx eliommod_persess.cmx \
	eliommod_sessadmin.cmx eliommod_cookies.cmx  \
	eliommod_naservices.cmx eliommod_sersess.cmx eliommod_sessexpl.cmx \
	eliommod_datasess.cmx eliommod_pagegen.cmx \
	eliommod_services.cmx eliommod_timeouts.cmx \
	eliommod.cmx eliom_sessions.cmx \
	eliom_parameters.cmx eliom_services.cmx \
	eliom_mkforms.cmx eliom_mkreg.cmx eliom_predefmod.cmx \
	eliom_tools.cmx

ocsigenmod.cma: ocsigenmod.cmo ocsigen.cmo ocsigenboxes.cmo
	$(CAMLC) -a -o ocsigenmod.cma ocsigenmod.cmo ocsigen.cmo \
	ocsigenboxes.cmo

ocsigenmod.cmxs: ocsigenmod.cmx ocsigen.cmx ocsigenboxes.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenmod.cmxs ocsigenmod.cmx \
	ocsigen.cmx ocsigenboxes.cmx

ocsigenduce.cma: xhtmltypes_duce.cmo ocsigenduce.cmo
	$(CAMLC) -a -o ocsigenduce.cma xhtmltypes_duce.cmo ocsigenduce.cmo 

ocsigenduce.cmxs: xhtmltypes_duce.cmx ocsigenduce.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenduce.cmxs xhtmltypes_duce.cmx ocsigenduce.cmx 

eliom_duce.cma: xhtmltypes_duce.cmo eliom_duce.cmo
	$(CAMLC) -a -o eliom_duce.cma xhtmltypes_duce.cmo eliom_duce.cmo eliom_duce_tools.cmo

eliom_duce.cmxs: xhtmltypes_duce.cmx eliom_duce.cmx
	$(CAMLOPT) -shared -linkall -o eliom_duce.cmxs xhtmltypes_duce.cmx eliom_duce.cmx eliom_duce_tools.cmx

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
	sed -i s%../extensions/ocsipersist.cmx%% .depend

FORCE:

-include .depend


