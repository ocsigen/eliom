include ../Makefile.config


# /!\ Simple expanded variable, because of camlzip below
FILES:= error_pages.ml \
	eliom_common_cli.ml eliom_common.ml \
	eliom_request_info.ml \
	eliommod_sessiongroups.ml eliommod_cookies.ml \
	eliommod_sersess.ml eliommod_datasess.ml eliommod_persess.ml \
	eliommod_gc.ml \
	eliommod_sessexpl.ml eliommod_sessadmin.ml \
	eliommod_timeouts.ml eliom_state.ml eliom_references.ml \
	eliom_extensions.ml eliommod_services.ml eliommod_naservices.ml \
        eliom_client_types.ml eliommod_pagegen.ml \
	eliommod.ml \
        eliom_parameters_cli.ml eliom_parameters.ml \
	eliom_process.ml \
	eliom_services_cli.ml eliom_services.ml \
        eliommod_cli.ml eliom_config.ml eliommod_mkforms.ml \
        eliom_uri.ml eliom_mkforms.ml eliom_mkreg.ml \
	eliom_common_comet.ml eliom_comet.ml  \
	eliommod_react.ml \
	eliom_output_cli.ml eliom_output.ml \
        eliom_tools_common.ml eliom_tools.ml eliom_extension_template.ml \
	eliom_react.ml \
	eliom_bus.ml \
	extensions/eliom_s2s.ml extensions/eliom_openid.ml \
	extensions/atom_feed.ml extensions/eliom_atom.ml
#       ocsigenmod.ml ocsigen.ml ocsigenboxes.ml

ifeq "$(NATDYNLINK)" "YES"
CMXS1=$(OBJSQLITE:.cma=.cmxs)
CMXS2=$(OBJDBM:.cma=.cmxs)
CMXS3=eliom.cmxs $(DUCEMODOPT)
else
CMXS1=$(OBJSQLITE:.cma=.cmxa)
CMXS2=$(OBJDBM:.cma=.cmxa)
CMXS3=eliom.cmxa $(DUCEMODOPT)
endif

ifeq "$(OCAMLDUCE)" "YES"
DUCEFILES= xhtmltypes_duce.ml xhtmlpretty_duce.ml eliom_duce.ml eliom_duce_tools.ml
#DUCEFILES= xhtmltypes_duce.ml ocsigenduce.ml eliom_duce.ml
# rss2.ml ocsigenrss.ml

  ifeq "$(BYTECODE)" "YES"
  DUCEMODBYTE= eliom_duce.cma
#  DUCEMODBYTE=ocsigenduce.cma eliom_duce.cma
# ocsigenrss.cma
  else
  DUCEMODBYTE=
  endif

  ifeq "$(NATDYNLINK)" "YES"
     EDUCECMXS=eliom_duce.cmxs
  else
     EDUCECMXS=eliom_duce.cmxa
  endif

  ifeq "$(NATIVECODE)" "YES"
  DUCEMODOPT= eliom_duce.cmxa $(EDUCECMXS)
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

LIB = -thread -package lwt.unix,netstring,cryptokit,lwt.react,calendar $(LIBDIRS)
CAMLC = $(OCAMLFIND) $(CAMLCNAME2) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep $(LIBDIRS)
PP = 
PP2 = 
# -pp "$(CAMLP4O) ../lib/xhtmlsyntax.cma -- -loc loc"


OBJS = $(OBJSQLITE) $(FILES:.ml=.cmo) $(DUCEFILES:.ml=.cmo) $(OBJDBM)

OBJSOPT = $(CMXS1) $(FILES:.ml=.cmx) $(DUCEFILES:.ml=.cmx) $(CMXS2)


byte: $(FILESSQLITE:.ml=.cmo) $(OBJS) eliom.cma $(DUCEMODBYTE) syntax client

opt: $(FILESSQLITE:.ml=.cmx) $(OBJSOPT) eliom.cmxa $(CMXS3) syntax client


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend client syntax

client:
	$(MAKE) -C client all

syntax:
	$(MAKE) -C syntax all

eliom_state.cmx: eliom_state.ml
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

eliom.cma: $(FILES:.ml=.cmo)
	$(CAMLC) -a -o eliom.cma $^
	cd ../extensions ; ln -f -s ../eliom/eliom.cma

eliom.cmxa: $(FILES:.ml=.cmx)
	$(CAMLOPT) -a -o eliom.cmxa -linkall $^
	cd ../extensions ; ln -f -s ../eliom/eliom.cmxa

eliom.cmxs: $(FILES:.ml=.cmx)
	$(CAMLOPT) -shared -linkall -o eliom.cmxs $^
	cd ../extensions ; ln -f -s ../eliom/eliom.cmxs

ocsigenmod.cma: ocsigenmod.cmo ocsigen.cmo ocsigenboxes.cmo
	$(CAMLC) -a -o ocsigenmod.cma ocsigenmod.cmo ocsigen.cmo \
	ocsigenboxes.cmo

ocsigenmod.cmxs: ocsigenmod.cmx ocsigen.cmx ocsigenboxes.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenmod.cmxs ocsigenmod.cmx \
	ocsigen.cmx ocsigenboxes.cmx

ocsigenmod.cmxa: ocsigenmod.cmx ocsigen.cmx ocsigenboxes.cmx
	$(CAMLOPT) -a -linkall -o ocsigenmod.cmxa ocsigenmod.cmx \
	ocsigen.cmx ocsigenboxes.cmx

ocsigenduce.cma: xhtmltypes_duce.cmo xhtmlpretty_duce.cmo ocsigenduce.cmo
	$(CAMLC) -a -o ocsigenduce.cma xhtmltypes_duce.cmo xhtmlpretty_duce.cmo ocsigenduce.cmo 

ocsigenduce.cmxs: xhtmltypes_duce.cmx xhtmlpretty_duce.cmx ocsigenduce.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenduce.cmxs xhtmltypes_duce.cmx xhtmlpretty_duce.cmx ocsigenduce.cmx 

ocsigenduce.cmxa: xhtmltypes_duce.cmx xhtmlpretty_duce.cmx ocsigenduce.cmx
	$(CAMLOPT) -a -linkall -o ocsigenduce.cmxa xhtmltypes_duce.cmx xhtmlpretty_duce.cmx ocsigenduce.cmx 

eliom_duce.cma: xhtmltypes_duce.cmo xhtmlpretty_duce.cmo eliom_duce.cmo
	$(CAMLC) -a -o eliom_duce.cma xhtmltypes_duce.cmo xhtmlpretty_duce.cmo eliom_duce.cmo eliom_duce_tools.cmo
	cd ../extensions ; ln -f -s ../eliom/eliom_duce.cma

eliom_duce.cmxs: xhtmltypes_duce.cmx xhtmlpretty_duce.cmx eliom_duce.cmx
	$(CAMLOPT) -shared -linkall -o eliom_duce.cmxs xhtmltypes_duce.cmx xhtmlpretty_duce.cmx eliom_duce.cmx eliom_duce_tools.cmx
	cd ../extensions ; ln -f -s ../eliom/eliom_duce.cmxs

eliom_duce.cmxa: xhtmltypes_duce.cmx xhtmlpretty_duce.cmx eliom_duce.cmx
	$(CAMLOPT) -a -linkall -o eliom_duce.cmxa xhtmltypes_duce.cmx xhtmlpretty_duce.cmx eliom_duce.cmx eliom_duce_tools.cmx
	cd ../extensions ; ln -f -s ../eliom/eliom_duce.cmxa

ocsigenrss.cma: rss2.cmo ocsigenrss.cmo
	$(CAMLC) -a -o ocsigenrss.cma $^

ocsigenrss.cmxs: rss2.cmx ocsigenrss.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenrss.cmxs $^

ocsigenrss.cmxa: rss2.cmx ocsigenrss.cmx
	$(CAMLOPT) -a -shared -linkall -o ocsigenrss.cmxa $^

# There is a bug with ocamlduce when -g is supplied
bugdbg:
	$(MAKE) DEBUG="NO" eliom_duce.cmo

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
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *~ doc/*  *.annot extensions/*.cm[iox] extensions/*.o extensions/*.cmxs
	$(MAKE) -C client clean
	$(MAKE) -C syntax clean

depend:
	$(MAKE) -C client depend
	$(MAKE) -C syntax depend
	$(CAMLDEP) $(PP2) $(FILESSQLITE:.ml=.mli) $(FILESSQLITE) $(FILES:.ml=.mli) $(FILES) $(DUCEFILES:.ml=.mli) $(DUCEFILES) > .depend
	sed -i.bak s%../extensions/ocsipersist.cmx%% .depend

FORCE:

-include .depend


