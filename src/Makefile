include ../Makefile.config

export OCAMLPATH := ../deriving/tmp:${OCAMLPATH}

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
        eliom_client_types_cli.ml eliom_client_types.ml eliommod_pagegen.ml \
	eliommod.ml \
        eliom_parameters_cli.ml eliom_parameters.ml \
	eliom_process.ml eliommod_cli.ml \
	eliom_services_cli.ml eliom_services.ml \
        eliom_config.ml eliommod_mkforms.ml \
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

#OCAMLFIND=$(OCAMLDUCEFIND)
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

LIBDUCE = -thread -package lwt.unix,netstring,cryptokit,lwt.react,calendar $(LIBDIRS)
LIB = $(LIBDUCE) -package deriving,deriving.syntax -syntax camlp4o
CAMLC = $(OCAMLFIND) $(CAMLCNAME2) $(DBG) $(LIB)
CAMLDUCEC = $(OCAMLDUCEFIND) $(CAMLCNAME2) $(DBG) $(LIBDUCE)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDUCEOPT = $(OCAMLDUCEFIND) $(CAMLOPTNAME) $(DBG) $(LIBDUCE)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep $(LIBDIRS) -package deriving.syntax -syntax camlp4o
PP = 
PP2 = 
# -pp "$(CAMLP4O) ../lib/xhtmlsyntax.cma -- -loc loc"


OBJS = $(OBJSQLITE) $(FILES:.ml=.cmo) $(DUCEFILES:.ml=.cmo) $(OBJDBM)

OBJSOPT = $(CMXS1) $(FILES:.ml=.cmx) $(DUCEFILES:.ml=.cmx) $(CMXS2)


include ../Makefile.config

all: metas conf
	${MAKE} -C server all
	${MAKE} -C client all
	${MAKE} -C syntax all

byte: metas conf
	${MAKE} -C server byte
	${MAKE} -C client all
	${MAKE} -C syntax all

opt: metas conf
	${MAKE} -C server opt

clean:
	${MAKE} -C server clean
	${MAKE} -C client clean
	${MAKE} -C syntax clean
	-rm -f files/META  files/META.${PROJECTNAME}

distclean:
	${MAKE} -C server distclean
	${MAKE} -C client distclean
	${MAKE} -C syntax distclean
	-rm -f *~ \#* .\#*

depend: server.depend client.depend syntax.depend
server.depend:
	${MAKE} -C server depend
client.depend:
	${MAKE} -C client depend
syntax.depend:
	${MAKE} -C syntax depend

VERSION := $(shell head -n 1 ../VERSION)

install: install.META
	${MAKE} -C server install
	${MAKE} -C client install
	${MAKE} -C syntax install

install.META:
	${OCAMLFIND} install ${PROJECTNAME} \
	   -destdir $(TEMPROOT)${LIBDIR} \
	   -patch-version ${VERSION} \
	   files/META

install.byte: install.META
	${MAKE} -C server install.byte
	${MAKE} -C client install
	${MAKE} -C syntax install

install.opt: install.META
	${MAKE} -C server install.opt
	${MAKE} -C client install
	${MAKE} -C syntax install

uninstall:
	${MAKE} -C server uninstall
	${MAKE} -C client uninstall
	${MAKE} -C syntax uninstall
	${OCAMLFIND} remove ${PROJECTNAME} \
	   -destdir $(TEMPROOT)${LIBDIR}

reinstall: uninstall install
reinstall.byte: uninstall install.byte
reinstall.opt: uninstall install.opt

metas: files/META files/META.${PROJECTNAME}

files/META: files/META.in ../Makefile.config  ../Makefile.options Makefile
	cat $< | sed s/%%NAME%%/$(PROJECTNAME)/g > $@

files/META.${PROJECTNAME}: files/META.in ../Makefile.config  ../Makefile.options Makefile
	echo "directory = \"..\"" > $@
	cat $< | sed s/%%NAME%%/$(PROJECTNAME)/g >> $@

conf: ../local/etc/$(PROJECTNAME).conf

CONF_SED := -e s%_LOGDIR_%$(SRC)/local/var/log%g
CONF_SED += -e s%_DATADIR_%$(SRC)/local/var/lib%g
CONF_SED += -e s%\<user\>_OCSIGENUSER_\</user\>%%g
CONF_SED += -e s%\<group\>_OCSIGENGROUP_\</group\>%%g
CONF_SED += -e s%_COMMANDPIPE_%$(SRC)/local/var/run/${PROJECTNAME}_command%g
CONF_SED += -e s%_MIMEFILE_%$(SRC)/src/files/mime.types%g
CONF_SED += -e s%_METADIR_%${LIBDIR}\"/\>\<findlib\ path=\"$(SRC)/src/files/\"/\>\<findlib\ path=\"$(shell ${OCAMLFIND} query stdlib)%g
CONF_SED += -e s%_EXTLIBDIR_%$(shell ${OCAMLFIND} query ocsigenserver.ext)%g
CONF_SED += -e s%_PROJECTNAME_%$(PROJECTNAME)%g
CONF_SED += -e s%_CONFIGDIR_%$(SRC)/local/etc%g
CONF_SED += -e s%_STATICPAGESDIR_%$(SRC)/local/var/www%g
CONF_SED += -e s%store\ dir=\"$(SRC)/var/lib\"%store\ dir=\"$(SRC)/local/var/lib/ocsipersist\"%g

ifeq "${OCAMLDUCE}" "YES"
CONF_SED += -e s%\<\!--\ DUCE%%
CONF_SED += -e s%DUCE\ --\>%%
endif

../local/etc/$(PROJECTNAME).conf: files/${PROJECTNAME}.conf.in ../Makefile.config Makefile
	cat $< | sed ${CONF_SED}  > $@
