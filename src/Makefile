include ../Makefile.config

all: metas conf
	${MAKE} -C server all
	${MAKE} -C client all
	${MAKE} -C syntax all
	${MAKE} -C tools all

byte: metas conf
	${MAKE} -C server byte
	${MAKE} -C client all
	${MAKE} -C syntax all
	${MAKE} -C tools all

opt: metas conf
	${MAKE} -C server opt
	${MAKE} -C client all
	${MAKE} -C syntax all
	${MAKE} -C tools all

clean:
	${MAKE} -C server clean
	${MAKE} -C client clean
	${MAKE} -C syntax clean
	${MAKE} -C tools clean
	-rm -f files/META  files/META.${PROJECTNAME}

distclean:
	${MAKE} -C server distclean
	${MAKE} -C client distclean
	${MAKE} -C syntax distclean
	${MAKE} -C tools distclean
	-rm -f *~ \#* .\#*

depend: server.depend client.depend syntax.depend tools.depend
server.depend:
	${MAKE} -C server depend
client.depend:
	${MAKE} -C client depend
syntax.depend:
	${MAKE} -C syntax depend
tools.depend:
	${MAKE} -C tools depend

VERSION := $(shell head -n 1 ../VERSION)

install: install.META
	${MAKE} -C server install
	${MAKE} -C client install
	${MAKE} -C syntax install
	${MAKE} -C tools install

install.META:
	${OCAMLFIND} install ${PROJECTNAME} \
	   -destdir $(TEMPROOT)${LIBDIR} \
	   -patch-version ${VERSION} \
	   files/META

install.byte: install.META
	${MAKE} -C server install.byte
	${MAKE} -C client install
	${MAKE} -C syntax install
	${MAKE} -C tools install

install.opt: install.META
	${MAKE} -C server install.opt
	${MAKE} -C client install
	${MAKE} -C syntax install
	${MAKE} -C tools install

uninstall:
	${MAKE} -C server uninstall
	${MAKE} -C client uninstall
	${MAKE} -C syntax uninstall
	${MAKE} -C tools uninstall
	${OCAMLFIND} remove ${PROJECTNAME} \
	   -destdir $(TEMPROOT)${LIBDIR}

reinstall: uninstall install
reinstall.byte: uninstall install.byte
reinstall.opt: uninstall install.opt

metas: files/META files/META.${PROJECTNAME}

SED_META := -e s/%%NAME%%/$(PROJECTNAME)/g \
	    -e s/%%DEPS%%/$(shell ${OCAMLFIND} query -p-format -separator ',' ${SERVER_PACKAGE})/g \
	    -e s/%%CDEPS%%/$(shell ${OCAMLFIND} query -p-format -separator ',' ${CLIENT_PACKAGE})/g \
	    -e s/%%SDEPS%%/$(shell ${OCAMLFIND} query -p-format -separator ',' ${SERVER_SYNTAX})/g \
	    -e s/%%CSDEPS%%/$(shell ${OCAMLFIND} query -p-format -separator ',' ${CLIENT_SYNTAX})/g \

files/META: files/META.in ../Makefile.config  ../Makefile.options Makefile
	sed ${SED_META} $< > $@

files/META.${PROJECTNAME}: files/META.in ../Makefile.config  ../Makefile.options Makefile
	echo "directory = \"..\"" > $@
	sed ${SED_META} $< >> $@

conf: ../local/etc/$(PROJECTNAME).conf

CONF_SED := -e s%_LOGDIR_%$(SRC)/local/var/log%g
CONF_SED += -e s%_DATADIR_%$(SRC)/local/var/lib%g
CONF_SED += -e s%\<user\>_OCSIGENUSER_\</user\>%%g
CONF_SED += -e s%\<group\>_OCSIGENGROUP_\</group\>%%g
CONF_SED += -e s%_COMMANDPIPE_%$(SRC)/local/var/run/${PROJECTNAME}_command%g
CONF_SED += -e s%_MIMEFILE_%$(SRC)/src/files/mime.types%g
CONF_SED += -e s%_METADIR_%${LIBDIR}\"/\>\<findlib\ path=\"$(SRC)/src/files/\"/\>\<findlib\ path=\"$(shell ${OCAMLFIND} query stdlib)%g
CONF_SED += -e s%_OCSDIR_%$(shell ${OCAMLFIND} query ocsigenserver)%g
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
