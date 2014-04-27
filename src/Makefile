include ../Makefile.config

all: metas conf
	${MAKE} -C syntax all
	${MAKE} -C server all
	${MAKE} -C client all
	${MAKE} -C tools all
	${MAKE} -C clientserver all
	${MAKE} -C server2 all
	${MAKE} -C client2 all
	${MAKE} -C ocamlbuild $@

byte: metas conf
	${MAKE} -C syntax all
	${MAKE} -C server byte
	${MAKE} -C client all
	${MAKE} -C tools all
	${MAKE} -C clientserver all
	${MAKE} -C server2 byte
	${MAKE} -C client2 all
	${MAKE} -C ocamlbuild $@

opt: metas conf
	${MAKE} -C syntax all
	${MAKE} -C server opt
	${MAKE} -C client all
	${MAKE} -C tools all
	${MAKE} -C clientserver all
	${MAKE} -C server2 opt
	${MAKE} -C client2 all
	${MAKE} -C ocamlbuild $@

odoc:
	${MAKE} -C server2 odoc DEPEND=no
	${MAKE} -C client2 odoc DEPEND=no

clean:
	${MAKE} -C server clean DEPEND=no
	${MAKE} -C client clean DEPEND=no
	${MAKE} -C clientserver clean DEPEND=no
	${MAKE} -C server2 clean DEPEND=no
	${MAKE} -C client2 clean DEPEND=no
	${MAKE} -C tools clean
	${MAKE} -C syntax clean
	${MAKE} -C ocamlbuild $@
	-rm -f files/META  files/META.${PROJECTNAME}
	-rm -f ../local/etc/$(PROJECTNAME).conf

distclean:
	${MAKE} -C server distclean DEPEND=no
	${MAKE} -C client distclean DEPEND=no
	${MAKE} -C clientserver distclean DEPEND=no
	${MAKE} -C server2 distclean DEPEND=no
	${MAKE} -C client2 distclean DEPEND=no
	${MAKE} -C tools distclean
	${MAKE} -C syntax distclean
	${MAKE} -C ocamlbuild $@
	-rm -f *~ \#* .\#*

depend:
	${MAKE} syntax.depend
	${MAKE} tools.depend
	${MAKE} server.depend client.depend
syntax.depend:
	find syntax -name .\*.deps -delete
	${MAKE} -C syntax depend
	${MAKE} -C syntax pa_include.cmo
server.depend: syntax.depend
	find server -name .\*.deps -delete
	rm -f server/.depend
	find clientserver -name .\*.deps -delete
	rm -f clientserver/.depend
	find server2 -name .\*.deps -delete
	rm -f server2/.depend
	 ${MAKE} -C server .depend
	 ${MAKE} -C clientserver .depend
	 ${MAKE} -C server2 .depend
client.depend: syntax.depend
	find client -name .\*.deps -delete
	rm -f client/.depend
	find client2 -name .\*.deps -delete
	rm -f client2/.depend
	${MAKE} -C client .depend
	${MAKE} -C client2 .depend
tools.depend: syntax.depend
	find tools -name .\*.deps -delete
	${MAKE} -C tools depend

VERSION := $(shell head -n 1 ../VERSION)

install: install.META install.man
	${MAKE} -C clientserver install
	${MAKE} -C server install
	${MAKE} -C client install
	${MAKE} -C server2 install
	${MAKE} -C client2 install
	${MAKE} -C syntax install
	${MAKE} -C tools install
	${MAKE} -C ocamlbuild $@

install.man: install.META
	$(INSTALL) -m 755 -d $(TEMPROOT)$(MANDIR)/man1
	$(INSTALL) -m 755 files/eliomc.1 $(TEMPROOT)$(MANDIR)/man1
	$(INSTALL) -m 755 files/eliom-distillery.1 $(TEMPROOT)$(MANDIR)/man1
	ln -sf eliomc.1 $(TEMPROOT)$(MANDIR)/man1/js_of_eliom.1
	ln -sf eliomc.1 $(TEMPROOT)$(MANDIR)/man1/eliomopt.1
	ln -sf eliomc.1 $(TEMPROOT)$(MANDIR)/man1/eliomcp.1
	ln -sf eliomc.1 $(TEMPROOT)$(MANDIR)/man1/eliomdep.1

install.META:
	${OCAMLFIND} install ${PROJECTNAME} \
	   -destdir $(TEMPROOT)${LIBDIR} \
	   -patch-version ${VERSION} \
	   files/META

install.byte: install.META install.man
	${MAKE} -C clientserver install.byte
	${MAKE} -C server install.byte
	${MAKE} -C client2 install
	${MAKE} -C server2 install.byte
	${MAKE} -C client install
	${MAKE} -C syntax install
	${MAKE} -C tools install
	${MAKE} -C ocamlbuild $@

install.opt: install.META install.man
	${MAKE} -C clientserver install
	${MAKE} -C server install.opt
	${MAKE} -C client install
	${MAKE} -C server2 install.opt
	${MAKE} -C client2 install
	${MAKE} -C syntax install
	${MAKE} -C tools install
	${MAKE} -C ocamlbuild $@

uninstall:
	${MAKE} -C clientserver uninstall
	${MAKE} -C server uninstall
	${MAKE} -C client uninstall
	${MAKE} -C server2 uninstall
	${MAKE} -C client2 uninstall
	${MAKE} -C syntax uninstall
	${MAKE} -C tools uninstall
	${MAKE} -C ocamlbuild $@
	${OCAMLFIND} remove ${PROJECTNAME} \
	   -destdir $(TEMPROOT)${LIBDIR}
	rm -f $(TEMPROOT)$(MANDIR)/man1/eliomc.1
	rm -f $(TEMPROOT)$(MANDIR)/man1/js_of_eliom.1
	rm -f $(TEMPROOT)$(MANDIR)/man1/eliomopt.1
	rm -f $(TEMPROOT)$(MANDIR)/man1/eliomcp.1
	rm -f $(TEMPROOT)$(MANDIR)/man1/eliomdep.1
	rm -f $(TEMPROOT)$(MANDIR)/man1/eliom-distillery.1

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
	echo "directory = \"../../local/lib\"" > $@
	sed ${SED_META} $< >> $@

conf: ../local/etc/$(PROJECTNAME).conf

CONF_SED := -e s%_LOGDIR_%$(SRC)/local/var/log%g
CONF_SED += -e s%_DATADIR_%$(SRC)/local/var/lib%g
CONF_SED += -e s%\<user\>_OCSIGENUSER_\</user\>%%g
CONF_SED += -e s%\<group\>_OCSIGENGROUP_\</group\>%\<debugmode/\>%g
CONF_SED += -e s%_COMMANDPIPE_%$(SRC)/local/var/run/${PROJECTNAME}_command%g
CONF_SED += -e s%_MIMEFILE_%$(SRC)/src/files/mime.types%g
CONF_SED += -e "s%_METADIR_%$(addsuffix \"/\>\\n    \<findlib path=\",$(shell ocamlfind printconf path))$(SRC)/src/files/%g" -e s%path=\"\ %path=\"%g
CONF_SED += -e s%_OCSDIR_%$(shell ${OCAMLFIND} query ocsigenserver)%g
CONF_SED += -e s%_OCSEXTDIR_%$(shell ${OCAMLFIND} query ocsigenserver.ext)%g
CONF_SED += -e s%_PROJECTNAME_%$(PROJECTNAME)%g
CONF_SED += -e s%_CONFIGDIR_%$(SRC)/local/etc%g
CONF_SED += -e s%_STATICPAGESDIR_%$(SRC)/local/var/www%g
CONF_SED += -e s%_LOCALDIR_%$(SRC)/local%g
CONF_SED += -e s%store\ dir=\"$(SRC)/var/lib\"%store\ dir=\"$(SRC)/local/var/lib/ocsipersist\"%g

../local/etc/$(PROJECTNAME).conf: files/${PROJECTNAME}.conf.in ../Makefile.config Makefile
	mkdir -p ../local/etc
	cat $< | sed ${CONF_SED}  > $@
