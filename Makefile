include Makefile.config

VERSION := $(shell head -n 1 VERSION)

DOCPREF=./

# sed commands used for generation of META files
SED_COMMAND_FOR_META =
SED_COMMAND_FOR_META += -e "s/_VERSION_/$(VERSION)/"
SED_COMMAND_FOR_META += -e "s/_CAMLZIPNAME_/$(CAMLZIPNAME)/"

ifeq "$(OCAMLDUCE)" "YES"
DUCECMA=eliom/eliom_duce.cma
DUCECMO=
#eliom/eliom_duce.cma
# eliom/ocsigenrss.cma
DUCECMI=eliom/eliom_duce.cmi eliom/xhtmltypes_duce.cmi eliom/eliom_duce_tools.cmi
# eliom/rss2.cmi eliom/ocsigenrss.cmi
DUCEEXAMPLES=examples/ocamlduce/exampleduce.cmo
# examples/ocamlduce/examplerss.cmo
DUCEDOC=$(DOCPREF)eliom/eliom_duce.mli $(DOCPREF)eliom/xhtmltypes_duce.ml $(DOCPREF)eliom/eliom_duce_tools.ml
CAMLDOC = $(OCAMLDUCEFIND) ocamldoc $(LIB)
DUCEPACK=,ocamlduce
else
DUCECMA=
DUCECMO=
DUCECMI=
DUCEEXAMPLES=
DUCEDOC=
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
DUCEPACK=
endif

ifeq "$(LOGDIR)" ""
LOGDIR = "error"
endif
ifeq "$(STATICPAGESDIR)" ""
STATICPAGESDIR = "error"
endif
ifeq "$(DATADIR)" ""
DATADIR = "error"
endif

ifeq "$(OCSIPERSISTSQLITE)" "YES"
SQLITECMATOINSTALL= extensions/ocsipersist-sqlite.cma
else
endif

ifeq "$(CAMLZIP)" "YES"
DEFLATEMODCMOTOINSTALL= extensions/deflatemod.cmo
else
endif

ifeq "$(OCSIPERSISTDBM)" "YES"
DBMCMATOINSTALL= extensions/ocsipersist-dbm/ocsipersist-dbm.cma 
else
endif

DOC= $(DOCPREF)eliom/eliom_mkforms.mli $(DOCPREF)eliom/eliom_mkreg.mli	\
	$(DOCPREF)eliom/eliom_predefmod.mli				\
	$(DOCPREF)eliom/eliom_common.mli				\
	$(DOCPREF)eliom/eliom_parameters.mli				\
	$(DOCPREF)eliom/eliom_services.mli				\
	$(DOCPREF)eliom/eliom_sessions.mli				\
	$(DOCPREF)eliom/eliom_extensions.mli				\
	$(DOCPREF)server/ocsigen_extensions.mli				\
	$(DOCPREF)server/ocsigen_parseconfig.mli			\
	$(DOCPREF)server/ocsigen_server.mli				\
	$(DOCPREF)xmlp4/xhtmlpretty_streams.mli				\
	$(DOCPREF)xmlp4/xhtmlcompact_streams.mli			\
	$(DOCPREF)xmlp4/xhtmlpretty.mli					\
	$(DOCPREF)xmlp4/xhtmlcompact.mli				\
	$(DOCPREF)xmlp4/ohl-xhtml/xHTML.mli				\
	$(DOCPREF)baselib/ocsigen_messages.mli				\
	$(DOCPREF)http/ocsigen_headers.mli				\
	$(DOCPREF)server/ocsigen_http_client.mli			\
	$(DOCPREF)http/ocsigen_http_frame.mli				\
	$(DOCPREF)http/ocsigen_http_com.mli				\
	$(DOCPREF)http/ocsigen_charset_mime.mli				\
	$(DOCPREF)http/ocsigen_senders.mli				\
	$(DOCPREF)baselib/ocsigen_stream.mli				\
	$(DOCPREF)eliom/eliom_tools_common.mli			 	\
	$(DOCPREF)eliom/eliom_tools.mli					\
	$(DOCPREF)eliom/eliom_obrowser.mli					\
	$(DOCPREF)baselib/polytables.mli				\
	$(DOCPREF)baselib/ocsigen_cache.mli				\
	$(DOCPREF)extensions/ocsipersist.mli				\
	$(DOCPREF)extensions/authbasic.mli				\
	$(DOCPREF)extensions/ocsigen_LocalFiles.mli			\
	$(DOCPREF)baselib/ocsigen_getcommandline.mli                    \
	$(DOCPREF)xmlp4/newocaml/pp/simplexmlparser.mli			\
	$(DOCPREF)xmlp4/newocaml/pp/xhtmltypes.ml			\
	$(DUCEDOC)

METAS = files/META files/META.ocsigen files/META.eliom_examples files/META.eliom_examples.global


INSTALL = install
TARGETSBYTE = baselib.byte xmlp4.byte http.byte server.byte extensions.byte eliom.byte examples.byte

# plugins are cma (and cmxs) that can be loaded dynamically by the server
PLUGINSCMATOINSTALL = $(SQLITECMATOINSTALL) $(DBMCMATOINSTALL) \
	eliom/eliom.cma $(DEFLATEMODCMATOINSTALL) $(DUCECMA) \
	baselib/parsecommandline.cma baselib/donotparsecommandline.cma
PLUGINSCMOTOINSTALL = \
	$(SQLITECMOTOINSTALL) $(DBMCMOTOINSTALL) $(DEFLATEMODCMOTOINSTALL) \
	extensions/staticmod.cmo extensions/cgimod.cmo \
        extensions/revproxy.cmo extensions/userconf.cmo \
        extensions/outputfilter.cmo extensions/authbasic.cmo \
	extensions/redirectmod.cmo extensions/rewritemod.cmo \
	extensions/accesscontrol.cmo extensions/extendconfiguration.cmo \
	baselib/polytables.cmo $(DUCECMO)
PLUGINSCMITOINSTALL = extensions/ocsipersist.cmi \
       eliom/eliom_mkforms.cmi eliom/eliom_mkreg.cmi \
       eliom/eliom_tools_common.cmi eliom/eliom_tools.cmi \
       eliom/eliom_obrowser.cmi \
       $(DUCECMI) \
       eliom/eliom_sessions.cmi eliom/eliom_parameters.cmi \
       eliom/eliom_services.cmi eliom/eliom_predefmod.cmi \
       eliom/eliommod.cmi eliom/eliom_common.cmi eliom/eliom_extensions.cmi \
       extensions/accesscontrol.cmi extensions/extendconfiguration.cmi \
       baselib/polytables.cmi

# Put here only those which do not have cmxs (Vincent: Why?)
CMATOINSTALL = xmlp4/xhtmlsyntax.cma xmlp4/xhtmlpretty.cma	\
	xmlp4/ohl-xhtml/xhtml.cma server/ocsigen.cma
CMOTOINSTALL = server/server_main.cmo
CMITOINSTALL = baselib/ocsigen_getcommandline.cmi			\
	server/ocsigen_extensions.cmi server/ocsigen_parseconfig.cmi	\
	server/ocsigen_server.cmi server/ocsigen_http_client.cmi	\
	xmlp4/xhtmlpretty.cmi xmlp4/xhtmlpretty_streams.cmi		\
	xmlp4/xhtmlcompact.cmi xmlp4/ohl-xhtml/xHTML.cmi		\
	xmlp4/ohl-xhtml/xML.cmi xmlp4/xhtmltypes.cmi			\
	xmlp4/simplexmlparser.cmi http/ocsigen_charset_mime.cmi		\
	http/ocsigen_senders.cmi http/framepp.cmi			\
	http/ocsigen_http_com.cmi http/http_headers.cmi			\
	baselib/ocsigen_cache.cmi                                       \
	baselib/ocsigen_lib.cmi baselib/ocsigen_config.cmi		\
	http/ocsigen_http_frame.cmi http/ocsigen_headers.cmi		\
	baselib/ocsigen_stream.cmi baselib/ocsigen_messages.cmi		\
	extensions/ocsigen_LocalFiles.cmi files/META
EXAMPLESCMO = examples/tutoeliom.cmo examples/monitoring.cmo	\
	examples/miniwiki/miniwiki.cmo $(DUCEEXAMPLES)

EXAMPLESCMI = examples/tutoeliom.cmi

ifeq "$(BYTECODE)" "YES"
TOINSTALLBYTE=$(CMATOINSTALL) $(CMOTOINSTALL)\
	      $(PLUGINSCMATOINSTALL) $(PLUGINSCMOTOINSTALL)
PLUGINSTOINSTALLBYTE=$(PLUGINSCMATOINSTALL) $(PLUGINSCMOTOINSTALL)
EXAMPLESBYTE=$(EXAMPLESCMO)
BYTE=byte
else
TOINSTALLBYTE=
PLUGINSTOINSTALLBYTE=
EXAMPLESBYTE=
BYTE=
endif

ifeq "$(NATDYNLINK)" "YES"
CMXS=$(PLUGINSCMOTOINSTALL:.cmo=.cmxs) $(PLUGINSCMATOINSTALL:.cma=.cmxs)
EXAMPLECMXS=$(EXAMPLESCMO:.cmo=.cmxs)
else
CMXS=
EXAMPLECMXS=
endif

ifeq "$(NATIVECODE)" "YES"
PLUGINSTOINSTALLX=$(CMXS)

TOINSTALLXTEMP=$(CMAOTOINSTALL:.cmo=.cmx)
TOINSTALLX=$(CMATOINSTALL:.cma=.cmxa) \
           $(CMATOINSTALL:.cma=.a) \
	   $(CMOTOINSTALL:.cmo=.cmx) \
	   $(CMOTOINSTALL:.cmo=.o) \
	   $(PLUGINSCMOTOINSTALL:.cmo=.cmx) \
	   $(PLUGINSCMOTOINSTALL:.cmo=.o) \
	   $(PLUGINSCMATOINSTALL:.cma=.cmxa) \
	   $(PLUGINSCMATOINSTALL:.cma=.a)
EXAMPLESOPT=$(EXAMPLECMXS)
OPT=opt
DEPOPT=xmlp4pre.opt
else
TOINSTALLX=
PLUGINSTOINSTALLX=
EXAMPLESOPT=
OPT=
endif

STATICSTUBS = server/lib$(OCSIGENNAME).a

PLUGINSTOINSTALL=$(PLUGINSTOINSTALLBYTE) $(PLUGINSTOINSTALLX)
TOINSTALL=$(TOINSTALLBYTE) $(TOINSTALLX) $(CMITOINSTALL) $(PLUGINSCMITOINSTALL) $(PLUGINSTOINSTALL) $(STATICSTUBS) eliom/pa_eliom_obrowser.cmo eliom/obrowser/_build/eliom_obrowser_client.cma eliom/obrowser/_build/eliom_obrowser_client.cmi eliom/obrowser/_build/lwt_obrowser.cmi eliom/obrowser/eliom_obrowser.js
EXAMPLES=$(EXAMPLESBYTE) $(EXAMPLESOPT) $(EXAMPLESCMI)

REPS=$(TARGETSBYTE:.byte=)

all: $(BYTE) $(OPT) $(OCSIGENNAME).conf.local $(METAS)

byte: xmlp4pre.byte $(TARGETSBYTE)

opt: xmlp4pre.opt $(TARGETSBYTE:.byte=.opt)

.PHONY: $(REPS) clean distclean


baselib: baselib.byte

baselib.byte:
	$(MAKE) -C baselib byte

baselib.opt:
	$(MAKE) -C baselib opt

xmlp4: xmlp4.byte

xmlp4.byte:
#	touch xmlp4/.depend
#	$(MAKE) -C xmlp4 depend
	$(MAKE) -C xmlp4 byte

xmlp4pre.byte:
#	$(MAKE) -C xmlp4 depend
	$(MAKE) -C xmlp4 xmlp4pre.byte

xmlp4pre.opt:
	$(MAKE) -C xmlp4 xmlp4pre.opt

xmlp4.opt:
#	touch xmlp4/.depend
#	$(MAKE) -C xmlp4 depend
	$(MAKE) -C xmlp4 opt

http: http.byte

http.byte:
	$(MAKE) -C http byte

http.opt:
	$(MAKE) -C http opt

extensions: extensions.byte

extensions.byte:
	$(MAKE) -C extensions byte

extensions.opt:
	$(MAKE) -C extensions opt

eliom: eliom.byte

eliom.byte:
	$(MAKE) -C eliom byte

eliom.opt:
	$(MAKE) -C eliom opt

examples: examples.byte

examples.byte:
	$(MAKE) -C examples byte

examples.opt:
	$(MAKE) -C examples opt

server: server.byte

server.byte:
	$(MAKE) -C server byte

server.opt:
	$(MAKE) -C server opt

doc:
	$(CAMLDOC) -package lwt.ssl,netstring$(DUCEPACK) $(LIBDIRS3) -I `$(CAMLP4) -where` -I +threads -intro files/indexdoc -d doc -html $(DOC)

doc/index.html: doc

files/META: files/META.in VERSION
	sed $(SED_COMMAND_FOR_META) < $< > $@

files/META.ocsigen: files/META.in VERSION
	-ln -sf ../eliom/eliom.cma extensions
	-ln -sf ../eliom/eliom_duce.cma extensions
	-ln -sf ../eliom/obrowser/_builde/eliom_obrowser_client.cma extensions
	-ln -sf ../eliom/obrowser/_build/lwt_obrowser.cmo extensions
	-ln -sf ../xmlp4/ohl-xhtml/xhtml.cma extensions
	-ln -sf ../xmlp4/xhtmlpretty.cma extensions
	-ln -sf ../xmlp4/xhtmlsyntax.cma extensions
	-ln -sf ../eliom/eliom.cmxa extensions
	-ln -sf ../eliom/eliom_duce.cmxa extensions
	-ln -sf ../xmlp4/ohl-xhtml/xhtml.cmxa extensions
	-ln -sf ../xmlp4/xhtmlpretty.cmxa extensions
	-ln -sf ../xmlp4/xhtmlsyntax.cmxa extensions
	-ln -sf ../eliom/eliom.cmxs extensions
	-ln -sf ../eliom/eliom_duce.cmxs extensions
	-ln -sf ../xmlp4/ohl-xhtml/xhtml.cmxs extensions
	-ln -sf ../xmlp4/xhtmlpretty.cmxs extensions
	-ln -sf ../xmlp4/xhtmlsyntax.cmxs extensions
	-ln -sf ../baselib/parsecommandline.cma extensions
	-ln -sf ../baselib/donotparsecommandline.cma extensions
	echo directory = \"$(SRC)/extensions\" > $@
	sed $(SED_COMMAND_FOR_META) -e "s%_MODULEINSTALLDIR_%$(SRC)/extensions%g" < $< >> $@
#	sed "s%\"xhtml\" (%\"xhtml\" (\n  directory = \"$(SRC)/xmlp4/ohl-xhtml/\"%g" >> $@

files/META.eliom_examples: files/META.eliom_examples.in VERSION
	sed $(SED_COMMAND_FOR_META) -e "s%_EXAMPLESINSTALLDIR_%$(SRC)/examples%g" < $< > $@

files/META.eliom_examples.global: files/META.eliom_examples.in VERSION
	sed $(SED_COMMAND_FOR_META) -e "s%_EXAMPLESINSTALLDIR_%$(EXAMPLESINSTALLDIR)%g"< $< > $@

$(OCSIGENNAME).conf.local: Makefile.config files/ocsigen.conf.in
	cat files/ocsigen.conf.in \
	| sed s%\<port\>80\</port\>%\<port\>8080\</port\>%g \
	| sed s%_LOGDIR_%$(SRC)/var/log%g \
	| sed s%_STATICPAGESDIR_%$(SRC)/files%g \
	| sed s%_CONFIGDIR_%$(SRC)/etc/ocsigen%g \
	| sed s%_DATADIR_%$(SRC)/var/lib%g \
	| sed s%_EXTRALIBDIR_%$(SRC)/extensions/ocsipersist-dbm%g \
	| sed s%_UP_%$(SRC)/tmp%g \
	| sed s%_OCSIGENUSER_%%g \
	| sed s%_OCSIGENGROUP_%%g \
	| sed s%_OCSIGENNAME_%$(OCSIGENNAME)%g \
	| sed s%_COMMANDPIPE_%$(SRC)/var/run/ocsigen_command%g \
	| sed s%_MIMEFILE_%$(SRC)/files/mime.types%g \
	| sed s%_MODULEINSTALLDIR_%$(SRC)/extensions%g \
	| sed s%_ELIOMINSTALLDIR_%$(SRC)/eliom%g \
	| sed s%_EXAMPLESINSTALLDIR_%$(SRC)/examples%g \
	| sed s%_METADIR_%$(SRC)/files%g \
	| sed s%_CAMLZIPNAME_%$(CAMLZIPNAME)%g \
	| sed s%files/miniwiki%examples/miniwiki/files%g \
	| sed s%var/lib/miniwiki%examples/miniwiki/wikidata%g \
	| sed s%\<\!--\ \<commandpipe%\<commandpipe%g \
	| sed s%\</commandpipe\>%\</commandpipe\>\ \<\!--%g \
	| sed s%\<\!--\ \<mimefile%\<mimefile%g \
	| sed s%\</mimefile\>%\</mimefile\>\ \<\!--%g \
	| sed s%ocsipersist-dbm.cma%ocsipersist-dbm/ocsipersist-dbm.cma%g \
	| sed s%store\ dir=\"$(SRC)/var/lib\"%store\ dir=\"$(SRC)/var/lib/ocsipersist\"%g \
	> $(OCSIGENNAME).conf.local
	cat $(OCSIGENNAME).conf.local \
	| sed s%[.]cmo%.cmxs%g \
	| sed s%[.]cma%.cmxs%g \
	| sed s%sist-dbm/ocsidbm\"%sist-dbm/ocsidbm.opt\"%g \
	| sed s%sqlite3.cmxs\"/\>%sqlite3.cmxs\"/\>\ \<\!--\ Create\ sqlite3.cmxs\ using:\ ocamlopt\ -shared\ -linkall\ -I\ \<path\ to\ ocaml\'s\ sqlite3\ directory\>\ -o\ sqlite3.cmxs\ \<path\ to\>/libsqlite3_stubs.a\ \<path\ to\>/sqlite3.cmxa\ --\>%g \
	> $(OCSIGENNAME).conf.opt.local

clean:
	-@for i in $(REPS) ; do $(MAKE) -C $$i clean ; done
	-rm -f $(OCSIGENNAME).conf.local $(OCSIGENNAME).conf.opt.local
	-rm -f $(METAS) $(OCSIGENNAME)-*.tar.gz
	-find . -name "*~" -delete

distclean: clean
	-find . -name "*depend" -delete
	-find doc -type f -delete
	-rm -f Makefile.config

depend:
	$(MAKE) -C xmlp4 depend
	$(MAKE) -C xmlp4 xmlp4pre.byte $(DEPOPT)
#	@for i in $(REPS) ; do touch "$$i"/.depend; $(MAKE) -C $$i depend ; done
	@for i in $(REPS) ; do $(MAKE) -C $$i depend ; done


.PHONY: partialinstall install doc docinstall installnodoc logrotate dist
partialinstall:
	mkdir -p $(TEMPROOT)$(MODULEINSTALLDIR)
	mkdir -p $(TEMPROOT)$(EXAMPLESINSTALLDIR)
	mkdir -p $(TEMPROOT)$(EXTRALIBDIR)/METAS
	mkdir -p $(TEMPROOT)$(EXTRALIBDIR)/extensions
	$(MAKE) -C server install
	mkdir -p "$(TEMPROOT)$(MODULEINSTALLDIR)"
	$(OCAMLFIND) install $(OCSIGENNAME) -destdir "$(TEMPROOT)$(MODULEINSTALLDIR)" $(TOINSTALL)
	$(INSTALL) -m 644 $(EXAMPLES) $(TEMPROOT)$(EXAMPLESINSTALLDIR)
#	$(INSTALL) -m 644 $(PLUGINSTOINSTALL) $(TEMPROOT)$(EXTRALIBDIR)/extensions
	-$(INSTALL) -m 755 extensions/ocsipersist-dbm/ocsidbm $(TEMPROOT)$(EXTRALIBDIR)/extensions
	[ ! -f extensions/ocsipersist-dbm/ocsidbm.opt ] || \
	$(INSTALL) -m 755 extensions/ocsipersist-dbm/ocsidbm.opt $(TEMPROOT)$(EXTRALIBDIR)/extensions
#	$(INSTALL) -m 644 META.ocsigen_ext.global $(TEMPROOT)$(EXTRALIBDIR)/METAS/META.ocsigen_ext
	$(INSTALL) -m 644 files/META.eliom_examples.global $(TEMPROOT)$(EXTRALIBDIR)/METAS/META.eliom_examples
	chmod a+rx $(TEMPROOT)$(MODULEINSTALLDIR)/$(OCSIGENNAME)
	chmod a+r $(TEMPROOT)$(MODULEINSTALLDIR)/$(OCSIGENNAME)/*
	chmod a+rx $(TEMPROOT)$(MODULEINSTALLDIR)
	chmod a+rx $(TEMPROOT)$(EXAMPLESINSTALLDIR)
	chmod a+rx $(TEMPROOT)$(EXTRALIBDIR)
	chmod a+rx $(TEMPROOT)$(EXTRALIBDIR)/METAS
	chmod a+rx $(TEMPROOT)$(EXTRALIBDIR)/extensions
	chmod a+rx "$(TEMPROOT)$(MODULEINSTALLDIR)"

docinstall: doc/index.html
	mkdir -p $(TEMPROOT)$(DOCDIR)
	$(INSTALL) -m 644 doc/* $(TEMPROOT)$(DOCDIR)
	chmod a+rx $(TEMPROOT)$(DOCDIR)
	chmod a+r $(TEMPROOT)$(DOCDIR)/*

installnodoc: partialinstall
	mkdir -p $(TEMPROOT)$(CONFIGDIR)
	mkdir -p $(TEMPROOT)$(CONFIGDIR)/conf.d
	mkdir -p $(TEMPROOT)$(STATICPAGESDIR)
	mkdir -p $(TEMPROOT)$(STATICPAGESDIR)/miniwiki
	mkdir -p $(TEMPROOT)$(STATICPAGESDIR)/tutorial
	mkdir -p $(TEMPROOT)$(STATICPAGESDIR)/ocsigenstuff
	mkdir -p $(TEMPROOT)$(DATADIR)
	mkdir -p $(TEMPROOT)$(DATADIR)/miniwiki
	mkdir -p `dirname $(TEMPROOT)$(COMMANDPIPE)`
	[ -p $(TEMPROOT)$(COMMANDPIPE) ] || { mkfifo $(TEMPROOT)$(COMMANDPIPE); \
	  chmod 660 $(TEMPROOT)$(COMMANDPIPE); \
	  $(CHOWN) -R $(OCSIGENUSER):$(OCSIGENGROUP) $(TEMPROOT)$(COMMANDPIPE);}
#	-mv $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf.old
	cat files/ocsigen.conf.in \
	| sed s%_LOGDIR_%$(LOGDIR)%g \
	| sed s%_STATICPAGESDIR_%$(STATICPAGESDIR)%g \
	| sed s%_CONFIGDIR_%$(CONFIGDIR)%g \
	| sed s%_DATADIR_%$(DATADIR)%g \
	| sed s%_BINDIR_%$(BINDIR)%g \
	| sed s%_EXTRALIBDIR_%$(EXTRALIBDIR)/extensions%g \
	| sed s%_UP_%$(UPLOADDIR)%g \
	| sed s%_OCSIGENUSER_%$(OCSIGENUSER)%g \
	| sed s%_OCSIGENGROUP_%$(OCSIGENGROUP)%g \
	| sed s%_OCSIGENNAME_%$(OCSIGENNAME)%g \
	| sed s%_COMMANDPIPE_%$(COMMANDPIPE)%g \
	| sed s%_MIMEFILE_%$(CONFIGDIR)/mime.types%g \
	| sed s%_MODULEINSTALLDIR_%$(MODULEINSTALLDIR)/$(OCSIGENNAME)%g \
	| sed s%_ELIOMINSTALLDIR_%$(MODULEINSTALLDIR)/$(OCSIGENNAME)%g \
	| sed s%_EXAMPLESINSTALLDIR_%$(EXAMPLESINSTALLDIR)%g \
	| sed s%_METADIR_%$(EXTRALIBDIR)/METAS%g \
	| sed s%_CAMLZIPNAME_%$(CAMLZIPNAME)%g \
	> $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf.sample
	cat $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf.sample \
	| sed s%[.]cmo%.cmxs%g \
	| sed s%[.]cma%.cmxs%g \
	> $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf.opt.sample
	-mv $(TEMPROOT)$(CONFIGDIR)/mime.types $(TEMPROOT)$(CONFIGDIR)/mime.types.old
	cp -f files/mime.types $(TEMPROOT)$(CONFIGDIR)
	mkdir -p $(TEMPROOT)$(LOGDIR)
	chmod u+rwx $(TEMPROOT)$(LOGDIR)
	chmod a+rx $(TEMPROOT)$(CONFIGDIR)
	chmod a+rx $(TEMPROOT)$(CONFIGDIR)/conf.d
	[ -f $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf ] || \
	{ cp $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf.sample \
             $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf; \
	  chmod a+r $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf; }
	chmod a+r $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf.sample
	[ -f $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf ] || \
	{ cp $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf.opt.sample \
             $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf.opt; \
	  chmod a+r $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf.opt; }
	chmod a+r $(TEMPROOT)$(CONFIGDIR)/$(OCSIGENNAME).conf.opt.sample
	chmod a+r $(TEMPROOT)$(CONFIGDIR)/mime.types
	$(INSTALL) -m 644 files/tutorial/style.css $(TEMPROOT)$(STATICPAGESDIR)/tutorial
	$(INSTALL) -m 644 files/tutorial/bulles-bleues.png $(TEMPROOT)$(STATICPAGESDIR)/tutorial
	$(INSTALL) -m 644 files/tutorial/ocsigen5.png $(TEMPROOT)$(STATICPAGESDIR)/tutorial
	$(INSTALL) -m 644 files/ocsigenstuff/* $(TEMPROOT)$(STATICPAGESDIR)/ocsigenstuff
	$(INSTALL) -m 644 examples/miniwiki/files/style.css $(TEMPROOT)$(STATICPAGESDIR)/miniwiki
	$(INSTALL) -m 644 examples/miniwiki/wikidata/* $(TEMPROOT)$(DATADIR)/miniwiki
	$(CHOWN) -R $(OCSIGENUSER):$(OCSIGENGROUP) $(TEMPROOT)$(LOGDIR)
	$(CHOWN) -R $(OCSIGENUSER):$(OCSIGENGROUP) $(TEMPROOT)$(STATICPAGESDIR)
	$(CHOWN) -R $(OCSIGENUSER):$(OCSIGENGROUP) $(TEMPROOT)$(DATADIR)
	chmod 750 $(TEMPROOT)$(DATADIR)
	$(INSTALL) -d -m 755 $(TEMPROOT)$(MANDIR)
	$(INSTALL) -m 644 files/ocsigen.1 $(TEMPROOT)$(MANDIR)

logrotate:
	[ -d /etc/logrotate.d ] && \
	 { mkdir -p $(TEMPROOT)/etc/logrotate.d ; \
	   cat files/logrotate.in \
	   | sed s%LOGDIR%$(LOGDIR)%g \
	   | sed s%USER%$(OCSIGENUSER)%g \
	   | sed s%GROUP%$(OCSIGENGROUP)%g \
	   | sed s%_COMMANDPIPE_%$(COMMANDPIPE)%g \
	  > $(TEMPROOT)/etc/logrotate.d/$(OCSIGENNAME); }

dist:
	DARCS_REPO=$(PWD) darcs dist -d $(OCSIGENNAME)-$(VERSION)

install: docinstall installnodoc


.PHONY: uninstall fulluninstall
uninstall:
	-rm -Rf $(TEMPROOT)$(DOCDIR)
	-rm -Rf $(TEMPROOT)$(EXTRALIBDIR)
	-$(MAKE) -C server uninstall
	-$(OCAMLFIND) remove $(OCSIGENNAME) -destdir "$(TEMPROOT)$(MODULEINSTALLDIR)"

fulluninstall: uninstall
# dangerous
#	rm -f $(CONFIGDIR)/$(OCSIGENNAME).conf
#	rm -f $(LOGDIR)/$(OCSIGENNAME).log
#	rm -rf $(MODULEINSTALLDIR)
