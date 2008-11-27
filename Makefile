include Makefile.config

VERSION := $(shell head -n 1 VERSION)

DOCPREF=./

# sed commands used for generation of META files
SED_COMMAND_FOR_META =
SED_COMMAND_FOR_META += -e "s/_VERSION_/$(VERSION)/"
SED_COMMAND_FOR_META += -e "s/_CAMLZIPNAME_/$(CAMLZIPNAME)/"

ifeq "$(OCAMLDUCE)" "YES"
DUCECMAO=eliom/eliom_duce.cma
# eliom/ocsigenrss.cma
DUCECMI=eliom/eliom_duce.cmi eliom/xhtmltypes_duce.cmi eliom/eliom_duce_tools.cmi
# eliom/rss2.cmi eliom/ocsigenrss.cmi
DUCEEXAMPLES=examples/ocamlduce/exampleduce.cmo
# examples/ocamlduce/examplerss.cmo
DUCEDOC=$(DOCPREF)eliom/eliom_duce.mli $(DOCPREF)eliom/xhtmltypes_duce.ml $(DOCPREF)eliom/eliom_duce_tools.ml
CAMLDOC = $(OCAMLDUCEFIND) ocamldoc $(LIB)
DUCEPACK=,ocamlduce
else
DUCECMAO=
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
SQLITEINSTALL= extensions/ocsipersist-sqlite.cma
else
endif

ifeq "$(CAMLZIP)" "YES"
DEFLATEMODINSTALL= extensions/deflatemod.cmo
else
endif

ifeq "$(OCSIPERSISTDBM)" "YES"
DBMINSTALL= extensions/ocsipersist-dbm/ocsipersist-dbm.cma 
else
endif

DOC= $(DOCPREF)eliom/eliom_mkforms.mli $(DOCPREF)eliom/eliom_mkreg.mli $(DOCPREF)eliom/eliom_predefmod.mli $(DOCPREF)eliom/eliom_common.mli $(DOCPREF)eliom/eliom_parameters.mli $(DOCPREF)eliom/eliom_services.mli $(DOCPREF)eliom/eliom_sessions.mli $(DOCPREF)eliom/eliom_extensions.mli $(DOCPREF)server/ocsigen_extensions.mli $(DOCPREF)server/ocsigen_parseconfig.mli $(DOCPREF)xmlp4/xhtmlpretty.mli $(DOCPREF)xmlp4/xhtmlcompact.mli $(DOCPREF)xmlp4/oldocaml/xhtmltypes.ml $(DOCPREF)xmlp4/ohl-xhtml/xHTML.mli $(DOCPREF)baselib/ocsigen_messages.mli $(DOCPREF)http/ocsigen_headers.mli $(DOCPREF)server/ocsigen_http_client.mli $(DOCPREF)http/ocsigen_http_frame.mli $(DOCPREF)http/ocsigen_http_com.mli $(DOCPREF)http/mime.mli $(DOCPREF)http/ocsigen_senders.mli $(DOCPREF)http/ocsigen_stream.mli $(DOCPREF)eliom/eliom_tools.mli $(DOCPREF)extensions/ocsipersist.mli $(DOCPREF)extensions/authbasic.mli $(DOCPREF)xmlp4/oldocaml/simplexmlparser.mli $(DUCEDOC)
METAS = META META.ocsigen_ext META.eliom_examples META.ocsigen_ext.global META.eliom_examples.global


INSTALL = install
TARGETSBYTE = baselib.byte http.byte xmlp4.byte server.byte extensions.byte eliom.byte examples.byte

PLUGINSCMAOTOINSTALL = $(SQLITEINSTALL) $(DBMINSTALL) \
	eliom/eliom.cma \
	extensions/staticmod.cmo extensions/cgimod.cmo $(DEFLATEMODINSTALL) \
        extensions/revproxy.cmo extensions/userconf.cmo \
        extensions/outputfilter.cmo extensions/authbasic.cmo \
	extensions/redirectmod.cmo extensions/rewritemod.cmo \
	extensions/accesscontrol.cmo \
	$(DUCECMAO)
PLUGINSCMITOINSTALL = extensions/ocsipersist.cmi \
       eliom/eliom_mkforms.cmi eliom/eliom_mkreg.cmi \
       eliom/eliom_tools.cmi \
       $(DUCECMI) \
       eliom/eliom_sessions.cmi eliom/eliom_parameters.cmi \
       eliom/eliom_services.cmi eliom/eliom_predefmod.cmi \
       eliom/eliommod.cmi eliom/eliom_common.cmi eliom/eliom_extensions.cmi \
       extensions/localFiles.cmi extensions/accesscontrol.cmi

CMAOTOINSTALL = xmlp4/xhtmlsyntax.cma xmlp4/ohl-xhtml/xhtml.cma server/ocsigen.cma
CMITOINSTALL = server/ocsigen_extensions.cmi server/ocsigen_parseconfig.cmi xmlp4/xhtmlpretty.cmi xmlp4/xhtmlcompact.cmi xmlp4/ohl-xhtml/xHTML.cmi xmlp4/ohl-xhtml/xML.cmi xmlp4/xhtmltypes.cmi xmlp4/simplexmlparser.cmi http/mime.cmi http/ocsigen_senders.cmi http/framepp.cmi http/ocsigen_http_com.cmi http/http_headers.cmi baselib/ocsigen_lib.cmi baselib/ocsigen_config.cmi http/ocsigen_http_frame.cmi http/ocsigen_headers.cmi http/ocsigen_stream.cmi baselib/ocsigen_messages.cmi extensions/localFiles.cmi META
EXAMPLESCMO = examples/tutoeliom.cmo examples/monitoring.cmo examples/miniwiki/miniwiki.cmo $(DUCEEXAMPLES)
EXAMPLESCMI = examples/tutoeliom.cmi

ifeq "$(BYTECODE)" "YES"
TOINSTALLBYTE=$(CMAOTOINSTALL)
PLUGINSTOINSTALLBYTE=$(PLUGINSCMAOTOINSTALL)
EXAMPLESBYTE=$(EXAMPLESCMO)
BYTE=byte
else
TOINSTALLBYTE=
PLUGINSTOINSTALLBYTE=
EXAMPLESBYTE=
BYTE=
endif

ifeq "$(NATIVECODE)" "YES"
TOINSTALLXTEMP1=$(PLUGINSCMAOTOINSTALL:.cmo=.cmxs)
PLUGINSTOINSTALLX=$(TOINSTALLXTEMP1:.cma=.cmxs)
TOINSTALLXTEMP=$(CMAOTOINSTALL:.cmo=.cmx)
TOINSTALLX=$(TOINSTALLXTEMP:.cma=.cmxa)
EXAMPLESOPT=$(EXAMPLESCMO:.cmo=.cmxs)
OPT=opt
DEPOPT=xmlp4pre.opt
else
TOINSTALLX=
PLUGINSTOINSTALLX=
EXAMPLESOPT=
OPT=
endif

STATICSTUBS = server/lib$(OCSIGENNAME).a

TOINSTALL=$(TOINSTALLBYTE) $(TOINSTALLX) $(CMITOINSTALL) $(PLUGINSCMITOINSTALL) $(STATICSTUBS)
PLUGINSTOINSTALL=$(PLUGINSTOINSTALLBYTE) $(PLUGINSTOINSTALLX)
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
	$(CAMLDOC) -package lwt,ssl,netstring$(DUCEPACK) $(LIBDIRS3) -I `$(CAMLP4) -where` -I +threads -intro files/indexdoc -d doc -html $(DOC)

doc/index.html: doc

META: files/META.in
	sed $(SED_COMMAND_FOR_META) < $< > $@

META.ocsigen_ext: files/META.ocsigen_ext.in
	-ln -sf ../eliom/eliom.cma extensions
	-ln -sf ../eliom/eliom_duce.cma extensions
	sed $(SED_COMMAND_FOR_META) -e "s%_MODULEINSTALLDIR_%$(SRC)/extensions%g" < $< > $@

META.ocsigen_ext.global: files/META.ocsigen_ext.in
	sed $(SED_COMMAND_FOR_META) -e "s%_MODULEINSTALLDIR_%$(EXTRALIBDIR)/extensions%g" < $< > $@

META.eliom_examples: files/META.eliom_examples.in
	sed $(SED_COMMAND_FOR_META) -e "s%_EXAMPLESINSTALLDIR_%$(SRC)/examples%g" < $< > $@

META.eliom_examples.global: files/META.eliom_examples.in
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
	| sed s%_MODULEINSTALLDIR_%$(SRC)/extensions%g \
	| sed s%_ELIOMINSTALLDIR_%$(SRC)/eliom%g \
	| sed s%_EXAMPLESINSTALLDIR_%$(SRC)/examples%g \
	| sed s%_METADIR_%$(SRC)%g \
	| sed s%_CAMLZIPNAME_%$(CAMLZIPNAME)%g \
	| sed s%files/miniwiki%examples/miniwiki/files%g \
	| sed s%var/lib/miniwiki%examples/miniwiki/wikidata%g \
	| sed s%\<\!--\ \<commandpipe%\<commandpipe%g \
	| sed s%\</commandpipe\>%\</commandpipe\>\ \<\!--%g \
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
	-rm $(OCSIGENNAME).conf.local $(OCSIGENNAME).conf.opt.local
	-rm -f $(METAS) $(OCSIGENNAME)-*.tar.gz
	-find . -name "*~" -delete

distclean: clean
	-find . -name "*depend" -delete
	-find doc -type f -delete
	-rm -f Makefile.config

depend: xmlp4pre.byte $(DEPOPT)
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
	$(INSTALL) -m 644 $(PLUGINSTOINSTALL) $(TEMPROOT)$(EXTRALIBDIR)/extensions
	-$(INSTALL) -m 755 extensions/ocsipersist-dbm/ocsidbm $(TEMPROOT)$(EXTRALIBDIR)/extensions
	[ ! -f extensions/ocsipersist-dbm/ocsidbm.opt ] || \
	$(INSTALL) -m 755 extensions/ocsipersist-dbm/ocsidbm.opt $(TEMPROOT)$(EXTRALIBDIR)/extensions
	$(INSTALL) -m 644 META.ocsigen_ext.global $(TEMPROOT)$(EXTRALIBDIR)/METAS/META.ocsigen_ext
	$(INSTALL) -m 644 META.eliom_examples.global $(TEMPROOT)$(EXTRALIBDIR)/METAS/META.eliom_examples

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
