include Makefile.config

VERSION := $(shell head -n 1 VERSION)

ifeq "$(OCAMLDUCE)" "YES"
DUCECMAO=eliom/eliomduce.cma
# eliom/ocsigenrss.cma
DUCECMI=eliom/eliomduce.cmi eliom/xhtml1_strict.cmi eliom/eliomducetools.cmi
# eliom/rss2.cmi eliom/ocsigenrss.cmi
DUCEEXAMPLES=examples/ocamlduce/exampleduce.cmo
# examples/ocamlduce/examplerss.cmo
DUCEDOC=./eliom/eliomduce.mli ./eliom/xhtml1_strict.ml ./eliom/eliomducetools.ml
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

DOC= ./lwt/lwt.mli ./lwt/lwt_unix.mli ./lwt/lwt_util.mli ./lwt/lwt_chan.mli ./lwt/lwt_ssl.mli ./lwt/lwt_timeout.mli ./lwt/preemptive.mli ./lwt/lwt_lib.mli ./eliom/eliommkforms.mli ./eliom/eliommkreg.mli ./eliom/eliompredefmod.mli ./eliom/eliommod.mli ./eliom/eliomparameters.mli ./eliom/eliomservices.mli ./eliom/eliomsessions.mli ./server/extensions.mli ./server/parseconfig.mli ./xmlp4/xhtmlpretty.mli ./xmlp4/oldocaml/xhtmltypes.ml ./xmlp4/ohl-xhtml/xHTML.mli ./baselib/messages.mli ./http/ocsiheaders.mli ./server/http_client.mli ./http/http_frame.mli ./http/http_com.mli ./http/predefined_senders.mli ./eliom/eliomtools.mli ./extensions/ocsipersist.mli ./xmlp4/oldocaml/simplexmlparser.mli $(DUCEDOC)


INSTALL = install
TARGETSBYTE = lwt.byte baselib.byte http.byte xmlp4.byte server.byte extensions.byte eliom.byte examples.byte

PLUGINSCMAOTOINSTALL = $(SQLITEINSTALL) $(DBMINSTALL) \
	eliom/eliom.cma \
	extensions/staticmod.cmo extensions/cgimod.cmo $(DEFLATEMODINSTALL) \
        extensions/revproxy.cmo extensions/redirectmod.cmo extensions/accesscontrol.cmo \
	$(DUCECMAO)
PLUGINSCMITOINSTALL = extensions/ocsipersist.cmi \
       eliom/eliommkforms.cmi eliom/eliommkreg.cmi \
       eliom/eliomtools.cmi \
       $(DUCECMI) \
       eliom/eliomsessions.cmi eliom/eliomparameters.cmi \
       eliom/eliomservices.cmi eliom/eliompredefmod.cmi \
       eliom/eliommod.cmi

CMAOTOINSTALL = xmlp4/xhtmlsyntax.cma
CMITOINSTALL = server/extensions.cmi server/parseconfig.cmi xmlp4/xhtmlpretty.cmi xmlp4/ohl-xhtml/xHTML.cmi xmlp4/ohl-xhtml/xML.cmi xmlp4/xhtmltypes.cmi xmlp4/simplexmlparser.cmi http/predefined_senders.cmi http/framepp.cmi http/http_com.cmi http/http_headers.cmi baselib/ocsimisc.cmi baselib/ocsiconfig.cmi http/http_frame.cmi http/ocsiheaders.cmi http/ocsistream.cmi baselib/messages.cmi META
#LWTCMITOINSTALL = lwt/lwt.cmi lwt/lwt_unix.cmi lwt/lwt_chan.cmi lwt/lwt_ssl.cmi lwt/lwt_timeout.cmi lwt/lwt_util.cmi lwt/META
EXAMPLESCMO = examples/tutoeliom.cmo examples/monitoring.cmo examples/miniwiki/miniwiki.cmo $(DUCEEXAMPLES)
EXAMPLESCMI = examples/tutoeliom.cmi

ifeq "$(BYTECODE)" "YES"
TOINSTALLBYTE=$(CMAOTOINSTALL) $(PLUGINSCMAOTOINSTALL)
EXAMPLESBYTE=$(EXAMPLESCMO)
BYTE=byte
else
TOINSTALLBYTE=
EXAMPLESBYTE=
BYTE=
endif

ifeq "$(NATIVECODE)" "YES"
TOINSTALLXTEMP1=$(PLUGINSCMAOTOINSTALL:.cmo=.cmxs)
TOINSTALLXTEMP=$(CMAOTOINSTALL:.cmo=.cmx)
TOINSTALLX=$(TOINSTALLXTEMP:.cma=.cmxa) $(TOINSTALLXTEMP1:.cma=.cmxs)
EXAMPLESOPT=$(EXAMPLESCMO:.cmo=.cmxs)
OPT=opt
DEPOPT=xmlp4pre.opt
else
TOINSTALLX=
EXAMPLESOPT=
OPT=
endif

TOINSTALL=$(TOINSTALLBYTE) $(TOINSTALLX) $(CMITOINSTALL) $(PLUGINSCMITOINSTALL)
EXAMPLES=$(EXAMPLESBYTE) $(EXAMPLESOPT) $(EXAMPLESCMI)

REPS=$(TARGETSBYTE:.byte=)

all: $(BYTE) $(OPT) $(OCSIGENNAME).conf.local

byte: xmlp4pre.byte $(TARGETSBYTE)

opt: xmlp4pre.opt $(TARGETSBYTE:.byte=.opt)

.PHONY: $(REPS) clean


baselib: baselib.byte

baselib.byte:
	$(MAKE) -C baselib byte

baselib.opt:
	$(MAKE) -C baselib opt

lwt: lwt.byte

lwt.byte:
	$(MAKE) -C lwt byte

lwt: lwt.opt

lwt.opt:
	$(MAKE) -C lwt opt

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
	$(CAMLDOC) -package ssl,netstring$(DUCEPACK) $(LIBDIRS3) -I `$(CAMLP4) -where` -I +threads -intro files/indexdoc -d doc -html $(DOC)

doc/index.html: doc

$(OCSIGENNAME).conf.local: Makefile.config files/ocsigen.conf
	cat files/ocsigen.conf \
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
	| sed s%_OCAMLSQLITE3DIR_%$(OCAMLSQLITE3DIR)%g \
	| sed s%_CRYPTOKITINSTALLDIR_%$(CRYPTOKITINSTALLDIR)%g \
	| sed s%_CAMLZIPDIR_%$(CAMLZIPDIR)%g \
	| sed s%_OCAMLLIBDIR_%$(shell ocamlc -where)%g \
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
#	-@for i in $(REPS) ; do touch "$$i"/.depend ; done
	-@for i in $(REPS) ; do $(MAKE) -C $$i clean ; done
	-rm -f lib/* lib/*~
	-rm -f bin/* bin/*~
	-rm -f doc/* doc/*~
	-rm $(OCSIGENNAME).conf.local $(OCSIGENNAME).conf.opt.local

depend: xmlp4pre.byte $(DEPOPT)
#	touch lwt/depend
#	@for i in $(REPS) ; do touch "$$i"/.depend; $(MAKE) -C $$i depend ; done
	@for i in $(REPS) ; do $(MAKE) -C $$i depend ; done


.PHONY: partialinstall install doc docinstall installnodoc logrotate dist
partialinstall:
	$(MAKE) -C lwt install
	mkdir -p $(TEMPROOT)$(MODULEINSTALLDIR)
	mkdir -p $(TEMPROOT)$(EXAMPLESINSTALLDIR)
	mkdir -p $(TEMPROOT)$(EXTRALIBDIR)
	$(MAKE) -C server install
	cat META.in | sed s/_VERSION_/$(VERSION)/ > META
	mkdir -p "$(TEMPROOT)$(MODULEINSTALLDIR)"
	$(OCAMLFIND) install $(OCSIGENNAME) -destdir "$(TEMPROOT)$(MODULEINSTALLDIR)" $(TOINSTALL)
	$(INSTALL) -m 644 $(EXAMPLES) $(TEMPROOT)$(EXAMPLESINSTALLDIR)
	-$(INSTALL) -m 755 extensions/ocsipersist-dbm/ocsidbm $(TEMPROOT)$(EXTRALIBDIR)
	[ ! -f extensions/ocsipersist-dbm/ocsidbm.opt ] || \
	$(INSTALL) -m 755 extensions/ocsipersist-dbm/ocsidbm.opt $(TEMPROOT)$(EXTRALIBDIR)
	-rm META

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
	cat files/ocsigen.conf \
	| sed s%_LOGDIR_%$(LOGDIR)%g \
	| sed s%_STATICPAGESDIR_%$(STATICPAGESDIR)%g \
	| sed s%_CONFIGDIR_%$(CONFIGDIR)%g \
	| sed s%_DATADIR_%$(DATADIR)%g \
	| sed s%_BINDIR_%$(BINDIR)%g \
	| sed s%_EXTRALIBDIR_%$(EXTRALIBDIR)%g \
	| sed s%_UP_%$(UPLOADDIR)%g \
	| sed s%_OCSIGENUSER_%$(OCSIGENUSER)%g \
	| sed s%_OCSIGENGROUP_%$(OCSIGENGROUP)%g \
	| sed s%_OCSIGENNAME_%$(OCSIGENNAME)%g \
	| sed s%_COMMANDPIPE_%$(COMMANDPIPE)%g \
	| sed s%_MODULEINSTALLDIR_%$(MODULEINSTALLDIR)/$(OCSIGENNAME)%g \
	| sed s%_ELIOMINSTALLDIR_%$(MODULEINSTALLDIR)/$(OCSIGENNAME)%g \
	| sed s%_EXAMPLESINSTALLDIR_%$(EXAMPLESINSTALLDIR)%g \
	| sed s%_OCAMLSQLITE3DIR_%$(OCAMLSQLITE3DIR)%g \
	| sed s%_CRYPTOKITINSTALLDIR_%$(CRYPTOKITINSTALLDIR)%g \
	| sed s%_CAMLZIPDIR_%$(CAMLZIPDIR)%g \
	| sed s%_OCAMLLIBDIR_%$(shell ocamlc -where)%g \
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
	$(INSTALL) -d -m 755 $(TEMPROOT)$(MANDIR)
	$(INSTALL) -m 644 files/ocsigen.1 $(TEMPROOT)$(MANDIR)

logrotate:
	[ -d /etc/logrotate.d ] && \
	 { mkdir -p $(TEMPROOT)/etc/logrotate.d ; \
	   cat files/logrotate.IN \
	   | sed s%LOGDIR%$(LOGDIR)%g \
	   | sed s%USER%$(OCSIGENUSER)%g \
	   | sed s%GROUP%$(OCSIGENGROUP)%g \
	  > $(TEMPROOT)/etc/logrotate.d/$(OCSIGENNAME); }

dist:
	darcs setpref predist "sh ./setperm; rm setperm"
	darcs dist -d ocsigen-$(VERSION)

install: docinstall installnodoc


.PHONY: uninstall fulluninstall
uninstall:
	-rm -Rf $(TEMPROOT)$(DOCDIR)
	-rm -f $(TEMPROOT)$(EXTRALIBDIR)/ocsidbm
	-rm -f $(TEMPROOT)$(EXTRALIBDIR)/ocsidbm.opt
	-$(MAKE) -C server uninstall
	-$(MAKE) -C lwt uninstall
	-$(OCAMLFIND) remove $(OCSIGENNAME) -destdir "$(TEMPROOT)$(MODULEINSTALLDIR)"

fulluninstall: uninstall
# dangerous
#	rm -f $(CONFIGDIR)/$(OCSIGENNAME).conf
#	rm -f $(LOGDIR)/$(OCSIGENNAME).log
#	rm -rf $(MODULEINSTALLDIR)

