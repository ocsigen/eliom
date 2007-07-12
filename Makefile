include Makefile.config

ifeq "$(OCAMLDUCE)" "YES"
DUCECMAO=modules/ocsigenduce.cma modules/eliomduce.cma
# modules/ocsigenrss.cma
DUCECMI=modules/ocsigenduce.cmi modules/eliomduce.cmi modules/xhtml1_strict.cmi
# modules/rss2.cmi modules/ocsigenrss.cmi
DUCEEXAMPLES=examples/ocamlduce/exampleduce.cmo
# examples/ocamlduce/examplerss.cmo
DUCEDOC=modules/eliomduce.mli modules/xhtml1_strict.ml
CAMLDOC = $(OCAMLDUCEFIND) ocamldoc $(LIB)
else
DUCECMAO=
DUCECMI=
DUCEEXAMPLES=
DUCEDOC=
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
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
SQLITEINSTALL= modules/ocsipersist-sqlite.cma
else
endif

ifeq "$(OCSIPERSISTDBM)" "YES"
DBMINSTALL= modules/ocsipersist-dbm/ocsipersist-dbm.cma 
else
endif



INSTALL = install
TARGETSBYTE = baselib.byte lwt.byte xmlp4.byte http.byte server.byte modules.byte examples.byte

PLUGINSCMAOTOINSTALL = $(SQLITEINSTALL) $(DBMINSTALL) modules/eliom.cma modules/ocsigenmod.cma modules/staticmod.cmo modules/cgimod.cmo $(DUCECMAO)
PLUGINSCMITOINSTALL = modules/ocsipersist.cmi modules/eliom.cmi modules/ocsigen.cmi modules/staticmod.cmi modules/cgimod.cmi modules/ocsigenboxes.cmi modules/eliomboxes.cmi $(DUCECMI)

CMAOTOINSTALL = xmlp4/xhtmlsyntax.cma
CMITOINSTALL = server/extensions.cmi server/parseconfig.cmi xmlp4/ohl-xhtml/xHTML.cmi xmlp4/ohl-xhtml/xML.cmi xmlp4/xhtmltypes.cmi xmlp4/simplexmlparser.cmi lwt/lwt.cmi lwt/lwt_unix.cmi server/preemptive.cmi http/predefined_senders.cmi http/framepp.cmi http/http_com.cmi baselib/ocsimisc.cmi baselib/ocsiconfig.cmi http/http_frame.cmi http/ocsistream.cmi baselib/messages.cmi META
EXAMPLESCMO = examples/tutoeliom.cmo examples/tutoocsigenmod.cmo examples/monitoring.cmo examples/nurpawiki/nurpawiki.cmo $(DUCEEXAMPLES)
EXAMPLESCMI = examples/tutoeliom.cmi examples/tutoocsigenmod.cmi

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
else
TOINSTALLX=
EXAMPLESOPT=
OPT=
endif

TOINSTALL=$(TOINSTALLBYTE) $(TOINSTALLX) $(CMITOINSTALL) $(PLUGINSCMITOINSTALL)
EXAMPLES=$(EXAMPLESBYTE) $(EXAMPLESOPT) $(EXAMPLESCMI)

REPS=$(TARGETSBYTE:.byte=)

all: $(BYTE) $(OPT) $(OCSIGENNAME).conf.local

byte: $(TARGETSBYTE)

opt: $(TARGETSBYTE:.byte=.opt)

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
	touch xmlp4/.depend
	$(MAKE) -C xmlp4 depend
	$(MAKE) -C xmlp4 byte

xmlp4.opt:
	touch xmlp4/.depend
	$(MAKE) -C xmlp4 depend
	$(MAKE) -C xmlp4 opt

http: http.byte

http.byte:
	$(MAKE) -C http byte

http.opt:
	$(MAKE) -C http opt

modules: modules.byte

modules.byte:
	$(MAKE) -C modules byte

modules.opt:
	$(MAKE) -C modules opt

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
	$(CAMLDOC) -package ssl $(LIBDIRS3) -d doc/lwt -html lwt/lwt.mli lwt/lwt_unix.mli
	$(CAMLDOC) -package netstring $(LIBDIRS3) -I `$(CAMLP4) -where` -I +threads -d doc/oc -html modules/eliom.mli modules/ocsigen.mli server/extensions.mli server/parseconfig.mli xmlp4/ohl-xhtml/xHTML.mli modules/ocsigenboxes.mli baselib/messages.ml http/predefined_senders.mli modules/eliomboxes.mli modules/ocsipersist.mli $(DUCEDOC)

$(OCSIGENNAME).conf.local:
	cat files/ocsigen.conf \
	| sed s%\<port\>80\</port\>%\<port\>8080\</port\>%g \
	| sed s%_LOGDIR_%$(SRC)/var/log%g \
	| sed s%_STATICPAGESDIR_%$(SRC)/files%g \
	| sed s%_DATADIR_%$(SRC)/var/lib%g \
	| sed s%_BINDIR_%$(SRC)/modules/ocsipersist-dbm%g \
	| sed s%_UP_%$(SRC)/tmp%g \
	| sed s%_OCSIGENUSER_%%g \
	| sed s%_OCSIGENGROUP_%%g \
	| sed s%_OCSIGENNAME_%$(OCSIGENNAME)%g \
	| sed s%_COMMANDPIPE_%$(SRC)/var/run/ocsigen_command%g \
	| sed s%_MODULEINSTALLDIR_%$(SRC)/modules%g \
	| sed s%_EXAMPLESINSTALLDIR_%$(SRC)/examples%g \
	| sed s%_OCAMLSQLITE3DIR_%$(OCAMLSQLITE3DIR)%g \
	| sed s%files/nurpawiki%examples/nurpawiki/files%g \
	| sed s%var/lib/nurpawiki%examples/nurpawiki/wikidata%g \
	| sed s%\<\!--\ commandpipe%\<commandpipe%g \
	| sed s%\</commandpipe\ --%\</commandpipe%g \
	| sed s%ocsipersist-dbm.cma%ocsipersist-dbm/ocsipersist-dbm.cma%g \
	| sed s%store\ dir=\"$(SRC)/var/lib\"%store\ dir=\"$(SRC)/var/lib/ocsipersist\"%g \
	> $(OCSIGENNAME).conf.local
	cat $(OCSIGENNAME).conf.local \
	| sed s%[.]cmo%.cmxs%g \
	| sed s%[.]cma%.cmxs%g \
	> $(OCSIGENNAME).conf.opt.local

clean:
	-@for i in $(REPS) ; do touch "$$i"/.depend ; done
	-@for i in $(REPS) ; do $(MAKE) -C $$i clean ; done
	-rm -f lib/* *~
	-rm -f bin/* *~
	-rm $(OCSIGENNAME).conf.local $(OCSIGENNAME).conf.opt.local

depend: xmlp4.byte
	touch lwt/depend
	@for i in $(REPS) ; do touch "$$i"/.depend; $(MAKE) -C $$i depend ; done


.PHONY: partialinstall install doc docinstall installnodoc logrotate
partialinstall:
	mkdir -p $(TEMPROOT)/$(MODULEINSTALLDIR)
	mkdir -p $(TEMPROOT)/$(EXAMPLESINSTALLDIR)
	$(MAKE) -C server install
	cat META.in | sed s/_VERSION_/`head -n 1 VERSION`/ > META
	$(OCAMLFIND) install $(OCSIGENNAME) -destdir "$(TEMPROOT)/$(MODULEINSTALLDIR)" $(TOINSTALL)
	$(INSTALL) -m 644 $(EXAMPLES) $(TEMPROOT)/$(EXAMPLESINSTALLDIR)
	-$(INSTALL) -m 755 modules/ocsipersist-dbm/ocsidbm $(TEMPROOT)/$(BINDIR)/
	-$(INSTALL) -m 755 modules/ocsipersist-dbm/ocsidbm.opt $(TEMPROOT)/$(BINDIR)/
	-rm META

docinstall: doc
	mkdir -p $(TEMPROOT)/$(DOCDIR)
	$(INSTALL) -d -m 755 $(TEMPROOT)/$(DOCDIR)/lwt
	$(INSTALL) -d -m 755 $(TEMPROOT)/$(DOCDIR)/oc
	-$(INSTALL) -m 644 doc/* $(TEMPROOT)/$(DOCDIR)
	$(INSTALL) -m 644 doc/lwt/* $(TEMPROOT)/$(DOCDIR)/lwt
	$(INSTALL) -m 644 doc/oc/* $(TEMPROOT)/$(DOCDIR)/oc
	chmod a+rx $(TEMPROOT)/$(DOCDIR)
	chmod a+r $(TEMPROOT)/$(DOCDIR)/*

installnodoc: partialinstall
	mkdir -p $(TEMPROOT)/$(CONFIGDIR)
	mkdir -p $(TEMPROOT)/$(STATICPAGESDIR)
	mkdir -p $(TEMPROOT)/$(STATICPAGESDIR)/nurpawiki
	mkdir -p $(TEMPROOT)/$(STATICPAGESDIR)/tutorial
	mkdir -p $(TEMPROOT)/$(DATADIR)
	mkdir -p $(TEMPROOT)/$(DATADIR)/nurpawiki
	mkdir -p `dirname $(TEMPROOT)/$(COMMANDPIPE)`
	[ -p $(TEMPROOT)/$(COMMANDPIPE) ] || { mkfifo $(TEMPROOT)/$(COMMANDPIPE); \
	  chmod 660 $(TEMPROOT)/$(COMMANDPIPE); \
	  $(CHOWN) -R $(OCSIGENUSER):$(OCSIGENGROUP) $(TEMPROOT)/$(COMMANDPIPE);}
#	-mv $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf.old
	cat files/ocsigen.conf \
	| sed s%_LOGDIR_%$(LOGDIR)%g \
	| sed s%_STATICPAGESDIR_%$(STATICPAGESDIR)%g \
	| sed s%_DATADIR_%$(DATADIR)%g \
	| sed s%_BINDIR_%$(BINDIR)%g \
	| sed s%_UP_%$(UPLOADDIR)%g \
	| sed s%_OCSIGENUSER_%$(OCSIGENUSER)%g \
	| sed s%_OCSIGENGROUP_%$(OCSIGENGROUP)%g \
	| sed s%_OCSIGENNAME_%$(OCSIGENNAME)%g \
	| sed s%_COMMANDPIPE_%$(COMMANDPIPE)%g \
	| sed s%_MODULEINSTALLDIR_%$(MODULEINSTALLDIR)/$(OCSIGENNAME)%g \
	| sed s%_EXAMPLESINSTALLDIR_%$(EXAMPLESINSTALLDIR)%g \
	| sed s%_OCAMLSQLITE3DIR_%$(OCAMLSQLITE3DIR)%g \
	> $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf.sample
	cat $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf.sample \
	| sed s%[.]cmo%.cmxs%g \
	| sed s%[.]cma%.cmxs%g \
	> $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf.opt.sample
	-mv $(TEMPROOT)/$(CONFIGDIR)/mime.types $(TEMPROOT)/$(CONFIGDIR)/mime.types.old
	cp -f files/mime.types $(TEMPROOT)/$(CONFIGDIR)
	mkdir -p $(TEMPROOT)/$(LOGDIR)
	chmod u+rwx $(TEMPROOT)/$(LOGDIR)
	chmod a+rx $(TEMPROOT)/$(CONFIGDIR)
	[ -f $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf ] || \
	{ cp $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf.sample \
             $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf; \
	  chmod a+r $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf; }
	chmod a+r $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf.sample
	[ -f $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf ] || \
	{ cp $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf.opt.sample \
             $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf.opt; \
	  chmod a+r $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf.opt; }
	chmod a+r $(TEMPROOT)/$(CONFIGDIR)/$(OCSIGENNAME).conf.opt.sample
	chmod a+r $(TEMPROOT)/$(CONFIGDIR)/mime.types
	$(INSTALL) -m 644 files/style.css $(TEMPROOT)/$(STATICPAGESDIR)/tutorial
	$(INSTALL) -m 644 files/ocsigen5.png $(TEMPROOT)/$(STATICPAGESDIR)/tutorial
	$(INSTALL) -m 644 examples/nurpawiki/files/style.css $(TEMPROOT)/$(STATICPAGESDIR)/nurpawiki
	$(INSTALL) -m 644 examples/nurpawiki/wikidata/* $(TEMPROOT)/$(DATADIR)/nurpawiki
	$(CHOWN) -R $(OCSIGENUSER):$(OCSIGENGROUP) $(TEMPROOT)/$(LOGDIR)
	$(CHOWN) -R $(OCSIGENUSER):$(OCSIGENGROUP) $(TEMPROOT)/$(STATICPAGESDIR)
	$(CHOWN) -R $(OCSIGENUSER):$(OCSIGENGROUP) $(TEMPROOT)/$(DATADIR)
	$(INSTALL) -d -m 755 $(TEMPROOT)/$(MANDIR)
	$(INSTALL) -m 644 files/ocsigen.1 $(TEMPROOT)/$(MANDIR)

logrotate:
	[ -d /etc/logrotate.d ] && \
	 { mkdir -p ${TEMPROOT}/etc/logrotate.d ; \
	   cat files/logrotate.IN \
	   | sed s%LOGDIR%$(LOGDIR)%g \
	   | sed s%USER%$(OCSIGENUSER)%g \
	   | sed s%GROUP%$(OCSIGENGROUP)%g \
	  > $(TEMPROOT)/etc/logrotate.d/$(OCSIGENNAME); }

install: docinstall installnodoc


.PHONY: uninstall fulluninstall
uninstall:
	$(MAKE) -C server uninstall
	$(OCAMLFIND) remove $(OCSIGENNAME) -destdir "$(TEMPROOT)/$(MODULEINSTALLDIR)"

fulluninstall: uninstall
# dangerous
#	rm -f $(CONFIGDIR)/$(OCSIGENNAME).conf
#	rm -f $(LOGDIR)/$(OCSIGENNAME).log
#	rm -rf $(MODULEINSTALLDIR)

