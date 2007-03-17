include Makefile.config

ifeq "$(OCAMLDUCE)" "YES"
DUCECMAO=server/ocsigenduce.cma
# server/ocsigenrss.cma
DUCECMI=server/ocsigenduce.cmi server/xhtml1_strict.cmi
# server/rss2.cmi server/ocsigenrss.cmi
DUCEEXAMPLES=modules/ocamlduce/exampleduce.cmo
# modules/ocamlduce/examplerss.cmo
else
DUCECMAO=
DUCECMI=
DUCEEXAMPLES=
endif



INSTALL = install
TARGETSBYTE = baselib.byte lwt.byte xmlp4.byte http.byte server.byte modules.byte
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
PLUGINSCMAOTOINSTALL = server/ocsigenmod.cma server/staticmod.cmo $(DUCECMAO)
PLUGINSCMITOINSTALL = server/ocsigen.cmi server/staticmod.cmi server/ocsigenboxes.cmi $(DUCECMI)
CMAOTOINSTALL = xmlp4/xhtmlsyntax.cma
CMITOINSTALL = server/parseconfig.cmi xmlp4/ohl-xhtml/xHTML.cmi xmlp4/ohl-xhtml/xML.cmi xmlp4/xhtmltypes.cmi xmlp4/simplexmlparser.cmi lwt/lwt.cmi lwt/lwt_unix.cmi server/preemptive.cmi http/predefined_senders.cmi baselib/messages.cmi META
EXAMPLESCMO = modules/tutorial.cmo modules/monitoring.cmo $(DUCEEXAMPLES)
EXAMPLESCMI = modules/tutorial.cmi
PP = -pp "camlp4o ./xmlp4/xhtmlsyntax.cma -loc loc"

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

all: $(BYTE) $(OPT)

byte: $(TARGETSBYTE)

opt: $(TARGETSBYTE:.byte=.opt)

.PHONY: $(REPS) clean


baselib.byte:
	$(MAKE) -C baselib byte

baselib.opt:
	$(MAKE) -C baselib opt

lwt.byte:
	$(MAKE) -C lwt byte

lwt.opt:
	$(MAKE) -C lwt opt

xmlp4.byte:
	touch xmlp4/.depend
	$(MAKE) -C xmlp4 depend
	$(MAKE) -C xmlp4 byte

xmlp4.opt:
	touch xmlp4/.depend
	$(MAKE) -C xmlp4 depend
	$(MAKE) -C xmlp4 opt

http.byte:
	$(MAKE) -C http byte

http.opt:
	$(MAKE) -C http opt

modules.byte:
	$(MAKE) -C modules byte

modules.opt:
	$(MAKE) -C modules opt

server.byte:
	$(MAKE) -C server byte

server.opt:
	$(MAKE) -C server opt

doc:
	$(CAMLDOC) $(PP) -package ssl -I lib -d doc/lwt -html lwt/lwt.mli lwt/lwt_unix.mli
	$(CAMLDOC) $(PP) -package netstring -I lib -I `$(CAMLP4) -where` -d doc/oc -html server/ocsigen.mli server/extensions.mli server/parseconfig.mli xmlp4/ohl-xhtml/xHTML.mli server/ocsigenboxes.mli baselib/messages.ml http/predefined_senders.mli

clean:
	-@for i in $(REPS) ; do touch "$$i"/.depend ; done
	-@for i in $(REPS) ; do $(MAKE) -C $$i clean ; done
	-rm -f lib/* *~
	-rm -f bin/* *~

depend: xmlp4.byte
	touch lwt/depend
	@for i in $(REPS) ; do touch "$$i"/.depend; $(MAKE) -C $$i depend ; done


.PHONY: install fullinstall doc
install:
	mkdir -p $(PREFIX)/$(MODULEINSTALLDIR)
	mkdir -p $(PREFIX)/$(EXAMPLESINSTALLDIR)
	$(MAKE) -C server install
	cat META.in | sed s/_VERSION_/`head -n 1 VERSION`/ > META
	$(OCAMLFIND) install $(OCSIGENNAME) -destdir "$(PREFIX)/$(MODULEINSTALLDIR)" $(TOINSTALL)
	install -m 644 $(EXAMPLES) $(PREFIX)/$(EXAMPLESINSTALLDIR)
	-rm META


fullinstall: install
	mkdir -p $(PREFIX)/$(CONFIGDIR)
	mkdir -p $(PREFIX)/$(STATICPAGESDIR)
	-mv $(PREFIX)/$(CONFIGDIR)/$(OCSIGENNAME).conf $(PREFIX)/$(CONFIGDIR)/$(OCSIGENNAME).conf.old
	cat files/ocsigen.conf \
	| sed s%_LOGDIR_%$(LOGDIR)%g \
	| sed s%_STATICPAGESDIR_%$(STATICPAGESDIR)%g \
	| sed s%_UP_%$(UPLOADDIR)%g \
	| sed s%_OCSIGENUSER_%$(OCSIGENUSER)%g \
	| sed s%_OCSIGENGROUP_%$(OCSIGENGROUP)%g \
	| sed s%_OCSIGENNAME_%$(OCSIGENNAME)%g \
	| sed s%_MODULEINSTALLDIR_%$(MODULEINSTALLDIR)/$(OCSIGENNAME)%g \
	> $(PREFIX)/$(CONFIGDIR)/$(OCSIGENNAME).conf
	cat $(PREFIX)/$(CONFIGDIR)/$(OCSIGENNAME).conf \
	| sed s%[.]cmo%.cmxs%g \
	| sed s%[.]cma%.cmxs%g \
	> $(PREFIX)/$(CONFIGDIR)/$(OCSIGENNAME).conf.opt
	-mv $(PREFIX)/$(CONFIGDIR)/mime.types $(PREFIX)/$(CONFIGDIR)/mime.types.old
	cp -f files/mime.types $(PREFIX)/$(CONFIGDIR)
	mkdir -p $(PREFIX)/$(LOGDIR)
	$(CHOWN) -R $(OCSIGENUSER):$(OCSIGENGROUP) $(PREFIX)/$(LOGDIR)
	$(CHOWN) -R $(OCSIGENUSER):$(OCSIGENGROUP) $(PREFIX)/$(STATICPAGESDIR)
	chmod u+rwx $(PREFIX)/$(LOGDIR)
	chmod a+rx $(PREFIX)/$(CONFIGDIR)
	chmod a+r $(PREFIX)/$(CONFIGDIR)/$(OCSIGENNAME).conf
	chmod a+r $(PREFIX)/$(CONFIGDIR)/mime.types
	mkdir -p $(PREFIX)/$(DOCDIR)
	install -d -m 755 $(PREFIX)/$(DOCDIR)/lwt
	install -d -m 755 $(PREFIX)/$(DOCDIR)/oc
	-install -m 644 doc/* $(PREFIX)/$(DOCDIR)
	install -m 644 doc/lwt/* $(PREFIX)/$(DOCDIR)/lwt
	install -m 644 doc/oc/* $(PREFIX)/$(DOCDIR)/oc
	chmod a+rx $(PREFIX)/$(DOCDIR)
	chmod a+r $(PREFIX)/$(DOCDIR)/*
	[ -d /etc/logrotate.d ] && \
	 { mkdir -p ${PREFIX}/etc/logrotate.d ; \
	   cat files/logrotate.IN \
	   | sed s%LOGDIR%$(LOGDIR)%g \
	   | sed s%USER%$(OCSIGENUSER)%g \
	   | sed s%GROUP%$(OCSIGENGROUP)%g \
	  > $(PREFIX)/etc/logrotate.d/$(OCSIGENNAME); }
	install -d -m 755 $(PREFIX)/$(MANDIR)
	install -m 644 files/ocsigen.1 $(PREFIX)/$(MANDIR)


.PHONY: uninstall fulluninstall
uninstall:
	$(MAKE) -C server uninstall
	$(OCAMLFIND) remove $(OCSIGENNAME) -destdir "$(PREFIX)/$(MODULEINSTALLDIR)"

fulluninstall: uninstall
# dangerous
#	rm -f $(CONFIGDIR)/$(OCSIGENNAME).conf
#	rm -f $(LOGDIR)/$(OCSIGENNAME).log
#	rm -rf $(MODULEINSTALLDIR)

