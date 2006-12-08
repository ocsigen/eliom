include Makefile.config

INSTALL = install
REPS = baselib lwt xmlp4 http server modules ocsimore
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
TOINSTALL = modules/tutorial.cmo modules/tutorial.cmi modules/monitoring.cmo server/ocsigen.cmi server/ocsigenboxes.cmi xmlp4/ohl-xhtml/xHTML.cmi xmlp4/ohl-xhtml/xML.cmi xmlp4/ohl-xhtml/xhtml.cma xmlp4/xhtmltypes.cmi xmlp4/xhtmlsyntax.cma META lwt/lwt.cmi lwt/lwt_unix.cmi server/preemptive.cmi
OCSIMOREINSTALL = ocsimore/ocsimore.cma ocsimore/db_create.cmi ocsimore/ocsipersist.cmi ocsimore/ocsicache.cmi ocsimore/ocsidata.cmi ocsimore/ocsipages.cmi ocsimore/ocsisav.cmi ocsimore/ocsiboxes.cmi ocsimore/ocsexample_util.cmo ocsimore/ocsexample3.cmo ocsimore/ocsexample1.cmo ocsimore/ocsexample2.cmo
EXAMPLES = modules/tutorial.cmo modules/tutorial.cmi modules/monitoring.cmo
OCSIMOREEXAMPLES = ocsimore/ocsexample_util.cmo ocsimore/ocsexample3.cmo ocsimore/ocsexample1.cmo ocsimore/ocsexample2.cmo
PP = -pp "camlp4o ./lib/xhtmlsyntax.cma -loc loc"

all: $(REPS)

.PHONY: $(REPS) clean


baselib:
#	$(MAKE) -C baselib depend
	$(MAKE) -C baselib all

lwt:
#	$(MAKE) -C lwt depend
	$(MAKE) -C lwt all

xmlp4:
	touch xmlp4/.depend
	$(MAKE) -C xmlp4 depend
	$(MAKE) -C xmlp4 all

http :
#	$(MAKE) -C http depend
	$(MAKE) -C http all

modules:
	$(MAKE) -C modules all

server:
#	$(MAKE) -C server depend
	$(MAKE) -C server all

ocsimore:
	@if (test '$(OCSIMORE)' = 'YES');\
	then echo "Compiling Ocsimore";\
	$(MAKE) -C ocsimore all;\
	else echo "Skipping Ocsimore compilation";\
	fi

doc:
	$(CAMLDOC) $(PP) -package ssl -I lib -d doc/lwt -html lwt/lwt.mli lwt/lwt_unix.mli
	$(CAMLDOC) $(PP) -I lib -d doc/oc -html server/ocsigen.mli xmlp4/ohl-xhtml/xHTML.mli server/ocsigenboxes.mli baselib/messages.ml
clean:
	@for i in $(REPS) ; do touch "$$i"/.depend ; done
	@for i in $(REPS) ; do $(MAKE) -C $$i clean ; rm -f "$$i"/.depend ; done
	-rm -f lib/* *~
	-rm -f bin/* *~

depend: xmlp4
	> lwt/depend
	@for i in $(REPS) ; do > "$$i"/.depend; $(MAKE) -C $$i depend ; done


.PHONY: install fullinstall doc
install:
	mkdir -p $(EXAMPLESINSTALLDIR)
	$(MAKE) -C server install
	cat META.in | sed s/_VERSION_/`head -n 1 VERSION`/ > META
	@if (test '$(OCSIMORE)' = 'YES') ;\
	then echo "Ocsimore installation";\
	$(OCAMLFIND) install $(OCSIGENNAME) -destdir "$(MODULEINSTALLDIR)" $(TOINSTALL) $(OCSIMOREINSTALL);\
	cp -f $(EXAMPLES) $(OCSIMOREEXAMPLES) $(EXAMPLESINSTALLDIR);\
	else $(OCAMLFIND) install $(OCSIGENNAME) -destdir "$(MODULEINSTALLDIR)" $(TOINSTALL);\
	cp $(EXAMPLES) $(EXAMPLESINSTALLDIR);\
	echo "Skipping Ocsimore installation";\
	fi
	-rm META


fullinstall: doc install
	mkdir -p $(CONFIGDIR)
	mkdir -p $(STATICPAGESDIR)
	-mv $(CONFIGDIR)/ocsigen.conf $(CONFIGDIR)/ocsigen.conf.old
	cat files/ocsigen.conf \
	| sed s%_LOGDIR_%$(LOGDIR)%g \
	| sed s%_STATICPAGESDIR_%$(STATICPAGESDIR)%g \
	| sed s%_UP_%$(UPLOADDIR)%g \
	| sed s%_OCSIGENUSER_%$(OCSIGENUSER)%g \
	| sed s%_OCSIGENGROUP_%$(OCSIGENGROUP)%g \
	| sed s%_MODULEINSTALLDIR_%$(MODULEINSTALLDIR)/$(OCSIGENNAME)%g \
	> $(CONFIGDIR)/ocsigen.conf
	-mv $(CONFIGDIR)/mime.types $(CONFIGDIR)/mime.types.old
	cp -f files/mime.types $(CONFIGDIR)
	mkdir -p $(LOGDIR)
	chown -R $(OCSIGENUSER):$(OCSIGENGROUP) $(LOGDIR)
	chown -R $(OCSIGENUSER):$(OCSIGENGROUP) $(STATICPAGESDIR)
	chmod u+rwx $(LOGDIR)
	chmod a+rx $(CONFIGDIR)
	chmod a+r $(CONFIGDIR)/ocsigen.conf
	chmod a+r $(CONFIGDIR)/mime.types
	mkdir -p $(DOCDIR)
	install -d -m 755 $(DOCDIR)/lwt
	install -d -m 755 $(DOCDIR)/oc
	-install -m 644 doc/* $(DOCDIR)
	install -m 644 doc/lwt/* $(DOCDIR)/lwt
	install -m 644 doc/oc/* $(DOCDIR)/oc
	chmod a+rx $(DOCDIR)
	chmod a+r $(DOCDIR)/*


.PHONY: uninstall fulluninstall
uninstall:
	$(MAKE) -C server uninstall
	$(OCAMLFIND) remove $(OCSIGENNAME) -destdir "$(MODULEINSTALLDIR)"

fulluninstall: uninstall
# dangerous
#	rm -f $(CONFIGDIR)/ocsigen.conf
#	rm -f $(LOGDIR)/ocsigen.log
#	rm -rf $(MODULEINSTALLDIR)



