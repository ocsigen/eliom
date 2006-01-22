include Makefile.config

INSTALL = install
REPS = lwt xmlp4 http server modules ocsimore
OCAMLFIND = ocamlfind
TOINSTALL = modules/tutorial.cmo modules/tutorial.cmi modules/ocsiprof.cmo server/ocsigen.cmi server/ocsigenboxes.cmi xmlp4/ohl-xhtml/xHTML.cmi xmlp4/ohl-xhtml/xML.cmi xmlp4/ohl-xhtml/xhtml.cma xmlp4/xhtmltypes.cmi xmlp4/xhtmlsyntax.cma META
OCSIMOREINSTALL = ocsimore/ocsimore.cma ocsimore/db_create.cmi ocsimore/ocsipersist.cmi ocsimore/ocsicache.cmi ocsimore/ocsidata.cmi ocsimore/ocsipages.cmi ocsimore/ocsisav.cmi ocsimore/ocsiboxes.cmi ocsimore/ocsexample_util.cmo ocsimore/ocsexample3.cmo ocsimore/ocsexample1.cmo ocsimore/ocsexample2.cmo

all: $(REPS)

.PHONY: $(REPS) clean


lwt:
	$(MAKE) -C lwt depend all

xmlp4:
	$(MAKE) -C xmlp4 depend all

http :
	$(MAKE) -C http depend all

modules:
	$(MAKE) -C modules all

server:
	$(MAKE) -C server depend all

ocsimore:
	@if (test '$(OCSIMORE)' = 'YES');\
	then echo "Compiling Ocsimore";\
	$(MAKE) -C ocsimore depend all;\
	else echo "Skiping Ocsimore compilation";\
	fi

clean:
	@for i in $(REPS) ocsimore ; do $(MAKE) -C $$i clean ; done
	-rm -f lib/* *~
	-rm -f bin/* *~

depend: xmlp4
	@for i in $(REPS) ; do > "$$i"/.depend; $(MAKE) -C $$i depend ; done


.PHONY: install fullinstall
install:
	$(MAKE) -C server install
	@if (test '$(OCSIMORE)' = 'YES') ;\
	then echo "Ocsimore installation";\
	$(OCAMLFIND) install ocsigen -destdir "$(MODULEINSTALLDIR)" $(TOINSTALL) $(OCSIMOREINSTALL);\
	else $(OCAMLFIND) install ocsigen -destdir "$(MODULEINSTALLDIR)" $(TOINSTALL);\
	echo "Skiping Ocsimore installation";\
	fi

fullinstall: install
	mkdir -p $(CONFIGDIR)
	mkdir -p $(STATICPAGESDIR)
	cat files/ocsigen.conf | sed s°_LOGDIR_°$(LOGDIR)°g \
	| sed s°_STATICPAGESDIR_°$(STATICPAGESDIR)°g \
	| sed s°_OCSIGENUSER_°$(OCSIGENUSER)°g \
	| sed s°_OCSIGENGROUP_°$(OCSIGENGROUP)°g \
	| sed s°_MODULEINSTALLDIR_°$(MODULEINSTALLDIR)/ocsigen°g \
	> $(CONFIGDIR)/ocsigen.conf
	mkdir -p $(LOGDIR)
	chown -R $(OCSIGENUSER):$(OCSIGENGROUP) $(LOGDIR)
	chown -R $(OCSIGENUSER):$(OCSIGENGROUP) $(STATICPAGESDIR)
	chmod u+rwx $(LOGDIR)
	chmod a+rx $(CONFIGDIR)
	chmod a+r $(CONFIGDIR)/ocsigen.conf


.PHONY: uninstall fulluninstall
uninstall:
	$(MAKE) -C server uninstall
	$(OCAMLFIND) remove ocsigen -destdir "$(MODULEINSTALLDIR)"

fulluninstall: uninstall
# dangerous
#	rm -f $(CONFIGDIR)/ocsigen.conf
#	rm -f $(LOGDIR)/ocsigen.log
#	rm -rf $(MODULEINSTALLDIR)



