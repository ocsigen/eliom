include Makefile.config

REPS = lwt xmlp4 http server modules ocsimore
OCAMLFIND = ocamlfind

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
	@if (test '$(OCSIMORE)' = 'YES');\
	then echo "Ocsimore installation";\
	$(MAKE) -C ocsimore install;\
	else echo "Skiping Ocsimore installation";\
	fi

fullinstall: install
	mkdir -p $(CONFIGDIR)
	mkdir -p $(MODULEINSTALLDIR)
	mkdir -p $(STATICPAGESDIR)
	cp files/ocsigen.conf $(CONFIGDIR)
	mkdir -p $(LOGDIR)


.PHONY: uninstall fulluninstall
uninstall:
	$(MAKE) -C server uninstall
	$(MAKE) -C ocsimore uninstall

fulluninstall: uninstall
# dangerous
#	rm -f $(CONFIGDIR)/ocsigen.conf
#	rm -f $(LOGDIR)/ocsigen.log
#	rm -rf $(MODULEINSTALLDIR)



