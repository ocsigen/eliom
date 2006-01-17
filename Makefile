include Makefile.config

REPS = lwt xmlp4 http server modules ocsimore
OCAMLFIND = ocamlfind

all: $(REPS)

.PHONY: $(REPS) clean


lwt:
	make -C lwt depend all

xmlp4:
	make -C xmlp4 depend all

http :
	make -C http depend all

modules:
	make -C modules all

server:
	make -C server depend all

ocsimore:
	@if (test '$(OCSIMORE)' = 'YES');\
	then echo "Compiling Ocsimore";\
	make -C ocsimore depend all;\
	else echo "Skiping Ocsimore compilation";\
	fi

clean:
	@for i in $(REPS) ocsimore ; do make -C $$i clean ; done
	-rm -f lib/* *~
	-rm -f bin/* *~

depend: xmlp4
	@for i in $(REPS) ; do > "$$i"/.depend; make -C $$i depend ; done


.PHONY: install fullinstall
install:
	make -C server install
	@if (test '$(OCSIMORE)' = 'YES');\
	then echo "Ocsimore installation";\
	make -C ocsimore install;\
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
	make -C server uninstall
	make -C ocsimore uninstall

fulluninstall: uninstall
# dangerous
#	rm -f $(CONFIGDIR)/ocsigen.conf
#	rm -f $(LOGDIR)/ocsigen.log
#	rm -rf $(MODULEINSTALLDIR)



