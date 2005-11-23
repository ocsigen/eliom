
REPS = lwt xmlp4 http server modules
OCAMLFIND = ocamlfind

all: $(REPS)

.PHONY: $(REPS) ocsimore clean


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
	make -C ocsimore depend all

clean:
	@for i in $(REPS) ocsimore ; do make -C $$i clean ; done
	-rm -f lib/* *~
	-rm -f bin/* *~

depend: xmlp4
	@for i in $(REPS) ; do > "$$i"/.depend; make -C $$i depend ; done


.PHONY: install
install:
	make -C server install
	make -C ocsimore install

.PHONY: uninstall
uninstall:
	make -C server uninstall
	make -C ocsimore uninstall



