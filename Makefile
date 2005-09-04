
REPS = lwt xmlp4 http server moduleexample krokoutils

all: $(REPS)

.PHONY: $(REPS) clean


lwt:
	make -C lwt depend all

xmlp4:
	make -C xmlp4 depend all

http :
	make -C http depend all

moduleexample:
	make -C moduleexample all

server:
	make -C server depend all

krokoutils:
	make -C krokoutils all

clean:
	@for i in $(REPS) ; do make -C $$i clean ; done
	-rm -f lib/* *~
	-rm -f bin/* *~
