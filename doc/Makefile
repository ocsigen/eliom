
doc: server.doc client.doc
server.doc:
	${MAKE} -C server doc
client.doc:
	${MAKE} -C client doc

clean:
	${MAKE} -C server clean
	${MAKE} -C client clean
	-rm -f *~ \#* .\#*

install:
	${MAKE} -C server install
	${MAKE} -C client install