
doc: server.doc client.doc ocamlbuild.doc
server.doc:
	${MAKE} -C server doc
client.doc:
	${MAKE} -C client doc
ocamlbuild.doc:
	${MAKE} -C ocamlbuild doc

clean:
	${MAKE} -C server clean
	${MAKE} -C client clean
	${MAKE} -C ocamlbuild $@
	-rm -f *~ \#* .\#*

install:
	${MAKE} -C server install
	${MAKE} -C client install
	${MAKE} -C ocamlbuild $@
