OCB=ocamlbuild -use-ocamlfind \
               -tag thread \
               -syntax camlp4o \
               -package eliom.server,tyxml.syntax

byte:
	$(OCB) monitoring.cma
opt:
	$(OCB) monitoring.cmxs

clean:
	$(OCB) -clean
distclean: clean
