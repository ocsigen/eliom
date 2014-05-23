OCB=ocamlbuild -use-ocamlfind -tag thread -package eliom.server

byte:
	$(OCB) miniwiki.cma
opt:
	$(OCB) miniwiki.cmxs

clean:
	$(OCB) -clean
distclean: clean
