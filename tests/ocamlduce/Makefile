include ../../Makefile.config

## Use local files
## (tests do not require global installation of Eliom)
export OCAMLPATH := ${SRC}/src/files:${OCAMLPATH}

PACKAGE  := eliom.server
LIBS := -I .. ${addprefix -package ,${PACKAGE}}

OCAMLDUCEC   := ${OCAMLDUCEFIND} ocamlc${BYTEDBG} -thread
OCAMLDUCEOPT := ${OCAMLDUCEFIND} ocamlopt ${OPTDBG} -thread
OCAMLDUCEDEP := ${OCAMLDUCEFIND} ocamldep

ifeq "${NATDYNLINK}" "YES"
all: byte opt
else
all: byte
endif

byte: exampleduce.cma
opt: exampleduce.cmxa

#### Library

FILES := exampleduce.ml

exampleduce.cma: ${FILES:.ml=.cmo}
	${OCAMLDUCEC} -linkall -a -o $@ $^
exampleduce.cmxa: ${FILES:.ml=.cmx}
	${OCAMLDUCEOPT} -linkall -a -o $@ $^

#####

%.cmi: %.mli
	$(OCAMLDUCEC) ${LIBS} -c $<
%.cmo: %.ml
	$(OCAMLDUCEC) ${LIBS} -c $<
%.cmx: %.ml
	$(OCAMLDUCEOPT) ${LIBS} -c $<
%.cmxs: %.cmxa
	$(OCAMLDUCEOPT) -shared -linkall -o $@ $<

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
distclean: clean
	-rm -f *~ \#* .\#*

## Dependencies

depend:
	${OCAMLDUCEDEP} ${LIBS} *.ml *.mli > .depend

FORCE:
-include .depend
