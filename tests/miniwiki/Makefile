include ../../Makefile.config

## Use local files
## (tests do not require global installation of Eliom)
export OCAMLPATH := ${SRC}/src/files:${OCAMLPATH}
export PATH := ${SRC}/src/tools:${PATH}

LIBS := -I ..

ELIOMC   := eliomc${BYTEDBG}
ELIOMOPT := eliomopt ${OPTDBG}
ELIOMDEP := eliomdep

ifeq "${NATDYNLINK}" "YES"
all: byte opt
else
all: byte
endif

### Library

FILES := miniwiki.ml

byte:: miniwiki.cma
opt:: miniwiki.cmxs

miniwiki.cma: ${FILES:.ml=.cmo}
	${ELIOMC} -a -o $@ $^
miniwiki.cmxa: ${FILES:.ml=.cmx}
	${ELIOMOPT} -a -o $@ $^

############

%.cmi: %.mli
	$(ELIOMC) ${LIBS} -c $<
%.cmo: %.ml
	$(ELIOMC) ${LIBS} -c $<
%.cmx: %.ml
	$(ELIOMOPT) ${LIBS} -c $<
%.cmxs: %.cmxa
	$(ELIOMOPT) -shared -linkall -o $@ $<

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f _server/*

distclean: clean
	-rm -f *~ \#* .\#* .depend

## Dependencies

depend:
	$(ELIOMDEP) -server ${LIBS} *.ml *.mli > .depend

FORCE:
-include .depend
