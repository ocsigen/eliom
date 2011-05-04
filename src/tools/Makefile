include ../../Makefile.config

LIBS     := -package unix,findlib
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte

### Common files ###

COMPILER := eliomc eliomcp eliomopt js_of_eliom eliomdep

byte: ${COMPILER}

eliomc: utils.cmo eliomc.cmo
	${OCAMLC} -o $@ -linkpkg ${LIBS} $^

eliomcp eliomopt js_of_eliom: eliomc
	ln -f $< $@

eliomdep: utils.cmo eliomdep.cmo
	${OCAMLC} -o $@ -linkpkg ${LIBS} $^

##########

%.cmi: %.mli
	$(OCAMLC) ${LIBS} -c $<
%.cmo: %.ml
	$(OCAMLC) ${LIBS} -c $<
%.cmx: %.ml
	$(OCAMLOPT) ${LIBS} -c $<

## INSTALL ##

install:
	${INSTALL} -m 755 ${COMPILER} ${BINDIR}
uninstall:
	rm -f $(addprefix ${BINDIR}/,${COMPILER})

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f ${COMPILER}
	-rm -f ${PREDEP}
distclean: clean
	-rm -f *~ \#* .\#*

## Dependencies

depend: ${PREDEP}
	$(OCAMLDEP) ${LIBS} *.ml *.mli > .depend

FORCE:
-include .depend
