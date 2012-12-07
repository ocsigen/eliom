include ../../Makefile.config

LIBS     := -package str,unix,findlib
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte

### Common files ###

COMPILER := eliomc eliomcp eliomopt js_of_eliom eliomdep
DESTILLERY := eliom-destillery

FILES := ../../src/files
DESTILLERY_BASIC :=destillery/basic

byte: ${COMPILER} $(DESTILLERY)

config.ml:
	echo "let datadir = \"$(DATADIR)\"" > $@
	echo "let destillery_basic = \"$(DESTILLERY_BASIC)\"" >> $@

$(DESTILLERY): utils.cmo config.cmo destillery.cmo
	$(OCAMLC) -o $@ -linkpkg $(LIBS) $^

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
	$(INSTALL) -m 755 -d $(TEMPROOT)$(BINDIR)
	${INSTALL} -m 755 ${COMPILER} $(DESTILLERY) $(TEMPROOT)${BINDIR}
	$(INSTALL) -m 755 -d $(TEMPROOT)$(DATADIR)/$(DESTILLERY_BASIC)
	$(INSTALL) -m 644 $(FILES)/$(DESTILLERY_BASIC)/* $(TEMPROOT)$(DATADIR)/$(DESTILLERY_BASIC)
uninstall:
	rm -f $(addprefix ${BINDIR}/,${COMPILER} ${DESTILLERY})
	rm -rf $(DATADIR)

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot config.ml
	-rm -f ${COMPILER}
	-rm -f ${PREDEP}
distclean: clean
	-rm -f *~ \#* .\#*

## Dependencies

depend: ${PREDEP}
	$(OCAMLDEP) ${LIBS} *.ml *.mli > .depend

FORCE:
-include .depend
