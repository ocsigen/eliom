include ../../Makefile.config

LIBS     := -package str,unix,findlib
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}
OCAMLDEP := $(OCAMLFIND) ocamldep
OCAMLLEX := ocamllex

all: byte

### Common files ###

COMPILER := eliomc eliomcp eliomopt js_of_eliom eliomdep eliomdoc eliompp
DISTILLERY := eliom-distillery

FILES := ../../src/files
DISTILLERY_TEMPLATES :=distillery
DISTILLERY_BASIC :=basic

IOPTCOMP := $(shell ocamlfind query optcomp)

PREDEP := eliompp_lexer.ml

byte: ${COMPILER} $(DISTILLERY)

config.ml: ../../Makefile.config
	echo "let datadir = \"$(DATADIR)\"" > $@
	echo "let templatedir = \"$(DISTILLERY_TEMPLATES)\"" >> $@
	echo "let distillery_basic = \"$(DISTILLERY_BASIC)\"" >> $@

$(DISTILLERY): utils.cmo config.cmo distillery.cmo
	$(OCAMLC) -o $@ -linkpkg $(LIBS) $^

eliomc: utils.cmo eliomc.cmo
	${OCAMLC} -o $@ -linkpkg ${LIBS} $^

eliomcp eliomopt js_of_eliom: eliomc
	ln -f $< $@

eliomdep: utils.cmo eliomdep.cmo
	${OCAMLC} -o $@ -linkpkg ${LIBS} $^

eliomdoc: utils.cmo eliomdoc.cmo
	${OCAMLC} -o $@ -linkpkg ${LIBS} $^

eliompp: eliompp_lexer.cmo eliompp.cmo
	${OCAMLC} -o $@ -linkpkg ${LIBS} $^

##########

%.ml: %.mll
	$(OCAMLLEX) $<
%.cmi: %.mli
	$(OCAMLC) ${LIBS} -c $<
%.cmo: %.ml
	$(OCAMLC) ${LIBS} -c $<
%.cmx: %.ml
	$(OCAMLOPT) ${LIBS} -c $<

eliomdoc.cmo: eliomdoc.ml
	$(OCAMLC) -pp "camlp4o $(IOPTCOMP)/optcomp.cma" ${LIBS} -c $<

## INSTALL ##

install:
	$(INSTALL) -m 755 -d $(TEMPROOT)$(BINDIR)
	${INSTALL} -m 755 ${COMPILER} $(DISTILLERY) $(TEMPROOT)${BINDIR}
	$(INSTALL) -m 755 -d $(TEMPROOT)$(DATADIR)/$(DISTILLERY_TEMPLATES)/$(DISTILLERY_BASIC)
	$(INSTALL) -m 644 $(FILES)/$(DISTILLERY_TEMPLATES)/$(DISTILLERY_BASIC)/* $(TEMPROOT)$(DATADIR)/$(DISTILLERY_TEMPLATES)/$(DISTILLERY_BASIC)
uninstall:
	rm -f $(addprefix ${BINDIR}/,${COMPILER} ${DISTILLERY})
	rm -rf $(DATADIR)

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot config.ml eliompp_lexer.ml
	-rm -f ${COMPILER} ${DISTILLERY}
	-rm -f ${PREDEP}
distclean: clean
	-rm -f *~ \#* .\#* .depend

## Dependencies

depend: .depend

.depend: ${PREDEP}
	$(OCAMLDEP) -pp "camlp4o $(IOPTCOMP)/optcomp.cma" ${LIBS} *.ml *.mli > .depend

include .depend
