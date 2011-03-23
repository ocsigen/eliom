include ../../Makefile.config

## Use local files
## (tests do not require global installation of Eliom)
export OCAMLPATH := ${SRC}/src/files:${OCAMLPATH}

PACKAGE  := eliom.server,lwt.unix,pcre
LIBS := -I .. ${addprefix -package ,${PACKAGE}}

OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG} -thread
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG} -thread
OCAMLDEP := $(OCAMLFIND) ocamldep

ifeq "${NATDYNLINK}" "YES"
all: byte opt
else
all: byte
endif

### Library

FILES := miniwiki.ml

byte:: miniwiki.cmo
opt:: miniwiki.cmxs

miniwiki.cma: ${FILES:.ml=.cmo}
	${OCAMLC} -a -o $@ $^
miniwiki.cmxa: ${FILES:.ml=.cmx}
	${OCAMLOPT} -a -o $@ $^

############

%.cmi: %.mli
	$(OCAMLC) ${LIBS} -c $<
%.cmo: %.ml
	$(OCAMLC) ${LIBS} -c $<
%.cmx: %.ml
	$(OCAMLOPT) ${LIBS} -c $<
%.cmxs: %.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $<

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot

distclean: clean
	-rm -f *~ \#* .\#*

## Dependencies

depend:
	$(OCAMLDEP) ${LIBS} *.ml *.mli > .depend

FORCE:
-include .depend