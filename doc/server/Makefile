include ../../Makefile.config
include ../../src/server/Makefile.filelist

PACKAGE :=  ${SERVER_PACKAGE}

ifeq "${OCAMLDUCE}" "NO"
OCAMLDOC := ${OCAMLFIND} ocamldoc
else
OCAMLDOC := ${OCAMLDUCEFIND} ocamldoc -package ocamlduce
endif

TMP := __tmp
DOC := $(addprefix ${TMP}/,$(DOC))

doc: odoc

$(filter-out $(addprefix ${TMP}/,${NOP4}), $(DOC)): \
${TMP}/%.mli: ../../src/server/%.mli
	camlp4o ../../src/syntax/pa_include.cmo -printer o $< > $@

$(addprefix ${TMP}/,${NOP4}): \
${TMP}/%.mli: ../../src/server/%.mli
	cp $< $@

${TMP}/extensions:
	mkdir -p $@/extensions


LIBS    := -I ../../src/server/ -I ../../src/server/extensions $(addprefix -package ,${PACKAGE})

odoc: ${TMP}/extensions ${DOC}
	mkdir -p api-html
	$(OCAMLDOC) ${LIBS} -d api-html -intro indexdoc -html ${DOC}
	rm -r ${TMP}

install:
	${INSTALL} -d -m 644 $(TEMPROOT)$(DOCDIR)/server
	$(INSTALL) -m 644 api-html/* $(TEMPROOT)$(DOCDIR)/server

clean:
	-rm -f api-html/*
	-rm -f *~ \#* .\#*
	-rm -rf ${TMP}
