include ../../Makefile.config
include ../../src/client/Makefile.filelist

PACKAGE :=  ${CLIENT_PACKAGE}
OCAMLDOC := ${OCAMLFIND} ocamldoc

LIBS    := -I ../../src/client/ $(addprefix -package ,${PACKAGE})

TMP := __tmp
DOC := $(addprefix ${TMP}/,$(DOC))

doc: odoc

$(filter-out $(addprefix ${TMP}/,${NOP4}), $(DOC)): \
${TMP}/%.mli: ../../src/client/%.mli
	camlp4o ../../src/syntax/pa_include.cmo -printer o $< > $@

$(addprefix ${TMP}/,${NOP4}): \
${TMP}/%.mli: ../../src/client/%.mli
	cp $< $@

${TMP}:
	mkdir -p $@

odoc: ${TMP} ${DOC}
	mkdir -p api-html
	$(OCAMLDOC) ${LIBS} -d api-html -intro indexdoc -html ${DOC}
	rm -r ${TMP}

install:
	${INSTALL} -d -m 755 $(TEMPROOT)$(DOCDIR)/client
	$(INSTALL) -m 644 api-html/* $(TEMPROOT)$(DOCDIR)/client

clean:
	-rm -f api-html/*
	-rm -f *~ \#* .\#*
	-rm -rf ${TMP}
