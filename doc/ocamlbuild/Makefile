include ../../Makefile.config
include ../../src/ocamlbuild/Makefile.filelist

OCAMLDOC := ${OCAMLFIND} ocamldoc

ODOC := $(addprefix ../../src/ocamlbuild/_build/,$(DOC))

doc: odoc api-html/index.html
wikidoc: odoc api-wiki/index.wiki

odoc:
	${MAKE} -C ../../src/ocamlbuild odoc

api-html/index.html: indexdoc ${ODOC}
	mkdir -p api-html
	$(OCAMLDOC) ${LIBS} -d api-html -intro indexdoc $(addprefix -load ,${ODOC}) -html

api-wiki/index.wiki: indexdoc ${ODOC}
	mkdir -p api-wiki
	ODOC_WIKI_SUBPROJECT=ocamlbuild $(OCAMLDOC) -d api-wiki -intro indexdoc \
	   -i $(shell ocamlfind query wikidoc) -g odoc_wiki.cma \
	   -colorize-code $(addprefix -load ,${ODOC})

install:
	${INSTALL} -d -m 755 $(TEMPROOT)$(DOCDIR)/ocamlbuild
	$(INSTALL) -m 644 api-html/* $(TEMPROOT)$(DOCDIR)/ocamlbuild

clean:
	-rm -f api-html/*
	-rm -f api-wiki/*
	-rm -f *~ \#* .\#*
