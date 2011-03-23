include ../Makefile.config

## Use local files
## (tests do not require global installation of Eliom)
export OCAMLPATH := ${SRC}/src/files:${OCAMLPATH}

OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}
OCAMLDEP := $(OCAMLFIND) ocamldep

ifeq "${NATDYNLINK}" "YES"
all: byte opt
else
all: byte
endif

#### Main site : eliom_testsuite ####

SERVER_PACKAGE  :=

CLIENT_PACKAGE  :=

SERVER_LIBS := ${addprefix -package ,${SERVER_PACKAGE}}
CLIENT_LIBS := ${addprefix -package ,${CLIENT_PACKAGE}}

SERVER_FILES := eliom_testsuite1.ml    \
                eliom_testsuite2.ml    \
		eliom_testsuite3.eliom \
		eliom_testsuite.ml     \

CLIENT_FILES := eliom_testsuite3.eliom \

SERVER_ML_FILES := $(filter %.ml,${SERVER_FILES})
ELIOM_FILES     := $(filter %.eliom,${SERVER_FILES})
CLIENT_ML_FILES := $(filter %.ml,${CLIENT_FILES})

SERVER_OBJS := $(patsubst %.eliom,%.cmo,${SERVER_FILES:.ml=.cmo})
CLIENT_OBJS := $(patsubst %.eliom,syntax_temp_files/%.cmo,${CLIENT_FILES:.ml=.cmo})

STATICDIR := ../local/var/www/tests

byte:: eliom_testsuite.cma ${STATICDIR}/eliom_testsuite.js
opt:: eliom_testsuite.cmxs ${STATICDIR}/eliom_testsuite.js

#### Server side #######

eliom_testsuite.cma: ${SERVER_OBJS}
	${OCAMLC} -a -o $@ $^
eliom_testsuite.cmxa: ${SERVER_OBJS:.cmo=.cmx}
	${OCAMLOPT} -a -o $@ $^

${addprefix syntax_temp_files/,${ELIOM_FILES:.eliom=.type.mli}}: \
syntax_temp_files/%.type.mli: %.eliom
	$(OCAMLC) -thread ${SERVER_LIBS} -package eliom.server -package eliom.syntax.type -syntax camlp4o -i -ppopt -impl -impl $< > $@ \
	  || (rm -f $@; exit 1)

${ELIOM_FILES:.eliom=.cmo}: \
%.cmo: %.eliom syntax_temp_files/%.type.mli
	$(OCAMLC) -thread ${SERVER_LIBS} -package eliom.server -package eliom.syntax.server -syntax camlp4o \
	  -ppopt -type -ppopt ${<:%.eliom=syntax_temp_files/%.type.mli} -ppopt -impl \
	  -c -impl $< -o $@

${ELIOM_FILES:.eliom=.cmx}: \
%.cmx: %.eliom syntax_temp_files/%.type.mli
	$(OCAMLOPT) -thread ${SERVER_LIBS} -package eliom.server -package eliom.syntax.server -syntax camlp4o \
	  -ppopt -type -ppopt ${<:%.eliom=syntax_temp_files/%.type.mli} -ppopt -impl \
	  -c -impl $< -o $@

##### Client side ####

${addprefix syntax_temp_files/,${ELIOM_FILES:.eliom=.cmo}}: \
syntax_temp_files/%.cmo: %.eliom syntax_temp_files/%.type.mli
	$(OCAMLC) ${CLIENT_LIBS} -package eliom.client -package eliom.syntax.client -syntax camlp4o \
	  -ppopt -type -ppopt ${<:%.eliom=syntax_temp_files/%.type.mli} -ppopt -impl \
	  -c -impl $< -o $@

syntax_temp_files/eliom_testsuite: ${CLIENT_OBJS}
	${OCAMLC} -o $@ ${CLIENT_LIBS} -package eliom.client -linkpkg $^

eliom_testsuite.js: ../src/client/eliom_client.js syntax_temp_files/eliom_testsuite
	${JS_OF_OCAML} -o $@ -pretty $^

${STATICDIR}/eliom_testsuite.js: eliom_testsuite.js
	cp -f $< $@

####### Aux site: Ocamlduce #######

ifeq "${OCAMLDUCE}" "YES"
byte:: ocamlduce.byte
opt:: ocamlduce.opt
endif

ocamlduce.byte: eliom_testsuite1.cmo
	$(MAKE) -C ocamlduce byte

ocamlduce.opt: eliom_testsuite1.cmx
	$(MAKE) -C ocamlduce opt

####### Aux site: miniwiki #######

byte:: miniwiki.byte
opt:: miniwiki.opt

miniwiki.byte:
	$(MAKE) -C miniwiki byte

miniwiki.opt:
	$(MAKE) -C miniwiki opt

############

${SERVER_ML_FILES:.ml=.cmo}:%.cmo: %.ml
	$(OCAMLC) -thread -syntax camlp4o -package eliom.server -package eliom.syntax ${SERVER_LIBS} -c $<
${SERVER_ML_FILES:.ml=.cmx}: %.cmx: %.ml
	$(OCAMLOPT) -thread -syntax camlp4o -package eliom.server -package eliom.syntax ${SERVER_LIBS} -c $<

${CLIENT_ML_FILES:.ml=.cmo}: %.cmo: %.ml
	$(OCAMLC) -thread -syntax camlp4o -package eliom.client -package eliom.syntax ${CLIENT_LIBS} -c $<
${CLIENT_ML_FILES:.ml=.cmx}: %.cmx: %.ml
	$(OCAMLOPT) -thread -syntax camlp4o -package eliom.client -package eliom.syntax ${CLIENT_LIBS} -c $<

%.cmxs: %.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $<

## Clean up

clean: clean.local
	${MAKE} -C ocamlduce clean
	${MAKE} -C miniwiki clean
clean.local:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f eliom_testsuite.js syntax_temp_files/*

distclean: clean.local
	-rm -f *~ \#* .\#*
	${MAKE} -C ocamlduce distclean
	${MAKE} -C miniwiki distclean

## Dependencies

depend:
ifeq "${OCAMLDUCE}" "YES"
	${MAKE} -C ocamlduce depend
endif
	${MAKE} -C miniwiki depend
	$(OCAMLDEP) -syntax camlp4o -package eliom.server -package eliom.syntax ${SERVER_LIBS} ${SERVER_FILES} > .depend
	$(OCAMLDEP) -syntax camlp4o -package eliom.client -package eliom.syntax ${CLIENT_LIBS} ${CLIENT_FILES} >> .depend


syntax_temp_files/eliom_testsuite3.type.mli: eliom_testsuite1.cmo
syntax_temp_files/eliom_testsuite3.cmo: eliom_testsuite1.cmo
eliom_testsuite3.cmo: eliom_testsuite1.cmo
eliom_testsuite.cmo: eliom_testsuite3.cmo
eliom_testsuite3.cmx: eliom_testsuite1.cmx
eliom_testsuite.cmx: eliom_testsuite3.cmx

FORCE:
-include .depend

# %.wiki: %.ml
	# cat $< | sed '1,/(\*wiki\*/d' | sed '/%<||2>%/,$$ d' | /bin/sh ./tutomake.sh > $@

