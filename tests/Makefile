include ../Makefile.config

## Use local files
## (tests do not require global installation of Eliom)
export OCAMLPATH := ${SRC}/src/files:${OCAMLPATH}
export PATH := ${SRC}/src/tools:${PATH}

ELIOMC   := eliomc -g
ELIOMOPT := eliomopt
ELIOMDEP := eliomdep
JS_OF_ELIOM := js_of_eliom

ifeq "${NATDYNLINK}" "YES"
all: byte opt
else
all: byte
endif

#### Main site : eliom_testsuite ####

SERVER_PACKAGE :=
CLIENT_PACKAGE :=

SERVER_FILES := eliom_testsuite_base.eliom \
                eliom_testsuite1.ml \
                eliom_testsuite2.ml \
                eliom_testsuite3.eliom \
                eliom_testsuite4.eliom \
		eliom_testsuite.ml     \

CLIENT_FILES := eliom_testsuite_base.eliom \
                eliom_testsuite3.eliom \
                eliom_testsuite4.eliom

STATICDIR := ../local/var/www/tests
TYPE_DIR =

byte:: _server/eliom_testsuite_global.cmo _server/eliom_testsuite_site.cmo eliom_testsuite.cma ${STATICDIR}/eliom_testsuite.js
opt:: _server/eliom_testsuite_global.cmx _server/eliom_testsuite_site.cmx eliom_testsuite.cmxs ${STATICDIR}/eliom_testsuite.js

#### Server side #######

SERVER_INC  := ${addprefix -package ,${SERVER_PACKAGE}}

SERVER_OBJS := $(patsubst %.eliom,_server/%.cmo, \
	          $(patsubst %.ml,_server/%.cmo,${SERVER_FILES}))

eliom_testsuite.cma: ${SERVER_OBJS}
	${ELIOMC} -a -o $@ $^
eliom_testsuite.cmxa: ${SERVER_OBJS:.cmo=.cmx}
	${ELIOMOPT} -a -o $@ $^

${or ${TYPE_DIR},_server}/%.type_mli: %.eliom
	${ELIOMC} -infer ${SERVER_INC} -o $@ $<

_server/%.cmi: %.mli
	${ELIOMC} -c ${SERVER_INC} $<

_server/%.cmo: %.ml
	${ELIOMC} -c ${SERVER_INC} $<
_server/%.cmo: %.eliom
	${ELIOMC} -c -noinfer ${addprefix -type-dir ,${TYPE_DIR}} ${SERVER_INC} $<

_server/%.cmx: %.ml
	${ELIOMOPT} -c ${SERVER_INC} $<
_server/%.cmx: %.eliom
	${ELIOMOPT} -c -noinfer ${addprefix -type-dir ,${TYPE_DIR}} ${SERVER_INC} $<

%.cmxs: %.cmxa
	$(ELIOMOPT) -shared -linkall -o $@ $<

##### Client side ####

CLIENT_LIBS := ${addprefix -package ,${CLIENT_PACKAGE}}
CLIENT_INC  := ${addprefix -package ,${CLIENT_PACKAGE}}

CLIENT_OBJS := $(patsubst %.eliom,_client/%.cmo, \
	          $(patsubst %.ml,_client/%.cmo,${CLIENT_FILES}))

${STATICDIR}/eliom_testsuite.js:  ${CLIENT_OBJS}
	${JS_OF_ELIOM} -o $@ -jsopt -pretty -jsopt -noinline ${CLIENT_LIBS} $^

_client/%.cmi: %.mli
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<

_client/%.cmo: %.eliom
	${JS_OF_ELIOM} -c -noinfer ${addprefix -type-dir ,${TYPE_DIR}} ${CLIENT_INC} $<
_client/%.cmo: %.ml
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<

####### Aux site: miniwiki #######

byte:: miniwiki.byte
opt:: miniwiki.opt

miniwiki.byte:
	$(MAKE) --no-print-directory -C miniwiki byte

miniwiki.opt:
	$(MAKE) --no-print-directory -C miniwiki opt

############

## Clean up

clean: clean.local
	${MAKE} -C clean
	${MAKE} -C miniwiki clean
clean.local:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f ${or ${TYPE_DIR},_server}/*.type_mli
	-rm -f ${STATICDIR}/eliom_testsuite.js
	-rm -rf _client/* _server/*

distclean: clean.local
	-rm -f *~ \#* .\#*
	${MAKE} -C distclean
	${MAKE} -C miniwiki distclean

## Dependencies

depend:
	${MAKE} -C miniwiki depend
	$(ELIOMDEP) -server ${SERVER_INC} ${addprefix -type-dir ,${TYPE_DIR}} ${SERVER_FILES} > .depend
	$(ELIOMDEP) -client ${CLIENT_INC} ${addprefix -type-dir ,${TYPE_DIR}} ${CLIENT_FILES} >> .depend

include .depend

## Dependencies towards *.eliom are not handled by eliomdep.
_server/eliom_testsuite4.cmo: _server/eliom_testsuite_base.cmo
_server/eliom_testsuite4.cmx: _server/eliom_testsuite_base.cmx
_server/eliom_testsuite.cmo: _server/eliom_testsuite3.cmo _server/eliom_testsuite4.cmo
_server/eliom_testsuite.cmx: _server/eliom_testsuite3.cmx _server/eliom_testsuite4.cmx
_client/eliom_testsuite.cmo: _client/eliom_testsuite3.cmx _client/eliom_testsuite4.cmo
_client/eliom_testsuite.cmx: _client/eliom_testsuite3.cmx _client/eliom_testsuite4.cmx

## TODO

# %.wiki: %.ml
# cat $< | sed '1,/(\*wiki\*/d' | sed '/%<||2>%/,$$ d' | /bin/sh ./tutomake.sh > $@
