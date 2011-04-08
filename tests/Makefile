include ../Makefile.config

## Use local files
## (tests do not require global installation of Eliom)
export OCAMLPATH := ${SRC}/src/files:${OCAMLPATH}
export PATH := ${SRC}/src/tools:${PATH}

ELIOMC   := eliomc${BYTEDBG}
ELIOMOPT := eliomopt ${OPTDBG}
ELIOMDEP := eliomdep
JS_OF_ELIOM := js_of_eliom

SERVER_BUILD_DIR := _server
CLIENT_BUILD_DIR := _client

ifeq "${NATDYNLINK}" "YES"
all: byte opt
else
all: byte
endif

#### Main site : eliom_testsuite ####

SERVER_PACKAGE :=
CLIENT_PACKAGE :=

SERVER_INC  := ${addprefix -package ,${SERVER_PACKAGE}}
CLIENT_LIBS := ${addprefix -package ,${CLIENT_PACKAGE}}
CLIENT_INC  := ${addprefix -package ,${CLIENT_PACKAGE}}

SERVER_FILES := eliom_testsuite1.ml    \
                eliom_testsuite2.ml    \
		eliom_testsuite3.eliom \
		eliom_testsuite.ml     \

CLIENT_FILES := eliom_testsuite3.eliom \

SERVER_OBJS := $(patsubst %.eliom,${SERVER_BUILD_DIR}/%.cmo, \
	          $(patsubst %.ml,${SERVER_BUILD_DIR}/%.cmo,${SERVER_FILES}))
CLIENT_OBJS := $(patsubst %.eliom,${CLIENT_BUILD_DIR}/%.cmo, \
	          $(patsubst %.ml,${CLIENT_BUILD_DIR}/%.cmo,${CLIENT_FILES}))

STATICDIR := ../local/var/www/tests

byte:: eliom_testsuite.cma ${STATICDIR}/eliom_testsuite.js
opt:: eliom_testsuite.cmxs ${STATICDIR}/eliom_testsuite.js

#### Server side #######

eliom_testsuite.cma: ${SERVER_OBJS}
	${ELIOMC} -a -o $@ $^
eliom_testsuite.cmxa: ${SERVER_OBJS:.cmo=.cmx}
	${ELIOMOPT} -a -o $@ $^

%.type_mli: %.eliom
	${ELIOMC} -infer ${SERVER_INC} -o $@ $<

${SERVER_BUILD_DIR}/%.cmi: %.mli
	${ELIOMC} -c -noinfer -dir ${SERVER_BUILD_DIR} ${SERVER_INC} $<

${SERVER_BUILD_DIR}/%.cmo: %.eliom
	${ELIOMC} -c -noinfer -dir ${SERVER_BUILD_DIR} ${SERVER_INC} $<
${SERVER_BUILD_DIR}/%.cmo: %.ml
	${ELIOMC} -c -noinfer -dir ${SERVER_BUILD_DIR} ${SERVER_INC} $<

${SERVER_BUILD_DIR}/%.cmx: %.ml
	${ELIOMOPT} -c -noinfer -dir ${SERVER_BUILD_DIR} ${SERVER_INC} $<
${SERVER_BUILD_DIR}/%.cmx: %.eliom
	${ELIOMOPT} -c -noinfer -dir ${SERVER_BUILD_DIR} ${SERVER_INC} $<

%.cmxs: %.cmxa
	$(ELIOMOPT) -shared -linkall -o $@ $<

##### Client side ####

${STATICDIR}/eliom_testsuite.js:  ${CLIENT_OBJS}
	${JS_OF_ELIOM} -o $@ -jsopt -pretty -jsopt -noinline ${CLIENT_LIBS} $^

${CLIENT_BUILD_DIR}/%.cmi: %.mli
	${JS_OF_ELIOM} -c -dir ${CLIENT_BUILD_DIR} ${CLIENT_INC} $<

${CLIENT_BUILD_DIR}/%.cmo: %.eliom
	${JS_OF_ELIOM} -c -dir ${CLIENT_BUILD_DIR} ${CLIENT_INC} $<
${CLIENT_BUILD_DIR}/%.cmo: %.ml
	${JS_OF_ELIOM} -c -dir ${CLIENT_BUILD_DIR} ${CLIENT_INC} $<

####### Aux site: Ocamlduce #######

ifeq "${OCAMLDUCE}" "YES"
byte:: ocamlduce.byte
opt:: ocamlduce.opt
endif

ocamlduce.byte: ${SERVER_BUILD_DIR}/eliom_testsuite1.cmo
	$(MAKE) -C ocamlduce byte

ocamlduce.opt: ${SERVER_BUILD_DIR}/eliom_testsuite1.cmx
	$(MAKE) -C ocamlduce opt

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
	${MAKE} -C ocamlduce clean
	${MAKE} -C miniwiki clean
clean.local:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f *.type_mli
	-rm -f ${STATICDIR}/eliom_testsuite.js
	-rm -rf ${CLIENT_BUILD_DIR} ${SERVER_BUILD_DIR}

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
	$(ELIOMDEP) -server -dir ${SERVER_BUILD_DIR} ${SERVER_INC} ${SERVER_FILES} > .depend
	$(ELIOMDEP) -client -dir ${CLIENT_BUILD_DIR} ${CLIENT_INC} ${CLIENT_FILES} >> .depend

include .depend

## Dependencies towards *.eliom are not handled by eliomdep.
eliom_testsuite.cmo: eliom_testsuite3.cmo
eliom_testsuite.cmx: eliom_testsuite3.cmx

## TODO

# %.wiki: %.ml
# cat $< | sed '1,/(\*wiki\*/d' | sed '/%<||2>%/,$$ d' | /bin/sh ./tutomake.sh > $@

