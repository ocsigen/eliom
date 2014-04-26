include ../Makefile.config

## Use local files
## (tests do not require global installation of Eliom)
export OCAMLPATH := ${SRC}/src/files:${OCAMLPATH}
export PATH := ${SRC}/src/tools:${PATH}
export ELIOM_DATA_DIR := ${SRC}/src/files

ifeq "${DEBUG}" "YES"
  DEBUG_OPTION := -g
else
  DEBUG_OPTION :=
endif

ELIOMC   := eliomc ${DEBUG_OPTION}
ELIOMOPT := eliomopt ${DEBUG_OPTION}
ELIOMDEPS := eliomdep -server
ELIOMDEPC := eliomdep -client
JS_OF_ELIOM := js_of_eliom ${DEBUG_OPTION}

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
                eliom_testsuite5.eliom \
		eliom_testsuite.ml     \
                atom_example.ml \

CLIENT_FILES := eliom_testsuite_base.eliom \
                eliom_testsuite3.eliom \
                eliom_testsuite4.eliom \
                eliom_testsuite5.eliom

OTHER_FILES := eliom_testsuite_global.eliom eliom_testsuite_site.eliom

STATICDIR := ../local/var/www/tests
TYPE_DIR =

DISTILLERY_FILES := $(addprefix $(STATICDIR)/distillery/basic/,test.cma test.cmxa test.js)

byte:: _server/eliom_testsuite_global.cmo _server/eliom_testsuite_site.cmo eliom_testsuite.cma ${STATICDIR}/eliom_testsuite.js distillery-basic-byte
opt:: _server/eliom_testsuite_global.cmxs _server/eliom_testsuite_site.cmxs eliom_testsuite.cmxs ${STATICDIR}/eliom_testsuite.js distillery-basic-opt

#### Distillery ####

.PHONY: distillery-basic-byte distillery-basic-opt

distillery/basic:
	mkdir -p $@
	eliom-distillery -name distillery_basic -target-directory $@
	echo 'PREFIX=$(CURDIR)/../local/' >> $@/Makefile.options
	echo 'STATICDIR=var/www/distillery-basic/static' >> $@/Makefile.options
	echo 'ELIOMSTATICDIR=var/www/distillery-basic/eliomstatic' >> $@/Makefile.options
	echo 'LIBDIR=var/www/distillery-basic/lib' >> $@/Makefile.options
	echo 'WWWUSER=' >> $@/Makefile.options
	echo 'WWWGROUP=' >> $@/Makefile.options
	make -C $@ byte install.static
distillery-basic-byte: | distillery/basic
	make -C distillery/basic byte install.lib.byte
distillery-basic-opt: | distillery/basic
	make -C distillery/basic opt install.lib.opt

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
	${ELIOMC} -c ${addprefix -type-dir ,${TYPE_DIR}} ${SERVER_INC} $<

_server/%.cmx: %.ml
	${ELIOMOPT} -c ${SERVER_INC} $<
_server/%.cmx: %.eliom
	${ELIOMOPT} -c ${addprefix -type-dir ,${TYPE_DIR}} ${SERVER_INC} $<

%.cmxs: %.cmxa
	$(ELIOMOPT) -shared -linkall -o $@ $<

%.cmxs: %.cmx
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
	${JS_OF_ELIOM} -c ${addprefix -type-dir ,${TYPE_DIR}} ${CLIENT_INC} $<
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
	${MAKE} -C miniwiki clean
clean.local:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f ${or ${TYPE_DIR},_server}/*.type_mli
	-rm -f ${STATICDIR}/eliom_testsuite.js
	-rm -rf _client/* _server/*
	-rm -rf distillery

distclean: clean.local
	-rm -f *~ \#* .\#* .depend
	${MAKE} -C miniwiki distclean

## Dependencies

depend: .depend

.depend:
	${MAKE} -C miniwiki depend
	$(ELIOMDEPS) ${SERVER_INC} ${addprefix -type-dir ,${TYPE_DIR}} ${SERVER_FILES} ${OTHER_FILES} | sed s%_server//%/% > .depend
	$(ELIOMDEPC) ${CLIENT_INC} ${addprefix -type-dir ,${TYPE_DIR}} ${CLIENT_FILES} $(OTHER_FILES) >> .depend

include .depend

## TODO

# %.wiki: %.ml
# cat $< | sed '1,/(\*wiki\*/d' | sed '/%<||2>%/,$$ d' | /bin/sh ./tutomake.sh > $@
