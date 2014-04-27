include ../../Makefile.config
include ../server/Makefile.filelist
include ../client/Makefile.filelist

export OCAMLPATH := ${SRC}/src/files:${OCAMLPATH}
export PATH := ${SRC}/src/tools:${PATH}
export ELIOM_DATA_DIR := ${SRC}/src/files

##

# Source files for the server
SERVER_FILES := $(wildcard *.eliom)
# Source files for the client
CLIENT_FILES := $(wildcard *.eliom)

# OCamlfind packages for the server
SERVER_PACKAGES :=
# OCamlfind packages for the client
CLIENT_PACKAGES :=

##----------------------------------------------------------------------
##			      Internals

## Required binaries
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}
OCAMLDOC := $(OCAMLFIND) ocamldoc
OCAMLDEP := $(OCAMLFIND) ocamldep
OCAMLDOC := ${OCAMLFIND} ocamldoc -ppopt o -ppopt -printer

ELIOMC            := ../tools/eliomc
ELIOMOPT          := ../tools/eliomopt
JS_OF_ELIOM       := ../tools/js_of_eliom
ELIOMDEP          := ../tools/eliomdep

## Where to put intermediate object files.
## - ELIOM_{SERVER,CLIENT}_DIR must be distinct
## - ELIOM_CLIENT_DIR must not be the local dir.
## - ELIOM_SERVER_DIR could be ".", but you need to
##   remove it from the "clean" rules...
export ELIOM_SERVER_DIR := _server
export ELIOM_CLIENT_DIR := _client
export ELIOM_TYPE_DIR   := _server
DEPSDIR := _deps

ifeq ($(DEBUG),yes)
  GENERATE_DEBUG ?= -g
  RUN_DEBUG ?= "-v"
  DEBUG_JS ?= -jsopt -pretty -jsopt -noinline -jsopt -debuginfo
endif

##----------------------------------------------------------------------
## Aux

# Use `eliomdep -sort' only in OCaml>4
ifeq ($(shell ocamlc -version|cut -c1),4)
eliomdep=$(shell $(ELIOMDEP) $(1) -sort $(2) $(filter %.eliom %.ml,$(3))))
else
eliomdep=$(3)
endif
objs=$(patsubst %.ml,$(1)/%.$(2),$(patsubst %.eliom,$(1)/%.$(2),$(filter %.eliom %.ml,$(3))))
depsort=$(call objs,$(1),$(2),$(call eliomdep,$(3),$(4),$(5)))


##----------------------------------------------------------------------
## General

.PHONY: all byte opt
all: $(SERVER_FILES:.eliom=.eliomi) \
     $(call objs,$(ELIOM_SERVER_DIR),cmi,$(SERVER_FILES)) \
     $(call objs,$(ELIOM_CLIENT_DIR),cmi,$(CLIENT_FILES)) \
     $(call objs,$(ELIOM_SERVER_DIR),cmo,$(SERVER_FILES)) \
     $(call objs,$(ELIOM_CLIENT_DIR),cmo,$(CLIENT_FILES))


##----------------------------------------------------------------------
## Server side compilation

SERVER_INC  := -I ../server/ -I ../server/private \
               ${addprefix -package ,${SERVER_PACKAGES}} \
               -ppopt ../syntax/pa_include.cmo

${ELIOM_TYPE_DIR}/%.type_mli: %.eliom
	${ELIOMC} -infer ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmi: %.mli
	${ELIOMC} -c ${SERVER_INC} $(GENERATE_DEBUG) $<

${ELIOM_SERVER_DIR}/%.cmi: %.eliomi
	${ELIOMC} -c ${SERVER_INC} $(GENERATE_DEBUG) $<

${ELIOM_SERVER_DIR}/%.cmo: %.ml
	${ELIOMC} -c ${SERVER_INC} $(GENERATE_DEBUG) $<
${ELIOM_SERVER_DIR}/%.cmo: %.eliom
	${ELIOMC} -c ${SERVER_INC} $(GENERATE_DEBUG) $<

${ELIOM_SERVER_DIR}/%.cmx: %.ml
	${ELIOMOPT} -c ${SERVER_INC} $(GENERATE_DEBUG) $<
${ELIOM_SERVER_DIR}/%.cmx: %.eliom
	${ELIOMOPT} -c ${SERVER_INC} $(GENERATE_DEBUG) $<

%.cmxs: %.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $<

%.eliomi: %.server.mli %.client.mli
	echo "{client{" > $@
	cat $(@:.eliomi=.client.mli) >> $@
	echo "}}" >> $@
	echo "{server{" >> $@
	cat $(@:.eliomi=.server.mli) >> $@
	echo "}}" >> $@

##----------------------------------------------------------------------
## Client side compilation

CLIENT_INC  := -I ../client/ -I ../client/private \
	       ${addprefix -package ,${CLIENT_PACKAGES}} \
               -ppopt ../syntax/pa_include.cmo

${ELIOM_CLIENT_DIR}/%.cmi: %.mli
	${JS_OF_ELIOM} -c ${CLIENT_INC} $(GENERATE_DEBUG) $<

${ELIOM_CLIENT_DIR}/%.cmo: %.eliom %.eliomi ${ELIOM_CLIENT_DIR}/%.cmi
	${JS_OF_ELIOM} -c ${CLIENT_INC} $(GENERATE_DEBUG) $<
${ELIOM_CLIENT_DIR}/%.cmo: %.ml
	${JS_OF_ELIOM} -c ${CLIENT_INC} $(GENERATE_DEBUG) $<

${ELIOM_CLIENT_DIR}/%.cmi: %.eliomi
	${JS_OF_ELIOM} -c ${CLIENT_INC} $(GENERATE_DEBUG) $<



##----------------------------------------------------------------------
## Doc

odoc: _server/eliom_content.odoc _client/eliom_content.odoc

_server/%.odoc: %.server.mli
	${OCAMLDOC} -syntax camlp4o ${LIBS} -dump ${ODOC_DIR}$@ $<

_client/%.odoc: %.client.mli
	${OCAMLDOC} -syntax camlp4o ${LIBS} -dump ${ODOC_DIR}$@ $<


##----------------------------------------------------------------------
## Install

odoc: ${DOC:.eliomi=.odoc}

INTF := $(wildcard *.eliomi)
MODULES := $(wildcard *.eliom)

install:
	${INSTALL} -m 755 -d ${SERVERDIR}
	${INSTALL} -m 644 ${INTF} $(addprefix _server/, ${MODULES:.eliom=.cmi}) ${SERVERDIR}
	${INSTALL} -m 755 -d ${CLIENTDIR}
	${INSTALL} -m 644 ${INTF} $(addprefix _client/, ${MODULES:.eliom=.cmi}) ${CLIENTDIR}

uninstall:



##----------------------------------------------------------------------
## Dependencies

include .depend

.depend: $(patsubst %,$(DEPSDIR)/%.server,$(SERVER_FILES)) $(patsubst %,$(DEPSDIR)/%.client,$(CLIENT_FILES))
	cat $^ > $@

$(DEPSDIR)/%.server: % | $(DEPSDIR)
	$(ELIOMDEP) -server $(SERVER_INC) $< > $@

$(DEPSDIR)/%.client: % | $(DEPSDIR)
	$(ELIOMDEP) -client $(CLIENT_INC) $< > $@

$(DEPSDIR):
	mkdir $@

_server/eliom_content.cmo: _server/eliom_content.cmi
_client/eliom_content.cmo: _client/eliom_content.cmi

##----------------------------------------------------------------------
## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f *.type_mli
	-rm -f *.eliomi
	-rm -f ${PROJECT_NAME}.js
	-rm -rf ${ELIOM_CLIENT_DIR} ${ELIOM_SERVER_DIR}

distclean: clean
	-rm -rf $(TEST_PREFIX) $(DEPSDIR) .depend
