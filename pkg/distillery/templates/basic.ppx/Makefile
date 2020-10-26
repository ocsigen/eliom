
##----------------------------------------------------------------------
## DISCLAIMER
##
## This file contains the rules to make an Eliom project. The project is
## configured through the variables in the file Makefile.options.
##----------------------------------------------------------------------

include Makefile.options

##----------------------------------------------------------------------
##			      Internals

## Required binaries
ELIOMC            := eliomc -ppx
ELIOMOPT          := eliomopt -ppx
JS_OF_ELIOM       := js_of_eliom -ppx
ELIOMDEP          := eliomdep
OCSIGENSERVER     := ocsigenserver
OCSIGENSERVER.OPT := ocsigenserver.opt

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
## General

.PHONY: all byte opt
all: byte opt
byte opt:: $(TEST_PREFIX)$(ELIOMSTATICDIR)/${PROJECT_NAME}.js
byte opt:: $(TEST_PREFIX)$(ETCDIR)/$(PROJECT_NAME).conf
byte opt:: $(TEST_PREFIX)$(ETCDIR)/$(PROJECT_NAME)-test.conf
byte:: $(TEST_PREFIX)$(LIBDIR)/${PROJECT_NAME}.cma
opt:: $(TEST_PREFIX)$(LIBDIR)/${PROJECT_NAME}.cmxs

DIST_DIRS = $(ETCDIR) $(DATADIR) $(LIBDIR) $(LOGDIR) $(STATICDIR) $(ELIOMSTATICDIR) $(shell dirname $(CMDPIPE))

##----------------------------------------------------------------------
## Testing

DIST_FILES = $(ELIOMSTATICDIR)/$(PROJECT_NAME).js $(LIBDIR)/$(PROJECT_NAME).cma

.PHONY: test.byte test.opt
test.byte: $(addprefix $(TEST_PREFIX),$(ETCDIR)/$(PROJECT_NAME)-test.conf $(DIST_DIRS) $(DIST_FILES))
	$(OCSIGENSERVER) $(RUN_DEBUG) -c $<
test.opt: $(addprefix $(TEST_PREFIX),$(ETCDIR)/$(PROJECT_NAME)-test.conf $(DIST_DIRS) $(patsubst %.cma,%.cmxs, $(DIST_FILES)))
	$(OCSIGENSERVER.OPT) $(RUN_DEBUG) -c $<

$(addprefix $(TEST_PREFIX), $(DIST_DIRS)):
	mkdir -p $@

##----------------------------------------------------------------------
## Installing & Running

.PHONY: install install.byte install.byte install.opt install.static install.etc install.lib install.lib.byte install.lib.opt run.byte run.opt
install: install.byte install.opt
install.byte: install.lib.byte install.etc install.static | $(addprefix $(PREFIX),$(DATADIR) $(LOGDIR) $(shell dirname $(CMDPIPE)))
install.opt: install.lib.opt install.etc install.static | $(addprefix $(PREFIX),$(DATADIR) $(LOGDIR) $(shell dirname $(CMDPIPE)))
install.lib: install.lib.byte install.lib.opt
install.lib.byte: $(TEST_PREFIX)$(LIBDIR)/$(PROJECT_NAME).cma | $(PREFIX)$(LIBDIR)
	install $< $(PREFIX)$(LIBDIR)
install.lib.opt: $(TEST_PREFIX)$(LIBDIR)/$(PROJECT_NAME).cmxs | $(PREFIX)$(LIBDIR)
	install $< $(PREFIX)$(LIBDIR)
install.static: $(TEST_PREFIX)$(ELIOMSTATICDIR)/$(PROJECT_NAME).js | $(PREFIX)$(STATICDIR) $(PREFIX)$(ELIOMSTATICDIR)
	cp -r $(LOCAL_STATIC)/* $(PREFIX)$(STATICDIR)
	[ -z $(WWWUSER) ] || chown -R $(WWWUSER) $(PREFIX)$(STATICDIR)
	install $(addprefix -o ,$(WWWUSER)) $< $(PREFIX)$(ELIOMSTATICDIR)
install.etc: $(TEST_PREFIX)$(ETCDIR)/$(PROJECT_NAME).conf | $(PREFIX)$(ETCDIR)
	install $< $(PREFIX)$(ETCDIR)/$(PROJECT_NAME).conf

.PHONY:
print-install-files:
	@echo $(PREFIX)$(LIBDIR)
	@echo $(PREFIX)$(STATICDIR)
	@echo $(PREFIX)$(ELIOMSTATICDIR)
	@echo $(PREFIX)$(ETCDIR)

$(addprefix $(PREFIX),$(ETCDIR) $(LIBDIR)):
	install -d $@
$(addprefix $(PREFIX),$(DATADIR) $(LOGDIR) $(STATICDIR) $(ELIOMSTATICDIR) $(shell dirname $(CMDPIPE))):
	install $(addprefix -o ,$(WWWUSER)) -d $@

run.byte:
	$(OCSIGENSERVER) $(RUN_DEBUG) -c ${PREFIX}${ETCDIR}/${PROJECT_NAME}.conf
run.opt:
	$(OCSIGENSERVER.OPT) $(RUN_DEBUG) -c ${PREFIX}${ETCDIR}/${PROJECT_NAME}.conf

##----------------------------------------------------------------------
## Aux

# Use `eliomdep -sort' only in OCaml>4
ifeq ($(shell ocamlc -version|cut -c1),4)
eliomdep=$(shell $(ELIOMDEP) $(1) -ppx -sort $(2) $(filter %.eliom %.ml,$(3))))
else
eliomdep=$(3)
endif
objs=$(patsubst %.ml,$(1)/%.$(2),$(patsubst %.eliom,$(1)/%.$(2),$(filter %.eliom %.ml,$(3))))
depsort=$(call objs,$(1),$(2),$(call eliomdep,$(3),$(4),$(5)))

##----------------------------------------------------------------------
## Config files

FINDLIB_PACKAGES=$(patsubst %,\<extension\ findlib-package=\"%\"\ /\>,$(SERVER_PACKAGES))
EDIT_WARNING=DON\'T EDIT THIS FILE! It is generated from $(PROJECT_NAME).conf.in, edit that one, or the variables in Makefile.options
SED_ARGS := -e "/^ *%%%/d"
SED_ARGS += -e "s|%%PROJECT_NAME%%|$(PROJECT_NAME)|g"
SED_ARGS += -e "s|%%DATABASE_NAME%%|$(DATABASE_NAME)|g"
SED_ARGS += -e "s|%%DATABASE_USER%%|$(DATABASE_USER)|g"
SED_ARGS += -e "s|%%CMDPIPE%%|%%PREFIX%%$(CMDPIPE)|g"
SED_ARGS += -e "s|%%LOGDIR%%|%%PREFIX%%$(LOGDIR)|g"
SED_ARGS += -e "s|%%DATADIR%%|%%PREFIX%%$(DATADIR)|g"
SED_ARGS += -e "s|%%PERSISTENT_DATA_BACKEND%%|$(PERSISTENT_DATA_BACKEND)|g"
SED_ARGS += -e "s|%%LIBDIR%%|%%PREFIX%%$(LIBDIR)|g"
SED_ARGS += -e "s|%%WARNING%%|$(EDIT_WARNING)|g"
SED_ARGS += -e "s|%%PACKAGES%%|$(FINDLIB_PACKAGES)|g"
SED_ARGS += -e "s|%%ELIOMSTATICDIR%%|%%PREFIX%%$(ELIOMSTATICDIR)|g"
ifeq ($(DEBUG),yes)
  SED_ARGS += -e "s|%%DEBUGMODE%%|\<debugmode /\>|g"
else
  SED_ARGS += -e "s|%%DEBUGMODE%%||g"
endif

LOCAL_SED_ARGS := -e "s|%%PORT%%|$(TEST_PORT)|g"
LOCAL_SED_ARGS += -e "s|%%STATICDIR%%|$(LOCAL_STATIC)|g"
LOCAL_SED_ARGS += -e "s|%%USERGROUP%%||g"
GLOBAL_SED_ARGS := -e "s|%%PORT%%|$(PORT)|g"
GLOBAL_SED_ARGS += -e "s|%%STATICDIR%%|%%PREFIX%%$(STATICDIR)|g"
ifeq ($(WWWUSER)$(WWWGROUP),)
  GLOBAL_SED_ARGS += -e "s|%%USERGROUP%%||g"
else
  GLOBAL_SED_ARGS += -e "s|%%USERGROUP%%|<user>$(WWWUSER)</user><group>$(WWWGROUP)</group>|g"
endif

$(TEST_PREFIX)${ETCDIR}/${PROJECT_NAME}.conf: ${PROJECT_NAME}.conf.in Makefile.options | $(TEST_PREFIX)$(ETCDIR)
	sed $(SED_ARGS) $(GLOBAL_SED_ARGS) $< | sed -e "s|%%PREFIX%%|$(PREFIX)|g" > $@
$(TEST_PREFIX)${ETCDIR}/${PROJECT_NAME}-test.conf: ${PROJECT_NAME}.conf.in Makefile.options | $(TEST_PREFIX)$(ETCDIR)
	sed $(SED_ARGS) $(LOCAL_SED_ARGS) $< | sed -e "s|%%PREFIX%%|$(TEST_PREFIX)|g" > $@

##----------------------------------------------------------------------
## Server side compilation

SERVER_INC  := ${addprefix -package ,${SERVER_PACKAGES}}

${ELIOM_TYPE_DIR}/%.type_mli: %.eliom
	${ELIOMC} -infer ${SERVER_INC} $<

$(TEST_PREFIX)$(LIBDIR)/$(PROJECT_NAME).cma: $(call objs,$(ELIOM_SERVER_DIR),cmo,$(SERVER_FILES)) | $(TEST_PREFIX)$(LIBDIR)
	${ELIOMC} -a -o $@ $(GENERATE_DEBUG) \
          $(call depsort,$(ELIOM_SERVER_DIR),cmo,-server,$(SERVER_INC),$(SERVER_FILES))

$(TEST_PREFIX)$(LIBDIR)/$(PROJECT_NAME).cmxa: $(call objs,$(ELIOM_SERVER_DIR),cmx,$(SERVER_FILES)) | $(TEST_PREFIX)$(LIBDIR)
	${ELIOMOPT} -a -o $@ $(GENERATE_DEBUG) \
          $(call depsort,$(ELIOM_SERVER_DIR),cmx,-server,$(SERVER_INC),$(SERVER_FILES))

%.cmxs: %.cmxa
	$(ELIOMOPT) -shared -linkall -o $@ $(GENERATE_DEBUG) $<

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


##----------------------------------------------------------------------
## Client side compilation

CLIENT_LIBS := ${addprefix -package ,${CLIENT_PACKAGES}}
CLIENT_INC  := ${addprefix -package ,${CLIENT_PACKAGES}}

CLIENT_OBJS := $(filter %.eliom %.ml, $(CLIENT_FILES))
CLIENT_OBJS := $(patsubst %.eliom,${ELIOM_CLIENT_DIR}/%.cmo, ${CLIENT_OBJS})
CLIENT_OBJS := $(patsubst %.ml,${ELIOM_CLIENT_DIR}/%.cmo, ${CLIENT_OBJS})

$(TEST_PREFIX)$(ELIOMSTATICDIR)/$(PROJECT_NAME).js: $(call objs,$(ELIOM_CLIENT_DIR),cmo,$(CLIENT_FILES)) | $(TEST_PREFIX)$(ELIOMSTATICDIR)
	${JS_OF_ELIOM} -o $@ $(GENERATE_DEBUG) $(CLIENT_INC) $(DEBUG_JS) \
          $(call depsort,$(ELIOM_CLIENT_DIR),cmo,-client,$(CLIENT_INC),$(CLIENT_FILES))

${ELIOM_CLIENT_DIR}/%.cmi: %.mli
	${JS_OF_ELIOM} -c ${CLIENT_INC} $(GENERATE_DEBUG) $<

${ELIOM_CLIENT_DIR}/%.cmo: %.eliom
	${JS_OF_ELIOM} -c ${CLIENT_INC} $(GENERATE_DEBUG) $<
${ELIOM_CLIENT_DIR}/%.cmo: %.ml
	${JS_OF_ELIOM} -c ${CLIENT_INC} $(GENERATE_DEBUG) $<

${ELIOM_CLIENT_DIR}/%.cmi: %.eliomi
	${JS_OF_ELIOM} -c ${CLIENT_INC} $(GENERATE_DEBUG) $<

##----------------------------------------------------------------------
## Dependencies

include .depend

.depend: $(patsubst %,$(DEPSDIR)/%.server,$(SERVER_FILES)) $(patsubst %,$(DEPSDIR)/%.client,$(CLIENT_FILES))
	cat $^ > $@

$(DEPSDIR)/%.server: % | $(DEPSDIR)
	$(ELIOMDEP) -server -ppx $(SERVER_INC) $< > $@

$(DEPSDIR)/%.client: % | $(DEPSDIR)
	$(ELIOMDEP) -client -ppx $(CLIENT_INC) $< > $@

$(DEPSDIR):
	mkdir $@

##----------------------------------------------------------------------
## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f *.type_mli
	-rm -f ${PROJECT_NAME}.js
	-rm -rf ${ELIOM_CLIENT_DIR} ${ELIOM_SERVER_DIR}

distclean: clean
	-rm -rf $(TEST_PREFIX) $(DEPSDIR) .depend
