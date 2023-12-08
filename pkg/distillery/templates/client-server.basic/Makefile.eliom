#----------------------------------------------------------------------
#           OCSIGEN-START MAKEFILE, NOT TO BE MODIFIED
#----------------------------------------------------------------------

##----------------------------------------------------------------------
## DISCLAIMER
##
## This file contains the rules to make an Eliom project. The project is
## configured through the variables in the file Makefile.options.
##----------------------------------------------------------------------

##----------------------------------------------------------------------
##                Internals

## Required binaries
OCSIGENSERVER     := ocsigenserver
OCSIGENSERVER.OPT := ocsigenserver.opt

ifneq ($(DEBUG),yes)
  DUNE_OPTIONS = --profile release
endif

##----------------------------------------------------------------------

##----------------------------------------------------------------------
## General

.PHONY: all css byte opt

DIST_DIRS          := $(ETCDIR) $(DATADIR) $(LIBDIR) $(LOGDIR) \
                      $(FILESDIR)/avatars/tmp $(ELIOMSTATICDIR) \
                      $(shell dirname $(CMDPIPE))
JS_PREFIX          := $(TEST_PREFIX)$(ELIOMSTATICDIR)/$(PROJECT_NAME)

CONF_IN            := $(wildcard *.conf.in)
CONFIG_FILES       := $(patsubst %.conf.in,$(TEST_PREFIX)$(ETCDIR)/%.conf,$(CONF_IN))
TEST_CONFIG_FILES  := $(patsubst %.conf.in,$(TEST_PREFIX)$(ETCDIR)/%-test.conf,$(CONF_IN))


all:: css
all byte opt:: ${VOLATILE_SCHEMA}

##----------------------------------------------------------------------

##----------------------------------------------------------------------
## Testing

DIST_FILES = $(ELIOMSTATICDIR)/$(PROJECT_NAME).js $(LIBDIR)/$(PROJECT_NAME).cma

.PHONY: test.byte test.opt staticfiles

test.byte:: byte | $(addprefix $(TEST_PREFIX),$(DIST_DIRS)) staticfiles
	@echo "==== The website is available at http://localhost:$(TEST_PORT) ===="
	$(OCSIGENSERVER) $(RUN_DEBUG) -c $(patsubst %.conf.in,$(TEST_PREFIX)$(ETCDIR)/%-test.conf,$(CONF_IN))
test.opt:: opt | $(addprefix $(TEST_PREFIX),$(DIST_DIRS)) staticfiles
	@echo "==== The website is available at http://localhost:$(TEST_PORT) ===="
	$(OCSIGENSERVER.OPT) $(RUN_DEBUG) -c $(patsubst %.conf.in,$(TEST_PREFIX)$(ETCDIR)/%-test.conf,$(CONF_IN))

$(addprefix $(TEST_PREFIX), $(DIST_DIRS)):
	mkdir -p $@

staticfiles:
	cp -rf $(LOCAL_STATIC_CSS) $(TEST_PREFIX)$(ELIOMSTATICDIR)

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
	cp -r $(LOCAL_STATIC_CSS) $(PREFIX)$(FILESDIR)
	[ -z $(WWWUSER) ] || chown -R $(WWWUSER) $(PREFIX)$(FILESDIR)
	install $(addprefix -o ,$(WWWUSER)) $< $(PREFIX)$(ELIOMSTATICDIR)
install.etc: $(TEST_PREFIX)$(ETCDIR)/$(PROJECT_NAME).conf | $(PREFIX)$(ETCDIR)
	install $< $(PREFIX)$(ETCDIR)/$(PROJECT_NAME).conf

.PHONY:
print-install-files:
	@echo $(PREFIX)$(LIBDIR)
	@echo $(PREFIX)$(ELIOMSTATICDIR)
	@echo $(PREFIX)$(ETCDIR)

$(addprefix $(PREFIX),$(ETCDIR) $(LIBDIR)):
	install -d $@
$(addprefix $(PREFIX),$(DATADIR) $(LOGDIR) $(ELIOMSTATICDIR) $(shell dirname $(CMDPIPE))):
	install $(addprefix -o ,$(WWWUSER)) -d $@

run.byte:
	@echo "==== The website is available at http://localhost:$(PORT) ===="
	$(OCSIGENSERVER) $(RUN_DEBUG) -c ${PREFIX}${ETCDIR}/${PROJECT_NAME}.conf
run.opt:
	@echo "==== The website is available at http://localhost:$(PORT) ===="
	$(OCSIGENSERVER.OPT) $(RUN_DEBUG) -c ${PREFIX}${ETCDIR}/${PROJECT_NAME}.conf

##----------------------------------------------------------------------

##----------------------------------------------------------------------
## Config files

ELIOM_MODULES=$(patsubst %,\<eliommodule\ findlib-package=\"%\"\ /\>,$(SERVER_ELIOM_PACKAGES))
FINDLIB_PACKAGES=$(patsubst %,\<extension\ findlib-package=\"%\"\ /\>,$(SERVER_PACKAGES))
EDIT_WARNING=DON\'T EDIT THIS FILE! It is generated from $(PROJECT_NAME).conf.in, edit that one, or the variables in Makefile.options
SED_ARGS = -e "/^ *%%%/d"
SED_ARGS += -e "s|%%PROJECT_NAME%%|$(PROJECT_NAME)|g"
SED_ARGS += -e "s|%%DB_NAME%%|$(DB_NAME)|g"
SED_ARGS += -e "s|%%DB_HOST%%|$(DB_HOST)|g"
SED_ARGS += -e "s|%%DB_PORT%%|$(DB_PORT)|g"
SED_ARGS += -e "s|%%DB_USER%%|$(DB_USER)|g"
SED_ARGS += -e "s|%%DB_PASSWORD%%|$(DB_PASSWORD)|g"
SED_ARGS += -e "s|%%CMDPIPE%%|%%PREFIX%%$(CMDPIPE)|g"
SED_ARGS += -e "s|%%LOGDIR%%|%%PREFIX%%$(LOGDIR)|g"
SED_ARGS += -e "s|%%DATADIR%%|%%PREFIX%%$(DATADIR)|g"
SED_ARGS += -e "s|%%LIBDIR%%|%%PREFIX%%$(LIBDIR)|g"
SED_ARGS += -e "s|%%WARNING%%|$(EDIT_WARNING)|g"
SED_ARGS += -e "s|%%PACKAGES%%|$(FINDLIB_PACKAGES)|g"
SED_ARGS += -e "s|%%ELIOM_MODULES%%|$(ELIOM_MODULES)|g"
SED_ARGS += -e "s|%%FILESDIR%%|%%PREFIX%%$(FILESDIR)|g"
SED_ARGS += -e "s|%%ELIOMSTATICDIR%%|%%PREFIX%%$(ELIOMSTATICDIR)|g"
SED_ARGS += -e "s|%%APPNAME%%|$(shell basename `readlink $(JS_PREFIX).js` .js)|g"
SED_ARGS += -e "s|%%CSSNAME%%|$(shell readlink $(CSS_PREFIX).css)|g"
ifeq ($(DEBUG),yes)
  SED_ARGS += -e "s|%%DEBUGMODE%%|\<debugmode /\>|g"
else
  SED_ARGS += -e "s|%%DEBUGMODE%%||g"
endif

LOCAL_SED_ARGS := -e "s|%%PORT%%|$(TEST_PORT)|g"
LOCAL_SED_ARGS += -e "s|%%USERGROUP%%||g"
GLOBAL_SED_ARGS := -e "s|%%PORT%%|$(PORT)|g"
ifeq ($(WWWUSER)$(WWWGROUP),)
  GLOBAL_SED_ARGS += -e "s|%%USERGROUP%%||g"
else
  GLOBAL_SED_ARGS += -e "s|%%USERGROUP%%|<user>$(WWWUSER)</user><group>$(WWWGROUP)</group>|g"
endif

JS_AND_CSS=$(JS_PREFIX).js $(CSS_PREFIX).css

$(CONFIG_FILES): $(TEST_PREFIX)$(ETCDIR)/%.conf: %.conf.in $(JS_AND_CSS) | $(TEST_PREFIX)$(ETCDIR)
	sed $(SED_ARGS) $(GLOBAL_SED_ARGS) $< | sed -e "s|%%PREFIX%%|$(PREFIX)|g" > $@

$(TEST_CONFIG_FILES): $(TEST_PREFIX)$(ETCDIR)/%-test.conf: %.conf.in $(JS_AND_CSS) | $(TEST_PREFIX)$(ETCDIR)
	sed $(SED_ARGS) $(LOCAL_SED_ARGS) $< | sed -e "s|%%PREFIX%%|$(TEST_PREFIX)|g" > $@

##----------------------------------------------------------------------

##----------------------------------------------------------------------
## Compilation

.PHONY: gen-dune config-files

config-files: | $(TEST_PREFIX)$(ELIOMSTATICDIR) $(TEST_PREFIX)$(LIBDIR)
	HASH=`md5sum _build/default/client/$(PROJECT_NAME).bc.js | cut -d ' ' -f 1` && \
	cp -f _build/default/client/$(PROJECT_NAME).bc.js $(JS_PREFIX)_$$HASH.js && \
	ln -sf $(PROJECT_NAME)_$$HASH.js $(JS_PREFIX).js
	cp -f _build/default/$(PROJECT_NAME).cm* $(TEST_PREFIX)$(LIBDIR)/
	$(MAKE) $(CONFIG_FILES) $(TEST_CONFIG_FILES) PROJECT_NAME=$(PROJECT_NAME)

all:: gen-dune
	$(ENV_PSQL) dune build $(DUNE_OPTIONS) @install @$(PROJECT_NAME) $(PROJECT_NAME).cmxs

byte:: gen-dune
	$(ENV_PSQL) dune build $(DUNE_OPTIONS) @$(PROJECT_NAME)
	make config-files PROJECT_NAME=$(PROJECT_NAME)

opt:: gen-dune
	$(ENV_PSQL) dune build $(DUNE_OPTIONS) $(PROJECT_NAME).cmxs @$(PROJECT_NAME)
	make config-files PROJECT_NAME=$(PROJECT_NAME)

gen-dune:
	@ocaml tools/gen_dune.ml > client/dune.client

##----------------------------------------------------------------------

##----------------------------------------------------------------------
## Clean up

.PHONY: clean

clean::
	dune clean
