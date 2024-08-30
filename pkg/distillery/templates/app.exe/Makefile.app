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

all:: css
all byte opt:: ${VOLATILE_SCHEMA}

##----------------------------------------------------------------------

##----------------------------------------------------------------------
## Testing

DIST_FILES = $(ELIOMSTATICDIR)/$(PROJECT_NAME).js $(LIBDIR)/$(PROJECT_NAME).cma

.PHONY: test.byte test.opt test.static.byte test.static.opt staticfiles

test.byte:: static.byte | $(addprefix $(TEST_PREFIX),$(DIST_DIRS)) staticfiles
	@echo "==== The website is available at http://localhost:$(TEST_PORT) ===="
	dune exec ./%%%PROJECT_NAME%%%_main.bc
test.opt:: static.opt | $(addprefix $(TEST_PREFIX),$(DIST_DIRS)) staticfiles
	@echo "==== The website is available at http://localhost:$(TEST_PORT) ===="
	dune exec ./%%%PROJECT_NAME%%%_main.exe

test.static.byte: test.byte

test.static.opt: test.opt

$(addprefix $(TEST_PREFIX), $(DIST_DIRS)):
	mkdir -p $@

staticfiles:
	cp -rf $(LOCAL_STATIC_CSS) $(TEST_PREFIX)$(ELIOMSTATICDIR)

##----------------------------------------------------------------------
## Static executable

static.byte: byte
	dune build %%%PROJECT_NAME%%%_main.bc

static.opt: opt
	dune build %%%PROJECT_NAME%%%_main.exe
##----------------------------------------------------------------------
## Installing & Running

.PHONY: install install.byte install.byte install.opt install.static install.lib install.lib.byte install.lib.opt run.byte run.opt
install: install.byte install.opt
install.byte: install.lib.byte install.static | $(addprefix $(PREFIX),$(DATADIR) $(LOGDIR) $(shell dirname $(CMDPIPE)))
install.opt: install.lib.opt install.static | $(addprefix $(PREFIX),$(DATADIR) $(LOGDIR) $(shell dirname $(CMDPIPE)))
install.lib: install.lib.byte install.lib.opt
install.lib.byte: $(TEST_PREFIX)$(LIBDIR)/$(PROJECT_NAME).cma | $(PREFIX)$(LIBDIR)
	install $< $(PREFIX)$(LIBDIR)
install.lib.opt: $(TEST_PREFIX)$(LIBDIR)/$(PROJECT_NAME).cmxs | $(PREFIX)$(LIBDIR)
	install $< $(PREFIX)$(LIBDIR)
install.static: $(TEST_PREFIX)$(ELIOMSTATICDIR)/$(PROJECT_NAME).js | $(PREFIX)$(STATICDIR) $(PREFIX)$(ELIOMSTATICDIR)
	cp -r $(LOCAL_STATIC_CSS) $(PREFIX)$(FILESDIR)
	[ -z $(WWWUSER) ] || chown -R $(WWWUSER) $(PREFIX)$(FILESDIR)
	install $(addprefix -o ,$(WWWUSER)) $< $(PREFIX)$(ELIOMSTATICDIR)

.PHONY:
print-install-files:
	@echo $(PREFIX)$(LIBDIR)
	@echo $(PREFIX)$(ELIOMSTATICDIR)
	@echo $(PREFIX)$(ETCDIR)

$(addprefix $(PREFIX),$(ETCDIR) $(LIBDIR)):
	install -d $@
$(addprefix $(PREFIX),$(DATADIR) $(LOGDIR) $(ELIOMSTATICDIR) $(shell dirname $(CMDPIPE))):
	install $(addprefix -o ,$(WWWUSER)) -d $@

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

all::
	$(ENV_PSQL) dune build $(DUNE_OPTIONS) @install @$(PROJECT_NAME) $(PROJECT_NAME).cmxs

byte::
	$(ENV_PSQL) dune build $(DUNE_OPTIONS) @$(PROJECT_NAME)
	make config-files PROJECT_NAME=$(PROJECT_NAME)

opt::
	$(ENV_PSQL) dune build $(DUNE_OPTIONS) $(PROJECT_NAME).cmxs @$(PROJECT_NAME)
	make config-files PROJECT_NAME=$(PROJECT_NAME)

##----------------------------------------------------------------------

##----------------------------------------------------------------------
## Clean up

.PHONY: clean

clean::
	dune clean
