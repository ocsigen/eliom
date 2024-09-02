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

all:: css byte opt staticfiles

##----------------------------------------------------------------------

##----------------------------------------------------------------------
## Testing

DIST_FILES = $(ELIOMSTATICDIR)/$(PROJECT_NAME).js $(LIBDIR)/$(PROJECT_NAME).cma

.PHONY: test.byte test.opt staticfiles

test.byte:: byte | $(addprefix $(TEST_PREFIX),$(DIST_DIRS)) staticfiles
	@echo "==== The website is available at http://localhost:$(TEST_PORT) ===="
	dune exec ./$(PROJECT_NAME)_main.bc
test.opt:: opt | $(addprefix $(TEST_PREFIX),$(DIST_DIRS)) staticfiles
	@echo "==== The website is available at http://localhost:$(TEST_PORT) ===="
	dune exec ./$(PROJECT_NAME)_main.exe

test.static.byte: test.byte

test.static.opt: test.opt

$(addprefix $(TEST_PREFIX), $(DIST_DIRS)):
	mkdir -p $@

staticfiles:
	cp -rf $(LOCAL_STATIC_CSS) $(TEST_PREFIX)$(ELIOMSTATICDIR)

##----------------------------------------------------------------------
## Installing & Running

.PHONY: install install.static install.lib run
install: all install.static | $(addprefix $(PREFIX),$(DATADIR) $(LOGDIR) $(shell dirname $(CMDPIPE)))
	dune install
install.static: $(TEST_PREFIX)$(ELIOMSTATICDIR)/$(PROJECT_NAME).js | $(PREFIX)$(STATICDIR) $(PREFIX)$(ELIOMSTATICDIR)
	cp -r $(LOCAL_STATIC_CSS) $(PREFIX)$(FILESDIR)
	HASH=`md5sum _build/default/client/$(PROJECT_NAME).bc.js | cut -d ' ' -f 1` && \
	install $(addprefix -o ,$(WWWUSER)) $(JS_PREFIX)_$$HASH.js $(PREFIX)$(ELIOMSTATICDIR) && \
	ln -sf $(PROJECT_NAME)_$$HASH.js $(PREFIX)$(ELIOMSTATICDIR)/$(PROJECT_NAME).js
	[ -z $(WWWUSER) ] || chown -R $(WWWUSER) $(PREFIX)$(FILESDIR)

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

all::
	$(ENV_PSQL) dune build $(DUNE_OPTIONS) @install @$(PROJECT_NAME)

js::
	$(ENV_PSQL) dune build $(DUNE_OPTIONS) client/$(PROJECT_NAME).bc.js

byte:: js
	$(ENV_PSQL) dune build $(DUNE_OPTIONS) $(PROJECT_NAME)_main.bc
	make config-files PROJECT_NAME=$(PROJECT_NAME)

opt:: js
	$(ENV_PSQL) dune build $(DUNE_OPTIONS) $(PROJECT_NAME)_main.exe
	make config-files PROJECT_NAME=$(PROJECT_NAME)

run:
	$(PREFIX)bin/$(PROJECT_NAME)


##----------------------------------------------------------------------

##----------------------------------------------------------------------
## Clean up

.PHONY: clean

clean::
	dune clean
