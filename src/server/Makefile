include ../../Makefile.config

LIBS     := -I private -I extensions ${addprefix -package ,${SERVER_PACKAGE} ${SERVER_SYNTAX}} \
	    -ppopt ../syntax/pa_include.cmo
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG} ${THREAD}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}  ${THREAD}
OCAMLDEP := $(OCAMLFIND) ocamldep

OCAMLDUCEC   := ${OCAMLDUCEFIND} ocamlc${BYTEDBG}  ${THREAD}
OCAMLDUCEOPT := ${OCAMLDUCEFIND} ocamlopt ${OPTDBG}  ${THREAD}
OCAMLDUCEDEP := ${OCAMLDUCEFIND} ocamldep

OCAMLDOC := ${OCAMLFIND} ocamldoc ${THREAD} -ppopt o -ppopt -printer
## WTF -ppopt options must be given in reverse order...

all: byte opt

### Common files ###

COMMON_FILES  := $(notdir $(wildcard ../common/*.mli) $(wildcard ../common/*.ml))
COMMON_PFILES := $(addprefix private/, \
		     $(notdir $(wildcard ../common/private/*.mli) \
	                      $(wildcard ../common/private/*.ml)))
COMMON_SFILES := $(addprefix sigs/, \
		     $(notdir $(wildcard ../common/sigs/*.mli) \
	                      $(wildcard ../common/sigs/*.ml)))

PREDEP := ${COMMON_FILES} ${COMMON_PFILES} ${COMMON_SFILES} eliom_duce_sigs.mli

.SECONDARY: ${PREDEP}
${COMMON_FILES}: %: ../common/%
	-@mkdir -p $(dir $@)
	ln -sf $< $@
${COMMON_PFILES} ${COMMON_SFILES}: %: ../common/%
	-@mkdir -p $(dir $@)
	ln -sf ../$< $@
eliom_duce_sigs.mli:: eliom_duce_sigs.mlp sigs/eliom_reg.mli sigs/eliom_reg_simpl.mli
	camlp4o ../syntax/pa_include.cmo -printer o -intf $< > $@
	# Ugly hack: copy time stamp to avoid dependencies re-computation
	# after a distclean
	touch -h -r $< $@

FILES := eliom_wrap.ml eliom_lazy.ml \
         eliom_lib_base.ml eliom_lib.ml \
	 eliom_content_core.ml \
	 eliom_cookie.ml \
	 eliom_common_base.ml eliom_common.ml \
	 eliom_request_info.ml \
	 private/eliommod_sessiongroups.ml \
	 eliom_cookie.ml \
	 private/eliommod_cookies.ml \
	 private/eliommod_sersess.ml \
         private/eliommod_datasess.ml \
	 private/eliommod_persess.ml \
	 private/eliommod_gc.ml \
	 private/eliommod_sessexpl.ml \
	 private/eliommod_sessadmin.ml \
	 private/eliommod_timeouts.ml \
	 eliom_state.ml \
	 eliom_reference.ml \
	 eliom_extension.ml \
	 private/eliommod_services.ml \
	 private/eliommod_naservices.ml \
         eliom_types_base.ml eliom_types.ml \
	 eliom_error_pages.ml \
	 private/eliommod_pagegen.ml \
	 private/eliommod.ml \
         eliom_parameter_base.ml eliom_parameter.ml \
	 eliom_process.ml \
	 private/eliommod_cli.ml \
	 eliom_service_base.ml eliom_service.ml \
         eliom_config.ml \
         eliom_uri.ml \
	 eliom_mkforms.ml \
	 eliom_registration_base.ml \
	 eliom_content.ml \
	 eliom_mkreg.ml \
	 eliom_registration.ml \
	 eliom_comet_base.ml eliom_comet.ml  \
         eliom_tools_common.ml \
	 eliom_tools.ml \
	 eliom_react.ml \
	 eliom_bus.ml \
	 eliom_compatibility_2_1.ml \
	 extensions/eliom_s2s.ml extensions/eliom_openid.ml \
	 extensions/atom_feed.ml extensions/eliom_atom.ml

	 # eliom_extension_template.ml \

INTF_NOP4 :=

byte:: eliom.cma
opt:: eliom.cmxa
ifeq "${NATDYNLINK}" "YES"
opt:: eliom.cmxs
endif

eliom.cma: ${FILES:.ml=.cmo}
	${OCAMLC} -a -o $@ $^
eliom.cmxa: ${FILES:.ml=.cmx}
	${OCAMLOPT} -a -o $@ $^

### OCamlduce ###

DUCE_FILES := eliom_duce.ml       \
	      eliom_duce_tools.ml \

DUCE_MLIS := eliom_duce_types.mli \
	     eliom_duce_sigs.mli  \
	     eliom_duce.mli       \
	     eliom_duce_tools.mli \

ifeq "${OCAMLDUCE}" "YES"
byte:: eliom_duce.cma
opt:: eliom_duce.cmxa
ifeq "${NATDYNLINK}" "YES"
opt:: eliom_duce.cmxs
endif
endif

eliom_duce.cma: ${DUCE_FILES:.ml=.cmo}
	${OCAMLC} -a -o $@ $^
eliom_duce.cmxa: ${DUCE_FILES:.ml=.cmx}
	${OCAMLOPT} -a -o $@ $^

##########

%.cmo: %.ml
	$(OCAMLC) -syntax camlp4o ${LIBS} -c $<
%.cmx: %.ml
	$(OCAMLOPT) -syntax camlp4o ${LIBS} -c $<

%.cmi: %.mli
	$(OCAMLC) -syntax camlp4o ${LIBS} -c $<
${INTF_NOP4:.mli=.cmi}: \
%.cmi: %.mli
	$(OCAMLC) ${LIBS} -c $<

%.odoc: %.mli
	${OCAMLDOC} -syntax camlp4o ${LIBS} -dump ${ODOC_DIR}$@ $<
${INTF_NOP4:.mli=.odoc}: \
%.odoc: %.mli
	${OCAMLDOC} ${LIBS} -dump ${ODOC_DIR}$@ $<

%.cmxs: %.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $<

${DUCE_FILES:.ml=.cmo}: %.cmo: %.ml
	$(OCAMLDUCEC) ${LIBS} -c $<
${DUCE_FILES:.ml=.cmx}: %.cmx: %.ml
	$(OCAMLDUCEOPT) ${LIBS} -c $<
${DUCE_MLIS:.mli=.cmi}: %.cmi: %.mli
	$(OCAMLDUCEC) ${LIBS} -c $<
${DUCE_MLIS:.mli=.odoc}: %.odoc: %.mli
	${OCAMLDOC} ${LIBS} -dump $@ $<

## INSTALL ##

include Makefile.filelist

odoc: ${DOC:.mli=.odoc}

install: INSTALL_IMPL:=${IMPL} ${NATIMPL}
install: raw_install

install.byte: INSTALL_IMPL:=${IMPL}
install.byte: raw_install

install.opt: INSTALL_IMPL:=${NATIMPL}
install.opt: raw_install

raw_install: ${PREDEP}
	${INSTALL} -m 755 -d ${SERVERDIR}
	${INSTALL} -m 644 ${INTF} ${INSTALL_IMPL} ${INTF:.cmi=.mli} ${SERVERDIR}

uninstall:
	-rm -fr ${SERVERDIR}

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot *.odoc
	-cd private && rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot *.odoc
	-cd extensions && rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot *.odoc
distclean: clean
	-rm -f *~ \#* .\#*
	-rm -f ${PREDEP}
	-rm -f .*.deps private/.*.deps extensions/.*.deps

## Manual dependencies for pa_include.
eliom_registration_base.cmi eliom_registration_base.cmo eliom_registration_base.cmx \
eliom_registration.cmi eliom_registration.cmo eliom_registration.cmx eliom_registration.odoc \
eliom_mkforms.cmi eliom_mkforms.cmo eliom_mkforms.cmx eliom_mkforms.odoc \
eliom_mkreg.cmi eliom_mkreg.odoc \
eliom_duce_sigs.cmi eliom_duce_sigs.odoc: ${COMMON_SFILES} $(wildcard sigs/*.mli)

# Automatic dependencies

ifneq (${DEPEND},no)

## Ugly side effect to force ${PREDEP} creation before dependencies calculation
dummy_var := $(shell $(MAKE) DEPEND=no ${PREDEP} >/tmp/log 2>&1)

DEPS := $(patsubst %,.%.deps,${COMMON_FILES}) \
	$(patsubst %,.%.deps,${COMMON_PFILES}) \
	.eliom_duce_sigs.mli.deps \
	$(patsubst %,.%.deps,$(wildcard *.mli *.ml)) \
	$(patsubst private/%,private/.%.deps,$(wildcard private/*.mli private/*.ml)) \
	$(patsubst extensions/%,extensions/.%.deps, \
	     $(wildcard extensions/*.mli extensions/*.ml))

.SECONDARY: ${DEPS}
.depend: ${DEPS}
	cat $(shell ls .*.deps private/.*.deps extensions/.*.deps) > .depend

.%.ml.deps: %.ml
	 ${OCAMLDEP} ${LIBS} -syntax camlp4o $< > $@
.%.mli.deps: %.mli
	 ${OCAMLDEP} ${LIBS} -syntax camlp4o $< > $@

private/.%.ml.deps: private/%.ml
	 ${OCAMLDEP} ${LIBS} -syntax camlp4o $< > $@
private/.%.mli.deps: private/%.mli
	 ${OCAMLDEP} ${LIBS} -syntax camlp4o $< > $@

extensions/.%.ml.deps: extensions/%.ml
	 ${OCAMLDEP} ${LIBS} -syntax camlp4o $< > $@
extensions/.%.mli.deps: extensions/%.mli
	 ${OCAMLDEP} ${LIBS} -syntax camlp4o $< > $@

$(patsubst %.mli,.%.mli.deps,${INTF_NOP4}): .%.mli.deps: %.mli
	 ${OCAMLDEP} ${LIBS} $< > $@

$(patsubst %.ml,.%.ml.deps,${DUCE_FILES}): .%.ml.deps: %.ml
ifeq "${OCAMLDUCE}" "YES"
	 ${OCAMLDUCEDEP} ${LIBS} $< > $@
else
	echo > $@
endif

$(patsubst %.mli,.%.mli.deps,${DUCE_MLIS}): .%.mli.deps: %.mli
ifeq "${OCAMLDUCE}" "YES"
	 ${OCAMLDUCEDEP} ${LIBS} $< > $@
else
	echo > $@
endif

include .depend

endif
