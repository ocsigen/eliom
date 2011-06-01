include ../../Makefile.config

LIBS     := -I private -I extensions ${addprefix -package ,${SERVER_PACKAGE} ${SERVER_SYNTAX}} \
	    -ppopt ../syntax/pa_include.cmo
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG} ${THREAD}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}  ${THREAD}
OCAMLDEP := $(OCAMLFIND) ocamldep

OCAMLDUCEC   := ${OCAMLDUCEFIND} ocamlc${BYTEDBG}  ${THREAD}
OCAMLDUCEOPT := ${OCAMLDUCEFIND} ocamlopt ${OPTDBG}  ${THREAD}
OCAMLDUCEDEP := ${OCAMLDUCEFIND} ocamldep

all: byte opt

### Common files ###

COMMON_FILES  := $(notdir $(wildcard ../common/*.mli) $(wildcard ../common/*.ml))
COMMON_PFILES := $(addprefix private/, \
		     $(notdir $(wildcard ../common/private/*.mli) \
	                      $(wildcard ../common/private/*.ml)))
COMMON_SFILES := $(addprefix sigs/, \
		     $(notdir $(wildcard ../common/sigs/*.mli) \
	                      $(wildcard ../common/sigs/*.ml)))

PREDEP := ${COMMON_FILES} ${COMMON_PFILES} eliom_duce_sigs.mli

${COMMON_FILES}:
	ln -fs ../common/$@ $@
${COMMON_PFILES} ${COMMON_SFILES}:
	ln -fs ../../common/$@ $@
eliom_duce_sigs.mli: eliom_duce_sigs.mlp
	camlp4o ../syntax/pa_include.cmo -printer o -intf $< > $@

FILES := eliom_pervasives_base.ml eliom_pervasives.ml \
	 eliom_wrap.ml \
	 eliom_cookies.ml \
	 eliom_common_base.ml eliom_common.ml \
	 eliom_request_info.ml \
	 private/eliommod_sessiongroups.ml \
	 private/eliommod_cookies.ml \
	 private/eliommod_sersess.ml \
         private/eliommod_datasess.ml \
	 private/eliommod_persess.ml \
	 private/eliommod_gc.ml \
	 private/eliommod_sessexpl.ml \
	 private/eliommod_sessadmin.ml \
	 private/eliommod_timeouts.ml \
	 eliom_state.ml \
	 eliom_references.ml \
	 eliom_extensions.ml \
	 private/eliommod_services.ml \
	 private/eliommod_naservices.ml \
         eliom_types_base.ml eliom_types.ml \
	 eliom_error_pages.ml \
	 private/eliommod_pagegen.ml \
	 private/eliommod.ml \
         eliom_parameters_base.ml eliom_parameters.ml \
	 eliom_process.ml \
	 private/eliommod_cli.ml \
	 eliom_services_base.ml eliom_services.ml \
         eliom_config.ml \
         eliom_uri.ml \
	 eliom_mkforms.ml \
	 eliom_mkreg.ml \
	 eliom_comet_base.ml eliom_comet.ml  \
	 eliom_output_base.ml eliom_output.ml \
         eliom_tools_common.ml \
	 eliom_tools.ml \
	 eliom_react.ml \
	 eliom_bus.ml \
	 extensions/eliom_s2s.ml extensions/eliom_openid.ml \
	 extensions/atom_feed.ml extensions/eliom_atom.ml

	 # eliom_extension_template.ml \

INTF_NOP4 := eliom_pervasives.mli

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

DUCE_INTF := eliom_duce_types.mli \
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


%.cmxs: %.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $<

${DUCE_INTF:.mli=.cmi}: %.cmi: %.mli
	$(OCAMLDUCEC) ${LIBS} -c $<
${DUCE_FILES:.ml=.cmo}: %.cmo: %.ml
	$(OCAMLDUCEC) ${LIBS} -c $<
${DUCE_FILES:.ml=.cmx}: %.cmx: %.ml
	$(OCAMLDUCEOPT) ${LIBS} -c $<

## INSTALL ##

include Makefile.filelist

install: INSTALL_IMPL:=${IMPL} ${NATIMPL}
install: raw_install

install.byte: INSTALL_IMPL:=${IMPL}
install.byte: raw_install

install.opt: INSTALL_IMPL:=${NATIMPL}
install.opt: raw_install

raw_install:
	${INSTALL} -m 755 -d ${SERVERDIR}
	${INSTALL} -m 644 ${INTF} ${INSTALL_IMPL} ${DOC} ${SERVERDIR}

uninstall:
	-rm -fr ${SERVERDIR}

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot *.deps
	-cd private && rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot *.deps
	-cd extensions && rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot *.deps
	-rm -f ${PREDEP}
distclean: clean
	-rm -f *~ \#* .\#*

## Dependencies

%.ml.deps: %.ml ${PREDEP}
	 ${OCAMLDEP} ${LIBS} -syntax camlp4o $< > $@
%.mli.deps: %.mli ${PREDEP}
	 ${OCAMLDEP} ${LIBS} -syntax camlp4o $< > $@
${INTF_NOP4:.mli=.mli.deps}: \
%.mli.deps: %.mli ${PREDEP}
	 ${OCAMLDEP} ${LIBS} $< > $@

${DUCE_FILES:.ml=.ml.deps}: %.ml.deps: %.ml ${PREDEP}
ifeq "${OCAMLDUCE}" "YES"
	 ${OCAMLDUCEDEP} ${LIBS} $< > $@
else
	echo > $@
endif

${DUCE_INTF:.mli=.mli.deps}: %.mli.deps: %.mli ${PREDEP}
ifeq "${OCAMLDUCE}" "YES"
	 ${OCAMLDUCEDEP} ${LIBS} $< > $@
else
	echo > $@
endif

.PHONY: predeps deps
predeps: ${PREDEP}
deps: ${patsubst %.ml,%.ml.deps,$(wildcard *.ml */*.ml)} \
      ${patsubst %.mli,%.mli.deps,$(wildcard *.mli */*.mli)}

.PHONY: depend
depend:
	@${MAKE} --no-print-directory predeps
	@${MAKE} --no-print-directory deps
	cat *.deps */*.deps > .depend

${FILES}: ${COMMON_SFILES}
${FILES:.ml=.mli}: ${COMMON_SFILES}

%.ml: $(wildcard sigs/*.mli)
%.mli: $(wildcard sigs/*.mli)

FORCE:
-include .depend
