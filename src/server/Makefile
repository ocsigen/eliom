include ../../Makefile.config

LIBS     := -I private -I extensions ${addprefix -package ,${SERVER_PACKAGE} ${SERVER_SYNTAX}}
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG} -thread
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG} -thread
OCAMLDEP := $(OCAMLFIND) ocamldep

OCAMLDUCEC   := ${OCAMLDUCEFIND} ocamlc${BYTEDBG} -thread
OCAMLDUCEOPT := ${OCAMLDUCEFIND} ocamlopt ${OPTDBG} -thread
OCAMLDUCEDEP := ${OCAMLDUCEFIND} ocamldep

all: byte opt

### Common files ###

COMMON_FILES  := $(notdir $(wildcard ../common/*.mli) $(wildcard ../common/*.ml))
COMMON_PFILES := $(addprefix private/, \
		     $(notdir $(wildcard ../common/private/*.mli) \
	                      $(wildcard ../common/private/*.ml)))

PREDEP := ${COMMON_FILES} ${COMMON_PFILES}

${COMMON_FILES}:
	ln -s ../common/$@ $@
${COMMON_PFILES}:
	ln -s ../../common/$@ $@

FILES := eliom_pervasives.ml \
	 eliom_wrap.ml \
	 eliom_cookies.ml \
	 eliom_common_base.ml eliom_common.ml \
	 eliom_xml.ml \
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
	 eliommod_cli.ml \
	 eliom_services_base.ml eliom_services.ml \
         eliom_config.ml \
	 private/eliommod_mkforms.ml \
         eliom_uri.ml \
	 eliom_mkforms.ml \
	 eliom_mkreg.ml \
	 eliom_comet_base.ml eliom_comet.ml  \
	 private/eliommod_react.ml \
	 eliom_output_base.ml eliom_output.ml \
         eliom_tools_common.ml \
	 eliom_tools.ml \
	 eliom_react.ml \
	 eliom_bus.ml \
	 extensions/eliom_s2s.ml extensions/eliom_openid.ml \
	 extensions/atom_feed.ml extensions/eliom_atom.ml
	 # eliom_extension_template.ml \

byte:: eliom.cma
opt:: eliom.cmxa
ifeq "${NATDYNLINK}" "YES"
opt:: eliom.cmxs
endif

eliom.cma: ${FILES:.ml=.cmo}
	${OCAMLC} -a -o $@ $^
eliom.cmxa: ${FILES:.ml=.cmx}
	${OCAMLOPT} -a -o $@ $^

# .mli to be compiled without camlp4
eliom_pervasives.cmi: \
%.cmi: %.mli
	$(OCAMLC) ${LIBS} -c $<

### OCamlduce ###

DUCE_FILES := eliom_duce.ml       \
	      eliom_duce_tools.ml \

DUCE_INTF :=

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

${DUCE_INTF:.mli=.cmi} ${DUCE_FILES:.ml=.cmi}: %.cmi: %.mli
	$(OCAMLDUCEC) ${LIBS} -c $<
${DUCE_FILES:.ml=.cmo}: %.cmo: %.ml
	$(OCAMLDUCEC) ${LIBS} -c $<
${DUCE_FILES:.ml=.cmx}: %.cmx: %.ml
	$(OCAMLDUCEOPT) ${LIBS} -c $<

%.cmi: %.mli
	$(OCAMLC) -syntax camlp4o ${LIBS} -c $<
%.cmo: %.ml
	$(OCAMLC) -syntax camlp4o ${LIBS} -c $<
%.cmx: %.ml
	$(OCAMLOPT) -syntax camlp4o ${LIBS} -c $<

%.cmxs: %.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $<

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
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-cd private && rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-cd extensions && rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f ${PREDEP}
distclean: clean
	-rm -f *~ \#* .\#*

## Dependencies

depend: ${PREDEP}
	ls ${FILES} ${FILES:.ml=.mli} | \
	  grep -v eliom_pervasives.mli | \
	  xargs $(OCAMLDEP) -syntax camlp4o  ${LIBS} > .depend
	${OCAMLDEP} ${LIBS} eliom_pervasives.mli >> .depend
ifeq "${OCAMLDUCE}" "YES"
	${OCAMLDUCEDEP} ${LIBS} ${DUCE_FILES} ${DUCE_FILES:.ml=.mli} >> .depend
endif

FORCE:
-include .depend
