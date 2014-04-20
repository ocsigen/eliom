include ../../Makefile.config

LIBS     := -I private ${addprefix -package ,${CLIENT_PACKAGE} ${CLIENT_SYNTAX} deriving.syntax} \
	    -ppopt ../syntax/pa_include.cmo

OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}
OCAMLDOC := $(OCAMLFIND) ocamldoc
OCAMLDEP := $(OCAMLFIND) ocamldep
OCAMLDOC := ${OCAMLFIND} ocamldoc -ppopt o -ppopt -printer

all: eliom_client.cma eliom_client_main.cmo

### Common files ###

COMMON_FILES  := $(notdir $(wildcard ../common/*.mli) $(wildcard ../common/*.ml))
COMMON_PFILES := $(addprefix private/, \
		     $(notdir $(wildcard ../common/private/*.mli) \
	                      $(wildcard ../common/private/*.ml)))
COMMON_SFILES := $(addprefix sigs/, \
		     $(notdir $(wildcard ../common/sigs/*.mli) \
	                      $(wildcard ../common/sigs/*.ml)))

PREDEP := ${COMMON_FILES} ${COMMON_PFILES} ${COMMON_SFILES}

.SECONDARY: ${PREDEP}
${COMMON_FILES}: %: ../common/%
	-@mkdir -p $(dir $@)
	ln -s $< $@
${COMMON_PFILES} ${COMMON_SFILES}: %: ../common/%
	-@mkdir -p $(dir $@)
	ln -s ../$< $@

### Library ###

FILES := private/eliommod_jstable.ml \
	 eliom_config.ml \
	 eliom_unwrap.ml eliom_lazy.ml \
	 eliom_lib_base.ml \
	 eliom_lib.ml \
	 private/eliommod_parameters.ml \
	 eliom_content_core.ml \
	 eliom_common_base.ml eliom_common.ml \
	 eliom_cookies_base.ml \
         private/eliommod_cookies.ml \
	 eliom_types_base.ml eliom_types.ml \
	 eliom_process.ml \
	 eliom_request_info.ml \
	 eliom_parameter_base.ml eliom_parameter.ml \
	 eliom_service_base.ml eliom_service.ml \
	 eliom_uri.ml \
	 eliom_request.ml \
         private/eliommod_dom.ml \
	 eliom_mkforms.ml eliom_registration_base.ml \
	 eliom_client.ml eliom_content.ml \
	 eliom_registration.ml \
	 eliom_comet_base.ml eliom_comet.ml \
	 eliom_bus.ml \
	 eliom_react.ml \
	 eliom_pervasives_base.ml \
	 eliom_pervasives.ml \
	 eliom_tools.ml \

INTF_NOP4 :=

eliom_client.cma: eliom_client_stubs.o ${FILES:.ml=.cmo}
	${OCAMLMKLIB} -o ${@:.cma=} $^

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

%.cmxs: %.cmx
	$(OCAMLOPT) -shared -linkall -o $@ $<
%.o: %.c
	$(OCAMLC) -c $<




## INSTALL ##

include Makefile.filelist

odoc: ${DOC:.mli=.odoc}

install: ${PREDEP}
	${INSTALL} -m 755 -d ${CLIENTDIR}
	${INSTALL} -m 644 ${INTF} ${IMPL} ${STUBS} ${DOC} ${CLIENTDIR}

uninstall:
	-rm -fr ${CLIENTDIR}

#####

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot *.deps *.odoc *.so
	-cd private && rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot *.odoc

distclean: clean
	-rm -f *~ \#* .\#*
	-rm -f ${PREDEP}
	-rm -f .*.deps private/.*.deps extensions/.*.deps

##  Manual dependencies for pa_include

eliom_registration.cmi eliom_registration.cmo eliom_registration.cmx eliom_registration.odoc \
eliom_mkforms.cmi eliom_mkforms.odoc: ${COMMON_SFILES} $(wildcard sigs/*.mli)

# Automatic dependencies

ifneq (${DEPEND},no)

## Ugly side effect to force ${PREDEP} creation before dependencies calculation
dummy_var := $(shell $(MAKE) DEPEND=no ${PREDEP} >/dev/null 2>&1)

DEPS := $(patsubst %,.%.deps,${COMMON_FILES}) \
	$(patsubst %,.%.deps,${COMMON_PFILES}) \
	$(patsubst %,.%.deps,$(wildcard *.mli *.ml)) \
        $(patsubst private/%,private/.%.deps,$(wildcard private/*.mli private/*.ml)) \

.SECONDARY: ${DEPS}
.depend: ${DEPS}
	cat $(shell ls .*.deps private/.*.deps) > .depend

.%.ml.deps: %.ml
	 ${OCAMLDEP} ${LIBS} -syntax camlp4o $< > $@
.%.mli.deps: %.mli
	 ${OCAMLDEP} ${LIBS} -syntax camlp4o $< > $@

private/.%.ml.deps: private/%.ml
	 ${OCAMLDEP} ${LIBS} -syntax camlp4o $< > $@
private/.%.mli.deps: private/%.mli
	 ${OCAMLDEP} ${LIBS} -syntax camlp4o $< > $@

$(patsubst %.mli,.%.mli.deps,${INTF_NOP4}): .%.mli.deps: %.mli
	 ${OCAMLDEP} ${LIBS} $< > $@

include .depend

endif
