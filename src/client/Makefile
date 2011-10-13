include ../../Makefile.config

LIBS     := -I private ${addprefix -package ,${CLIENT_PACKAGE} ${CLIENT_SYNTAX}} \
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

${COMMON_FILES}:
	ln -sf ../common/$@ $@
${COMMON_PFILES} ${COMMON_SFILES}:
	ln -sf ../../common/$@ $@

### Library ###

FILES := eliom_unwrap.ml eliom_lazy.ml \
	 eliom_pervasives_base.ml eliom_pervasives.ml \
	 eliom_common_base.ml eliom_common.ml \
	 eliom_cookies_base.ml \
         private/eliommod_cookies.ml \
	 eliom_types_base.ml eliom_types.ml \
	 eliom_process.ml \
	 eliom_config.ml \
	 eliom_request_info.ml \
	 eliom_parameters_base.ml eliom_parameters.ml \
	 eliom_services_base.ml eliom_services.ml \
	 eliom_uri.ml \
	 eliom_request.ml \
         private/eliommod_dom.ml \
	 eliom_client.ml \
	 eliom_mkforms.ml \
	 eliom_output_base.ml eliom_output.ml \
	 eliom_comet_base.ml eliom_comet.ml \
	 eliom_bus.ml \
	 eliom_react.ml \

INTF_NOP4 := eliom_pervasives.mli \

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
	${OCAMLDOC} -syntax camlp4o ${LIBS} -dump $@ $<
${INTF_NOP4:.mli=.odoc}: \
%.odoc: %.mli
	${OCAMLDOC} ${LIBS} -dump $@ $<

%.cmxs: %.cmx
	$(OCAMLOPT) -shared -linkall -o $@ $<
%.o: %.c
	$(OCAMLC) -c $<




## INSTALL ##

include Makefile.filelist

odoc: ${DOC:.mli=.odoc}

install:
	${INSTALL} -m 755 -d ${CLIENTDIR}
	${INSTALL} -m 644 ${INTF} ${IMPL} ${STUBS} ${DOC} ${CLIENTDIR}

uninstall:
	-rm -fr ${CLIENTDIR}

#####

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot *.deps
	-cd private && rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot *.deps
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
