include ../../Makefile.config
include Makefile.filelist

LIBS     := -I private ${addprefix -package ,${SERVER_PACKAGE} ${SERVER_SYNTAX}} \
	    -ppopt ../syntax/pa_include.cmo
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG} ${THREAD}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}  ${THREAD}
OCAMLDEP := $(OCAMLFIND) ocamldep

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

PREDEP := ${COMMON_FILES} ${COMMON_PFILES} ${COMMON_SFILES}

.SECONDARY: ${PREDEP}
${COMMON_FILES}: %: ../common/%
	-@mkdir -p $(dir $@)
	ln -sf $< $@
${COMMON_PFILES} ${COMMON_SFILES}: %: ../common/%
	-@mkdir -p $(dir $@)
	ln -sf ../$< $@

FILES := ${SFILES}

INTF_NOP4 :=

byte:: ${FILES:.ml=.cmo}
opt:: ${FILES:.ml=.cmx}


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

## INSTALL ##

include Makefile.filelist

odoc: ${SDOC:.mli=.odoc}

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
distclean: clean
	-rm -f *~ \#* .\#* .depend
	-rm -f ${PREDEP}
	-rm -f .*.deps private/.*.deps

## Manual dependencies for pa_include.
eliom_registration_base.cmi eliom_registration_base.cmo eliom_registration_base.cmx \
eliom_registration.cmi eliom_registration.cmo eliom_registration.cmx eliom_registration.odoc \
eliom_mkforms.cmi eliom_mkforms.cmo eliom_mkforms.cmx eliom_mkforms.odoc \
eliom_mkreg.cmi eliom_mkreg.odoc: ${COMMON_SFILES} $(wildcard sigs/*.mli)

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
