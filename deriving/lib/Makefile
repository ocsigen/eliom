
DERIVING_DIR=..
include ${DERIVING_DIR}/Makefile.config

.PHONY: all byte opt
all:
	@cd syntax && ${MAKE} all
	@${RMAKE} byte-code-preprocessor native-code-preprocessor
	@cd modules && ${MAKE} all
	@${RMAKE} library META

byte:
	@cd syntax && ${MAKE} ${if ${CAMLP4O_IS_BYTE},byte,all}
	@${RMAKE} byte-code-preprocessor	${if ${CAMLP4O_IS_BYTE},,native-code-preprocessor}
	@cd modules && ${MAKE} ${if ${CAMLP4O_IS_BYTE},,native-code-modules-syntax} byte
	@${RMAKE} library-byte META

opt:
	@cd syntax && ${MAKE} ${if ${CAMLP4O_IS_BYTE},all,opt}
	@${RMAKE} ${if ${CAMLP4O_IS_BYTE},byte-code-preprocessor} native-code-preprocessor
	@cd modules && ${MAKE} ${if ${CAMLP4O_IS_BYTE},byte-code-modules-syntax} opt
	@${RMAKE} library-opt META

clean::
	@cd syntax && ${MAKE} clean
	@cd modules && ${MAKE} clean

##

.PHONY: byte-code-preprocessor native-code-preprocessor
byte-code-preprocessor: byte-code-nolink
native-code-preprocessor: native-code-nolink native-code-pp-shared

##

.PHONY: native-code-pp-shared
native-code-pp-shared:: pa_deriving.cmxs
pa_deriving.cmxs: pa_deriving.cmx
	${OCAMLOPT} -shared -o $@ $^
clean::
	rm -f pa_deriving.cmxs

.PHONY: library library-byte library-opt
library: library-byte library-opt
library-byte:: pa_deriving.cma
library-opt:: pa_deriving.cmxa pa_deriving.cmxas

INCDIRS = syntax
USE_CAMLP4 = yes
SYNTAX_MODULES_OBJS = \
  $(addprefix modules/, \
    $(shell cd modules && ${RMAKE} echo-syntax-obj))
SYNTAX_OBJS = \
  syntax/pa_deriving_common.cmo ${SYNTAX_MODULES_OBJS} pa_deriving.cmo
SOURCES = pa_deriving.ml

pa_deriving.cma: $(SYNTAX_OBJS)
	${OCAMLC} -a -o $@ $^
pa_deriving.cmxa: $(SYNTAX_OBJS:cmo=cmx)
	${OCAMLOPT} -a -o $@ $^
pa_deriving.cmxas: pa_deriving.cmxa
	${OCAMLOPT} -shared -o $@ $^

clean::
	rm -f pa_deriving.cma pa_deriving.cmxa pa_deriving.cmxas pa_deriving.a

##

ifneq (${strip ${TYPECONV_DIR}},)

native-code-pp-shared:: pa_deriving_type_conv.cmxs
pa_deriving_type_conv.cmxs: pa_deriving_type_conv.cmx
	${OCAMLOPT} -shared -o $@ $^
clean::
	rm -f pa_deriving_type_conv.cmxs

library-byte:: pa_deriving_type_conv.cma
library-opt:: pa_deriving_type_conv.cmxa pa_deriving_type_conv.cmxas

INCDIRS += ${TYPECONV_DIR}
SYNTAX_TC_OBJS = \
  syntax/pa_deriving_common.cmo ${SYNTAX_MODULES_OBJS} pa_deriving_type_conv.cmo
SOURCES += pa_deriving_type_conv.ml

pa_deriving_type_conv.cma: $(SYNTAX_TC_OBJS)
	${OCAMLC} -a -o $@ $^
pa_deriving_type_conv.cmxa: $(SYNTAX_TC_OBJS:cmo=cmx)
	${OCAMLOPT} -a -o $@ $^
pa_deriving_type_conv.cmxas: pa_deriving_type_conv.cmxa
	${OCAMLOPT} -shared -o $@ $^

clean::
	rm -f pa_deriving_type_conv.cma pa_deriving_type_conv.cmxa \
	  pa_deriving_type_conv.cmxas pa_deriving_type_conv.a

endif

##

META: META.in modules/Makefile.modules modules/utils/META.in ${foreach module,${MODULES}, modules/${module}/Makefile}
	cp META.in META
	@cd modules/utils && ${MAKE} --print-directory META
	${QUIET}${foreach module,${MODULES}, \
	   cd modules/${module} && ${MAKE} --print-directory META && cd ../.. &&} true

clean::
	rm -f META

-include ${OCAMLMAKEFILE}

pa_deriving.cmo: syntax/pa_deriving_common.cmi
