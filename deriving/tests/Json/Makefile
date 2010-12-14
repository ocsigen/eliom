
DERIVING_DIR=../..
include ${DERIVING_DIR}/Makefile.config

COMMON_SOURCES=decl.mli decl.ml
BC_SOURCES=defs.ml generate.ml
JS_SOURCES=read.ml

DERIVING_PA_OBJS := \
	../../lib/pa_deriving.cma \
	${foreach module,${MODULES}, ../../lib/modules/${module}/pa_deriving_${module}.cma}
ifneq (${CAMLP4O_IS_BYTE},yes)
$(error TODO)
# DERIVING_PA_OBJS := ${DERIVING_PA_OBJS:cma=cmxa}
endif

.PHONY: all
all: read.js a.html run

INCDIRS = ../../lib/ ../../lib/modules ${addprefix ../../lib/modules/,${MODULES}} ${JS_OF_OCAML_DIR} ${LWT_DIR}
DERIVING_LIBS := \
	deriving \
	${foreach module,${MODULES}, ../../lib/modules/${module}/deriving_${module}} \

LIBS := lwt ${DERIVING_LIBS}

DERIVING = ${CAMLP4O} ${DERIVING_PA_OBJS}
export DERIVING
JS_OF_OCAML = ${CAMLP4O} ${DERIVING_PA_OBJS} -I ${JS_OF_OCAML_DIR} pa_js.cmo
export JS_OF_OCAML

.PHONY: generate
generate:
	${MAKE} -f ${OCAMLMAKEFILE} SOURCES="${COMMON_SOURCES} ${BC_SOURCES}" RESULT="generate" byte-code

.PHONY: read
read:
	${MAKE} -f ${OCAMLMAKEFILE} SOURCES="${COMMON_SOURCES} ${JS_SOURCES}" LIBS="lwt js_of_ocaml ${DERIVING_LIBS}" RESULT="read" byte-code

read.js: read
	js_of_ocaml -pretty read

.PHONY: run
run: generate
	./generate

clean::
	${MAKE} -f ${OCAMLMAKEFILE} SOURCES="${COMMON_SOURCES} ${BC_SOURCES}" RESULT="read" clean
	${MAKE} -f ${OCAMLMAKEFILE} SOURCES="${COMMON_SOURCES} ${JS_SOURCES}" RESULT="generate" clean
	rm -f read.js data.json

%.dump: %.ml
	${DERIVING} -printer "o" $^

-include ${OCAMLMAKEFILE}
