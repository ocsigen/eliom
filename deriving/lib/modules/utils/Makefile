
DERIVING_DIR=../../..
include ${DERIVING_DIR}/Makefile.config
export OCAMLMAKEFILE

all: byte opt
byte: byte-code-syntax byte-code-runtime
opt: native-code-syntax native-code-runtime

LIB_SOURCES = \
        deriving_stdlib.ml deriving_stdlib.mli \
	deriving_monad.mli deriving_monad.ml

echo-syntax-obj:
	@echo ${filter %.cmo,${SYNTAX_SOURCES:ml=cmo}}

echo-runtime-obj:
	@echo ${filter %.cmo,${LIB_SOURCES:ml=cmo}}

byte-code-syntax:
	@${MAKE} --no-print-directory -f ${OCAMLMAKEFILE} \
		SOURCES="${SYNTAX_SOURCES}" byte-code-nolink
byte-code-runtime:
	@${MAKE} --no-print-directory -f ${OCAMLMAKEFILE} \
		SOURCES="${LIB_SOURCES}" byte-code-nolink

native-code-syntax:
	@${MAKE} --no-print-directory -f ${OCAMLMAKEFILE} \
		SOURCES="${SYNTAX_SOURCES}" native-code-nolink
native-code-runtime:
	@${MAKE} --no-print-directory -f ${OCAMLMAKEFILE} \
		SOURCES="${LIB_SOURCES}" native-code-nolink

clean:
	@${MAKE} --no-print-directory -f ${OCAMLMAKEFILE} \
		SOURCES="${SYNTAX_SOURCES}" clean
	@${MAKE} --no-print-directory -f ${OCAMLMAKEFILE} \
		SOURCES="${LIB_SOURCES}" clean

.phony: META
META:
	echo >> ../../META
	cat META.in >> ../../META
	echo >> ../../META
