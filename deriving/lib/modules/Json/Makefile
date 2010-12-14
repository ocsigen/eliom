NAME := $(notdir $(PWD))

PACK_DEPENDS   = js_of_ocaml
UTIL_DEPENDS   = stdlib
MODULE_DEPENDS =

export PACKS = js_of_ocaml

RUNTIME_SOURCES = \
	json.ml json.mli \
	deriving_bi_outbuf.mli deriving_bi_outbuf.ml \
	deriving_Json_lexer.mli deriving_Json_lexer.ml \
	deriving_Json.mli deriving_Json.ml \
	deriving_Json_stdlib.mli deriving_Json_stdlib.ml

JS_OF_OCAML = ${CAMLP4O} -I ${JS_OF_OCAML_DIR} pa_js.cmo
export JS_OF_OCAML

include ../Makefile.modules