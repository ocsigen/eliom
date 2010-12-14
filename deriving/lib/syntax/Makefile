
DERIVING_DIR=../..
include ${DERIVING_DIR}/Makefile.config

SOURCES = id.ml                 \
          utils.ml              \
          type.mli type.ml      \
	  context.mli context.ml \
          base.mli base.ml      \
          extend.ml             \

LIB_PACK_NAME = pa_deriving_common
USE_CAMLP4 = yes
RESULT = pa_deriving_common

.PHONY: all byte opt
all: byte opt
byte: context.ml pack-byte-code
opt: context.ml pack-native-code pa_deriving_common.cmxs

pa_deriving_common.cmxs: pa_deriving_common.cmx
	${OCAMLOPT} -shared -o $@ $^

clean::
	rm -f pa_deriving_common.cmxs

context.ml: context.mli
	cp context.mli context.ml

-include $(OCAMLMAKEFILE)
