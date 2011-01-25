
DERIVING_DIR=../..
include ${DERIVING_DIR}/Makefile.config

.PHONY: all byte opt
all: all-modules byte-code-library native-code-library
byte: byte-code-modules byte-code-library
opt: native-code-modules native-code-library
clean: clean-modules clean-library

##

.PHONY: all-modules byte-code-modules native-code-modules
all-modules:
	@cd utils && ${MAKE} all
	@${foreach module,${MODULES}, \
	   cd ${module} && ${MAKE} all && cd .. &&} true

byte-code-modules:
	@cd utils && ${MAKE} byte
	@${foreach module,${MODULES}, \
	   cd ${module} && ${MAKE} byte && cd .. &&} true

native-code-modules:
	@cd utils && ${MAKE} opt
	@${foreach module,${MODULES}, \
	   cd ${module} && ${MAKE} opt && cd .. &&} true

.PHONY: byte-code-modules-syntax native-code-modules-syntax
byte-code-modules-syntax:
	@${foreach module,${MODULES}, \
	   cd ${module} && ${MAKE} byte-code-syntax && cd .. &&} true

native-code-modules-syntax:
	@${foreach module,${MODULES}, \
	   cd ${module} && ${MAKE} native-code-syntax && cd .. &&} true

.PHONY: byte-code-modules-runtime native-code-modules-runtime
byte-code-modules-runtime:
	@cd utils && ${MAKE} byte
	@${foreach module,${MODULES}, \
	   cd ${module} && ${MAKE} byte-code-runtime && cd .. &&} true

native-code-modules-runtime:
	@cd utils && ${MAKE} opt
	@${foreach module,${MODULES}, \
	   cd ${module} && ${MAKE} native-code-runtime && cd .. &&} true

.PHONY: clean-modules
clean-modules:
	@cd utils && ${MAKE} clean
	@${foreach module,${MODULES}, \
	   cd ${module} && ${MAKE} clean && cd .. &&} true

##

SYNTAX_OBJS = \
  $(foreach module,${MODULES}, \
    ${addprefix ${module}/, \
       ${shell cd ${module} && $(MAKE) --no-print-directory echo-syntax-obj}}) \
  ${addprefix utils/, \
    ${shell cd utils && $(MAKE) --no-print-directory echo-syntax-obj}}

echo-syntax-obj:
	@echo ${SYNTAX_OBJS}

RUNTIME_OBJS :=	\
  ${addprefix utils/, \
    ${shell cd utils && $(MAKE) --no-print-directory echo-runtime-obj}} \
  $(foreach module,${MODULES}, \
    ${addprefix ${module}/, \
       ${shell cd ${module} && $(MAKE) --no-print-directory echo-runtime-obj}})

echo-runtime-obj:
	@echo ${RUNTIME_OBJS}

.PHONY: byte-code-library native-code-library clean-library
byte-code-library: pa_deriving_modules.cma deriving.cma
native-code-library: pa_deriving_modules.cmxa pa_deriving_modules.cmxas deriving.cmxa deriving.cmxs

pa_deriving_modules.cma: $(SYNTAX_OBJS)
	${OCAMLC} -a -o $@ $^
deriving.cma: $(RUNTIME_OBJS)
	${OCAMLC} -a -o $@ $^

pa_deriving_modules.cmxa: $(SYNTAX_OBJS:cmo=cmx)
	${OCAMLOPT} -a -o $@ $^
pa_deriving_modules.cmxas: pa_deriving_modules.cmxa
	${OCAMLOPT} -shared -o $@ $^
deriving.cmxa: $(RUNTIME_OBJS:cmo=cmx)
	${OCAMLOPT} -a -o $@ $^
deriving.cmxs: deriving.cmxa
	${OCAMLOPT} -shared -o $@ $^

clean-library:
	rm -f pa_deriving_modules.cma deriving.cma
	rm -f pa_deriving_modules.cmxa pa_deriving_modules.cmxas deriving.cmxa deriving.cmxs
	rm -f pa_deriving_modules.a deriving.a

