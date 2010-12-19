
DERIVING_DIR=.
include ${DERIVING_DIR}/Makefile.config

all:
	${MAKE} -C lib all
byte:
	${MAKE} -C lib byte
opt:
	${MAKE} -C lib opt
clean::
	${MAKE} -C lib clean

test:
	${MAKE} -C tests run
clean::
	${MAKE} -C tests clean

MODULE_UTIL_OBJS = \
  $(addprefix lib/modules/utils/, \
    ${shell cd lib/modules/utils && ${MAKE} --no-print-directory echo-runtime-obj})

LIBINSTALL_FILES = \
 lib/syntax/pa_deriving_common.cmi lib/syntax/pa_deriving_common.cmo \
 lib/syntax/pa_deriving_common.cmx \
 lib/syntax/pa_deriving_common.cmxs lib/syntax/pa_deriving_common.o \
 lib/syntax/pa_deriving_common.cmi lib/syntax/*.mli \
 lib/pa_deriving.cmo lib/pa_deriving.cmx lib/pa_deriving.cmxs \
 lib/pa_deriving.o \
 lib/pa_deriving.cma lib/pa_deriving.cmxa lib/pa_deriving.cmxas \
 lib/pa_deriving.a \
 $(if $(strip $(TYPECONV)), \
 lib/pa_deriving_type_conv.cmo lib/pa_deriving_type_conv.cmx \
 lib/pa_deriving_type_conv.cmxs lib/pa_deriving_type_conv.o \
 lib/pa_deriving_type_conv.cma lib/pa_deriving_type_conv.cmxa \
 lib/pa_deriving_type_conv.cmxas lib/pa_deriving_type_conv.a, ) \
 lib/modules/pa_deriving_modules.cma lib/modules/pa_deriving_modules.cmxa \
 lib/modules/pa_deriving_modules.cmxas lib/modules/pa_deriving_modules.a \
 lib/modules/deriving.cma \
 lib/modules/deriving.cmxa lib/modules/deriving.a \
 ${MODULE_UTIL_OBJS} \
 $(foreach module,${MODULES}, lib/modules/${module}/pa_deriving_${module}.cma) \
 $(foreach module,${MODULES}, lib/modules/${module}/pa_deriving_${module}.cmxa) \
 $(foreach module,${MODULES}, lib/modules/${module}/pa_deriving_${module}.cmxas) \
 $(foreach module,${MODULES}, lib/modules/${module}/pa_deriving_${module}.a) \
 $(foreach module,${MODULES}, lib/modules/${module}/*.cmi) \
 $(foreach module,${MODULES}, lib/modules/${module}/*.mli) \
 $(foreach module,${MODULES}, lib/modules/${module}/deriving_${module}.cma) \
 $(foreach module,${MODULES}, lib/modules/${module}/deriving_${module}.cmxa) \
 $(foreach module,${MODULES}, lib/modules/${module}/deriving_${module}.a) \
 lib/META
 # ${MODULE_UTIL_OBJS:cmo=mli}

.PHONY: install
install:
	$(QUIET)printf "\nInstalling library with ocamlfind\n"
	$(OCAMLFIND) install deriving $(LIBINSTALL_FILES)
	$(QUIET)printf "\nInstallation successful.\n"

.PHONY: install-byte
install-byte:
	$(QUIET)printf "\nInstalling byte-code library with ocamlfind\n"
	$(OCAMLFIND) install deriving \
	  $(filter-out %.o %.cmx %.a %.cmxa,$(LIBINSTALL_FILES))
	$(QUIET)printf "\nInstallation successful.\n"

.PHONY: install-opt
install-opt:
	$(QUIET)printf "\nInstalling native-code library with ocamlfind\n"
	$(OCAMLFIND) install deriving $(filter-out %.cmo %.cma,$(LIBINSTALL_FILES))
	$(QUIET)printf "\nInstallation successful.\n"

.PHONY: uninstall
uninstall:
	$(QUIET)printf "\nUninstalling library with ocamlfind\n"
	$(OCAMLFIND) remove deriving
	$(QUIET)printf "\nUninstallation successful.\n"

