
DERIVING_DIR=..
include ${DERIVING_DIR}/Makefile.config

AUXLIB_DIRS  = ../lib/ ../lib/modules ${foreach module,${MODULES},../lib/modules/${module}}

ifeq (${CAMLP4O_IS_BYTE},yes)
DERIVING = ${CAMLP4O} -I ../lib pa_deriving.cma
else
${error tests/Makefile needs to be fixed for native code pre-processor}
endif
export DERIVING

OCAMLFLAGS   = -w Aef
# -no-app-funct

SOURCES = defs.ml               \
          bimap.ml bimap.mli    \
          sigs.ml               \
          pickle_tests.ml       \
	  typeable_tests.ml     \
	  bounded_tests.ml      \
	  eq_tests.ml           \
	  dump_tests.ml         \
	  enum_tests.ml         \
	  show_tests.ml         \
          exp.ml                \
          inline.ml             \
	  # functor_tests.ml      \

LIBS    = deriving
RESULT  = tests
CLIBS 	=

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

run: byte-code
	@./tests

%.dump: %.ml
	${DERIVING} -printer "o" $^ > $@

clean::
	${MAKE} -C Json clean

include $(OCAMLMAKEFILE)
