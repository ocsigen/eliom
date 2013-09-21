include ../../Makefile.config

OCAMLBUILD := ocamlbuild -use-ocamlfind

NAME := ocamlbuild_eliom

BYTE_TARGETS := $(NAME)_core.cma $(NAME).cma
OPT_TARGETS := $(NAME)_core.cmxa $(NAME)_core.a $(NAME).cmxa $(NAME).a

ifeq "${NATDYNLINK}" "YES"
    OPT_TARGETS += $(NAME)_core.cmxs $(NAME).cmxs
endif

all: byte opt

byte:
	$(OCAMLBUILD) $(BYTE_TARGETS)

opt:
	$(OCAMLBUILD) $(OPT_TARGETS)

clean:
	$(OCAMLBUILD) -clean

distclean: clean

install_dir:
	$(INSTALL) -m 755 -d $(OCAMLBUILDDIR)
	$(INSTALL) -m 644 _build/$(NAME)_core.cmi _build/$(NAME).cmi $(OCAMLBUILDDIR)

install.opt: install_dir
	$(INSTALL) -m 644 $(addprefix _build/,$(OPT_TARGETS)) $(OCAMLBUILDDIR)

install.byte: install_dir
	$(INSTALL) -m 644 $(addprefix _build/,$(BYTE_TARGETS)) $(OCAMLBUILDDIR)

install: install.opt install.byte

uninstall:
	-rm -fr $(OCAMLBUILDDIR)
