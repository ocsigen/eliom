include ../../Makefile.config

OCAMLBUILD := ocamlbuild -use-ocamlfind

BYTE_TARGETS := ocamlbuild_eliom.cma
OPT_TARGETS := ocamlbuild_eliom.cmxa ocamlbuild_eliom.a

ifeq "${NATDYNLINK}" "YES"
    OPT_TARGETS += ocamlbuild_eliom.cmxs
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
	$(INSTALL) -m 644 _build/ocamlbuild_eliom.cmi $(OCAMLBUILDDIR)

install.opt: install_dir
	$(INSTALL) -m 644 $(addprefix _build/,$(OPT_TARGETS)) $(OCAMLBUILDDIR)

install.byte: install_dir
	$(INSTALL) -m 644 $(addprefix _build/,$(BYTE_TARGETS)) $(OCAMLBUILDDIR)

install: install.opt install.byte

uninstall:
	-rm -fr $(OCAMLBUILDDIR)
