include Makefile.filelist

OCAMLBUILD := ocamlbuild -use-ocamlfind

all: byte opt

byte:
	$(OCAMLBUILD) $(BYTE_TARGETS)

opt: byte
	$(OCAMLBUILD) $(OPT_TARGETS)

clean:
	$(OCAMLBUILD) -clean

distclean: clean

odoc:
	$(OCAMLBUILD) $(DOC)

install_dir:
	$(INSTALL) -m 755 -d $(OCAMLBUILDDIR)
	$(INSTALL) -m 644 $(addprefix _build/,$(INTF)) $(OCAMLBUILDDIR)

install.opt: install_dir
	$(INSTALL) -m 644 $(addprefix _build/,$(OPT_TARGETS)) $(OCAMLBUILDDIR)

install.byte: install_dir
	$(INSTALL) -m 644 $(addprefix _build/,$(BYTE_TARGETS)) $(OCAMLBUILDDIR)

install: install.opt install.byte

uninstall:
	-rm -fr $(OCAMLBUILDDIR)
