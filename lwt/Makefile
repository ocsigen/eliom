include ../Makefile.config

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

NAME = lwt

OCAMLC = $(OCAMLFIND) ocamlc $(DBG)
OCAMLOPT = $(OCAMLFIND) ocamlopt $(DBG)
OCAMLDEP = $(OCAMLFIND) ocamldep

OBJECTS  = pqueue.cmo lwt.cmo lwt_util.cmo lwt_unix.cmo
XOBJECTS = $(OBJECTS:cmo=cmx)

ARCHIVE  = $(NAME).cma
XARCHIVE = $(NAME).cmxa

SRCLIBREP = ../lib

REQUIRES = ssl
PREDICATES =


all: $(ARCHIVE) 
	-cp $(NAME).cma *.cmi $(SRCLIBREP)
opt: $(XARCHIVE)

.PHONY: depend

$(ARCHIVE): $(OBJECTS)
	$(OCAMLC) -a -o $(ARCHIVE) -package "$(REQUIRES)" \
	          -predicates "$(PREDICATES)" -I $(SRCLIBREP) $(OBJECTS)
# -linkpkg
$(XARCHIVE): $(XOBJECTS)
	$(OCAMLOPT) -a -o $(XARCHIVE) -package "$(REQUIRES)" \
	          -predicates "$(PREDICATES)" -I $(SRCLIBREP) $(XOBJECTS)
# -linkpkg

.SUFFIXES: .cmo .cmi .cmx .ml .mli

.ml.cmo:
	$(OCAMLC) -thread -package "$(REQUIRES)" -predicates "$(PREDICATES)" \
	           -I $(SRCLIBREP) -c $<
.mli.cmi:
	$(OCAMLC) -thread -package "$(REQUIRES)" -predicates "$(PREDICATES)" \
	           -I $(SRCLIBREP) -c $<
.ml.cmx:
	$(OCAMLOPT) -package "$(REQUIRES)" -predicates "$(PREDICATES)" \
	            -I $(SRCLIBREP) -c $<

depend: *.ml *.mli
	$(OCAMLDEP)  -I $(SRCLIBREP) *.ml *.mli > depend
include depend

install: all
	{ test ! -f $(XARCHIVE) || extra="$(XARCHIVE) "`basename $(XARCHIVE) .cmxa`.a; }; \
	$(OCAMLFIND) install $(NAME) *.mli *.cmi $(ARCHIVE) META $$extra

uninstall:
	$(OCAMLFIND) remove $(NAME)

clean::
	rm -f *.cmi *.cmo *.cmx *.cma *.cmxa *.a *.o *~ *.bak *.annot

clean::
	cd example && make clean
