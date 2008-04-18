include ../Makefile.config

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

NAME = lwt


OCAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG)
OCAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG)
OCAMLDEP = $(OCAMLFIND) ocamldep

OBJECTS  = pqueue.cmo lwt.cmo lwt_util.cmo lwt_unix.cmo lwt_chan.cmo lwt_ssl.cmo lwt_timeout.cmo lwt_mutex.cmo lwt_preemptive.cmo lwt_lib.cmo lwt_pool.cmo
XOBJECTS = $(OBJECTS:cmo=cmx)

ARCHIVE  = $(NAME).cma
XARCHIVE = $(NAME).cmxa

REQUIRES = ssl
PREDICATES =

SRCLIBREP=$(LIBDIRS)

byte: $(ARCHIVE) 
#	-cp -f $(NAME).cma *.cmi $(SRCLIBREP)
opt: $(XARCHIVE)
#	-cp -f $(NAME).cmxa $(NAME).a *.cmi $(SRCLIBREP)

$(ARCHIVE): $(OBJECTS)
	$(OCAMLC) -a -o $(ARCHIVE) -package "$(REQUIRES)" \
	          -predicates "$(PREDICATES)" $(SRCLIBREP) $(OBJECTS)
# -linkpkg
$(XARCHIVE): $(XOBJECTS)
	$(OCAMLOPT) -a -o $(XARCHIVE) -package "$(REQUIRES)" \
	          -predicates "$(PREDICATES)" $(SRCLIBREP) $(XOBJECTS)
# -linkpkg

.SUFFIXES: .cmo .cmi .cmx .ml .mli

.ml.cmo:
	$(OCAMLC) -thread -package "$(REQUIRES)" -predicates "$(PREDICATES)" \
	           $(SRCLIBREP) -c $<
.mli.cmi:
	$(OCAMLC) -thread -package "$(REQUIRES)" -predicates "$(PREDICATES)" \
	           $(SRCLIBREP) -c $<
.ml.cmx:
	$(OCAMLOPT) -thread -package "$(REQUIRES)" -predicates "$(PREDICATES)" \
	            $(SRCLIBREP) -c $<

depend: *.ml *.mli
	$(OCAMLDEP)  $(SRCLIBREP) *.ml *.mli > depend
-include depend

install:
	mkdir -p "$(TEMPROOT)$(MODULEINSTALLDIR)"
	$(OCAMLFIND) install $(NAME) -destdir "$(TEMPROOT)$(MODULEINSTALLDIR)" *.mli *.cmi $(ARCHIVE) $(wildcard lwt.cmxa) $(wildcard lwt.a) META

uninstall:
	$(OCAMLFIND) remove $(NAME) -destdir "$(TEMPROOT)$(MODULEINSTALLDIR)"

clean::
	-rm -f *.cmi *.cmo *.cmx *.cma *.cmxa *.a *.o *~ *.bak *.annot

clean::
	cd example && $(MAKE) clean
