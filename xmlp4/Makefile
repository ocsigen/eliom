include ../Makefile.config

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

FILES = xhtmlpretty.ml xhtmlcompact.ml \
	xhtmlpretty_streams.ml xhtmlcompact_streams.ml

CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB) -package camlp4
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB) -package camlp4
CAMLDEP = $(OCAMLFIND) ocamldep $(LIB) -package camlp4
LIB = $(LIBDIRS)
# PP = -syntax camlp4o -ppopt "pa_macro.cmo"
# PPLEXER = -syntax camlp4of -ppopt "-loc loc"

OBJS = $(FILES:.ml=.cmo)
OBJSOPT = $(FILES:.ml=.cmx)


byte: bytedep $(OBJS) xhtmlpretty.cma
xmlp4pre.byte: bytedep
xmlp4pre.opt: optdep
opt: optdep $(OBJSOPT) xhtmlpretty.cmxa

bytedep:
ifeq "$(OCAMLVERSION)" "OLD"
	$(MAKE) -C oldocaml byte
else 
	$(MAKE) -C newocaml byte
endif

optdep:
ifeq "$(OCAMLVERSION)" "OLD"
	$(MAKE) -C oldocaml opt
else 
	$(MAKE) -C newocaml opt
endif


xhtmlpretty.cma: xhtmlpretty.cmo xhtmlcompact.cmo
	$(CAMLC) -a -o xhtmlpretty.cma xhtmlpretty.cmo xhtmlcompact.cmo

xhtmlpretty.cmxa: xhtmlpretty.cmx xhtmlcompact.cmx
	$(CAMLOPT) -a -o xhtmlpretty.cmxa xhtmlpretty.cmx xhtmlcompact.cmx

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(PP) -c $<
.mli.cmi:
	$(CAMLC) $(PP) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<



clean:
#	touch oldocaml/.depend
#	touch newocaml/.depend
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *~ *.annot
	-$(MAKE) -C oldocaml clean
	-$(MAKE) -C newocaml clean

depend:
#	touch oldocaml/.depend
#	touch newocaml/.depend
	$(CAMLDEP) -I ohl-xhtml $(PP) $(FILES) > .depend
ifeq "$(OCAMLVERSION)" "OLD"
	$(MAKE) -C oldocaml depend
else 
	$(MAKE) -C newocaml depend
endif


-include .depend
