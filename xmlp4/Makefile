include ../Makefile.config

FILES = xhtmlpretty.ml xhtmlcompact.ml \
	xhtmlpretty_streams.ml xhtmlcompact_streams.ml \
	xhtml5pretty.ml xhtml5compact.ml \
	xhtml5pretty_streams.ml xhtml5compact_streams.ml

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
	$(MAKE) -C newocaml byte

optdep:
	$(MAKE) -C newocaml opt


xhtmlpretty.cma: xhtmlpretty.cmo xhtmlcompact.cmo xhtml5pretty.cmo xhtml5compact.cmo
	$(CAMLC) -a -o $@ $^

xhtmlpretty.cmxa: xhtmlpretty.cmx xhtmlcompact.cmx xhtml5pretty.cmx xhtml5compact.cmx
	$(CAMLOPT) -a -o $@ $^

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
	-$(MAKE) -C newocaml clean

depend:
#	touch oldocaml/.depend
#	touch newocaml/.depend
	$(CAMLDEP) -I ohl-xhtml $(PP) $(FILES) > .depend
	$(MAKE) -C newocaml depend


-include .depend
