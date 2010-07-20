# $Id: Makefile,v 1.18 2005/06/20 17:57:58 ohl Exp $
#
# Copyright (C) 2004 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
#
# XHTML is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by 
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# XHTML is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
########################################################################
include ../../Makefile.config


FILES=uri.ml xML.ml xhtml5types.ml xHTML5.ml xhtmltypes.ml xHTML.ml

LIB = -package netstring
OCAMLC = $(OCAMLFIND) $(CAMLCNAME2) $(DBG) $(LIB)
OCAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
SED=sed
RM=rm
MV=mv
DIFF=diff
DOLLAR=$$
byte: ../xhtml.cma
	cp -f $(FILES:.ml=.cmi) ..

opt: ../xhtml.cmxa
	cp -f $(FILES:.ml=.cmi) $(FILES:.ml=.o) ..

../xhtml.cma: $(FILES:.ml=.cmo)
	$(OCAMLC) -a $^ -o $@

../xhtml.cmxa: $(FILES:.ml=.cmx)
	$(OCAMLOPT) -a $^ -o $@

xML.cmi: xML.mli
	$(OCAMLC) -c $^

xML.cmo: xML.cmi xML.ml
	$(OCAMLC) -c xML.ml

xML.cmx: xML.cmi xML.ml
	$(OCAMLOPT) -c xML.ml

%.cmi: %.mli	
	$(OCAMLC) -c $^

xML.mli: xML.ml
	true
%.mli: %.ml
	awk ' BEGIN { ok = 1; } /^include/ { next; } /\(\*I/ { $$1=""; $$NF=""; } \
	 /= struct/ { ok = 0; } ok == 1; /end/ { ok = 1; }' $^ | sed -e s/struct/sig/g -e 's/module \(.*\) =/module \1 :/g'> $@

xHTML5.cmo: xHTML5.cmi xHTML5.ml
	$(OCAMLC) -c xHTML5.ml

xHTML5.cmx: xHTML5.cmi xHTML5.ml
	$(OCAMLOPT) -c xHTML5.ml

xHTML.cmo: xHTML.cmi xHTML.ml
	$(OCAMLC) -c xHTML.ml

xHTML.cmx: xHTML.cmi xHTML.ml
	$(OCAMLOPT) -c xHTML.ml

uri.cmo: uri.cmi uri.ml	
	$(OCAMLC) uri.ml -c
uri.cmx: uri.cmi uri.ml	
	$(OCAMLOPT) uri.ml -c

uri.cmi: uri.mli
	$(OCAMLC) -c $<

xhtmltypes.mli:
xhtml5types.mli: 

xhtmltypes.cmx: xhtmltypes.cmi xhtmltypes.ml
	$(OCAMLOPT) -c xhtmltypes.ml

xhtmltypes.cmo: xhtmltypes.cmi xhtmltypes.ml
	$(OCAMLC) -c xhtmltypes.ml

xhtml5types.cmo: xhtml5types.cmi xhtml5types.ml
	$(OCAMLC) -c xhtml5types.ml

xhtml5types.cmx: xhtml5types.cmi xhtml5types.ml
	$(OCAMLOPT) -c xhtml5types.ml

xHTML.mli: xHTML.ml
	@$(SED) -n '/BEGIN INTERFACE/,/END INTERFACE/p' $< \
	  | $(SED) -e '/BEGIN INTERFACE/d' -e '/END INTERFACE/d' > $@.new
	@if [ -e $@ ] && $(DIFF) -q -I'\$(DOLLAR)Id: ' $@.new $@; then $(RM) $@.new; else $(MV) $@.new $@; fi
xHTML5.mli: xHTML5.ml
	@$(SED) -n '/BEGIN INTERFACE/,/END INTERFACE/p' $< \
	  | $(SED) -e '/BEGIN INTERFACE/d' -e '/END INTERFACE/d' > $@.new
	@if [ -e $@ ] && $(DIFF) -q -I'\$(DOLLAR)Id: ' $@.new $@; then $(RM) $@.new; else $(MV) $@.new $@; fi
clean:
	rm -f *cm* xHTML5.mli xHTML.mli xhtmltypes.mli xhtml5types.mli

depend: *.mli *.ml
	-rm -f xmllexer.ml camllexer.ml
#	ocamldep *.mli *.ml > .depend

FORCE:

-include .depend
