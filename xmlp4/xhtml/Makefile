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


FILES=xML.ml xhtml5types.ml xHTML5.ml xHTML.ml xhtmltypes.ml

LIB = -package netstring
OCAMLC = $(OCAMLFIND) $(CAMLCNAME2) $(DBG) $(LIB)
OCAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)


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

xHTML5.cmo: xHTML5.ml
	$(OCAMLC) -c xHTML5.ml -pp 'camlp4r'

xHTML5.cmx: xHTML5.ml
	$(OCAMLOPT) -c xHTML5.ml  -pp 'camlp4r'

xHTML.cmo: xHTML.ml
	$(OCAMLC) -c xHTML.ml

xHTML.cmx: xHTML.ml
	$(OCAMLOPT) -c xHTML.ml


xhtmltypes.cmo: xhtmltypes.ml
	$(OCAMLC) -c $^  -pp 'camlp4r'

xhtmltypes.cmx: xhtmltypes.ml
	$(OCAMLOPT) -c $^  -pp 'camlp4r'

xhtml5types.cmo: xhtml5types.ml
	$(OCAMLC) -c $^ -pp 'camlp4r'

xhtml5types.cmx: xhtml5types.ml
	$(OCAMLOPT) -c $^ -pp 'camlp4r'


clean:
	rm -f *cm* 

depend: *.mli *.ml
	-rm -f xmllexer.ml camllexer.ml
#	ocamldep *.mli *.ml > .depend

FORCE:

-include .depend
