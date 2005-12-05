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

SED = sed
RM = rm
MV = mv
DIFF = diff

OCAMLC = ocamlc.opt
OCAMLOPT = ocamlopt.opt

DOLLAR = $$

SUBDIRS = examples experimental private

SRC_ML = xML.ml xHTML.ml
SRC_MLI = $(SRC_ML:.ml=.mli)

DISTFILES = README ChangeLog COPYING Makefile $(SRC_MLI) $(SRC_ML) \
	examples/Makefile examples/www_ls.ml examples/homepage.ml \
	experimental/zipper.mli experimental/zipper.ml \
	experimental/document.mli experimental/document.ml

########################################################################

all: bin opt

bin: xhtml.cma
opt: xhtml.cmxa

xhtml.cmxa: xML.cmx xHTML.cmx
	$(OCAMLOPT) -a -o $@ $^

xhtml.cma: xML.cmo xHTML.cmo
	$(OCAMLC) -a -g -o $@ $^

%.cmx: %.ml
	$(OCAMLOPT) -c -o $@ $<

%.cmo: %.ml
	$(OCAMLC) -g -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<

clean:
	rm -f *.o *.cm[iox] *.cma *.cmxa *.a *~
	@for d in $(SUBDIRS); do test -d $$d && $(MAKE) -C $$d clean; done

########################################################################

doc: doc/index.html

doc/index.html: xML.mli xHTML.mli
	ocamldoc -html -d doc $^

xHTML.mli: xHTML.ml
	@$(SED) -n '/BEGIN INTERFACE/,/END INTERFACE/p' $< \
	  | $(SED) -e '/BEGIN INTERFACE/d' -e '/END INTERFACE/d' > $@.new
	@if $(DIFF) -q -I'\$(DOLLAR)Id: ' $@.new $@; then $(RM) $@.new; else $(MV) $@.new $@; fi

########################################################################

PUBLIC_HTML = $(HOME)/public_html/xhtml

VERSION = 20050620

SNAP = xhtml-$(VERSION).tar.gz

snap: $(SNAP)

$(SNAP): $(DISTFILES)
	tar cf - $^ | gzip -9 >$@

export: examples/homepage $(SNAP)
	cp $(SNAP) $(PUBLIC_HTML)
	tar xzf	$(SNAP) -C $(PUBLIC_HTML)
	examples/homepage $(SNAP) >$(PUBLIC_HTML)/index.html

examples/homepage: all
	$(MAKE) -C examples homepage

########################################################################

xHTML.cmo: xML.cmi xHTML.cmi 
xHTML.cmx: xML.cmx xHTML.cmi 
xHTML_sample.cmo: xHTML.cmi 
xHTML_sample.cmx: xHTML.cmx 
xML.cmo: xML.cmi 
xML.cmx: xML.cmi 

########################################################################
