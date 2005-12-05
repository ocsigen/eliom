# $Id: Makefile,v 1.11 2004/01/27 10:57:18 ohl Exp $
########################################################################

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
LIBS = -I .. 

SRC_MLI = 
SRC_ML = www_ls.ml homepage.ml

SRC_CMI = $(SRC_MLI:.mli=.cmi)
SRC_CMO = $(SRC_ML:.ml=.cmo)
SRC_CMX = $(SRC_ML:.ml=.cmx)

all: bin opt

bin: $(SRC_ML:.ml=.bin)
opt: $(SRC_ML:.ml=)

www_ls: www_ls.cmx
	$(OCAMLOPT) $(LIBS) -o $@ xhtml.cmxa $^

www_ls.bin: www_ls.cmo 
	$(OCAMLC) -g $(LIBS) -o $@ xhtml.cma $^

homepage: homepage.cmx
	$(OCAMLOPT) $(LIBS) -o $@ xhtml.cmxa str.cmxa $^

homepage.bin: homepage.cmo 
	$(OCAMLC) -g $(LIBS) -o $@ xhtml.cma str.cma $^ 

%.cmx: %.ml
	$(OCAMLOPT) -c $(LIBS) -o $@ $<

%.cmo: %.ml
	$(OCAMLC) -g -c $(LIBS) -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c $(LIBS) -o $@ $<

clean:
	rm -f homepage www_ls *.bin *.o *.cm[iox] *~ *.html

########################################################################
