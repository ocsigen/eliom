include ../Makefile.config

FILES= eliommod.ml eliom.ml eliomboxes.ml \
       ocsigenmod.ml ocsigen.ml ocsigenboxes.ml staticmod.ml

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

ifeq "$(PROFILING)" "YES"
CAMLCNAME= ocamlcp -p a
else
CAMLCNAME= ocamlc
endif

ifeq "$(OCAMLDUCE)" "YES"
DUCEFILES= xhtml1_strict.ml ocsigenduce.ml
# rss2.ml ocsigenrss.ml

  ifeq "$(BYTECODE)" "YES"
  DUCEMODBYTE=ocsigenduce.cma
# ocsigenrss.cma
  else
  DUCEMODBYTE=
  endif

  ifeq "$(NATIVECODE)" "YES"
  DUCEMODOPT=ocsigenduce.cmxs
# ocsigenrss.cmxs
  else
  DUCEMODOPT=
  endif

OCAMLFIND=$(OCAMLDUCEFIND)
else
DUCEFILES=
DUCEMODBYTE=
DUCEMODOPT=
endif


ifeq "$(NEWOCAMLNET)" "YES"
NETSYS=netsys
NETSYSCMA=netsys.cma
NETSYSCMXA=netsys.cmxa
else
NETSYS=
NETSYSCMA=
NETSYSCMXA=
endif

LIB=$(LIBDIRS)

CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
LIB1 = -thread -package netstring,$(NETSYS) $(LIBDIRS)
PP = 
PP2 = 
# -pp "camlp4o ../lib/xhtmlsyntax.cma -- -loc loc"


# cma and cmo to be linked in the server:

OBJS = $(FILES:.ml=.cmo) $(DUCEFILES:.ml=.cmo)

OBJSOPT = $(FILES:.ml=.cmx) $(DUCEFILES:.ml=.cmx)


byte: $(OBJS) eliommod.cma ocsigenmod.cma staticmod.cmo $(DUCEMODBYTE)

opt: $(OBJSX) eliommod.cmxs ocsigenmod.cmxs staticmod.cmxs $(DUCEMODOPT)


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

eliommod.cma: eliommod.cmo eliom.cmo eliomboxes.cmo
	$(CAMLC) -a -o eliommod.cma eliommod.cmo eliom.cmo eliomboxes.cmo

eliommod.cmxs: eliommod.cmx eliom.cmx eliomboxes.cmx
	$(CAMLOPT) -shared -linkall -o eliommod.cmxs eliommod.cmx eliom.cmx \
	eliomboxes.cmx

ocsigenmod.cma: ocsigenmod.cmo ocsigen.cmo ocsigenboxes.cmo
	$(CAMLC) -a -o ocsigenmod.cma ocsigenmod.cmo ocsigen.cmo \
	ocsigenboxes.cmo

ocsigenmod.cmxs: ocsigenmod.cmx ocsigen.cmx ocsigenboxes.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenmod.cmxs ocsigenmod.cmx \
	ocsigen.cmx ocsigenboxes.cmx

staticmod.cmxs: staticmod.cmx
	$(CAMLOPT) -shared -linkall -o staticmod.cmxs staticmod.cmx

ocsigenduce.cma: xhtml1_strict.cmo ocsigenduce.cmo
	$(CAMLC) -a -o ocsigenduce.cma xhtml1_strict.cmo ocsigenduce.cmo 

ocsigenduce.cmxs: xhtml1_strict.cmx ocsigenduce.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenduce.cmxs xhtml1_strict.cmx ocsigenduce.cmx 

ocsigenrss.cma: rss2.cmo ocsigenrss.cmo
	$(CAMLC) -a -o ocsigenrss.cma $^

ocsigenrss.cmxs: rss2.cmx ocsigenrss.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenrss.cmxs $^

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

#doc:
#	$(CAMLDOC) -d doc -html eliom.mli ocsigen.mli extensions.mli

.PHONY: clean
clean:
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *~ doc/* $(NAME) $(NAME).opt *.annot

depend:
	$(CAMLDEP) $(PP2) $(FILES:.ml=.mli) $(FILES) $(DUCEFILES:.ml=.mli) $(DUCEFILES) > .depend


FORCE:

include .depend


