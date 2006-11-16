include ../Makefile.config

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

FILES= ocsimisc.ml messages.ml ocsiconfig.ml parseconfig.ml

CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) ocamlopt $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
LIB = -package netstring -I +camlp4 -I ../lib
PP =

OBJS = $(FILES:.ml=.cmo)
CMI = $(FILES:.ml=.cmi)

all: $(OBJS)
	cp $(OBJS) $(CMI) ../lib


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

ocsiconfig.ml: ocsiconfig.ml.IN
	cat ocsiconfig.ml.IN \
	| sed s°0000000000000000°`head -n 1 ../VERSION`° \
	| sed s°_WARNING_°"Warning: this file has been generated from ocsiconfig.ml.IN - DO NOT MODIFY MANUALLY!"° \
	| sed s°_LOGDIR_°$(LOGDIR)° \
	| sed s°_STATICPAGESDIR_°$(STATICPAGESDIR)° \
	| sed s%_UP_%$(UPLOADDIR)%g \
	| sed s%_OCSIGENUSER_%$(OCSIGENUSER)%g \
	| sed s%_OCSIGENGROUP_%$(OCSIGENGROUP)%g \
	| sed s°_CONFIGDIR_°$(CONFIGDIR)° \
	> ocsiconfig.ml

doc:
	$(CAMLDOC) -d doc -html ocsicache.mli

clean:
	-rm -f *.cm[iox] *~ *.annot ocsiconfig.ml

depend: ocsiconfig.ml
	$(CAMLDEP) $(PP) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


