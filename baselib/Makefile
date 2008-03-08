include ../Makefile.config

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

ifeq "$(NATIVECODE)" "YES"
NATIVECODE_RUNTIME_DETECT=Dynlink.is_native
else
NATIVECODE_RUNTIME_DETECT=false
endif

FILES= ocsigen_lib.ml ocsigen_messages.ml ocsigen_config.ml ocsigen_loader.ml

CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
LIB = -package netstring,findlib -I +camlp4 $(LIBDIRS)
PP =

OBJS = $(FILES:.ml=.cmo)
OBJSOPT = $(FILES:.ml=.cmx)
CMI = $(FILES:.ml=.cmi)

byte: $(OBJS)
#	cp -f $(OBJS) $(CMI) ../lib

opt: $(OBJSOPT)
#	cp -f $(OBJSOPT) $(OBJSOPT:.cmx=.o) $(CMI) ../lib


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

ocsigen_config.ml: ocsigen_config.ml.IN
	cat ocsigen_config.ml.IN \
	| sed s%0000000000000000%`head -n 1 ../VERSION`% \
	| sed s%_WARNING_%"Warning: this file has been generated from ocsigen_config.ml.IN - DO NOT MODIFY MANUALLY!"% \
	| sed s%_LOGDIR_%$(LOGDIR)% \
	| sed s%_DATADIR_%$(DATADIR)%g \
	| sed s%_BINDIR_%$(BINDIR)%g \
	| sed s%_EXTRALIBDIR_%$(EXTRALIBDIR)%g \
	| sed s%_STATICPAGESDIR_%$(STATICPAGESDIR)% \
	| sed s%_UP_%$(UPLOADDIR)%g \
	| sed s%_OCSIGENUSER_%$(OCSIGENUSER)%g \
	| sed s%_OCSIGENGROUP_%$(OCSIGENGROUP)%g \
	| sed s%_OCSIGENNAME_%$(OCSIGENNAME)%g \
	| sed s%_COMMANDPIPE_%$(COMMANDPIPE)%g \
	| sed s%_CONFIGDIR_%$(CONFIGDIR)% \
	| sed s%_ISNATIVE_%$(NATIVECODE_RUNTIME_DETECT)%g \
	> ocsigen_config.ml

doc:
	$(CAMLDOC) -d doc -html ocsicache.mli

clean:
	-rm -f *.cm[ioax] *.cmxa *~ *.annot ocsigen_config.ml

depend: ocsigen_config.ml
	$(CAMLDEP) $(PP) $(LIBDIRS) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

-include .depend


