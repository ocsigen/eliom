include ../Makefile.config

ifeq "$(NATIVECODE)" "YES"
  ifeq "$(NATDYNLINK)" "YES"
  NATIVECODE_RUNTIME_DETECT=Dynlink.is_native
  DYNLINKWRAPPER=dynlink_wrapper.ml
  else
  NATIVECODE_RUNTIME_DETECT=false
  DYNLINKWRAPPER=dynlink_wrapper.mlnonatdynlink
  endif
else
NATIVECODE_RUNTIME_DETECT=false
DYNLINKWRAPPER=dynlink_wrapper.ml
endif

FILES= ocsigen_cache.ml ocsigen_lib_cli.ml ocsigen_lib.ml \
       ocsigen_messages.ml ocsigen_config.ml ocsigen_stream.ml \
       ocsigen_commandline.ml ocsigen_loader.ml \
       dynlink_wrapper.ml \
       polytables.ml

CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
LIB = -package lwt.unix,netstring,findlib,cryptokit -I +camlp4 $(LIBDIRS)
PP =

OBJS = $(FILES:.ml=.cmo)
OBJSOPT = $(FILES:.ml=.cmx)
CMI = ocsigen_getcommandline.cmi $(FILES:.ml=.cmi)

byte: $(OBJS) $(CMI) parsecommandline.cma donotparsecommandline.cma
#	cp -f $(OBJS) $(CMI) ../lib


# please do not forget CMXS!
CMXA= polytables.cmxa parsecommandline.cmxa \
	donotparsecommandline.cmxa
CMXS=$(CMXA:.cmxa=.cmxs)
DEPOPTBASE=$(OBJSOPT) $(CMI) $(CMXA)
ifeq "$(NATDYNLINK)" "YES"
  DEPOPT=$(DEPOPTBASE) $(CMXS)
else
  DEPOPT=$(DEPOPTBASE)
endif
opt: $(DEPOPT)
#	cp -f $(OBJSOPT) $(OBJSOPT:.cmx=.o) $(CMI) ../lib

parsecommandline.cma: commandline/ocsigen_getcommandline.cmo
	$(CAMLC) -a -o $@  $<

donotparsecommandline.cma: nocommandline/ocsigen_getcommandline.cmo
	$(CAMLC) -a -o $@  $<

parsecommandline.cmxa: commandline/ocsigen_getcommandline.cmx
	$(CAMLOPT) -a -o $@  $<

donotparsecommandline.cmxa: nocommandline/ocsigen_getcommandline.cmx
	$(CAMLOPT) -a -o $@  $<

polytables.cmxa: polytables.cmx
	$(CAMLOPT) -a -o $@  $<

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmxa .cmx .cmxs

.PHONY: doc depend

dynlink_wrapper.cmx:
	$(CAMLOPT) $(PP) -c -impl $(DYNLINKWRAPPER)

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

.cmxa.cmxs:
	$(CAMLOPT) -shared -linkall -o $@ $<


ocsigen_config.ml: ocsigen_config.ml.in
	cat ocsigen_config.ml.in \
	| sed s%0000000000000000%`head -n 1 ../VERSION`% \
	| sed s%_WARNING_%"Warning: this file has been generated from ocsigen_config.ml.in - DO NOT MODIFY MANUALLY!"% \
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
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *~ *.annot ocsigen_config.ml
	cd commandline; rm -f *.cm[iox]* *.annot *.o
	cd nocommandline; rm -f *.cm[iox]* *.annot *.o

depend: ocsigen_config.ml
	$(CAMLDEP) $(PP) $(LIBDIRS) $(FILES:.ml=.mli) $(FILES) $(CMXS) > .depend

FORCE:

-include .depend


