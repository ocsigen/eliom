include ../Makefile.config

FILES=$(wildcard *.ml)

TUTOWIKI= tutoeliom1.wiki tutoeliom2.wiki tutoeliom3.wiki tutoeliom4.wiki

ifeq "$(OCAMLDUCE)" "YES"
DUCEMODBYTE=exampleduce.cmo
DUCEMODOPT=$(CMXSDUCE)
else
DUCEMODBYTE=
DUCEMODOPT=
endif

ifeq "$(NATDYNLINK)" "YES"
CMXS= $(FILES:.ml=.cmxs)
CMXSDUCE=exampleduce.cmxs
else
CMXS=
endif

#ELIOMCLIENTDIR := $(shell ocamlfind query ocsigen.eliom_client)
#PAELIOMCLIENTDIR := $(shell ocamlfind query ocsigen.eliom_client_syntax)
LWTDIR := $(shell ocamlfind query lwt)
REACTDIR := $(shell ocamlfind query react)
ELIOMCLIENTDIR := ../eliom/client
PAELIOMCLIENTDIR := ../eliom

LIB = $(LIBDIRS) -package lwt.unix,netstring,camlp4,react,lwt.react
CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep $(LIB)
PP = -syntax camlp4o -ppopt "../xmlp4/xhtmlsyntax.cma" -ppopt "-loc loc"

OBJS = $(FILES:.ml=.cmo)
OBJSOPT = $(CMXS)

byte: $(OBJS) tutoeliom.cma $(DUCEMODBYTE) miniwiki.byte $(TUTOWIKI) ../files/tutorial/tutoeliom_ocsigen2_client.js

opt: $(OBJSOPT) tutoeliom.cmxs $(DUCEMODOPT) miniwiki.opt $(TUTOWIKI) ../files/tutorial/tutoeliom_ocsigen2_client.js

exampleduce.cmo: ocamlduce/exampleduce.cmo
	cp ocamlduce/exampleduce.cmo .

ocamlduce/exampleduce.cmo: ocamlduce/exampleduce.ml tutoeliom.cmo
	$(OCAMLDUCEFIND) ocamlc $(DBG) $(LIB) -c ocamlduce/exampleduce.ml

exampleduce.cmx: ocamlduce/exampleduce.cmx
	cp ocamlduce/exampleduce.cmx ocamlduce/exampleduce.o .

ocamlduce/exampleduce.cmx: ocamlduce/exampleduce.ml
	$(OCAMLDUCEFIND) ocamlopt $(DBG) $(LIB) -c ocamlduce/exampleduce.ml

exampleduce.cmxs: exampleduce.cmx
	$(OCAMLDUCEFIND) ocamlopt $(DBG) $(LIB) -linkall -shared \
	  -o ocamlduce/$*.cmxs $<
	cp ocamlduce/exampleduce.cmxs .

miniwiki.byte:
	$(MAKE) -C miniwiki byte

miniwiki.opt:
	$(MAKE) -C miniwiki opt

tutoeliom1.wiki: tutoeliom.ml
	cat tutoeliom.ml | sed '1,/(\*wiki\*/d' | sed '/%<||2>%/,$$ d' | /bin/sh ./tutomake.sh > tutoeliom1.wiki

tutoeliom2.wiki: tutoeliom.ml
	cat tutoeliom.ml | sed '/%<||3>%/,$$ d' | sed '1,/%<||2>%/d' | /bin/sh ./tutomake.sh > tutoeliom2.wiki

tutoeliom3.wiki: tutoeliom.ml
	cat tutoeliom.ml | sed '/%<||4>%/,$$ d' | sed '1,/%<||3>%/d' | /bin/sh ./tutomake.sh > tutoeliom3.wiki

tutoeliom4.wiki: tutoeliom_ocsigen2.eliom
	cat tutoeliom_ocsigen2.eliom | sed '/%<||2>%/,$$ d' | sed '1,/%<||1>%/d' | /bin/sh ./tutomake.sh > tutoeliom4.wiki

tutoeliom_ocsigen2_client.js: tutoeliom_ocsigen2_client
	js_of_ocaml -pretty $(ELIOMCLIENTDIR)/eliom_client.js $< -o $@

../files/tutorial/tutoeliom_ocsigen2_client.js: tutoeliom_ocsigen2_client.js
	cp -f $< $@

tutoeliom_ocsigen2_client: $(ELIOMCLIENTDIR)/eliom_client.cma client/tutoeliom_ocsigen2_client.ml
	ocamlfind ocamlc -o $@ \
	  -I ../eliom/client \
	  -package lwt.react,react,js_of_ocaml -syntax camlp4o -linkpkg \
	  -verbose \
	  ../eliom/client/eliom_client.cma client/tutoeliom_ocsigen2_client.ml

tutoeliom_ocsigen2.ml: tutoeliom_ocsigen2.eliom ../eliom/pa_eliom_client.cmo
	camlp4of $(PAELIOMCLIENTDIR)/pa_eliom_client.cmo \
	  -I /usr/local/lib/ocaml/3.11.2/js_of_ocaml pa_js.cmo \
	  -impl $< -o $@ \
	  -pretty -verbose \
	  -client client/tutoeliom_ocsigen2_client.ml

client/tutoeliom_ocsigen2_client.ml: tutoeliom_ocsigen2.ml


tutoeliom.cma: tutoeliom.cmo tutoeliom_ocsigen2.cmo
	$(OCAMLFIND) ocamlc -a -o $@ $^

tutoeliom.cmxs: tutoeliom.cmx tutoeliom_ocsigen2.cmx
	$(OCAMLFIND) ocamlopt -linkall -shared -o $@ $^

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs

.PHONY: depend


.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
#	-rm ../extensions/ocsipersist.cmx
	$(CAMLOPT) $(PP) -c $<
#	touch ../extensions/ocsipersist.cmx

.cmx.cmxs:
	$(CAMLOPT) -shared -linkall -o $@ $<


clean:
	$(MAKE) -C miniwiki clean
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *~ $(NAME) *.annot ocamlduce/*.cm[ioax] ocamlduce/*.cmxa ocamlduce/*.cmxs ocamlduce/*~ ocamlduce/*.annot tutoeliom_ocsigen2_client* tutoeliom_ocsigen2.ml client/tutoeliom_ocsigen2_client.ml client/tutoeliom_ocsigen2.ml client/tutoeliom_ocsigen2_client.js ../files/tutorial/tutoeliom_ocsigen2_client.js tutoeliom_ocsigen2_client client/*.cm[ioax] client/*.cmxa client/*.cmxs client/*.o client/*~ 

depend:
	$(MAKE) -C miniwiki depend
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) | sed s%ocsipersist.cmx%ocsipersist.cmi%g > .depend

FORCE:

-include .depend


