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


LWTDIR := $(shell ocamlfind query lwt)
REACTDIR := $(shell ocamlfind query react)

ELIOMDIR := ../eliom
ELIOMCLIENTDIR := $(ELIOMDIR)/client
PAELIOMCLIENTDIR := $(ELIOMDIR)/syntax

JSOFOCAMLDIR := $(shell ocamlfind query js_of_ocaml)

LIB = $(LIBDIRS) -package lwt.unix,netstring,camlp4,lwt.react,calendar

CLIENTLIB = -package lwt.react,js_of_ocaml


CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep $(LIB)

PP = -syntax camlp4o -ppopt "../xmlp4/xhtmlsyntax.cma" -ppopt "-loc loc"

OBJS = $(FILES:.ml=.cmo)
OBJSOPT = $(CMXS)

byte: $(OBJS) tutoeliom.cma $(DUCEMODBYTE) miniwiki.byte $(TUTOWIKI) ../files/tutorial/testsuite_eliom2.js

opt: $(OBJSOPT) tutoeliom.cmxs $(DUCEMODOPT) miniwiki.opt $(TUTOWIKI) ../files/tutorial/testsuite_eliom2.js

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

tutoeliom4.wiki: testsuite_eliom2.eliom
	cat testsuite_eliom2.eliom | sed '/%<||2>%/,$$ d' | sed '1,/%<||1>%/d' | /bin/sh ./tutomake.sh > tutoeliom4.wiki

testsuite_eliom2.js: $(ELIOMCLIENTDIR)/eliom_client.js syntax_temp_files/testsuite_eliom2
	js_of_ocaml -pretty $^ -o $@

../files/tutorial/testsuite_eliom2.js: testsuite_eliom2.js
	cp -f $< $@

testsuite_eliom2.cmo: syntax_temp_files/testsuite_eliom2.ml
	$(CAMLC) -c $< -o $@
testsuite_eliom2.cmx: syntax_temp_files/testsuite_eliom2.ml
	$(CAMLOPT) -c $< -o $@


#HERE IS THE SECTION CONCERNING ELIOM'S SYNTAX EXTENSIONS

$(ELIOMCLIENTDIR)/%:
	make -C $(ELIOMCLIENTDIR) all
$(PAELIOMCLIENTDIR)/%:
	make -C $(PAELIOMCLIENTDIR) all

syntax_temp_files/testsuite_eliom2_type_inference.ml: testsuite_eliom2.eliom $(PAELIOMCLIENTDIR)/pa_eliom_seed.cmo
	camlp4of \
	  $(PAELIOMCLIENTDIR)/pa_eliom_seed.cmo \
	  -pass type \
	  $(PAELIOMCLIENTDIR)/pa_eliom_type_filter.cmo \
	  -I $(JSOFOCAMLDIR) pa_js.cmo \
	  -o $@ -impl $< \
	  -verbose

syntax_temp_files/testsuite_eliom2_type_inference.mli: syntax_temp_files/testsuite_eliom2_type_inference.ml $(PAELIOMCLIENTDIR)/pa_eliom_seed.cmo
	$(CAMLC) -i $< >$@

syntax_temp_files/testsuite_eliom2.ml: testsuite_eliom2.eliom syntax_temp_files/testsuite_eliom2_type_inference.mli $(PAELIOMCLIENTDIR)/pa_eliom_seed.cmo
	camlp4of \
	  -I $(PAELIOMCLIENTDIR) pa_eliom_seed.cmo \
	  -pass server \
	  $(PAELIOMCLIENTDIR)/pa_eliom_client_server.cmo \
	  -typefile syntax_temp_files/testsuite_eliom2_type_inference.mli  \
	  -I $(JSOFOCAMLDIR) pa_js.cmo \
	  -impl $< -printer p -o $@ \
          -verbose

syntax_temp_files/testsuite_eliom2_client.ml: testsuite_eliom2.eliom syntax_temp_files/testsuite_eliom2_type_inference.mli $(PAELIOMCLIENTDIR)/pa_eliom_seed.cmo
	camlp4of \
	  -I $(PAELIOMCLIENTDIR) pa_eliom_seed.cmo \
	  -pass client \
	  $(PAELIOMCLIENTDIR)/pa_eliom_client_client.cmo \
	  -typefile syntax_temp_files/testsuite_eliom2_type_inference.mli  \
	  -I $(JSOFOCAMLDIR) pa_js.cmo \
	  -impl $< -printer p -o $@ \
          -verbose

syntax_temp_files/testsuite_eliom2: $(ELIOMCLIENTDIR)/eliom_client.cma $(ELIOMCLIENTDIR)/eliom_client_main.cmo syntax_temp_files/testsuite_eliom2_client.ml
	ocamlfind ocamlc -o $@ \
	  -I $(ELIOMCLIENTDIR) \
	  $(CLIENTLIB) -linkpkg \
	  -verbose \
	  $^

#END OF SECTION ABOUT ELIOM'S SYNTAX EXTENSIONS


tutoeliom.cma: tutoeliom.cmo testsuite_eliom2.cmo
	$(OCAMLFIND) ocamlc -a -o $@ $^

tutoeliom.cmxs: tutoeliom.cmx testsuite_eliom2.cmx
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
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *~ $(NAME) *.annot ocamlduce/*.cm[ioax] ocamlduce/*.cmxa ocamlduce/*.cmxs ocamlduce/*~ ocamlduce/*.annot testsuite_eliom2.js testsuite_eliom2_client* testsuite_eliom2.ml ../files/tutorial/testsuite_eliom2_client.js testsuite_eliom2 syntax_temp_files/*

depend:
	$(MAKE) -C miniwiki depend
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) | sed s%ocsipersist.cmx%ocsipersist.cmi%g > .depend

FORCE:

-include .depend


