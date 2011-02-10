include ../../Makefile.config

export OCAMLPATH := ../../deriving/tmp:${OCAMLPATH}

FILES=$(wildcard *.ml)

#TUTOWIKI= eliom_testsuite11.wiki eliom_testsuite12.wiki eliom_testsuite13.wiki eliom_testsuite14.wiki

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

DERIVINGDIR := ../../deriving/tmp/deriving
ELIOMDIR := ../../eliom
ELIOMCLIENTDIR := $(ELIOMDIR)/client
PAELIOMCLIENTDIR := $(ELIOMDIR)/syntax

JSOFOCAMLDIR := $(shell ocamlfind query js_of_ocaml)

LIB = $(LIBDIRS2) -package lwt.unix,netstring,camlp4,lwt.react,calendar,deriving

CLIENTLIB = -package lwt.react,js_of_ocaml,deriving


CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep $(LIB)

PP = -syntax camlp4o -ppopt "../../xmlp4/xhtmlsyntax.cma" -ppopt "-loc loc"

OBJS = $(FILES:.ml=.cmo)
OBJSOPT = $(CMXS)

byte: eliom_testsuite.cma $(OBJS) $(DUCEMODBYTE) miniwiki.byte $(TUTOWIKI) ../../files/tutorial/eliom_testsuite3.js

opt: $(OBJSOPT) eliom_testsuite.cmxs $(DUCEMODOPT) miniwiki.opt $(TUTOWIKI) ../../files/tutorial/eliom_testsuite3.js

exampleduce.cmo: ocamlduce/exampleduce.cmo
	cp ocamlduce/exampleduce.cmo .

ocamlduce/exampleduce.cmo: ocamlduce/exampleduce.ml eliom_testsuite1.cmo
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

# eliom_testsuite11.wiki: eliom_testsuite1.ml
# 	cat eliom_testsuite1.ml | sed '1,/(\*wiki\*/d' | sed '/%<||2>%/,$$ d' | /bin/sh ./tutomake.sh > eliom_testsuite11.wiki

# eliom_testsuite12.wiki: eliom_testsuite1.ml
# 	cat eliom_testsuite1.ml | sed '/%<||3>%/,$$ d' | sed '1,/%<||2>%/d' | /bin/sh ./tutomake.sh > eliom_testsuite12.wiki

# eliom_testsuite13.wiki: eliom_testsuite1.ml
# 	cat eliom_testsuite1.ml | sed '/%<||4>%/,$$ d' | sed '1,/%<||3>%/d' | /bin/sh ./tutomake.sh > eliom_testsuite13.wiki

# eliom_testsuite14.wiki: eliom_testsuite3.eliom
# 	cat eliom_testsuite3.eliom | sed '/%<||2>%/,$$ d' | sed '1,/%<||1>%/d' | /bin/sh ./tutomake.sh > eliom_testsuite14.wiki

eliom_testsuite3.js: $(ELIOMCLIENTDIR)/eliom_client.js syntax_temp_files/eliom_testsuite3
	js_of_ocaml -pretty $^ -o $@

../../files/tutorial/eliom_testsuite3.js: eliom_testsuite3.js
	cp -f $< $@


#HERE IS THE SECTION CONCERNING ELIOM'S SYNTAX EXTENSIONS

$(ELIOMCLIENTDIR)/%:
	$(MAKE) -C $(ELIOMCLIENTDIR) all
$(PAELIOMCLIENTDIR)/%:
	$(MAKE) -C $(PAELIOMCLIENTDIR) all

CAMLP4_TYPE =	camlp4o \
		-I $(PAELIOMCLIENTDIR) pa_eliom_seed.cmo pa_eliom_type_filter.cmo \
		-I $(JSOFOCAMLDIR) pa_js.cmo \
		-I $(DERIVINGDIR) $(DERIVINGDIR)/pa_deriving.cma \
		-impl

CAMLP4_SERVER =	camlp4o \
		-I $(PAELIOMCLIENTDIR) pa_eliom_seed.cmo pa_eliom_client_server.cmo \
		-type syntax_temp_files/eliom_testsuite3_type_inference.mli \
		-I $(JSOFOCAMLDIR) pa_js.cmo \
		-I $(DERIVINGDIR) $(DERIVINGDIR)/pa_deriving.cma \
		-impl

CAMLP4_CLIENT =	camlp4o \
		-I $(PAELIOMCLIENTDIR) pa_eliom_seed.cmo pa_eliom_client_client.cmo \
		-type syntax_temp_files/eliom_testsuite3_type_inference.mli \
		-I $(JSOFOCAMLDIR) pa_js.cmo \
		-I $(DERIVINGDIR) $(DERIVINGDIR)/pa_deriving.cma \
		-impl

syntax_temp_files/eliom_testsuite3_type_inference.mli: eliom_testsuite3.eliom $(PAELIOMCLIENTDIR)/pa_eliom_seed.cmo $(PAELIOMCLIENTDIR)/pa_eliom_type_filter.cmo
	$(CAMLC) -pp "${CAMLP4_TYPE}" -i -impl $< >$@

eliom_testsuite3.cmo: eliom_testsuite3.eliom syntax_temp_files/eliom_testsuite3_type_inference.mli $(PAELIOMCLIENTDIR)/pa_eliom_seed.cmo $(PAELIOMCLIENTDIR)/pa_eliom_client_server.cmo
	$(CAMLC) -c -pp "${CAMLP4_SERVER}" -impl $< -o $@

eliom_testsuite3.cmx: eliom_testsuite3.eliom syntax_temp_files/eliom_testsuite3_type_inference.mli $(PAELIOMCLIENTDIR)/pa_eliom_seed.cmo $(PAELIOMCLIENTDIR)/pa_eliom_client_server.cmo
	$(CAMLOPT) -c -pp "${CAMLP4_SERVER}" -impl $< -o $@

syntax_temp_files/eliom_testsuite3_client.cmo: eliom_testsuite3.eliom syntax_temp_files/eliom_testsuite3_type_inference.mli $(PAELIOMCLIENTDIR)/pa_eliom_seed.cmo $(PAELIOMCLIENTDIR)/pa_eliom_client_client.cmo
	ocamlfind ocamlc -pp "${CAMLP4_CLIENT}" $(CLIENTLIB) -I $(ELIOMCLIENTDIR) -c -o $@ -impl $<

syntax_temp_files/eliom_testsuite3: $(ELIOMCLIENTDIR)/eliom_client.cma $(ELIOMCLIENTDIR)/eliom_client_main.cmo syntax_temp_files/eliom_testsuite3_client.cmo
	ocamlfind ocamlc -o $@ \
	  -I $(ELIOMCLIENTDIR) \
	  $(CLIENTLIB) -linkpkg \
	  $^

#END OF SECTION ABOUT ELIOM'S SYNTAX EXTENSIONS


eliom_testsuite.cma: eliom_testsuite1.cmo eliom_testsuite2.cmo eliom_testsuite3.cmo eliom_testsuite.cmo
	$(OCAMLFIND) ocamlc -a -o $@ $^

eliom_testsuite.cmxs: eliom_testsuite1.cmx eliom_testsuite2.cmx eliom_testsuite3.cmx eliom_testsuite.cmx
	$(OCAMLFIND) ocamlopt -linkall -shared -o $@ $^

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs

.PHONY: depend


.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
#	-rm ../../extensions/ocsipersist.cmx
	$(CAMLOPT) $(PP) -c $<
#	touch ../../extensions/ocsipersist.cmx

.cmx.cmxs:
	$(CAMLOPT) -shared -linkall -o $@ $<


clean:
	$(MAKE) -C miniwiki clean
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *~ $(NAME) *.annot ocamlduce/*.cm[ioax] ocamlduce/*.cmxa ocamlduce/*.cmxs ocamlduce/*~ ocamlduce/*.annot eliom_testsuite3.js eliom_testsuite3_client* eliom_testsuite3.ml ../../files/tutorial/eliom_testsuite3_client.js eliom_testsuite3 syntax_temp_files/*

depend:
	$(MAKE) -C miniwiki depend
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) | sed s%ocsipersist.cmx%ocsipersist.cmi%g > .depend

FORCE:

-include .depend


