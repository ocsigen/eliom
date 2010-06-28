CMA:= eliom_client.cma
STUBDIR:=$(shell ocamlfind printconf stdlib)/stublibs
OCAMLFIND = ocamlfind
OCAMLC:= $(OCAMLFIND) ocamlc -verbose
OCAMLDEP:= $(OCAMLFIND) ocamldep
OCAMLMKLIB:= ocamlmklib
PACKAGES:= -package netstring,str,lwt.react,js_of_ocaml -syntax camlp4o
LIBS:=
MLS:= xML.ml xHTML.ml \
	eliom_client_types.ml \
	regexp.ml lwt_obrowser.ml \
	ocsigen_lib_cli.ml ocsigen_lib.ml \
	eliom_common.ml eliom_sessions.ml eliom_parameters.ml \
	eliom_services.ml eliom_uri.ml \
	eliommod_client.ml eliom_client.ml \
	eliom_mkforms.ml polytables.ml \
	xhtmltypes.ml eliom_predefmod.ml \
	eliom_common_comet.ml eliom_client_comet.ml \
	eliom_client_event.ml
MLIS:= eliom_mkforms.mli eliom_client_types.mli eliom_predefmod.mli
CMOS=$(MLS:.ml=.cmo)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: depend clean


all: $(MLIS) $(MLS) $(CMA)

$(CMA):  eliom_client_stubs.o $(CMOS)
	$(OCAMLMKLIB) -o $(CMA:.cma=) eliom_client_stubs.o polytables.cmo $(CMOS)


.c.o:
	$(OCAMLC) -c $<

.ml.cmo:
	$(OCAMLC) $(LIBS) $(PACKAGES) -c $<

.mli.cmi:
	$(OCAMLC) $(LIBS) $(PACKAGES) -c $<


xHTML.ml: ../../xmlp4/ohl-xhtml/xHTML.ml
	cp -f $< $@ 

xHTML.mli: ../../xmlp4/ohl-xhtml/xHTML.mli
	cp -f $< $@ 

xhtmltypes.ml: ../../xmlp4/newocaml/pp/xhtmltypes.ml
	cp -f $< $@ 

eliom_parameters.ml: ../eliom_parameters_cli.ml
	cp -f $< $@ 

eliom_services.ml: ../eliom_services_cli.ml
	cp -f $< $@ 

eliom_client_types.ml: ../eliom_client_types.ml
	cp -f $< $@ 

eliom_client_types.mli: ../eliom_client_types.mli
	cp -f $< $@ 

eliom_common.ml: ../eliom_common_cli.ml
	cp -f $< $@ 

eliom_mkforms.ml: ../eliom_mkforms.ml
	cp -f $< $@ 

eliom_mkforms.mli: ../eliom_mkforms.mli
	cp -f $< $@ 

eliom_uri.ml: ../eliom_uri.ml
	cp -f $< $@ 

eliom_uri.mli: ../eliom_uri.mli
	cp -f $< $@ 

eliom_predefmod.ml: ../eliom_predefmod_client.ml
	cp -f $< $@
	echo "module Xhtml = Xhtmlforms" >> $@

eliom_predefmod.mli: ../eliom_predefmod_client.mli
	cp -f $< $@ 
	echo "module Xhtml : XHTMLFORMSSIG" >> $@

ocsigen_lib_cli.ml: ../../baselib/ocsigen_lib_cli.ml
	cp -f $< $@ 

polytables.ml: ../../baselib/polytables.ml
	cp -f $< $@ 

eliom_common_comet.ml: ../eliom_common_comet.ml
	cp -f $< $@ 

eliom_common_comet.mli: ../eliom_common_comet.mli
	cp -f $< $@ 



depend: $(MLS) $(MLIS)
	$(OCAMLDEP) $(MLS) $(MLS:.ml=.mli) $(PACKAGES) > .depend

clean: 
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *~ doc/*  *.annot *.so
	-rm eliom_parameters.ml eliom_services.ml eliom_client_types.ml* \
	    eliom_common.ml eliom_mkforms.ml* eliom_uri.ml* \
            eliom_predefmod.ml* xHTML.ml*\
	    ocsigen_lib_cli.ml \
            polytables.ml

-include .depend
