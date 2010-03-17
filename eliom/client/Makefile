CMA:= eliom_obrowser_client.cma
OBROWSERLIB:= $(shell ocamlfind query obrowser)
STUBDIR:=$(shell ocamlfind printconf stdlib)/stublibs
OCAMLFIND = CAML_LD_LIBRARY_PATH=$(OBROWSERLIB) CAMLLIB=$(OBROWSERLIB) ocamlfind
OCAMLC:= $(OCAMLFIND) ocamlc
OCAMLDEP:= $(OCAMLFIND) ocamldep
OCAMLMKLIB:= ocamlmklib
PACKAGES:= -package netstring,obrowser,str,lwt
MLS:= ocsigen_lib.ml eliom_client_types.ml ocsigen_lib_obrowser.ml ocsigen_lib.ml eliom_common.ml eliom_sessions.ml eliom_parameters.ml eliom_services.ml eliom_mkforms.ml polytables.ml eliom_obrowser_client.ml eliom_client.ml xML.ml xHTML.ml
CMOS=$(MLS:.ml=.cmo)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: depend clean


all: $(MLS) $(CMA)

$(CMA):  eliom_obrowser_client_stubs.o $(CMOS) ../../files/tutorial/eliom_obrowser.js
	$(OCAMLMKLIB) -o $(CMA:.cma=) eliom_obrowser_client_stubs.o polytables.cmo $(CMOS)

../../files/tutorial/eliom_obrowser.js: eliom_obrowser.js
	cp -f $< $@


.c.o:
	$(OCAMLC) -c $<

.ml.cmo:
	$(OCAMLC) $(PACKAGES) -c $<

.mli.cmi:
	$(OCAMLC) $(PACKAGES) -c $<


eliom_parameters.ml: ../eliom_parameters_obrowser.ml
	cp -f $< $@ 

eliom_services.ml: ../eliom_services_obrowser.ml
	cp -f $< $@ 

eliom_client_types.ml: ../eliom_client_types.ml
	cp -f $< $@ 

eliom_common.ml: ../eliom_common_obrowser.ml
	cp -f $< $@ 

eliom_mkforms.ml: ../eliom_mkforms.ml
	cp -f $< $@ 

ocsigen_lib_obrowser.ml: ../../baselib/ocsigen_lib_obrowser.ml
	cp -f $< $@ 

polytables.ml: ../../baselib/polytables.ml
	cp -f $< $@ 

#eliom_obrowser_client.cmo:
#	$(CAMLC) -package obrowser $(PP) -c eliom_obrowser_client.ml
#
#lwt_obrowser.cmo:
#	ocamlfind ocamlc -package obrowser,lwt.syntax -syntax camlp4o -c lwt_obrowser.ml
#
#eliom_obrowser_client.cma: lwt_obrowser.cmo eliom_obrowser_client.cmo
#	$(CAMLC) -a -o $@ $^

depend: $(MLS)
	$(OCAMLDEP) $(MLS) > .depend

clean: 
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *~ doc/*  *.annot *.so
	-rm eliom_parameters.ml eliom_services.ml eliom_client_types.ml \
	    eliom_common.ml eliom_mkforms.ml ocsigen_lib_obrowser.ml \
            polytables.ml

-include .depend
