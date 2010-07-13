CMA:= eliom_client.cma
STUBDIR:=$(shell ocamlfind printconf stdlib)/stublibs
OCAMLFIND = ocamlfind
OCAMLC:= $(OCAMLFIND) ocamlc -verbose
OCAMLDEP:= $(OCAMLFIND) ocamldep
OCAMLMKLIB:= ocamlmklib
PACKAGES:= -package netstring,str,lwt.react,js_of_ocaml -syntax camlp4o
LIBS:=
MLS:= xML.ml xHTML.ml xhtml5types.ml xHTML5.ml \
	ocsigen_lib_cli.ml ocsigen_lib.ml \
	ocsigen_cookies.ml eliom_client_types.ml \
	regexp.ml \
	ocsigen_lib_cli.ml ocsigen_lib.ml \
	ocsigen_cookies.ml eliom_client_types.ml \
	eliom_common.ml eliom_sessions.ml eliom_parameters.ml \
	eliom_process.ml \
	eliom_services.ml eliom_uri.ml \
	eliommod_cli.ml eliom_client.ml eliommod_mkforms.ml \
	eliom_mkforms.ml polytables.ml \
	xhtmltypes.ml eliom_predefmod.ml \
	eliom_common_comet.ml eliom_client_comet.ml \
	eliom_client_event.ml
MLIS:= eliom_mkforms.mli eliom_client_types.mli eliom_predefmod.mli eliom_common_comet.mli
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


xHTML.ml: ../../xmlp4/xhtml/xHTML.ml
	cp -f $< $@ 

#xHTML.mli: ../../xmlp4/xhtml/xHTML.mli
#	cp -f $< $@ 

xHTML5.ml: ../../xmlp4/xhtml/xHTML5.ml
	camlp4r -printer o $< > $@

#xHTML5.mli: ../../xmlp4/xhtml/xHTML5.mli
#	cp -f $< $@ 

xhtmltypes.ml: ../../xmlp4/syntax/pp/xhtmltypes.ml
	cp -f $< $@ 

xhtml5types.ml: ../../xmlp4/syntax/pp/xhtml5types.ml
	cp -f $< $@ 

ocsigen_cookies.ml: ../../http/ocsigen_cookies.ml ocsigen_cookies.mli
	cp -f $< $@ 

ocsigen_cookies.mli: ../../http/ocsigen_cookies.mli
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

eliom_predefmod.ml: ../eliom_predefmod_cli.ml
	cp -f $< $@
	echo "module Xhtml = Xhtmlforms" >> $@
	echo "module Xhtml5 = Xhtml5forms" >> $@

eliom_predefmod.mli: ../eliom_predefmod_cli.mli
	cp -f $< $@ 
	echo "module Xhtml : XHTMLFORMSSIG" >> $@
	echo "module Xhtml5 : XHTML5FORMSSIG" >> $@

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
            eliom_predefmod.ml* xHTML.ml xHTML5.ml xhtml5types.ml \
	    ocsigen_lib_cli.ml \
            polytables.ml

-include .depend
