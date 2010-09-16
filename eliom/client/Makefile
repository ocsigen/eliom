CMA:= eliom_client.cma
STUBDIR:=$(shell ocamlfind printconf stdlib)/stublibs
OCAMLFIND = ocamlfind
OCAMLC:= $(OCAMLFIND) ocamlc -verbose
OCAMLDEP:= $(OCAMLFIND) ocamldep
OCAMLMKLIB:= ocamlmklib
PACKAGES:= -package netstring,str,lwt.react,js_of_ocaml -syntax camlp4o
LIBS:=
MLS:= xML.ml uri.ml xhtmltypes.ml xHTML.ml svgtypes.ml sVG.ml xhtml5types.ml xHTML5.ml \
	reactive_dom.ml \
	regexp.ml \
	ocsigen_lib_cli.ml ocsigen_lib.ml \
	ocsigen_cookies.ml eliommod_client_cookies.ml eliom_client_types.ml \
	eliom_common.ml	eliom_process.ml \
	eliom_sessions.ml eliom_parameters.ml \
	eliom_services.ml eliom_uri.ml \
	eliom_request.ml \
	xhtmltypes.ml polytables.ml \
	eliommod_cli.ml eliom_client.ml eliommod_mkforms.ml \
	polytables.ml \
	eliom_predefmod.ml \
	eliom_common_comet.ml eliom_client_comet.ml \
        eliommod_mkforms.ml eliom_mkforms.ml \
	eliom_predefmod.ml \
	eliom_client_event.ml
MLIS:= uri.mli xhtml5types.mli xhtmltypes.mli xHTML.mli xHTML5.mli eliom_mkforms.mli eliom_client_types.mli eliom_predefmod.mli eliom_common_comet.mli eliommod_mkforms.ml
CMOS=$(MLS:.ml=.cmo)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: depend clean


all: $(MLIS) $(MLS) $(CMA) eliom_client_main.cmo


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

xHTML.mli: ../../xmlp4/xhtml/xHTML.mli
	cp -f $< $@ 

xHTML5.ml: ../../xmlp4/xhtml/xHTML5.ml
	cp -f $< $@

uri.ml: ../../xmlp4/xhtml/uri.ml
	cp -f $< $@

uri.mli: ../../xmlp4/xhtml/uri.mli
	cp -f $< $@

xHTML5.mli: ../../xmlp4/xhtml/xHTML5.mli
	cp -f $< $@ 

xhtmltypes.ml: ../../xmlp4/xhtml/xhtmltypes.ml
	cp -f $< $@ 

svgtypes.ml: ../../xmlp4/xhtml/svgtypes.ml
	cp -f $< $@ 

svgtypes.mli: ../../xmlp4/xhtml/svgtypes.mli
	cp -f $< $@ 

sVG.mli: ../../xmlp4/xhtml/sVG.mli
	cp -f $< $@ 
sVG.ml: ../../xmlp4/xhtml/sVG.ml
	cp -f $< $@ 

xhtml5types.ml: ../../xmlp4/xhtml/xhtml5types.ml
	cp -f $< $@ 

xhtmltypes.mli: ../../xmlp4/xhtml/xhtmltypes.mli
	cp -f $< $@ 

xhtml5types.mli: ../../xmlp4/xhtml/xhtml5types.mli
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

eliommod_mkforms.mli: ../eliommod_mkforms.mli
	cp -f $< $@ 

eliom_predefmod.ml: ../eliom_predefmod_cli.ml
	cp -f $< $@
	echo "module Xhtml5 = Xhtml5forms" >> $@

eliom_predefmod.mli: ../eliom_predefmod_cli.mli
	cp -f $< $@ 
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
	$(OCAMLDEP) $(MLS) eliom_client_main.ml $(MLS:.ml=.mli) $(PACKAGES) > .depend

clean: 
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *~ doc/*  *.annot *.so
	-rm eliom_parameters.ml eliom_services.ml eliom_client_types.ml* \
	    eliom_common.ml eliom_mkforms.ml* eliom_uri.ml* \
            eliom_predefmod.ml* xHTML.ml* xHTML5.ml* xhtml5types.ml* \
            xhtmltypes.ml* uri.ml* sVG.ml* svgtypes.ml* \
	    ocsigen_lib_cli.ml \
            polytables.ml

-include .depend
