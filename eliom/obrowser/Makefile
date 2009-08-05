
CMA:= eliom_obrowser_client.cma
OCAMLBUILD:= ocamlbuild -classic-display

all: $(CMA)

$(CMA):
	cp -f ../eliom_parameters_obrowser.ml eliom_parameters.ml
	cp -f ../eliom_services_obrowser.ml eliom_services.ml
	cp -f ../eliom_common_obrowser.ml eliom_common.ml
	cp -f ../eliom_mkforms.ml .
	cp -f ../../baselib/ocsigen_lib_obrowser.ml .
	cp -f ../../baselib/polytables.ml .
	$(OCAMLBUILD) $(CMA)

#_build/pa_eliom_obrowser.cmo:
#	$(OCAMLBUILD) $@


#eliom_obrowser_client.cmo:
#	$(CAMLC) -package obrowser $(PP) -c eliom_obrowser_client.ml
#
#lwt_obrowser.cmo:
#	ocamlfind ocamlc -package obrowser,lwt.syntax -syntax camlp4o -c lwt_obrowser.ml
#
#eliom_obrowser_client.cma: lwt_obrowser.cmo eliom_obrowser_client.cmo
#	$(CAMLC) -a -o $@ $^

clean:
	rm -rf _build