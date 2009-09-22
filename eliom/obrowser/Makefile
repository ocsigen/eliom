
CMA:= eliom_obrowser_client.cma
OCAMLBUILD:= ocamlbuild -classic-display

all: $(CMA)

$(CMA): mls
	$(OCAMLBUILD) $(CMA)

mls: eliom_parameters.ml eliom_services.ml eliom_common.ml eliom_mkforms.ml ocsigen_lib_obrowser.ml polytables.ml

eliom_parameters.ml: ../eliom_parameters_obrowser.ml
	cp -f ../eliom_parameters_obrowser.ml eliom_parameters.ml

eliom_services.ml: ../eliom_services_obrowser.ml
	cp -f ../eliom_services_obrowser.ml eliom_services.ml

eliom_common.ml: ../eliom_common_obrowser.ml
	cp -f ../eliom_common_obrowser.ml eliom_common.ml

eliom_mkforms.ml: ../eliom_mkforms.ml
	cp -f ../eliom_mkforms.ml .

ocsigen_lib_obrowser.ml: ../../baselib/ocsigen_lib_obrowser.ml .
	cp -f ../../baselib/ocsigen_lib_obrowser.ml .

polytables.ml: ../../baselib/polytables.ml
	cp -f ../../baselib/polytables.ml .

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