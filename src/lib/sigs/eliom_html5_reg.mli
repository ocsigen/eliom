include "sigs/eliom_reg.mli"
  subst type page    := Html5_types.html Eliom_content.Html5.elt
    and type options := unit
    and type return  := http_service
    and type returnB := [> http_service ]
    and type returnT := [< http_service ]
    and type result  := browser_content kind
