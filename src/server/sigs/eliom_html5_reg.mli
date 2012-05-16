include "eliom_reg.mli"
  subst type page    := Html5_types.html Eliom_content_core.Html5.elt
    and type options := unit
    and type return  := http_service
    and type result  := (browser_content, http_service) kind
