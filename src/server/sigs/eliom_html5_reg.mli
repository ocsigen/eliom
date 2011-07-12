include "eliom_reg.mli"
  subst type page    := HTML5_types.html HTML5.M.elt
    and type options := unit
    and type return  := http_service
    and type result  := (browser_content, http_service) kind
