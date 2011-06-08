include "eliom_reg.mli"
  subst type page    := HTML5_types.html HTML5.M.elt
    and type options := unit
    and type return  := Eliom_services.http
    and type result  := browser_content kind
