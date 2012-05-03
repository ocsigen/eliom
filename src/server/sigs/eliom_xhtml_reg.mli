include "eliom_reg.mli"
  subst type page    := XHTML_types.xhtml XHTML.F.elt
    and type options := unit
    and type return  := http_service
    and type result  := (browser_content, http_service) kind
