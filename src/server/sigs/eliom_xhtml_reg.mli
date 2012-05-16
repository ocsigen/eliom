include "eliom_reg.mli"
  subst type page    := Xhtml_types.xhtml Eliom_content_core.Xhtml.F.elt
    and type options := unit
    and type return  := http_service
    and type result  := (browser_content, http_service) kind
