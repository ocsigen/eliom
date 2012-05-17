include "eliom_html5_event_handler_base.mli"
  subst type event := Dom_html.event Eliom_content_core.Xml.caml_event_handler
   and type mouseEvent := Dom_html.mouseEvent Eliom_content_core.Xml.caml_event_handler
   and type keyboardEvent := Dom_html.keyboardEvent Eliom_content_core.Xml.caml_event_handler
