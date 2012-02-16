include "eliom_html5_event_handler_base.mli"
  subst type event := Dom_html.event XML.caml_event_handler
   and type mouseEvent := Dom_html.mouseEvent XML.caml_event_handler
   and type keyboardEvent := Dom_html.keyboardEvent XML.caml_event_handler
