module Raw = Eliom_content_html_raw.F

include module type of Raw

include Eliom_content_sigs.LINKS_AND_FORMS
  with type +'a elt := 'a elt
   and type +'a attrib := 'a attrib
   and type uri := uri
   and type ('a, 'b, 'c) star := ('a, 'b, 'c) star
   and type 'a form_param := 'a Eliom_form.param
