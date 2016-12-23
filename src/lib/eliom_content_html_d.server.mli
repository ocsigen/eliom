(** {2 Content creation}

    See {% <<a_api project="tyxml" | module Html_sigs.T >> %}.
    If you want to create an untyped form, you will have to use {%
    <<a_api|module Eliom_content.Html.F.Raw>> %} otherwise, use
    Eliom form widgets.  For more information, see
    {{:http://ocsigen.org/howto/forms/}"how to make forms"} *)

(** See {% <<a_api project="tyxml" | module Html_sigs.T >> %}. *)
module Raw = Eliom_content_html_raw.D

include module type of Raw

include Eliom_content_sigs.LINKS_AND_FORMS
  with type +'a elt := 'a elt
   and type +'a attrib := 'a attrib
   and type uri := uri
   and type ('a, 'b, 'c) star := ('a, 'b, 'c) star
   and type 'a form_param := 'a Eliom_form.param
