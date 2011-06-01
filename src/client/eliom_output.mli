
(** {2 Creating links and forms with {!HTML5.M}} *)

(** Eliom service registration and forms creation for HTML5 page *)
module Html5 : sig
  include "sigs/eliom_html5_forms.mli"
  (**/**)
  type return = Eliom_services.http
end

(** Eliom forms creation for HTML5 *)
module Html5_forms : "sigs/eliom_html5_forms.mli"

type eliom_appl_answer =
  | EAContent of ((Eliom_types.eliom_js_page_data * string) * string)
  | EAHalfRedir of string
  | EAFullRedir of
      (unit, unit, Eliom_services.get_service_kind,
       [ `WithoutSuffix ],
       unit, unit, Eliom_services.registrable, Eliom_services.http)
	Eliom_services.service
        (* We send a service in case of full XHR, so that we can
           add tab cookies easily.
           An alternative would be to send the URL,
           and then parse it on client side,
           to compute cookies from the URL information
           (but it is more complicated to implement because current function
           to generate tab cookies takes a service).
        *)

(**/**)

module type Base = sig
  type return = Eliom_services.http
end

module Xhtml : Base
module XhtmlForms : Base
module Redirection : Base
module Blocks : Base
module Blocks5 : Base

module HtmlText : Base
module CssText : Base
module Text : Base
module Action : Base
module Unit : Base

module String_redirection : Base

module Any : Base
module Streamlist : Base

module Caml : Base

