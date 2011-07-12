
(** {2 Creating links and forms with {!HTML5.M}} *)

type appl_service = [ `Appl ]
type http_service = [ `Http ]
type non_caml_service = [ appl_service | http_service ]

(** Eliom service registration and forms creation for HTML5 page *)
module Html5 : sig
  include "sigs/eliom_html5_forms.mli"
  (**/**)
  type return = http_service
end

(** Eliom forms creation for HTML5 *)
module Html5_forms : "sigs/eliom_html5_forms.mli"

(**/**)

module type Base = sig
  type return = http_service
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
