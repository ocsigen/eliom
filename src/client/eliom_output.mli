
(** {2 Creating links and forms with {!HTML5.M}} *)

(** Eliom service registration and forms creation for HTML5 page *)
module Html5 : sig
  include "sigs/eliom_html5_forms.mli"
  (**/**)
  type return = Eliom_services.http
end

(** Eliom forms creation for HTML5 *)
module Html5_forms : "sigs/eliom_html5_forms.mli"

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
