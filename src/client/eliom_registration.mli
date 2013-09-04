type appl_service = Eliom_service.appl_service
type http_service = Eliom_service.http_service
type non_caml_service = Eliom_service.non_caml_service

(**/**)

module type Base = sig
  type return = http_service
end

module Html5 : Base
module Redirection : Base
module Block5 : Base

module Html_text : Base
module CssText : Base
module Text : Base
module Action : Base
module Unit : Base

module String_redirection : Base

module Any : Base
module Streamlist : Base

module Ocaml : Base
