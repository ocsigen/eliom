(**/**)

module type Base = sig
  type return = Eliom_service.http Eliom_service.non_ocaml
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
