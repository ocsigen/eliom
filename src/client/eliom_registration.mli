
type appl_service = [ `Appl ]
type http_service = [ `Http ]
type non_caml_service = [ appl_service | http_service ]

(**/**)

module type Base = sig
  type return = http_service
end

module Html5 : Base
module Xhtml : Base
module Redirection : Base
module Block : Base
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
