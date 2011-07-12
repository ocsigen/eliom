
open Eliom_pervasives

include Eliom_output_base

module type Base =
sig
  type return = http_service
end

module Base =
struct
  type return = http_service
end

module Html5 =
struct
  include Html5_forms
  include Base
end

module Xhtml = Base
module XhtmlForms = Base
module Redirection = Base
module Blocks = Base
module Blocks5 = Base


module HtmlText = Base
module CssText = Base
module Text = Base
module Action = Base
module Unit = Base

module String_redirection = Base

module Any = Base
module Streamlist = Base

module Caml = Base
