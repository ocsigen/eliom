
open Eliom_pervasives

module Html5_forms = Eliom_output_base.Html5_forms(struct
  let register_event ?(keep_default = true) elt name f v =
    let keep_default = Js.bool keep_default in
    assert(String.sub name 0 2 = "on");
    Js.Unsafe.set elt (Js.string name) (Dom_html.handler (fun _ -> f v; keep_default))
end)

module type Base =
sig
  type return = Eliom_services.http
end

module Base =
struct
  type return = Eliom_services.http
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
