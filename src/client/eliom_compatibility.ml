
module Eliom_pervasives = struct
  include Eliom_lib
  module XML = Eliom_content_core.XML
  module SVG = Eliom_content_core.SVG
  module HTML5 = struct
    module M = Eliom_content_core.HTML5.F
    module DOM = Eliom_content_core.HTML5.D
    include Eliom_content.HTML5.Id
    include DOM
  end
  let iter_option = Eliom_lib.Option.iter
end

include Eliom_pervasives

module Eliom_output = struct
  type http_service = Eliom_output.http_service
  type appl_service = Eliom_output.appl_service
  module Html5_forms : "sigs/eliom_html5_forms.mli" = Eliom_content.HTML5.D
  module Html5 = struct
    include Eliom_output.Html5
    include Html5_forms
  end
  module Xhtml = Eliom_output.Xhtml
  module Redirection = Eliom_output.Redirection
  module Blocks = Eliom_output.Blocks
  module Blocks5 = Eliom_output.Blocks5
  module Html_text = Eliom_output.Html_text
  module CssText = Eliom_output.CssText
  module Text = Eliom_output.Text
  module Action = Eliom_output.Action
  module Unit = Eliom_output.Unit
  module String_redirection = Eliom_output.String_redirection
  module Any = Eliom_output.Any
  module Streamlist = Eliom_output.Streamlist
  module Caml = Eliom_output.Caml
end

module Eliom_client = struct
  include Eliom_client
  module Html5 = Eliom_content.HTML5.To_dom
end

module Eliom_dom = Eliom_content.HTML5.Manip
