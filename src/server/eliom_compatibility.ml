
module Ocsigen_pervasives = Eliom_lib

module Eliom_pervasives = struct
  include Eliom_lib
  module XML = Eliom_content_core.XML
  module SVG = Eliom_content_core.SVG
  module HTML5 = struct
    module M = struct
      include Eliom_content.HTML5.F
      let a = raw_a
      let input = raw_input
    end
    module DOM = struct
      include Eliom_content.HTML5.D
      let a = raw_a
      let input = raw_input
    end
    include Eliom_content.HTML5.Id
    include DOM
  end
  module XHTML = struct
    module M = Eliom_content_core.XHTML.F
    include M
  end
end

include Eliom_pervasives

module Eliom_output = struct
  (*BB TODO complete! *)
  type http_service = Eliom_output.http_service
  type appl_service = Eliom_output.appl_service
  let set_exn_handler = Eliom_output.set_exn_handler
  type appl_service_options = Eliom_output.appl_service_options = {
    do_not_launch : bool;
  }
  let appl_self_redirect = Eliom_output.appl_self_redirect
  module Html5_forms = struct
    module M : "sigs/eliom_html5_forms.mli" = Eliom_content.HTML5.F
    module DOM : "sigs/eliom_html5_forms.mli" = Eliom_content.HTML5.D
    include DOM
  end
  module Html5 = struct
    include Eliom_output.Html5
    include Html5_forms
  end
  module Xhtml_forms : "sigs/eliom_xhtml_forms.mli" = Eliom_content.XHTML.F
  module Xhtml = struct
    include Eliom_output.Xhtml
    include Xhtml_forms
  end
  module Redirection = Eliom_output.Redirection
  module Blocks = Eliom_output.Blocks
  module Blocks5 = Eliom_output.Blocks5
  module HtmlText = struct
    include Eliom_output.Html_text
    include Eliom_content.Html_text
  end
  module CssText = Eliom_output.CssText
  module Text = Eliom_output.Text
  module Action = Eliom_output.Action
  module Unit = Eliom_output.Unit
  module String_redirection = Eliom_output.String_redirection
  module Any = Eliom_output.Any
  module Streamlist = Eliom_output.Streamlist
  module Caml = Eliom_output.Caml
  module Files = Eliom_output.Files
  module Eliom_appl = Eliom_output.Eliom_appl
  module Eliom_tmpl = Eliom_output.Eliom_tmpl
end

