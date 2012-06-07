(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2012 Benedikt Becker
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

module Ocsigen_pervasives = Eliom_lib

module Eliom_pervasives = struct
  include Eliom_lib
  module XML = Eliom_content_core.Xml
  module SVG = Eliom_content_core.Svg
  module HTML5 = struct
    module M = struct
      include Eliom_content.Html5.F
      let a = Raw.a
    end
    module DOM = struct
      include Eliom_content.Html5.D
      let a = Raw.a
    end
    include Eliom_content.Html5.Id
    include DOM
  end
  module XHTML = struct
    module M = Eliom_content_core.Xhtml.F
    include M
  end
  (*BB How to provide without getting this error?
     ocsigenserver: main: Fatal - While loading .../server/eliom.cma: error while linking .../server/eliom.cma.
     ocsigenserver: main: Reference to undefined global `Svg_types' *)
  (* module SVG_types = Svg_types *)
  (* module HTML5_types = Html5_types *)
  (* module XHTML_types = Xhtml_types *)
end

include Eliom_pervasives

module Eliom_output = struct
  (*BB TODO complete! *)
  type http_service = Eliom_registration.http_service
  type appl_service = Eliom_registration.appl_service
  let set_exn_handler = Eliom_registration.set_exn_handler
  type appl_service_options = Eliom_registration.appl_service_options = {
    do_not_launch : bool;
  }
  let appl_self_redirect = Eliom_registration.appl_self_redirect
  module Html5_forms = struct
    module M : "sigs/eliom_html5_forms.mli" = Eliom_content.Html5.F
    module DOM : "sigs/eliom_html5_forms.mli" = Eliom_content.Html5.D
    include DOM
  end
  module Html5 = struct
    include Eliom_registration.Html5
    include Html5_forms
  end
  module Xhtml_forms : "sigs/eliom_xhtml_forms.mli" = Eliom_content.Xhtml.F
  module Xhtml = struct
    include Eliom_registration.Xhtml
    include Xhtml_forms
  end
  module Redirection = Eliom_registration.Redirection
  module Blocks = Eliom_registration.Block
  module Blocks5 = Eliom_registration.Block5
  module HtmlText = struct
    include Eliom_registration.Html_text
    include Eliom_content.Html_text
  end
  module CssText = Eliom_registration.CssText
  module Text = Eliom_registration.Text
  module Action = Eliom_registration.Action
  module Unit = Eliom_registration.Unit
  module String_redirection = Eliom_registration.String_redirection
  module Any = Eliom_registration.Any
  module Streamlist = Eliom_registration.Streamlist
  module Caml = Eliom_registration.Ocaml
  module Files = Eliom_registration.File
  module Eliom_appl = Eliom_registration.App
  module Eliom_tmpl = Eliom_registration.Eliom_tmpl
end

module Eliom_extensions = Eliom_extension
module Eliom_parameters = Eliom_parameter
module Eliom_references = Eliom_reference
module Eliom_services = Eliom_service
module Eliom_cookies = Eliom_cookie
