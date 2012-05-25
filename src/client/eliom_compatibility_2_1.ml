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
  let iter_option = Eliom_lib.Option.iter
  (*BB How to provide them without getting this error?
     Error: Error while linking .../eliom_client.cma(Eliom_compatibility_2_1):
     Reference to undefined global `Svg_types' at the end? *)
  (* module SVG_types = Svg_types *)
  (* module HTML5_types = Html5_types *)
end

include Eliom_pervasives

module Eliom_output = struct
  type http_service = Eliom_output.http_service
  type appl_service = Eliom_output.appl_service
  module Html5_forms : "sigs/eliom_html5_forms.mli" = Eliom_content.Html5.D
  module Html5 = struct
    include Eliom_output.Html5
    include Html5_forms
  end
  module Xhtml = Eliom_output.Xhtml
  module Redirection = Eliom_output.Redirection
  module Blocks = Eliom_output.Block
  module Blocks5 = Eliom_output.Block5
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
  module Html5 = Eliom_content.Html5.To_dom
end

module Eliom_dom = Eliom_content.Html5.Manip
module Eliom_parameters = Eliom_parameter
