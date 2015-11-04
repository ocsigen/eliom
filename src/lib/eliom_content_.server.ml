(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2012 Vincent Balat, Benedikt Becker
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


open Eliom_lib
open Eliom_content_core

module Xml = Eliom_content_core.Xml

module Xml_shared = Eliom_shared_content.Xml

module Svg = struct

  module F = Svg.F
  module D = Svg.D
  module R = struct
    module Raw = Eliom_shared_content.Svg.R
    include Raw
    let pcdata _ = `Unimplemented
  end

  module Id = Svg.Id

  type +'a elt = 'a F.elt
  type 'a wrap = 'a F.wrap
  type 'a list_wrap = 'a F.list_wrap
  type 'a attrib = 'a F.attrib
  type uri = F.uri

  module Printer = Svg.Printer

end

module Html5 = struct

  module F = struct
    include Html5.F
    module PARAM = struct
      include Html5.F
      module Svg = Eliom_content_core.Svg.F
      let uri_of_fun = Eliom_content_core.Xml.uri_of_fun
      let attrib_of_service s info =
        Eliom_content_core.
          (Html5.F.to_attrib
             (Xml.internal_event_handler_attrib
                s (Xml.internal_event_handler_of_service info)))
    end
    include Eliom_form.Make_links(PARAM)
    module Form = Eliom_form.Make(PARAM)
  end

  module D = struct
    include Html5.D
    module PARAM = struct
      include Html5.D
      module Svg = Eliom_content_core.Svg.D
      let uri_of_fun = Eliom_content_core.Xml.uri_of_fun
      let attrib_of_service s info =
        Eliom_content_core.
          (Html5.D.to_attrib
             (Xml.internal_event_handler_attrib
                s (Xml.internal_event_handler_of_service info)))
    end
    include Eliom_form.Make_links(PARAM)
    module Form = Eliom_form.Make(PARAM)
  end

  module R = Eliom_shared_content.Html5.R

  module Custom_data = Eliom_content_core.Html5.Custom_data

  module Id = Html5.Id

  module Printer = Html5.Printer

  type +'a elt = 'a F.elt
  type 'a wrap = 'a F.wrap
  type 'a list_wrap = 'a F.list_wrap
  type 'a attrib = 'a F.attrib
  type uri = F.uri
end
