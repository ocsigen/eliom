(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015
 * Vasilis Papavasileiou
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

open Js_of_ocaml

module Xml :
  Xml_sigs.T
  with type 'a W.t = 'a Shared.React.S.t
   and type 'a W.tlist = 'a Shared.ReactiveData.RList.t
   and type event_handler = (Dom_html.event Js.t -> unit) Client_value.t
   and type mouse_event_handler =
    (Dom_html.mouseEvent Js.t -> unit) Client_value.t
   and type keyboard_event_handler =
    (Dom_html.keyboardEvent Js.t -> unit) Client_value.t
   and type touch_event_handler =
    (Dom_html.touchEvent Js.t -> unit) Client_value.t

module Svg : sig
  module R : sig
    include
      Svg_sigs.Make(Xml).T
      with type 'a elt = 'a Content_core.Svg.elt
       and type 'a attrib = 'a Content_core.Svg.attrib

    val node : 'a elt Shared.React.S.t -> 'a elt
  end
end

module Html : sig
  module R : sig
    include
      Html_sigs.Make(Xml)(Svg.R).T
      with type 'a elt = 'a Content_core.Html.elt
       and type 'a attrib = 'a Content_core.Html.attrib

    val pcdata : string Shared.React.S.t -> [> Html_types.span] elt
    val node : 'a elt Shared.React.S.t -> 'a elt
    val filter_attrib : 'a attrib -> bool Shared.React.S.t -> 'a attrib
  end
end
