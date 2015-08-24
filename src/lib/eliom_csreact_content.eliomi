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

module Xml : Xml_sigs.T
  with type 'a W.t = 'a Eliom_csreact.SharedReact.S.t
   and type 'a W.tlist = 'a Eliom_csreact.SharedReactiveData.RList.t
   and type event_handler =
         (Dom_html.event Js.t -> unit) Eliom_lib.client_value
   and type mouse_event_handler =
         (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value
   and type keyboard_event_handler =
         (Dom_html.keyboardEvent Js.t -> unit) Eliom_lib.client_value

module Svg : sig

  module R : sig

    include Svg_sigs.Make(Xml).T
      with type 'a elt = 'a Eliom_content_core.Svg.elt
       and type 'a attrib = 'a Eliom_content_core.Svg.attrib

    val node : 'a elt Eliom_csreact.SharedReact.S.t -> 'a elt

  end

end

module Html5 : sig

  module R : sig

    include Html5_sigs.Make(Xml)(Svg.R).T
      with type 'a elt = 'a Eliom_content_core.Html5.elt
       and type 'a attrib = 'a Eliom_content_core.Html5.attrib

    val pcdata :
      string Eliom_csreact.SharedReact.S.t ->
      [> | Html5_types.span] elt

    val node : 'a elt Eliom_csreact.SharedReact.S.t -> 'a elt

    val filter_attrib :
      'a attrib -> bool Eliom_csreact.SharedReact.S.t -> 'a attrib

  end

end
