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


{client{

  include Eliom_content_

  open Html5
  open Html5.F

  let force_link = ()

}}

{shared{
type boxed
external boxed   : 'a client_value -> boxed client_value = "%identity"
external unboxed : boxed client_value -> 'a client_value = "%identity"
}}


{server{

module type Forms = "sigs/eliom_forms.mli"

module Xml = Eliom_content_.Xml

module Svg = struct
  include Eliom_content_.Svg

  module C = struct
    let node ?(init=D.Unsafe.node "g" []) x =
      let dummy_elt = D.toelt init in
      (* We need to box / unbox the client_value to convince eliom it's not polymorphic *)
      let client_boxed = boxed x in
      let _ = {unit{
          let dummy_dom = Svg.To_dom.of_element (Svg.D.tot %((dummy_elt : Xml.elt))) in
          let client_boxed = %client_boxed in
          let real = Svg.To_dom.of_element (unboxed client_boxed) in
          Js.Opt.iter
            (dummy_dom##parentNode)
            (fun parent -> parent##replaceChild(real, dummy_dom));
        }} in
      init

    let attr ?init x : 'a attrib = D.client_attrib ?init x
  end


end
module Html_text = Eliom_content_.Html_text

module Html5 = struct

  include Eliom_content_.Html5

  module C = struct
    let node ?(init=D.Unsafe.node "span" []) x =
      let dummy_elt = D.toelt init in
      (* We need to box / unbox the client_value to convince eliom it's not polymorphic *)
      let client_boxed : boxed client_value = boxed x in
      let _ = {unit{
          let dummy_dom = Html5.To_dom.of_element (Html5.D.tot %((dummy_elt : Xml.elt))) in
          let client_boxed = %client_boxed in
          let real = Html5.To_dom.of_element (unboxed client_boxed) in
          Js.Opt.iter
            (dummy_dom##parentNode)
            (fun parent -> ignore (parent##replaceChild(real, dummy_dom)));
        }} in
      init

    let attr ?init x : 'a attrib = D.client_attrib ?init x
  end

end


}}
