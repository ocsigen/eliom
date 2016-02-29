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
  let force_link = ()

}}

{shared{
type boxed
external boxed : 'a Eliom_client_common.client_value
  -> boxed Eliom_client_common.client_value
  = "%identity"
external unboxed : boxed Eliom_client_common.client_value
  -> 'a Eliom_client_common.client_value
  = "%identity"
}}


{server{

module Xml = Eliom_content_.Xml

module Xml_shared = Eliom_content_.Xml_shared

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

module Html5 = struct

  include Eliom_content_.Html5

  module C = struct
    let node ?(init=D.Unsafe.node "span" []) x =
      let dummy_elt = D.toelt init in
      (* We need to box / unbox the client_value to convince eliom it's not polymorphic *)
      let client_boxed : boxed Eliom_client_common.client_value = boxed x in
      let _ = {unit{
          let dummy_dom = Html5.To_dom.of_element (Html5.D.tot %((dummy_elt : Xml.elt))) in
          let client_boxed = %client_boxed in
          let real = Html5.To_dom.of_element (unboxed client_boxed) in
          Js.Opt.iter
            (dummy_dom##parentNode)
            (fun parent -> Dom.replaceChild parent real dummy_dom)
        }} in
      init

    let attr ?init x : 'a attrib = D.client_attrib ?init x
  end

end


}}

{shared{
   let set_client_fun = Eliom_service.set_client_fun_
 }}
{client{
   let _ = Eliom_client0.of_element_ := Html5.To_dom.of_element
 }}
