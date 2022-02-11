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

[%%shared type boxed]

[%%client
open Js_of_ocaml
include Eliom_content_

let force_link = ()

external unboxed
  :  boxed Eliom_client_value.t
  -> 'a Eliom_client_value.t
  = "%identity"]

[%%server
external boxed
  :  'a Eliom_client_value.t
  -> boxed Eliom_client_value.t
  = "%identity"

module Xml = Eliom_content_.Xml
module Xml_shared = Eliom_content_.Xml_shared

module Svg = struct
  include Eliom_content_.Svg

  module C = struct
    let node ?(init = D.Unsafe.node "g" []) x =
      let dummy_elt = D.toelt init in
      (* We need to box / unbox the client_value to convince eliom it's not polymorphic *)
      let client_boxed = boxed x in
      let _ =
        [%client
          (let dummy_dom =
             Svg.To_dom.of_element (Svg.D.tot ~%(dummy_elt : Xml.elt))
           in
           let client_boxed = ~%client_boxed in
           let real = Svg.To_dom.of_element (unboxed client_boxed) in
           Js.Opt.iter dummy_dom##.parentNode (fun parent ->
               parent ## (replaceChild real dummy_dom))
            : unit)]
      in
      init

    let attr ?init x : 'a attrib = D.client_attrib ?init x
  end
end

module Html = struct
  include Eliom_content_.Html

  module C = struct
    let node ?(init = D.Unsafe.node "span" []) x =
      let dummy_elt = D.toelt init in
      (* We need to box / unbox the client_value to convince eliom it's not polymorphic *)
      let client_boxed : boxed Eliom_client_value.t = boxed x in
      let _ =
        [%client
          (let dummy_dom =
             Html.To_dom.of_element (Html.D.tot ~%(dummy_elt : Xml.elt))
           in
           let client_boxed = ~%client_boxed in
           let real = Html.To_dom.of_element (unboxed client_boxed) in
           Js.Opt.iter dummy_dom##.parentNode (fun parent ->
               Dom.replaceChild parent real dummy_dom)
            : unit)]
      in
      init

    let attr ?init x : 'a attrib = D.client_attrib ?init x
  end
end]

let%client set_client_fun = Eliom_service.set_client_fun
let%client set_form_error_handler = Eliom_form.set_error_handler
