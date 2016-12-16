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

[%%client
module Xml = Eliom_content_xml.Xml
module Svg = Eliom_content_svg
module Html = Eliom_content_html
]

[%%server
module Xml = Eliom_content_xml.Xml
module Xml_shared = Eliom_shared_content.Xml
module Svg = Eliom_content_svg
module Html = Eliom_content_html
]

let%shared set_client_fun = Eliom_service.set_client_fun

let%client wrap_client_fun f get_params post_params =
  let%lwt content = f get_params post_params in
  let content = Html.To_dom.of_element content in
  Eliom_client.set_content_local content

let%client set_form_error_handler = Eliom_form.set_error_handler
