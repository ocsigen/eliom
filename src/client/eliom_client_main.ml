(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010
 * Vincent Balat
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

open Eliom_pervasives

(* The following lines are for Eliommod_mkforms and Eliom_client_react to be linked. *)
let _a = Eliommod_mkforms.make_a_with_onclick
let _b = Eliom_react.force_link

let _ =
  Dom_html.window##onload <- Dom_html.handler (fun _ ->
    let eliom_data = Eliom_unwrap.unwrap (unmarshal_js_var "eliom_data") in
    ignore (Eliom_client.load_eliom_data eliom_data Dom_html.document##body);
    Js._false)
