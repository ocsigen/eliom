(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010
 * Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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

let _ =
  Dom_html.window##onload <- Dom_html.handler (fun _ ->
    let eliom_data = Eliom_client.unmarshal_js_var "eliom_data" in
    Eliom_client.load_eliom_data_ eliom_data Dom_html.document##body;

    (* ===change page event *)
    let change_page_event
        : Eliom_client_types.eliom_appl_answer React.E.t = 
      (Eliom_client_event.Down.unwrap
         (Eliom_client.unmarshal_js_var "change_page_event"))
    in
    let retain_event = 
      React.E.map Eliom_client.set_content change_page_event
    in
    
    let `R r = React.E.retain change_page_event (fun () -> ()) in
    ignore 
      (React.E.retain change_page_event (fun () -> r (); ignore retain_event));

    Js._false)
