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

open Eliom_lib

let request_data =
  Eliom_request_info.get_request_data ()

let onload ev =
  trace "onload";
  if !Eliom_config.debug_timings then
    Firebug.console##time(Js.string "onload");
  Eliommod_cookies.update_cookie_table (Some Url.Current.host)
    (Eliom_request_info.get_request_cookies ());
  ignore (lwt () = Lwt_js.sleep 0.001 in
          Eliom_client.relink_request_nodes (Dom_html.document##documentElement);
          let on_load =
            Eliom_client.load_eliom_data
              request_data
              (Dom_html.document##documentElement) in
          Eliom_client.force_unwrapped_elts ();
          Eliom_client.reset_request_node ();
          Lwt.return (List.for_all (fun f -> f ev) on_load));
  if !Eliom_config.debug_timings then
    Firebug.console##timeEnd(Js.string "onload");
  Js._false

let load_ev = Dom.Event.make "load"

let _ =
  Dom.addEventListener Dom_html.window load_ev
    (Dom.handler onload) Js._true

(* The following lines are for Eliom_bus, Eliom_comet and Eliom_react to be linked. *)
let _a = Eliom_react.force_link
let _b = Eliom_comet.force_link
let _c = Eliom_bus.force_link
