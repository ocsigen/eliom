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
open Eliom_client

let js_data =
  Eliom_request_info.get_request_data ()

let onload ev =
  trace "onload (client main)";
  Eliom_client.set_initial_load ();
  Lwt.async
    (fun () ->
      if !Eliom_config.debug_timings then
        Firebug.console##time(Js.string "onload");
      Eliommod_cookies.update_cookie_table (Some Url.Current.host)
        (Eliom_request_info.get_request_cookies ());
      Eliom_request_info.set_session_info js_data.Eliom_common.ejs_sess_info;
      (* Give the browser the chance to actually display the page NOW *)
      lwt () = Lwt_js.sleep 0.001 in
      (* Ordering matters. See [Eliom_client.set_content] for explanations *)
      relink_request_nodes (Dom_html.document##documentElement);
      let root = Dom_html.document##documentElement in
      let closure_nodeList = relink_page_but_closure_nodes root in
      do_request_data js_data.Eliom_common.ejs_request_data;
      ((* A similar check is necessary in Injection.initialize *)
       match Eliom_unwrap.remaining_values_for_late_unwrapping () with
         | [] -> ()
         | unwrap_ids ->
           alert "Values marked for unwrapping remain (for unwrapping id %s)."
             (String.concat ", " (List.map string_of_int unwrap_ids)));
      let onload_closure_nodes =
        relink_closure_nodes root js_data.Eliom_common.ejs_event_handler_table
          closure_nodeList
      in
      reset_request_nodes ();
      run_callbacks
        (Eliommod_dom.add_formdata_hack_onclick_handler ::
           flush_onload () @
           [ onload_closure_nodes; broadcast_load_end ]);
      if !Eliom_config.debug_timings then
        Firebug.console##timeEnd(Js.string "onload");
      Lwt.return ());
  Js._false

let () =
  trace "Set load/onload events";
  let onunload _ = leave_page (); Js._true in
  (* IE<9: Script438: Object doesn't support property or method
     addEventListener.
     Other browsers: Ask whether you really want to navigate away if
     onbeforeunload is assigned *)
  if Js.Unsafe.get Dom_html.window (Js.string "addEventListener") == Js.undefined then
    ( Dom_html.window##onload <- Dom_html.handler onload;
      Dom_html.window##onbeforeunload <- Dom_html.handler onunload )
  else
    ( ignore
        (Dom.addEventListener Dom_html.window (Dom.Event.make "load")
           (Dom.handler onload) Js._true);
      ignore
        (Dom.addEventListener Dom_html.window (Dom.Event.make "unload")
           (Dom_html.handler onunload) Js._false) )

(* The following lines are for Eliom_bus, Eliom_comet and Eliom_react to be linked. *)
let _force_link =
  Eliom_react.force_link,
  Eliom_comet.force_link,
  Eliom_bus.force_link,
  Eliom_pervasives._force_link
