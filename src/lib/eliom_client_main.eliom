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

let%client _ = Client.init ()

let _ =
  Eliommod.register_site_init (fun () ->
    let sitedata = Request_info.get_sitedata () in
    let ignored_get_params =
      List.map fst sitedata.Common.ignored_get_params
    in
    let ignored_post_params =
      List.map fst sitedata.Common.ignored_post_params
    in
    let _ =
      [%client
        (Eliom_process.set_ignored_params ~%ignored_get_params
           ~%ignored_post_params;
         Eliom_process.set_ignored_params ~%ignored_get_params
           ~%ignored_post_params
         : unit)]
    in
    ())

[%%client.start]

(* The following lines are for Bus, Comet and Eliom_react
   to be linked. *)
let _force_link =
  Eliom_react.force_link, Comet.force_link, Bus.force_link

(* Client side implementation of reload actions *)
let%shared _ =
  Service.internal_set_client_fun ~service:Service.reload_action
    [%client
      fun () () ->
        Lwt.return (Service.Reload_action {hidden = false; https = false})];
  Service.internal_set_client_fun
    ~service:Service.reload_action_https
    [%client
      fun () () ->
        Lwt.return (Service.Reload_action {hidden = false; https = true})];
  Service.internal_set_client_fun
    ~service:Service.reload_action_hidden
    [%client
      fun () () ->
        Lwt.return (Service.Reload_action {hidden = true; https = false})];
  Service.internal_set_client_fun
    ~service:Service.reload_action_https_hidden
    [%client
      fun () () ->
        Lwt.return (Service.Reload_action {hidden = true; https = true})]
