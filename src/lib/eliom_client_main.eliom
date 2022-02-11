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

let%client _ = Eliom_client.init ()

let _ =
  Eliommod.register_site_init (fun () ->
      let sitedata = Eliom_request_info.get_sitedata () in
      let ignored_get_params =
        List.map fst sitedata.Eliom_common.ignored_get_params
      in
      let ignored_post_params =
        List.map fst sitedata.Eliom_common.ignored_post_params
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

(* The following lines are for Eliom_bus, Eliom_comet and Eliom_react
   to be linked. *)
let _force_link =
  Eliom_react.force_link, Eliom_comet.force_link, Eliom_bus.force_link

(* Client side implementation of reload actions *)
let%shared _ =
  Eliom_service.internal_set_client_fun ~service:Eliom_service.reload_action
    [%client
      fun () () ->
        Lwt.return (Eliom_service.Reload_action {hidden = false; https = false})];
  Eliom_service.internal_set_client_fun
    ~service:Eliom_service.reload_action_https
    [%client
      fun () () ->
        Lwt.return (Eliom_service.Reload_action {hidden = false; https = true})];
  Eliom_service.internal_set_client_fun
    ~service:Eliom_service.reload_action_hidden
    [%client
      fun () () ->
        Lwt.return (Eliom_service.Reload_action {hidden = true; https = false})];
  Eliom_service.internal_set_client_fun
    ~service:Eliom_service.reload_action_https_hidden
    [%client
      fun () () ->
        Lwt.return (Eliom_service.Reload_action {hidden = true; https = true})]
