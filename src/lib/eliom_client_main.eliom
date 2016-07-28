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

[%%client.start]

let _ = Eliom_client.init ()

(* The following lines are for Eliom_bus, Eliom_comet and Eliom_react
   to be linked. *)
let _force_link =
  Eliom_react.force_link,
  Eliom_comet.force_link,
  Eliom_bus.force_link

let default_reload () =
  Dom_html.window##.location##reload;
  Lwt.return ()

let reload () =
  match !Eliom_client.reload_function with
  | Some f ->
    f () ()
  | None ->
    default_reload ()

let reload_with_warning () () =
  let f = !Eliom_client.reload_function in
  (*VVV When calling server side (non hidden) void coservice, GET
      non-attached parameters are removed.  But not when implemented
      on client side ...  I display a warning to remember that.  We
      should probably remember in service reload_function with
      na_param and reload_function without ...  It is probably very
      rarely used anyway ...  *)
  match f with
  | Some f ->
    print_endline
      "Warning: (non hidden) calling void coservice' on client side does\
       not remone GET non-attached parameters (FIX in Eliom)";
    f () ()
  | None ->
    default_reload ()

let switch_to_https () =
  let info = Eliom_process.get_info () in
  Eliom_process.set_info {info with Eliom_common.cpi_ssl = true }

(* Client side implementation of void_coservices *)
let%shared _ =
  Eliom_service.internal_set_client_fun
    ~service:Eliom_service.reload_action
     [%client  reload_with_warning ];
  Eliom_service.internal_set_client_fun
    ~service:Eliom_service.reload_action_https
     [%client  fun () () -> switch_to_https (); reload_with_warning () () ];
  Eliom_service.internal_set_client_fun
    ~service:Eliom_service.reload_action_hidden
     [%client  fun () () -> reload () ];
  Eliom_service.internal_set_client_fun
    ~service:Eliom_service.reload_action_https_hidden
     [%client  fun () () -> switch_to_https (); reload () ]
