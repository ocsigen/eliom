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

let current_path_and_args () =
  let path_of_string s =
    match Url.path_of_path_string s with
    | "." :: path ->
      path
    | path ->
      path
  in
  let uri = !Eliom_client.current_uri in
  match Url.url_of_string uri with
  | Some (Url.Http url | Url.Https url) ->
    url.Url.hu_path, url.Url.hu_arguments
  | _ ->
    match
      try
        Some (String.index uri '?')
      with Not_found ->
        None
    with
    | Some n ->
      path_of_string String.(sub uri 0 n),
      Url.decode_arguments String.(sub uri (n + 1) (length uri - n - 1))
    | None ->
      path_of_string uri, []

let reload ~fallback () =
  let path, args = current_path_and_args () in
  try%lwt
    Eliom_client.change_page_unknown path args []
  with _ ->
    Eliom_client.change_page
      ~ignore_client_fun:true
      ~service:fallback
      () ()

let reload_without_na_params ~fallback () () =
  let path, args = current_path_and_args () in
  let args = Eliom_common.remove_na_prefix_params args in
  try%lwt
    Eliom_client.change_page_unknown path args []
  with _ ->
    Eliom_client.change_page
      ~ignore_client_fun:true
      ~service:fallback
      () ()

let switch_to_https () =
  let info = Eliom_process.get_info () in
  Eliom_process.set_info {info with Eliom_common.cpi_ssl = true }

(* Client side implementation of reload actions *)
let%shared _ =
  Eliom_service.internal_set_client_fun
    ~service:Eliom_service.reload_action
    [%client
      reload_without_na_params
        ~fallback:Eliom_service.reload_action
    ];
  Eliom_service.internal_set_client_fun
    ~service:Eliom_service.reload_action_https
    [%client
      fun () () ->
        switch_to_https ();
        reload_without_na_params
          ~fallback:Eliom_service.reload_action_https
          () ()
    ];
  Eliom_service.internal_set_client_fun
    ~service:Eliom_service.reload_action_hidden
    [%client
      fun () () ->
        reload ~fallback:Eliom_service.reload_action_hidden ()
    ];
  Eliom_service.internal_set_client_fun
    ~service:Eliom_service.reload_action_https_hidden
    [%client
      fun () () ->
        switch_to_https ();
        reload ~fallback:Eliom_service.reload_action_https_hidden ()
    ]
