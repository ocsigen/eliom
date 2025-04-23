(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
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

open Js_of_ocaml
open Eliom_lib

(* Logs *)
let section = Lwt_log.Section.make "eliom:process"
let log_section = section
let history_api = Dom_html.hasPushState ()

let get_set_js_serverside_value r name =
  ( (fun s -> r := Some s),
    (fun () -> not (!r = None)),
    (fun () ->
      match !r with
      | Some s -> s
      | None ->
          (* if variable toto does not exist,
         Js.Unsafe.variable "toto" fails, but
         Js.Unsafe.get Js.Unsafe.global (Js.string "toto") returns undefined *)
          Js.Optdef.case
            (Js.def (Js.Unsafe.get Js.Unsafe.global (Js.string name)))
            (fun () ->
              failwith
                (name
               ^ " not defined. A client Eliom application must either be sent \
                  by an Eliom server application or you must call \
                  Eliom_client.init_client_app."))
            (fun var ->
              let s = unmarshal_js var in
              r := Some s;
              s)),
    fun () -> r := None )

let ( set_sitedata,
      is_set_sitedata,
      (get_sitedata : unit -> Eliom_types.sitedata),
      reset_sitedata ) =
  get_set_js_serverside_value Eliom_common.sitedata "__eliom_appl_sitedata"

let ignored_get_params = ref []
let ignored_post_params = ref []

let set_ignored_params get post =
  let compile =
    List.map (fun s -> Re.seq [ Re.start; Re.Pcre.re s; Re.stop ] |> Re.compile)
  in
  ignored_get_params := compile get;
  ignored_post_params := compile post

let ( set_info,
      is_set_info,
      (get_info : unit -> Eliom_common.client_process_info),
      reset_info ) =
  get_set_js_serverside_value (ref None) "__eliom_appl_process_info"

let ( set_request_cookies,
      is_set_request_cookies,
      (get_request_cookies : unit -> Ocsigen_cookie_map.t),
      reset_request_cookies ) =
  get_set_js_serverside_value (ref None) "__eliom_request_cookies"

let ( set_request_template,
      is_set_request_template,
      (get_request_template : unit -> string option),
      reset_request_template ) =
  get_set_js_serverside_value (ref None) "__eliom_request_template"

let appl_name =
  lazy
    (let _, v, _ =
       (* We are using an appl cookie for this,
          and not a JS variable,
          because we want to send it back with each request.
          For mobile apps, we set the cookie from JS variable. *)
       Ocsigen_cookie_map.Map_inner.find Eliom_common.appl_name_cookie_name
         (Ocsigen_cookie_map.Map_path.find
            (get_sitedata ()).Eliom_types.site_dir
            (Eliommod_cookies.get_table (Some (get_info ()).cpi_hostname)))
     in
     v)

let set_base_url, get_base_url =
  let r : string option ref = ref None in
  ( (fun s -> r := Some s),
    fun () ->
      match !r with
      | Some s -> s
      | None ->
          failwith
            "base_url not set. Did you forget to call \
             Eliom_client.init_client_app?" )

(** None on server side *)
let appl_name_r = ref None
(* Set by Eliom_client.init_client_app *)

let get_application_name () =
  match !appl_name_r with
  | None -> (
      try !!appl_name
      with Not_found -> raise_error ~section "Application name not defined")
  | Some n -> n

let client_side = true
