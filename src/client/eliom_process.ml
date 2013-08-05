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

open Eliom_lib
open Ocsigen_cookies

let history_api =
  Js.def Dom_html.window##history != Js.undefined
  && Js.Unsafe.variable "window.history.pushState" != Js.undefined

let get_set_js_serverside_value r name =
  (fun s -> r := Some s),
  (fun () -> not (!r = None)),
  (fun () -> match !r with
    | Some s -> s
    | None ->
      (* if variable toto does not exist,
         Js.Unsafe.variable "toto" fails, but
         Js.Unsafe.variable "this.toto" returns undefined *)
      Js.Optdef.case (Js.def (Js.Unsafe.variable ("this."^name)))
        (fun () -> failwith (name^" not defined. A client Eliom application must either be sent by an Eliom server application of you must call Eliom_process.init_client_app."))
        (fun _ ->
          let s = unmarshal_js_var name in
          r := Some s;
          s))

let set_sitedata, is_set_sitedata,
  (get_sitedata : unit -> Eliom_types.sitedata) =
  get_set_js_serverside_value Eliom_common.sitedata "__eliom_appl_sitedata"

let set_info, is_set_info,
  (get_info : unit -> Eliom_common.client_process_info) =
  get_set_js_serverside_value (ref None) "__eliom_appl_process_info"

let set_request_cookies, is_set_request_cookies,
  (get_request_cookies : unit -> Eliommod_cookies.cookie
   Ocsigen_cookies.CookiesTable.t Ocsigen_cookies.Cookies.t) =
  get_set_js_serverside_value (ref None) "__eliom_request_cookies"

let set_request_template, is_set_request_template,
  (get_request_template : unit -> string option) =
  get_set_js_serverside_value (ref None) "__eliom_request_template"

let appl_name =
  lazy
    (let (_, v, _) =
       (CookiesTable.find
          Eliom_common.appl_name_cookie_name
          (Cookies.find
             (get_sitedata ()).Eliom_types.site_dir
             (Eliommod_cookies.get_table (Some Url.Current.host))))
     in v)

(** None on server side *)
let get_application_name () = Some (!!appl_name)

let client_side = true
