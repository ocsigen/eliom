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

include Eliom_cookies_base

(* CCC The tables are indexed by the hostname, not the port appear.
   there are no particular reason. If needed it is possible to add it *)
let cookie_tables = Jstable.create ()

(** [in_local_storage] implements cookie substitutes for iOS WKWebView *)
let get_table ?(in_local_storage=false) = function
  | None -> Cookies.empty
  | Some host ->
    if in_local_storage then
      let host = Js.string (host ^ "/substitutes") in
      Js.Optdef.case (Dom_html.window##.localStorage) (fun () -> Cookies.empty)
        (fun st ->
           Js.Opt.case (st##(getItem host))
             (fun () -> Cookies.empty)
             (fun v -> Json.unsafe_input v))
    else
      Js.Optdef.get (Jstable.find cookie_tables (Js.string host))
        (fun () -> Cookies.empty)

(** [in_local_storage] implements cookie substitutes for iOS WKWebView *)
let set_table ?(in_local_storage=false) host t =
  match host with
    | None -> ()
    | Some host ->
      if in_local_storage then
        let host = Js.string (host ^ "/substitutes") in
        Js.Optdef.case (Dom_html.window##.localStorage)
          (fun () -> ())
          (fun st -> st##(setItem host ((Json.output t))))
      else
        Jstable.add cookie_tables (Js.string host) t

let now () =
  let date = new%js Js.date_now in
  Js.to_float (date##getTime) /. 1000.

(** [in_local_storage] implements cookie substitutes for iOS WKWebView *)
let update_cookie_table ?(in_local_storage=false) host cookieset =
  let now = now () in
  Cookies.iter
    (fun path table ->
      CookiesTable.iter
        (fun name -> function
          | OSet (Some exp, _, _) when exp <= now ->
            set_table ~in_local_storage
              host (remove_cookie path name (get_table ~in_local_storage host))
          | OUnset ->
            set_table ~in_local_storage
              host (remove_cookie path name (get_table ~in_local_storage host))
          | OSet (exp, value, secure) ->
            set_table ~in_local_storage
              host (add_cookie path name (exp, value, secure)
                      (get_table ~in_local_storage host)))
        table
    )
    cookieset

(** [in_local_storage] implements cookie substitutes for iOS WKWebView *)
let get_cookies_to_send ?(in_local_storage=false) host https path =
  let now = now () in
  Cookies.fold
    (fun cpath t cookies_to_send ->
      if Url.is_prefix_skip_end_slash
          (Url.remove_slash_at_beginning cpath)
          (Url.remove_slash_at_beginning path)
      then CookiesTable.fold
        (fun name (exp, value, secure) cookies_to_send ->
          match exp with
            | Some exp when exp <= now ->
              set_table ~in_local_storage host
                (remove_cookie cpath name (get_table host));
              cookies_to_send
            | _ ->
              if (not secure) || https
              then (name,value)::cookies_to_send
              else cookies_to_send
        )
        t
        cookies_to_send
      else cookies_to_send
    )
    (get_table ~in_local_storage host)
    []


let make_new_session_id () =
  failwith "Cannot define anonymous coservices on client side. Ask their values to the server."
