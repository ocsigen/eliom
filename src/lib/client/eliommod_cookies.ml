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

module JsTable = Eliommod_jstable

(* CCC The tables are indexed by the hostname, not the port appear.
   there are no particular reason. If needed it is possible to add it *)
let cookie_tables = JsTable.create ()
let get_table = function
  | None -> Cookies.empty
  | Some host ->
    Js.Optdef.get (JsTable.find cookie_tables (Js.string host))
      (fun () -> Cookies.empty)
let set_table host t =
  match host with
    | None -> ()
    | Some host ->
      JsTable.add cookie_tables (Js.string host) t

let now () =
  let date = jsnew Js.date_now () in
  Js.to_float (date##getTime ()) /. 1000.

let update_cookie_table host cookieset =
  let now = now () in
  Cookies.iter
    (fun path table ->
      CookiesTable.iter
        (fun name -> function
          | OSet (Some exp, _, _) when exp <= now ->
            set_table host (remove_cookie path name (get_table host))
          | OUnset ->
            set_table host (remove_cookie path name (get_table host))
          | OSet (exp, value, secure) ->
            set_table host
              (add_cookie
                 path name (exp, value, secure)
                 (get_table host)))
        table
    )
    cookieset

let get_cookies_to_send host https path =
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
              set_table host
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
    (get_table host)
    []


let make_new_session_id () =
  failwith "Cannot define anonymous coservices on client side. Ask their values to the server."
