(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_sessions.ml
 * Copyright (C) 2009 Vincent Balat
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

let get_default_hostname () = Url.Current.host
let get_default_port () = 80

(*VVV take from server? !!!!!!!!! *)
(*RRR ??? Url.default_http_port ???*)

let get_default_sslport () = 443

(*VVV take from server? !!!!!!!!! *)
(*RRR ??? replace by Url.default_https_port ???*)

let default_protocol_is_https () = Url.Current.protocol = "https"

(*VVV take from server? !!!!!!!!! *)
(*RRR ??? replace by Url.default_https_port ???*)

let get_default_links_xhr () = true (*BBB take from server? !!!!!!!!!! *)
let debug_timings = ref false
let is_tracing = ref false
let set_tracing value = is_tracing := value
let get_tracing () = !is_tracing

(* let () = *)
(*   if Js.to_string Dom_html.window##location##hash = "#__trace" then *)
(*     set_tracing true; *)
(*   if Js.to_string Dom_html.window##location##hash = "#__timings" then *)
(*     debug_timings := true *)

let get_debugmode () =
  try Js.to_bool Js.Unsafe.global##.___eliom_debug_mode_ with _ -> false
