(* Ocsigen
 * ocsiheaders.mli Copyright (C) 2005 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(** This module is for getting informations from HTTP header. *)
(** It uses the lowel level module Http_frame.Http_header.    *)
(** It is very basic and must be completed for exhaustiveness. *)


val get_keepalive : Http_frame.Http_header.http_header -> bool
val parse_cookies : string  -> (string * string) list
val parse_mime_type : string -> string option * string option
val get_host_port :
  Predefined_senders.Stream_http_frame.http_frame ->
  (string * int option) option
val get_user_agent :
  Predefined_senders.Stream_http_frame.http_frame -> string
val get_cookie_string :
  Predefined_senders.Stream_http_frame.http_frame -> string option
val get_if_modified_since :
  Predefined_senders.Stream_http_frame.http_frame -> float option
val get_if_unmodified_since :
  Predefined_senders.Stream_http_frame.http_frame -> float option
val get_if_none_match :
  Predefined_senders.Stream_http_frame.http_frame -> string list
val get_if_match :
  Predefined_senders.Stream_http_frame.http_frame -> string list option
val get_content_type :
  Predefined_senders.Stream_http_frame.http_frame -> string option
val get_content_length :
  Predefined_senders.Stream_http_frame.http_frame -> int64 option
val get_referer :
  Predefined_senders.Stream_http_frame.http_frame -> string option
val get_referrer :
  Predefined_senders.Stream_http_frame.http_frame -> string option
val get_accept :
  Predefined_senders.Stream_http_frame.http_frame ->
    ((string option * string option) * float option * (string * string) list)
      list
val get_accept_charset :
  Predefined_senders.Stream_http_frame.http_frame ->
  (string option * float option) list
val get_accept_encoding :
  Predefined_senders.Stream_http_frame.http_frame ->
  (string option * float option) list
val get_accept_language :
  Predefined_senders.Stream_http_frame.http_frame ->
  (string * float option) list
