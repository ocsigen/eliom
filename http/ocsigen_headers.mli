(* Ocsigen
 * ocsigen_headers.mli Copyright (C) 2005 Vincent Balat
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

(** Getting informations from HTTP header. *)
(** This module uses the lowel level module Http_frame.Http_header. 
     It is very basic and must be completed for exhaustiveness. *)


val get_keepalive : Http_frame.Http_header.http_header -> bool
val parse_cookies : string  -> string Http_frame.Cookievalues.t
val parse_mime_type : string -> string option * string option
val get_host_and_port : Http_frame.t -> (string * int) option
val get_user_agent : Http_frame.t -> string
val get_cookie_string : Http_frame.t -> string option
val get_if_modified_since : Http_frame.t -> float option
val get_if_unmodified_since : Http_frame.t -> float option
val get_if_none_match : Http_frame.t -> string list
val get_if_match : Http_frame.t -> string list option
val get_content_type : Http_frame.t -> string option
val get_content_length : Http_frame.t -> int64 option
val get_referer : Http_frame.t -> string option
val get_referrer : Http_frame.t -> string option
val get_accept :
  Http_frame.t ->
  ((string option * string option) * float option * (string * string) list)
    list
val get_accept_charset : Http_frame.t -> (string option * float option) list
val get_accept_encoding : Http_frame.t -> (string option * float option) list
val get_accept_language : Http_frame.t -> (string * float option) list
