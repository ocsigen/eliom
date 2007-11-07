(* Ocsigen
 * http://www.ocsigen.org
 * sender_helpers.ml Copyright (C) 2005 Denis Berthod
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
(** Functions to create results for various kinds of documents *)


module File_content :
  Http_frame.HTTP_CONTENT with type t = string

module Xhtml_content :
  Http_frame.HTTP_CONTENT with type t = [ `Html ] XHTML.M.elt

(** content * content-type *)
module Text_content :
  Http_frame.HTTP_CONTENT with type t = string * string

module Stream_content :
  Http_frame.HTTP_CONTENT with type t = string Ocsistream.t

(** streams and content-type *)
module Streamlist_content :
  Http_frame.HTTP_CONTENT 
with type t = (unit -> string Ocsistream.t Lwt.t) list
      * string

module Empty_content :
  Http_frame.HTTP_CONTENT with type t = unit

(** directory name and corresponding URL path *)
module Directory_content :
  Http_frame.HTTP_CONTENT with type t = string * string list

(** error code and/or exception *)
module Error_content :
  Http_frame.HTTP_CONTENT with type t = int option * exn option



(** Sending an error page *)
val send_error :
    ?code:int ->
    ?exn:exn ->
    Http_com.slot ->
    clientproto:Http_frame.Http_header.proto ->
    ?mode:Http_frame.Http_header.http_mode ->
    ?proto:Http_frame.Http_header.proto ->
    keep_alive:bool ->
    head:bool -> 
    sender:Http_com.sender_type -> 
    unit -> 
    unit Lwt.t
