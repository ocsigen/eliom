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
(** Functions to create senders for various kinds of documents *)

type mycookieslist = 
  (string list option * float option * (string * string) list) list

type stream_filter_type =
    string option (* content-type *) ->
    Http_frame.full_stream -> Http_frame.full_stream Lwt.t
(** A function to transform a stream into another one. *)

type send_page_type =
    (* no content
       no content-type *)
    ?filter:stream_filter_type ->
    ?cookies:mycookieslist ->
    Http_com.slot ->
    clientproto:Http_frame.Http_header.proto ->
    ?mode:Http_frame.Http_header.http_mode ->
    ?code:int ->
    ?etag:Http_frame.etag ->
    keep_alive:bool ->
    ?last_modified:float ->
    ?location:string ->
    head:bool ->
    ?headers:Http_headers.t ->
    ?charset:string ->
    Http_com.sender_type ->
    unit Lwt.t

(** Sending xhtml *)
val send_xhtml_page : content: [ `Html ] XHTML.M.elt -> send_page_type

(** Sending a file *)
val send_file : content: string -> send_page_type

(** Sending an empty page (no content) *)
val send_empty : content: unit -> send_page_type

(** Sending a text page *)
val send_text_page : ?contenttype: string -> content:string -> send_page_type

(** fonction that uses a stream to send a (text) answer step by step *)
val send_stream_page : ?contenttype: string -> 
  content: string Ocsistream.t -> send_page_type

(** fonction that uses a stream list to send a (text) answer step by step.
    Calling the function opens the stream and returns
    the stream and the function to call to close it.
 *)
val send_stream_list_page : ?contenttype: string -> 
  content: (unit -> (string Ocsistream.t * (unit -> unit)) Lwt.t) list -> 
    send_page_type

(** Headers for a non cachable request *)
val dyn_headers : Http_headers.t


(**/**)

(** Sending an error page *)
val send_error : ?http_exception:exn -> send_page_type

module File_content :
  Http_frame.HTTP_CONTENT with type t = string

val send_generic :
    (?filter:stream_filter_type ->
     Http_com.slot ->
     clientproto:Http_frame.Http_header.proto ->
     ?etag:Http_frame.etag ->
     mode:Http_frame.Http_header.http_mode ->
     ?proto:Http_frame.Http_header.proto ->
     ?headers:Http_headers.t ->
     ?contenttype:string ->
     content:'a ->
     head:bool ->
     Http_com.sender_type ->
     unit Lwt.t) ->
  ?contenttype:string ->
  content:'a ->
  ?filter:stream_filter_type ->
  ?cookies:mycookieslist ->
  Http_com.slot ->
  clientproto:Http_frame.Http_header.proto ->
  ?mode:Http_frame.Http_header.http_mode ->
  ?code:int ->
  ?etag:Http_frame.etag ->
  keep_alive:bool ->
  ?last_modified:float ->
  ?location:string ->
  head:bool ->
  ?headers:Http_headers.t ->
  ?charset:string ->
  Http_com.sender_type ->
  unit Lwt.t
