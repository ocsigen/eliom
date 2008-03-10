(* Ocsigen
 * http://www.ocsigen.org
 * http_client.ml Copyright (C) 2005 Vincent Balat
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

(** Using Ocsigen as a HTTP client *)

val get :
    ?https: bool ->
    ?port:int ->
    host:string ->
    uri:string ->
    unit ->
    Http_frame.t Lwt.t
(** EXPERIMENTAL -- May evolve in the future. Do a GET HTTP request.
   The default port is 80 for HTTP, 443 for HTTPS.
   The default protocol is http ([https=false]).
 *)

(*VVV missing: post *)


val raw_request :
    ?client: Ocsigen_extensions.client ->
    ?keep_alive: bool ->
    ?headers: Http_headers.t ->
    ?https: bool ->
    ?port:int ->
    content: string Ocsigen_stream.t option ->
    ?content_length: int64 ->
    http_method: Http_frame.Http_header.http_method ->
    host:string ->
    inet_addr:Unix.inet_addr ->
    uri:string ->
    unit ->
    unit ->
    Http_frame.t Lwt.t
(** 
   Do an HTTP request (low level). 

   If the optional argument [headers] is present, no headers will be 
   added by Ocsigen, but those in this argument and host, and 
   [connection: close] or [connection: keep-alive].
   Be carefull to respect HTTP/1.1 in this case!

   The default port is 80 for HTTP, 443 for HTTPS.

   The default protocol is http ([https=false]).

   The optional parameter [~keep_alive] asks to keep the connection opened
   after the request for a short amount of time 
   to allow other requests to the same server to use the same connection.
   It is true by default.
   If there is one opened free connection, we will use it instead of opening
   a new one.

   If you do this request to serve it later to a client or to generate a page
   for a client, add the optional parameter [~client]. 
   Thus, the request you do will be pipelined
   with other requests coming from the same connection. 
   A request will never be pipelined after a request from another client
   connection.
   Pipelining will be used only for requests to server we know supporting it
   (according to previous requests).
   It is recommended to specify this optional parameter for all requests
   (with the value found in field
   [ri_client] of type {!Ocsigen_extensions.request_info}).

   The optional parameter [?head] asks to do a [HEAD] HTTP request.
   It is [false] by default.

    When called without the last parameter, the function will pipeline 
    the request (if needed), then return the function to get the page.
    This allows to keep pipeline order when writing an extension.
 *)
(*VVV Dangerous!! *)


val basic_raw_request :
    ?headers: Http_headers.t ->
    ?https: bool ->
    ?port:int ->
    content: string Ocsigen_stream.t option ->
    ?content_length: int64 ->
    http_method: Http_frame.Http_header.http_method ->
    host:string ->
    inet_addr:Unix.inet_addr ->
    uri:string ->
    unit ->
    Http_frame.t Lwt.t
(** Same as {!Http_client.raw_request}, but does not try to reuse connections.
    Opens a new connections for each request. Much less efficient.
*)


(**/**)
val sslcontext : Ssl.context ref
