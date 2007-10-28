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
    ?headers: Http_headers.t ->
    ?https: bool ->
    ?port:int ->
    ?content: string Ocsistream.t ->
    http_method: Http_frame.Http_header.http_method ->
    host:string ->
    inet_addr:Unix.inet_addr ->
    uri:string ->
    unit ->
    Http_frame.t Lwt.t
(** EXPERIMENTAL -- Will evolve in the future. 
   Do an HTTP request (low level). 
   If the optional argument [headers] is present, no headers will be 
   added by Ocsigen, but those in this argument and host, and 
   [connection: close].
   No way to do [Keep-alive] for now.
   Be carefull to respect HTTP/1.1 in this case!
   The default port is 80 for HTTP, 443 for HTTPS.
   The default protocol is http ([https=false]).
 *)
(*VVV Dangerous!! *)


(**/**)
val sslcontext : Ssl.context ref
