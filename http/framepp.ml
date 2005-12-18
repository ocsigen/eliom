(* Ocsigen
 * framepp.ml Copyright (C) 2005 Denis Berthod
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** pretty printer for http frames*)
open Http_frame

module H = Http_header

exception Framepp_exception of string

(** try to retrieve the value of an option variable *)
let try_option opt err_mesg =
  match opt with
  |Some v -> v
  |None -> raise (Framepp_exception err_mesg)

(** converts the method into a string*)
let string_of_method =
  function
    |H.GET -> "GET "
    |H.POST -> "POST "
    |H.HEAD -> "HEAD "
    |H.PUT -> "PUT "
    |H.DELETE -> "DELETE "
    |H.TRACE -> "TRACE "
    |H.OPTIONS -> "OPTIONS "
    |H.CONNECT -> "CONNECT "
    |H.LINK -> "LINK "
    |H.UNLINK -> "UNLINK "
    |H.PATCH -> "PATCH "

(** create the first line for an http frame in query mode*)
let string_of_fst_line_query header =
  (string_of_method (try_option header.H.meth "method not defined"))^
  (try_option header.H.url "url not defined")^" "^header.H.proto^"\r\n"

  
(** create the first line for an http frame in answer mode*)
let string_of_fst_line_answer header =
  let code = try_option header.H.code "code not defined" in
  header.H.proto^" "^(string_of_int code)^" "^
  (Http_error.expl_of_code code)^"\r\n"

(** creates a string from an headers line *)
let string_of_headers_line =
  fun (name , value) ->
    name^": "^value^"\r\n"
  
(** creates the lines from the headers*)
let string_of_headers header =
  let rec string_of_headers_aux result =
    function
      |[] -> result
      |hd::tl -> string_of_headers_aux (result^(string_of_headers_line hd)) tl
  in string_of_headers_aux "" header.H.headers
  

(**convert the header into a string *)
let string_of_header header =
match header.H.mode with
|H.Query ->
     (string_of_fst_line_query header)^(string_of_headers header)^"\r\n"
|H.Answer ->
    (string_of_fst_line_answer header)^(string_of_headers header)^"\r\n"

module Fframepp =
  functor (C:HTTP_CONTENT) ->
    struct

      module Http = FHttp_frame(C)
      
      let string_of_http_frame http_frame =
	let h = string_of_header (http_frame.Http.header) in
        let body =
          match http_frame.Http.content with
          |None -> ""
          |Some c -> C.string_of_content c
	in (* debug *)
	   Messages.debug h;
	h^body 
        
    end
