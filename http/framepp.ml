(* Ocsigen
 * framepp.ml Copyright (C) 2005 Denis Berthod
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

(** pretty printer for http frames*)
open Http_frame

module H = Http_header

(** converts the method into a string*)
let string_of_method =
  function
    | H.GET -> "GET"
    | H.POST -> "POST"
    | H.HEAD -> "HEAD"
    | H.PUT -> "PUT"
    | H.DELETE -> "DELETE"
    | H.TRACE -> "TRACE"
    | H.OPTIONS -> "OPTIONS"
    | H.CONNECT -> "CONNECT"
    | H.LINK -> "LINK"
    | H.UNLINK -> "UNLINK"
    | H.PATCH -> "PATCH"

(** converts the protocol into a string*)
let string_of_proto = function
  | H.HTTP10 -> "HTTP/1.0"
  | H.HTTP11 -> "HTTP/1.1"

  
(** create the first line for an http frame *)
let string_of_fst_line header =
  match header.H.mode with
  | H.Nofirstline -> ""
  | H.Answer code ->
      (string_of_proto header.H.proto)^" "^(string_of_int code)^" "^
      (Http_error.expl_of_code code)^"\r\n"
  | H.Query (meth, url) -> 
      (string_of_method meth)^" "^url^" "^
      (string_of_proto header.H.proto)^"\r\n"
  

(** creates a string from an headers line *)
let string_of_headers_line =
  fun (name , value) ->
    name^": "^value^"\r\n"
  
(** creates the lines from the headers*)
let string_of_headers header =
  let rec string_of_headers_aux result =
    function
      | [] -> result
      | hd::tl -> string_of_headers_aux (result^(string_of_headers_line hd)) tl
  in string_of_headers_aux "" header.H.headers
  

(**convert the header into a string *)
let string_of_header header =
  (string_of_fst_line header)^(string_of_headers header)^"\r\n"


module Fframepp =
  functor (C:HTTP_CONTENT) ->
    struct

      module Http = FHttp_frame(C)
      
      let string_of_http_frame http_frame content =
        let h = string_of_header (http_frame.Http.header) in
        Messages.debug h;
        let body =
          match content with
          | None -> ""
          | Some c -> c
        in h^body 
        
    end
