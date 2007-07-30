(* Ocsigen
 * http://www.ocsigen.org
 * http_frame.ml Copyright (C) 2005 Denis Berthod
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

(** this set of modules discribes the http protocol and
the operation on this protocol*)

(** this signature provides a template to discribe the content of a http
    frame *)

open Ocsistream

type etag = string

module type HTTP_CONTENT =
  sig
    (** abstract type of the content*)
    type t
    
    (** convert a string into the content type *)
    val content_of_stream : stream -> t Lwt.t

    (** convert a content type into a thread returning its size,etag,stream*)
    val stream_of_content : t -> 
      (int64 option * etag * stream * (unit -> unit)) Lwt.t
        (* unit -> unit is the close function for the stream *)
        (* The int64 option is the content-length. 
           None means Transfer-encoding: chunked *)
        (* The last function is the termination function
           (for ex closing the file), 
           that will be called after the stream has been fully read. *)

    (** compute etag for content *)
    val get_etag : t -> etag
  end


(** this module describes the type of an http header *)
module Http_header =
  struct
      
      (**type of the http_method*)
      type http_method =
        | GET
        | POST
        | HEAD
        | PUT
        | DELETE
        | TRACE
        | OPTIONS
        | CONNECT
        | LINK
        | UNLINK
        | PATCH

      (** type of http_frame mode. The int is the HTTP answer code *)
      type http_mode = 
        | Query of (http_method * string)
        | Answer of int
        | Nofirstline

      type proto = HTTP10 | HTTP11
  
        (** type of the http headers *)
        type http_header =
          {
            (** the mode of the header : Query or Answer *)
            mode:http_mode;
            (** protocol used for the Query or the Answer *)
            proto: proto;
            (** list of the headers options *)
            headers: (string*string) list
          }

(*        (** gets the url raise Not_found if Answer *)
        let get_url header =
          match header.mode with
          | Query (_, s) -> s
          | _ -> raise Not_found *)

        (** gets the firstline of the header *)
        let get_firstline header = header.mode

        (** gets the headers *)
	let get_headers header = header.headers

        (** gets the value of a given header's option *)
        let get_headers_value header key = 
          List.assoc (String.lowercase key) header.headers

        (** gets the value of the protocol used *)
        let get_proto header = header.proto

(*        (** gets the value of the http method used *)
        let get_method header =
          match header.mode with
          | Query (meth, _) -> meth
          | _ -> raise Not_found *)
        
        (** adds an header option in the header option list*)
        let add_headers header key value =
          {
            mode=header.mode;
            proto=header.proto;
            headers=(String.lowercase key, value)::header.headers
          }
  end
  
module Http_error =
  struct

      (** exception raised on an http error . It's possible to pass the code of
      the error ans some args*)
      exception Http_exception of int option * string list

        (* this fonction provides the translation mecanisme between a code and
         * its explanation *)
        let expl_of_code =
          function
            | 100 -> "Continue"
            | 101 -> "Switching Protocol"
            | 200 -> "OK"
            | 201 -> "Created"
            | 202 -> "Accepted"
            | 203 -> "Non-Authoritative information"
            | 204 -> "No Content"
            | 205 -> "Reset Content"
            | 206 -> "Partial Content"
            | 300 -> "Multiple Choices"
            | 301 -> "Moved Permanently"
            | 302 -> "Found"
            | 303 -> "See Other"
            | 304 -> "Not Modified"
            | 305 -> "Use Proxy"
            | 307 -> "Moved Temporarily"
            | 400 -> "Bad Request"
            | 401 -> "Unauthorized"
            | 402 -> "Payment Required"
            | 403 -> "Forbidden"
            | 404 -> "Not Found"
            | 405 -> "Method Not Allowed"
            | 406 -> "Not Acceptable"
            | 407 -> "Proxy Authentication Required"
            | 408 -> "Request Time-out"
            | 409 -> "Conflict"
            | 410 -> "Gone"
            | 411 -> "Length Required"
            | 412 -> "Precondition Failed"
            | 413 -> "Request Entity Too Large"
            | 414 -> "Request URL Too Long"
            | 415 -> "Unsupported Media type"
            | 416 -> "Request Range Not Satisfiable"
            | 417 -> "Expectation Failed"
            | 500 -> "Internal Server Error"
            | 501 -> "Not Implemented"
            | 502 -> "Bad Gateway"
            | 503 -> "Service Unavailable"
            | 504 -> "Gateway Time-out"
            | 505 -> "Version Not Supported"
            | _ -> raise (Http_exception (Some 500,["Bad Server Code"]))

        let rec display_list =
            function
              | [] -> ()
              | hd::tl -> 
                  Messages.debug hd;
                  display_list tl

        let string_of_list l =
          let rec string_of_list_aux acc =
          function
            | [] -> acc
            | hd :: tl -> string_of_list_aux (acc^hd) tl
          in string_of_list_aux "" l

        let display_http_exception =
          function
            | Http_exception (Some n,l) ->
                Messages.debug (expl_of_code n);
                display_list l
            | Http_exception (None,l) ->
                display_list l
            | e -> raise e

        let string_of_http_exception =
          function
            | Http_exception (Some n, l) ->
                "error "^(string_of_int n)^" : "^(expl_of_code n)^
                " : "^(string_of_list l)
            | Http_exception (None,l) ->
                string_of_list l
            | e -> raise e 
      
  end

(** this module describes an http frame *)
module FHttp_frame = 
  functor(C:HTTP_CONTENT) ->
    struct

        (** type of the http frames*)
        type frame_content = C.t option

        type http_frame = 
            {header: Http_header.http_header;
             content: frame_content;
             waiter_thread: unit Lwt.t; (** A waiting Lwt thread that will be automatically awoken when the full frame has been read *)}

    end


