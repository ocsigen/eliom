(* Ocsigen
 * http://www.ocsigen.org
 * ocsigen_http_frame.ml Copyright (C) 2005
 * Denis Berthod, Vincent Balat, Jérôme Vouillon
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

open Ocsigen_stream
open Ocsigen_cookies

type etag = string


(* The following cookie function is not in Ocsigen_cookies
   because ri is not used client side *)


(* [compute_new_ri_cookies now path ri_cookies cookies_to_set]
   adds the cookies from [cookies_to_set]
   to [ri_cookies], as if the cookies
   had been send to the browser and the browser
   was doing a new request to the url [path].
   Only the cookies that match [path] (current path) are added. *)
let compute_new_ri_cookies
    now
    ripath
    ricookies
    cookies_set_by_page =

  let prefix path p =
    Ocsigen_lib.list_is_prefix_skip_end_slash
      (Ocsigen_lib.remove_slash_at_beginning path)
      (Ocsigen_lib.remove_slash_at_beginning p)
  in
  Cookies.fold
    (fun path ct t ->
      if prefix path ripath then
        Ocsigen_lib.String_Table.fold
          (fun n v beg ->
            match v with
            | OSet (Some ti, v, _) when ti>now ->
                Ocsigen_lib.String_Table.add n v t
            | OSet (None, v, _) -> Ocsigen_lib.String_Table.add n v t
            | OSet (_, _, _)
            | OUnset -> Ocsigen_lib.String_Table.remove n t
          )
          ct
          t
      else t
    )
    cookies_set_by_page
    ricookies



(** The type of answers to send *)
type result =
    {res_cookies: cookieset; (** cookies to set (with optional path) *)
     res_lastmodified: float option; (** Default: [None] *)
     res_etag: string option;
     res_code: int; (** HTTP code, if not 200 *)
     res_stream: string Ocsigen_stream.t *
       (string Ocsigen_stream.t -> 
          int64 -> 
            string Ocsigen_stream.step Lwt.t) option
     ; (** Default: empty stream. 
           The second field is (optionaly)
           the function used to skip a part of the 
           stream, if you do not you want to use
           a basic reading of the stream. 
           For example, for static files, you can optimize it by using
           a [seek] function.
       *)
     (* It is not a new field of the record to remember to change it
        if we change the stream. *)
     res_content_length: int64 option; (** [None] means Transfer-encoding: chunked *)
     res_content_type: string option;
     res_headers: Http_headers.t; (** The headers you want to add *)
     res_charset: string option; (** Default: None *)
     res_location: string option; (** Default: None *)
   }

(** Default [result] to use as a base for constructing others. *)
let default_result () =
  {
   res_cookies = Cookies.empty;
   res_lastmodified = None;
   (* No date => proxies use etag *)
   res_etag = None;
   res_code = 200;
   res_stream = (Ocsigen_stream.make (fun () -> Ocsigen_stream.empty None), 
                 None);
   res_content_length = Some 0L;
   res_content_type = None;
   res_headers= Http_headers.empty;
   res_charset= None;
   res_location= None;
 }

(** [result] for an empty page. *)
let empty_result () =
  {
   res_cookies = Cookies.empty;
   res_lastmodified = None;
   res_etag = None;
   res_code = 204; (* No content *)
   res_stream = (Ocsigen_stream.make (fun () -> Ocsigen_stream.empty None), 
                 None);
   res_content_length = Some 0L;
   res_content_type = None;
   res_headers= Http_headers.empty;
   res_charset= None;
   res_location= None;
 }

module type HTTP_CONTENT =
  sig
    (** abstract type of the content *)
    type t

    type options

    (** convert a content into a thread returning the default
        [result] for this content *)
    val result_of_content : ?options:options -> t -> result Lwt.t

    (** compute etag for content *)
    val get_etag : ?options:options -> t -> etag option
  end


(** this module describes the type of an http header *)
module Http_header =
  struct

      (** type of the http_method *)
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

      (** type of ocsigen_http_frame mode. The int is the HTTP answer code *)
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
            headers: Http_headers.t;
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
          Http_headers.find key header.headers

        (** gets all the values of a given header's option *)
        let get_headers_values header key =
          Http_headers.find_all key header.headers

        (** gets the value of the protocol used *)
        let get_proto header = header.proto

(*        (** gets the value of the http method used *)
        let get_method header =
          match header.mode with
          | Query (meth, _) -> meth
          | _ -> raise Not_found *)

        (** adds an header option in the header option list*)
        let add_headers header key value =
          { header with
            headers = Http_headers.add key value header.headers }
  end

module Http_error =
  struct

      (** Exception raised on an http error. It is possible to pass the code of
          the error, some comment, and some headers. *)
      exception Http_exception of int * string option * Http_headers.t option

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
            | _   -> "Unknown Error" (*!!!*)

        let display_http_exception e =
          match e with
          | Http_exception (n, Some s, Some _) ->
              Ocsigen_messages.debug
                (fun () -> Format.sprintf "%s: %s (with headers)" (expl_of_code n) s)
          | Http_exception (n, Some s, None) ->
              Ocsigen_messages.debug
                (fun () -> Format.sprintf "%s: %s" (expl_of_code n) s)
          | Http_exception (n, None, _) ->
              Ocsigen_messages.debug
                (fun () -> Format.sprintf "%s" (expl_of_code n))
          | _ ->
              raise e

        let string_of_http_exception e =
          match e with
          | Http_exception (n, Some s, Some _) ->
              Format.sprintf "Error %d, %s: %s (with headers)" n (expl_of_code n) s
          | Http_exception (n, Some s, None) ->
              Format.sprintf "Error %d, %s: %s" n (expl_of_code n) s
          | Http_exception (n, None, _) ->
              Format.sprintf "Error %d, %s" n (expl_of_code n)
          | _ ->
              raise e

  end

(** HTTP messages *)
type t =
  { frame_header : Http_header.http_header;
    frame_content : string Ocsigen_stream.t option;
    frame_abort : unit -> unit Lwt.t
(*VVV abort looks like a hack.
It has been added for the reverse proxy, to enable closing the connection
if the request is cancelled ...
à revoir...
*)
  }
