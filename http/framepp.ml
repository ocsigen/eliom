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
open Ocsigen_http_frame

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

(** converts a string to a method *)
let method_of_string =
  function
    | "GET" -> H.GET
    | "POST" -> H.POST
    | "HEAD" -> H.HEAD
    | "PUT" -> H.PUT
    | "DELETE" -> H.DELETE
    | "TRACE" -> H.TRACE
    | "OPTIONS" -> H.OPTIONS
    | "CONNECT" -> H.CONNECT
    | "LINK" -> H.LINK
    | "UNLINK" -> H.UNLINK
    | "PATCH" -> H.PATCH
    | _ -> failwith "method_of_string"

(** converts the protocol into a string *)
let string_of_proto = function
  | H.HTTP10 -> "HTTP/1.0"
  | H.HTTP11 -> "HTTP/1.1"

(** converts a string to a protocol *)
let proto_of_string = function
  | "HTTP/1.0" -> H.HTTP10
  | "HTTP/1.1" -> H.HTTP11
  | _ -> failwith "proto_of_string"

(** Write the first line of an HTTP frame to a string buffer *)
let fst_line buf header =
  match header.H.mode with
  | H.Nofirstline -> ()
  | H.Answer code ->
      Printf.bprintf buf "%s %i %s\r\n" (string_of_proto header.H.proto)
        code (Http_error.expl_of_code code)
  | H.Query (meth, url) ->
      Printf.bprintf buf "%s %s %s\r\n"
        (string_of_method meth) url (string_of_proto header.H.proto)


(** Prints the content of a header. To prevent http header injection,
    we insert spaces (' ') after CRLF, in case the user has not done this
    himself. Also, if we find single CR or LF, we replace them by spaces .
    (This is correct according to the RFC, as the headers content should not
    contain single CR or LF anyway) *)
let print_header_content buf content =
  let s = String.length content in
  let rec aux prev i =
    if i = s then
      (if prev < s then
         Buffer.add_substring buf content prev (s-prev))
    else
      let add_prev () = Buffer.add_substring buf content prev (i-prev) in
      let c = content.[i] in
      if c = '\r' then
        let i' = i+1 in
        if i' < s && content.[i'] = '\n' then (
          add_prev ();
          Buffer.add_string buf "\r\n ";
          aux (i+2) (i+2)
        ) else (
          add_prev ();
          Buffer.add_char buf ' ';
          aux (i+1) (i+1)
        )
      else
        if c = '\n' then (
          add_prev ();
          Buffer.add_char buf ' ';
          aux (i+1) (i+1)
        )
        else
          aux prev (i+1)
  in
  aux 0 0

(** Write the header lines to a string buffer *)
let headers buf header =
  Http_headers.iter
    (fun name value ->
       Printf.bprintf buf "%s: %a\r\n"
         (Http_headers.name_to_string name) print_header_content value)
    header.H.headers

(** Convert a HTTP header into a string *)
let string_of_header hds =
  let buf = Buffer.create 200 in
  fst_line buf hds;
  headers buf hds;
  Printf.bprintf buf "\r\n%!";
  Buffer.contents buf
