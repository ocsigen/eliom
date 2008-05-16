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
      Format.fprintf buf "%s %i %s\r\n" (string_of_proto header.H.proto)
        code (Http_error.expl_of_code code)
  | H.Query (meth, url) ->
      Format.fprintf buf "%s %s %s\r\n"
        (string_of_method meth) url (string_of_proto header.H.proto)

(** Write the header lines to a string buffer *)
let headers buf header =
  Http_headers.iter
    (fun name value ->
       Format.fprintf buf "%s: %s\r\n"
         (Http_headers.name_to_string name) value)
    header.H.headers

(** Convert a HTTP header into a string *)
let string_of_header hds =
  let buf = Buffer.create 200 in
  let f = Format.formatter_of_buffer buf in
  fst_line f hds;
  headers f hds;
  Format.fprintf f "\r\n@?";
  Buffer.contents buf
