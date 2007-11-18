(* Ocsigen
 * ocsiheaders.ml Copyright (C) 2005 Vincent Balat
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

(** This module is for getting informations from HTTP header. *)
(** It uses the lowel level module Http_frame.Http_header.    *)
(** It is very basic and must be completed for exhaustiveness. *)
(* Operation on strings are hand-written ... *)
(* Include in a better cooperative parser for header or use regexp?. *)

open Http_frame
open Predefined_senders
open Ocsimisc

(*
XXX Get rid of all "try ... with _ -> ..."
*)
let list_flat_map f l = List.flatten (List.map f l)

(* splits a quoted string, for ex "azert", "  sdfmlskdf",    "dfdsfs" *)
(* We are too kind ... We accept even if the separator is not ok :-( ? *)
let rec quoted_split char (* char is not used in that version *) s =
  let longueur = String.length s in
  let rec aux deb =
    let rec nextquote s i = 
      if i>=longueur
      then failwith ""
      else
        if s.[i] = '"' 
        then i 
        else 
          if s.[i] = '\\' 
          then nextquote s (i+2)
          else nextquote s (i+1)
    in
    try
      let first = (nextquote s deb) + 1 in
      let afterlast = nextquote s first in
      let value = String.sub s first (afterlast - first) in
      value::
      (if (afterlast + 1) < longueur
      then aux (afterlast + 1)
      else [])
    with Failure _ | Invalid_argument _ -> []
  in 
  aux 0


let parse_quality parse_name s =
  try
    let a,b = sep ';' s in
    let q,qv = sep '=' b in
    if q="q"
    then ((parse_name a), Some (float_of_string qv))
    else failwith "Parse error"
  with _ -> ((parse_name s), None)

let parse_star a =
  if a = "*"
  then None
  else Some a

let parse_mime_type a =
  let b,c = sep '/' a in
  ((parse_star b), (parse_star c))

let parse_extensions parse_name s =
  try
    let a,b = sep ';' s in
    ((parse_name a), List.map (sep '=') (split ';' b))
  with _ -> ((parse_name s), [])

let parse_list_with_quality parse_name s =
  let splitted = list_flat_map (split ',') s in
  List.map (parse_quality parse_name) splitted

let parse_list_with_extensions parse_name s =
  let splitted = list_flat_map (split ',') s in
  List.map (parse_extensions parse_name) splitted


(*****************************************************************************)
let rec parse_cookies s =
  let splitted = split ';' s in
  try
    List.fold_left 
      (fun beg a -> 
        let (n, v) = sep '=' a in
        Http_frame.Cookievalues.add n v beg)
      Http_frame.Cookievalues.empty 
      splitted
  with _ -> Http_frame.Cookievalues.empty
(*VVV Actually the real syntax of cookies is more complex! *)
(*
Mozilla spec + RFC2109
http://ws.bokeland.com/blog/376/1043/2006/10/27/76832
*)


let get_keepalive http_header =
  Http_header.get_proto http_header = Http_frame.Http_header.HTTP11
    &&
  try
    String.lowercase
      (Http_header.get_headers_value http_header Http_headers.connection)
      <> "close"
  with Not_found ->
    true

(* RFC 2616, sect. 14.23 *)
(* XXX Not so simple: the host name may contain a colon! (RFC 3986) *)
let get_host_and_port http_frame =
  try
    let hostport =
      Http_header.get_headers_value
        http_frame.Http_frame.header Http_headers.host
    in
    try
      let h,p = sep ':' hostport in
      try
        Some (h, int_of_string p)
      with Failure _ ->
        None
    with Not_found ->
      Some (hostport, 80)
      (* port 80 is the default for HTTP *)
  with Not_found ->
    None

let get_user_agent http_frame =
  try (Http_header.get_headers_value
         http_frame.Http_frame.header Http_headers.user_agent)
  with Not_found -> ""

let get_cookie_string http_frame =
  try
    Some (Http_header.get_headers_value
            http_frame.Http_frame.header Http_headers.cookie)
  with Not_found ->
    None

let get_if_modified_since http_frame =
  try 
    Some (Netdate.parse_epoch 
            (Http_header.get_headers_value
               http_frame.Http_frame.header
               Http_headers.if_modified_since))
  with _ -> None


let get_if_unmodified_since http_frame =
  try 
    Some (Netdate.parse_epoch 
            (Http_header.get_headers_value
               http_frame.Http_frame.header
               Http_headers.if_unmodified_since))
  with _ -> None


let get_if_none_match http_frame =
  try 
    list_flat_map
      (quoted_split ',')
      (Http_header.get_headers_values
         http_frame.Http_frame.header Http_headers.if_none_match)
  with _ -> []


let get_if_match http_frame =
  try 
    Some
      (list_flat_map
         (quoted_split ',')
         (Http_header.get_headers_values
            http_frame.Http_frame.header Http_headers.if_match))
  with _ -> None


let get_content_type http_frame =
  try
    Some (Http_header.get_headers_value
            http_frame.Http_frame.header Http_headers.content_type)
  with _ -> None


let get_content_length http_frame =
  try
    Some 
      (Int64.of_string 
         (Http_header.get_headers_value 
            http_frame.Http_frame.header Http_headers.content_length))
  with _ -> None


let get_referer http_frame =
  try
    Some 
      (Http_header.get_headers_value 
         http_frame.Http_frame.header Http_headers.referer)
  with _ -> None


let get_referrer = get_referer


let get_accept http_frame =
  try
    let l =
      parse_list_with_extensions
        parse_mime_type
        (Http_header.get_headers_values
           http_frame.Http_frame.header Http_headers.accept)
    in
    let change_quality (a, l) =
      try
        let q,ll = list_assoc_remove "q" l in
        (a, Some (float_of_string q), ll)
      with _ -> (a, None, l)
    in
    List.map change_quality l
  with _ -> []


let get_accept_charset http_frame =
  try
    parse_list_with_quality
      parse_star
      (Http_header.get_headers_values
         http_frame.Http_frame.header Http_headers.accept_charset)
  with _ -> []


let get_accept_encoding http_frame =
  try
    parse_list_with_quality
      parse_star
      (Http_header.get_headers_values
         http_frame.Http_frame.header Http_headers.accept_encoding)
  with _ -> []


let get_accept_language http_frame =
  try
    parse_list_with_quality
      Ocsimisc.id
      (Http_header.get_headers_values
         http_frame.Http_frame.header Http_headers.accept_language)
  with _ -> []


