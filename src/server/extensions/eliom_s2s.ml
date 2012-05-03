(* Ocsigen
 * Copyright (C) 2010 Simon Castellan
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
(* Server To Server communication *)

open Eliom_lib

open Lwt
open Ocsigen_stream
open Ocsigen_http_frame

let filter_map f l =
  List.fold_left (fun l t -> match f t with
    | Some t' -> t' :: l
    | None -> l) [] l

let rec find_map f = function
  | [] -> raise Not_found
  | t :: q -> match f t with
      | Some x -> x
      | None -> find_map f q

let strip ?(sep = '.') v s = try
                  let start, rest = String.basic_sep sep s in
                  if start = v then Some rest
                  else None
  with Not_found -> None

let strip2 ?sep v (name, value) =
    Option.map (fun y -> y, value) (strip ?sep v name)


(* Parameters *)
let format_url base params = base ^ "?" ^ Netencoding.Url.mk_url_encoded_parameters params
let push_ns ?(sep = ".") ?namespace_param namespace params = 
  List.map (fun (name, value) -> String.may_append namespace ~sep name, value) 
    (match namespace_param with
      | Some a -> a :: params
      | None -> params)

let strip_ns namespace params = filter_map (strip2 namespace) params
let find_in_ns ?(namespace_param = "ns") ?default_namespace url params =
  let ns = 
    try 
      let (name, _) = List.find (fun (_, value) -> value = url) params in
      match strip namespace_param name with
        | Some namespace -> namespace
        | None -> raise Not_found
    with Not_found -> (match default_namespace with
      | Some x -> x
      | None -> raise Not_found)
  in 
  filter_map (strip2 ns) params


(* Request *)
let rec do_request f uri = 
  let url = Neturl.parse_url uri in
  let host, https = Neturl.url_host url, Neturl.url_scheme url = "https" in
  f ~host ~https ~uri >>=
    (fun frame ->
      (* Follow redirection *)
      match frame.frame_header.Http_header.mode with
        | Http_header.Answer 301 | Http_header.Answer 302 
        | Http_header.Answer 303 | Http_header.Answer 307 ->
          let uri = Ocsigen_http_frame.Http_header.get_headers_value frame.frame_header
            Http_headers.location in
          do_request f uri
        | _ -> Lwt.return frame)


let do_post_request params = 
  do_request 
    (fun ~host ~https ~uri ->
      Ocsigen_http_client.post_string ~host ~https ~uri
        ~content_type:("application", "x-www-form-urlencoded")
        ~content: (Netencoding.Url.mk_url_encoded_parameters params) ()) 

let do_get_request ?(params=[]) = 
  do_request (fun ~host ~https ~uri -> 
    let uri =
      if params = [] then uri
      else format_url uri params
    in
    Ocsigen_http_client.get ~host ~https ~uri ())


let get_frame_content frame = 
  match frame.frame_content with
    | Some st -> string_of_stream 1000000 (get st)
    | None -> Lwt.return ""

(* Parsing body answers *)
let parse_key_pairs contents = 
  let parse_line values s = 
    try Scanf.sscanf s "%[^:]: %[^\n]" (fun a b -> (a, b) :: values)
    with _ -> values
  in
  let lines = Netstring_pcre.split (Netstring_pcre.regexp "\n") contents in
  List.fold_left parse_line [] lines
let direct_request params endpoint = 
  do_post_request params endpoint >>=
  get_frame_content >>= (fun s -> return (parse_key_pairs s))
