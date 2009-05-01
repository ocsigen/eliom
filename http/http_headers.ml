(* Ocsigen
 * http://www.ocsigen.org
 * Module http_headers.mli
 * Copyright (C) 2007 Jérôme Vouillon
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

type name = string
let name s = String.lowercase s
let name_to_string s = s

let accept = name "Accept"
let accept_charset = name "Accept-Charset"
let accept_encoding = name "Accept-Encoding"
let accept_language = name "Accept-Language"
let accept_ranges = name "Accept-Ranges"
let cache_control = name "Cache-Control"
let connection = name "Connection"
let content_encoding = name "Content-Encoding"
let content_range = name "Content-Range"
let content_length = name "Content-Length"
let content_type = name "Content-Type"
let cookie = name "Cookie"
let date = name "Date"
let etag = name "ETag"
let expires = name "Expires"
let host = name "Host"
let if_match = name "If-Match"
let if_modified_since = name "If-Modified-Since"
let if_none_match = name "If-None-Match"
let if_unmodified_since = name "If-Unmodified-Since"
let if_range = name "If-Range"
let last_modified = name "Last-Modified"
let location = name "Location"
let server = name "Server"
let set_cookie = name "Set-Cookie"
let status = name "Status"
let transfer_encoding = name "Transfer-Encoding"
let user_agent = name "User-Agent"
let referer = name "Referer"
let range = name "Range"

module NameHtbl =
  Hashtbl.Make
    (struct
       type t = name
       let equal (n : string) n' = n = n'
       let hash = Hashtbl.hash
     end)

(****)

module Map = Map.Make (String)

type t = string list Map.t

let empty = Map.empty

let find_all n h = List.rev (Map.find n h)

(*XXX We currently return the last header.
  Should we fail if there is more than one? *)
let find n h =
  match Map.find n h with
    v :: _ -> v
  | _      -> assert false

let replace n v h = Map.add n [v] h

let replace_opt n v h =
 match v with
   None   -> Map.remove n h
 | Some v -> replace n v h

let add n v h =
  let vl = try find_all n h with Not_found -> [] in
  Map.add n (v :: vl) h

let iter f h =
  Map.iter
    (fun n vl ->
       match vl with
         [v] -> f n v
       | _   -> List.iter (fun v -> f n v) (List.rev vl))
    h

let fold f h acc =
  Map.fold
    (fun n vl acc -> f n (List.rev vl) acc)
    h acc

let with_defaults h h' = Map.fold Map.add h h'



(****)
let (<<) h (n, v) = replace n v h

let dyn_headers =
  empty
  << (cache_control, "no-cache")
  << (expires, "0")

