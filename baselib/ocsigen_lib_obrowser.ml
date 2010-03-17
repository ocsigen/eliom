(* Ocsigen
 * Copyright (C) 2005-2008 Vincent Balat, Stéphane Glondu
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

exception Ocsigen_Internal_Error of string

external id : 'a -> 'a = "%identity"
let (>>=) = Lwt.bind

type ('a, 'b) leftright = Left of 'a | Right of 'b

let comp f g x = f (g x)

let apply_option f = function
  | None -> None
  | Some v -> Some (f v)

(*****************************************************************************)
let add_to_string s1 sep = function
  | "" -> s1
  | s2 -> s1^sep^s2

let concat_strings s1 sep s2 = match s1, s2 with
| _, "" -> s1
| "", _ -> s2
| _ -> s1^sep^s2


type url_path = string list

(************************************************************************)
(* absolute urls *)

let make_absolute_url ~https ~host ~port uri =
  (if https
   then "https://"
   else "http://"
  )^
    host^
    (if (port = 80 && not https) || (https && port = 443)
     then ""
     else ":"^string_of_int port)^
    uri


(*****************************************************************************)
module String_Table = Map.Make(struct
  type t = string
  let compare = compare
end)

type file_info = {tmp_filename: string;
                  filesize: int64;
                  raw_original_filename: string;
                  original_basename: string;
                  file_content_type: (string * string option) option;
                 }

(*****************************************************************************)
type ip_address =
  | IPv4 of int32
  | IPv6 of int64 * int64




(*****************************************************************************)
let remove_internal_slash u =
  let rec aux = function
    | [] -> []
    | [a] -> [a]
    | ""::l -> aux l
    | a::l -> a::(aux l)
  in match u with
  | [] -> []
  | a::l -> a::(aux l)



(*****************************************************************************)



