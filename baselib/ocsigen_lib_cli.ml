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

let uncurry2 f (x, y) = f x y

let rec list_remove_first_if_any a = function
  |  [] -> []
  | b::l when a = b -> l
  | b::l -> b::(list_remove_first_if_any a l)

let rec list_remove_first_if_any_q a = function
  |  [] -> []
  | b::l when a == b -> l
  | b::l -> b::(list_remove_first_if_any_q a l)

let rec list_remove_first a = function
  |  [] -> raise Not_found
  | b::l when a = b -> l
  | b::l -> b::(list_remove_first a l)

let rec list_remove_first_q a = function
  | [] -> raise Not_found
  | b::l when a == b -> l
  | b::l -> b::(list_remove_first_q a l)

let rec list_remove_all a = function
  | [] -> []
  | b::l when a = b -> list_remove_all a l
  | b::l -> b::(list_remove_all a l)

let rec list_remove_all_q a = function
  | [] -> []
  | b::l when a == b -> list_remove_all_q a l
  | b::l -> b::(list_remove_all_q a l)

let rec list_remove_all_assoc a = function
  | [] -> []
  | (b, _)::l when a = b -> list_remove_all_assoc a l
  | b::l -> b::(list_remove_all_assoc a l)

let rec list_remove_all_assoc_q a = function
  | [] -> []
  | (b,_)::l when a == b -> list_remove_all_assoc_q a l
  | b::l -> b::(list_remove_all_assoc_q a l)



let rec list_last = function
  |  [] -> raise Not_found
  | [b] -> b
  | _::l -> list_last l

let rec list_assoc_remove a = function
  | [] -> raise Not_found
  | (b, c)::l when a = b -> c, l
  | b::l -> let v, ll = list_assoc_remove a l in (v, b::ll)



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
| _ -> String.concat sep [s1;s2]


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
module String_Table = Map.Make(String)

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

let rec list_is_prefix l1 l2 =
  match (l1, l2) with
  | [], _ -> true
  | a::ll1, b::ll2 when a=b -> list_is_prefix ll1 ll2
  | _ -> false

let rec list_is_prefix_skip_end_slash l1 l2 =
  match (l1, l2) with
  | [""], _
  | [], _ -> true
  | a::ll1, b::ll2 when a=b -> list_is_prefix_skip_end_slash ll1 ll2
  | _ -> false


(*****************************************************************************)

(* Cut a string to the next separator *)
let basic_sep char s =
  try
    let seppos = String.index s char in
    ((String.sub s 0 seppos),
     (String.sub s (seppos+1)
        (1 + (String.length s) - seppos)))
  with Invalid_argument _ -> raise Not_found


(* Returns a copy of the string from beg to endd,
   removing spaces at the beginning and at the end *)
let remove_spaces s beg endd =
  let rec find_not_space s i step =
    if (i>endd) || (i < beg)
    then i
    else
      if s.[i] = ' '
      then find_not_space s (i+step) step
      else i
  in
  let first = find_not_space s beg 1 in
  let last = find_not_space s endd (-1) in
  if last >= first
  then String.sub s first (1+ last - first)
  else ""


(** Cut a string to the next separator, removing spaces.
   Raises Not_found if the separator connot be found.
 *)
let sep char s =
  let len = String.length s in
  let seppos = String.index s char in
  ((remove_spaces s 0 (seppos-1)),
   (remove_spaces s (seppos+1) (len-1)))


(** splits a string, for ex azert,   sdfmlskdf,    dfdsfs *)
let rec split ?(multisep=false) char s =
  let longueur = String.length s in
  let rec aux deb =
    if deb >= longueur
    then []
    else
      try
        let firstsep = String.index_from s deb char in
        if multisep && firstsep = deb then
          aux (deb + 1)
        else
        (remove_spaces s deb (firstsep-1))::
        (aux (firstsep+1))
      with Not_found -> [remove_spaces s deb (longueur-1)]
  in
  aux 0


(** various functions for URLs *)

let remove_dotdot =
  (* removes "../" *) 
  let rec aux = function
    | [] -> []
    | [""] as l -> l
(*    | ""::l -> aux l *) (* we do not remove "//" any more, 
                             because of optional suffixes in Eliom *)
    | ".."::l -> aux l
    | a::l -> a::(aux l)
  in function
    | [] -> []
    | ""::l -> ""::(aux l)
    | l -> aux l

let remove_slash_at_beginning = function
  | [] -> []
  | [""] -> [""]
  | ""::l -> l
  | l -> l

let rec recursively_remove_slash_at_beginning = function
  | [] -> []
  | [""] -> [""]
  | ""::l -> recursively_remove_slash_at_beginning l
  | l -> l

let rec remove_slash_at_end = function
  | []
  | [""] -> []
  | a::l -> a::(remove_slash_at_end l)

let rec add_end_slash_if_missing = function
  | [] -> [""]
  | [""] as a -> a
  | a::l -> a::(add_end_slash_if_missing l)

let change_empty_list = function
  | [] -> [""] (* It is not possible to register an empty URL *)
  | l -> l

let remove_end_slash s =
  try
    if s.[(String.length s) - 1] = '/'
    then String.sub s 0 ((String.length s) - 1)
    else s
  with Invalid_argument _ -> s


let rec string_first_diff s1 s2 n last =
(* returns the index of the first difference between s1 and s2,
   starting from n and ending at last.
   returns (last + 1) if no difference is found.
 *)
  try
    if s1.[n] = s2.[n]
    then begin
      if n = last
      then last+1
      else string_first_diff s1 s2 (n+1) last
    end
    else n
  with Invalid_argument _ -> n

