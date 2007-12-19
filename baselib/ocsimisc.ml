(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
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

exception Input_is_too_large
exception Ocsigen_Bad_Request
exception Ocsigen_Request_too_long

let id x = x

let comp f g x = f (g x)

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
  | (b,_)::l when a = b -> list_remove_all_assoc a l
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
    [] -> raise Not_found
  | (b,c)::l when a = b -> c,l
  | b::l -> let v,ll = list_assoc_remove a l in v,b::ll

let rec list_is_prefix l1 l2 = 
  match (l1, l2) with
  | [], _ -> true
  | a::ll1, b::ll2 when a=b -> list_is_prefix ll1 ll2
  | _ -> false


(** various functions for URLs *)

let defaultpagename = "./" 
(* should be "" but this does not work with firefox.
   "index" works but one page may have two different URLs *)

let remove_dotdot = 
    let rec aux = function
      | [] -> []
      | [""] as l -> l
      | ""::l -> aux l
      | ".."::l -> aux l
      | a::l -> a::(aux l)
    in function
      | [] -> []
      | ""::l -> ""::(aux l)
      | l -> aux l
                     
let remove_slash_at_beginning = function
  | [] -> []
  | ""::l -> l
  | l -> l
    
let rec recursively_remove_slash_at_beginning = function
  | [] -> []
  | ""::l -> recursively_remove_slash_at_beginning l
  | l -> l
    
let rec remove_slash_at_end = function
  | []
  | [""] -> []
  | a::l -> a::(remove_slash_at_end l)
    
let remove_internal_slash u =
  let rec aux = function
    | [] -> []
    | [a] -> [a]
    | ""::l -> aux l
    | a::l -> a::(aux l)
  in match u with
    [] -> []
  | a::l -> a::(aux l)
    
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



(* This function is in Neturl (split_path)
   let rec cut_url s = 
   try
   let length = String.length s in
   if length = 0 then []
   else
   let pos_slash = String.index s '/' in
   if pos_slash = 0 
   then cut_url (String.sub s 1 (length-1))
   else 
   let prefix = String.sub s 0 pos_slash in
   (*  if length > (pos_slash+1)
      then *)
   prefix::(cut_url (String.sub s (pos_slash+1) (length - pos_slash - 1)))
   (* else [prefix] *)
   with ? -> [s]
 *)


let rec string_of_url_path = function
  | [] -> ""
  | [a] -> a
  | a::l -> a^"/"^(string_of_url_path l)

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

(*****************************************************************************)

let add_to_string s1 sep = function
  | "" -> s1
  | s2 -> s1^sep^s2

let concat_strings s1 sep s2 = match s1,s2 with
| _,"" -> s1
| "",_ -> s2
| _ -> s1^sep^s2

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
    if s.[i] = ' '
    then 
      let i' = i+step in
      if (i'>endd) || (i' < beg)
      then i' 
      else find_not_space s i' step
    else i
  in    
  let first = find_not_space s beg 1 in
  let last = find_not_space s endd (-1) in
  if last >= first
  then String.sub s first (1+ last - first)
  else ""


(* Cut a string to the next separator, removing spaces.
   Raises Not_found if the separator connaot be found.
 *)
let sep char s =
  let len = String.length s in
  let seppos = String.index s char in
  ((remove_spaces s 0 (seppos-1)),
   (remove_spaces s (seppos+1) (len-1)))


(** splits a string, for ex azert,   sdfmlskdf,    dfdsfs *)
let rec split char s =
  let longueur = String.length s in
  let rec aux deb =
    if deb >= longueur
    then [""]
    else
      try
        let firstsep = String.index_from s deb char in
        (remove_spaces s deb (firstsep-1))::
        (aux (firstsep+1))
      with Not_found -> [remove_spaces s deb (longueur-1)]
  in 
  aux 0

(* printing exceptions *)
let rec string_of_exn = function
  | Dynlink.Error err ->
      "Dynlink.Error: " ^ (Dynlink.error_message err)
  | Unix.Unix_error (ee, func, param) -> 
      (Unix.error_message ee)^" in function "^func^" ("^param^")"
  | e -> Printexc.to_string e


(* Unix.inet_addr is abstract and nothing to convert it :-( *)
let parse_ip s =
  Scanf.sscanf s "%d.%d.%ld.%ld" 
    (fun a b c d -> 
      if a>255 || b>255 || c>255l || d>255l || a<0 || b<0 || c<0l || d<0l then
        failwith "parse_ip"
      else
        Int32.add
          (Int32.mul (Int32.add (Int32.of_int ((a*256+b)*256)) c) 256l) d)

let parse_ip_netmask s =
  try
    let (s1, s2) = sep '/' s in
    let mask =
      try
        let n = int_of_string s2 in
        if n < 0 || n > 32 then failwith "parse_ip_netmask";
        Int32.shift_left 
          0b11111111111111111111111111111111l
          (32 - n)
      with Failure _ -> 
        let mask = parse_ip s2 in
        match mask with
        | 0b11111111111111110000000000000000l
        | 0b11111111111111111111111100000000l
        | 0b11111111111111111111111110000000l
        | 0b11111111111111111111111111000000l
        | 0b11111111111111111111111111100000l
        | 0b11111111111111111111111111110000l
        | 0b11111111111111111111111111111000l
        | 0b11111111111111111111111111111100l
        | 0b11111111111111111111111111111110l
        | 0b11111111111111111111111111111111l
        | 0b11111111111111111111111000000000l
        | 0b11111111111111111111110000000000l
        | 0b11111111111111111111100000000000l
        | 0b11111111111111111111000000000000l
        | 0b11111111111111111110000000000000l
        | 0b11111111111111111100000000000000l
        | 0b11111111111111111000000000000000l
        | 0b11111111111111100000000000000000l
        | 0b11111111111111000000000000000000l
        | 0b11111111111110000000000000000000l
        | 0b11111111111100000000000000000000l
        | 0b11111111111000000000000000000000l
        | 0b11111111110000000000000000000000l
        | 0b11111111100000000000000000000000l
        | 0b11111111000000000000000000000000l
        | 0b11111110000000000000000000000000l
        | 0b11111100000000000000000000000000l
        | 0b11111000000000000000000000000000l
        | 0b11110000000000000000000000000000l
        | 0b11100000000000000000000000000000l
        | 0b11000000000000000000000000000000l
        | 0b10000000000000000000000000000000l
        | 0b00000000000000000000000000000000l -> mask
        | _ -> failwith "parse_ip_netmask"
    in
      (Int32.logand (parse_ip s1) mask, mask)
  with Not_found -> 
    let mask = 0b11111111111111111111111111111111l in
    (parse_ip s, mask)




(* *)
let fst3 (a, _, _) = a
let snd3 (_, a, _) = a
let thd3 (_, _, a) = a

let get_inet_addr host =
  let rec aux = function
    | [] -> Lwt.fail Not_found
    | {Unix.ai_addr=Unix.ADDR_INET (inet_addr, _)}::_ -> Lwt.return inet_addr
    | _::l -> aux l
  in
  Lwt.bind
    (Lwt_lib.getaddrinfo host "" [])
    aux
