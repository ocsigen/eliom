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

exception Input_is_too_large
exception Ocsigen_Bad_Request
exception Ocsigen_Request_too_long

external id : 'a -> 'a = "%identity"
let (>>=) = Lwt.bind

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

let rec list_is_prefix_skip_end_slash l1 l2 =
  match (l1, l2) with
  | [""], _
  | [], _ -> true
  | a::ll1, b::ll2 when a=b -> list_is_prefix ll1 ll2
  | _ -> false


(** various functions for URLs *)

let remove_dotdot =
  (* removes "../" and "//" *)
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

let remove_internal_slash u =
  let rec aux = function
    | [] -> []
    | [a] -> [a]
    | ""::l -> aux l
    | a::l -> a::(aux l)
  in match u with
  | [] -> []
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

(* printing exceptions *)

let (string_of_exn, register_exn_printer) =
  let current = ref
    (fun f_rec -> function
       | Unix.Unix_error (ee, func, param) ->
           Printf.sprintf "%s in function %s (%s)"
             (Unix.error_message ee) func param
       | e -> Printexc.to_string e)
  in
  let rec string_of_exn e = !current string_of_exn e in
  (string_of_exn,
   (fun p ->
      let old = !current in
      current :=
        (fun f_rec s ->
           try p f_rec s
           with e -> old f_rec s)))


(* Unix.inet_addr is abstract and nothing to convert it :-( *)

type ip_address =
  | IPv4 of int32
  | IPv6 of int64 * int64

exception Invalid_ip_address of string


let parse_ip s =
  let s = String.lowercase s in
  let n = String.length s in
  let is6 = String.contains s ':' in
  let failwith fmt = Printf.ksprintf (fun s -> raise (Invalid_ip_address s)) fmt in

  let rec parse_hex i accu =
    match (if i < n then s.[i] else ':') with
      | '0'..'9' as c -> parse_hex (i+1) (16*accu+(int_of_char c)-48)
      | 'a'..'f' as c -> parse_hex (i+1) (16*accu+(int_of_char c)-87)
      | _ -> (i, accu)
  in
  let rec parse_dec i accu =
    match (if i < n then s.[i] else '.') with
      | '0'..'9' as c -> parse_dec (i+1) (10*accu+(int_of_char c)-48)
      | _ -> (i, accu)
  in
  let rec next_is_dec i =
    if i < n then
      match s.[i] with
        | ':' -> false
        | '.' -> true
        | _ -> next_is_dec (i+1)
    else false
  in
  let rec parse_component i accu nb =
    if i < n then
      if next_is_dec i then
        let (i1, a) = parse_dec i 0 in
        if i1 = i || (i1 < n && s.[i1] <> '.') then failwith "invalid dot notation in %s (1)" s;
        let (i2, b) = parse_dec (i1+1) 0 in
        if i2 = i1 then failwith "invalid dot notation in %s (2)" s;
        let component =
          if a < 0 || a > 255 || b < 0 || b > 255 then
            failwith "invalid dot notation in %s (3)" s
          else (a lsl 8) lor b
        in
        if i2 < n-1 && (s.[i2] = ':' || s.[i2] = '.') then
          parse_component (i2+1) (component::accu) (nb+1)
        else
          (i2, component::accu, nb+1)
      else if s.[i] = ':' then
        parse_component (i+1) ((-1)::accu) nb
      else
        let (i1, a) = parse_hex i 0 in
        if a < 0 || a > 0xffff then failwith "invalid colon notation in %s" s;
        if i1 = i then
          (i, accu, nb)
        else if i1 < n-1 && s.[i1] = ':' then
          parse_component (i1+1) (a::accu) (nb+1)
        else
          (i1, a::accu, nb+1)
    else
      (i, accu, nb)
  in

  let (i, addr_list, size_list) =
    if 1 < n && s.[0] = ':' && s.[1] = ':' then
      parse_component 2 [-1] 0
    else
      parse_component 0 [] 0
  in

  if size_list > 8 then failwith "too many components in %s" s;

  let maybe_mask =
    if i < n && s.[i] = '/' then
      let (i1, m) = parse_dec (i+1) 0 in
      if i1 = i+1 || i1 < n || m < 0 || m > (if is6 then 128 else 32) then
        failwith "invalid /n suffix in %s" s
      else
        Some m
    else if i < n then
      failwith "invalid suffix in %s (from index %i)" s i
    else
      None
  in

  if is6 then
    let (++) a b = Int64.logor (Int64.shift_left a 16) (Int64.of_int b) in
    let normalized =
      let rec aux_add n accu =
        if n = 0 then accu else aux_add (n-1) (0::accu)
      in
      let rec aux_rev accu = function
        | [] -> accu
        | (-1)::q -> aux_rev (aux_add (8-size_list) accu) q
        | a::q -> aux_rev (a::accu) q
      in
      aux_rev [] addr_list
    in
    let maybe_mask = match maybe_mask with
      | Some n when n > 64 ->
          Some (IPv6 (Int64.minus_one, Int64.shift_left Int64.minus_one (128-n)))
      | Some n ->
          Some (IPv6 (Int64.shift_left Int64.minus_one (64-n), Int64.zero))
      | None -> None
    in
    match normalized with
      | [a; b; c; d; e; f; g; h] ->
          IPv6 (Int64.zero ++ a ++ b ++ c ++ d,
                Int64.zero ++ e ++ f ++ g ++ h), maybe_mask
      | _ -> failwith "invalid IPv6 address: %s (%d components)" s (List.length normalized)
  else
    let (++) a b = Int32.logor (Int32.shift_left a 16) (Int32.of_int b) in
    let maybe_mask = match maybe_mask with
      | Some n ->
          Some (IPv4 (Int32.shift_left Int32.minus_one (32-n)))
      | None -> None
    in
    match addr_list with
      | [b; a] ->
          IPv4 (Int32.zero ++ a ++ b), maybe_mask
      | _ -> failwith "invalid IPv4 address: %s" s


let match_ip (base, mask) ip =
  match ip,  base, mask with
    | IPv4 a, IPv4 b, Some (IPv4 m) -> Int32.logand a m = Int32.logand b m
    | IPv4 a, IPv4 b, None -> a = b
    | IPv6 (a1,a2), IPv6 (b1,b2), Some (IPv6 (m1,m2)) ->
        Int64.logand a1 m1 = Int64.logand b1 m1 &&
        Int64.logand a2 m2 = Int64.logand b2 m2
    | IPv6 (a1,a2), IPv6 (b1,b2), None -> a1 = b1 && a2 = b2
    | IPv6 (a1,a2), IPv4 b, c
        when a1 = 0L && Int64.logand a2 0xffffffff00000000L = 0xffff00000000L ->
        (* might be insecure, cf
           http://tools.ietf.org/internet-drafts/draft-itojun-v6ops-v4mapped-harmful-02.txt *)
        let a = Int64.to_int32 a2 in
        begin match c with
          | Some (IPv4 m) -> Int32.logand a m = Int32.logand b m
          | Some (IPv6 _) -> invalid_arg "match_ip"
          | None -> a = b
        end
    | _ -> false

(* *)
let fst3 (a, _, _) = a
let snd3 (_, a, _) = a
let thd3 (_, _, a) = a

exception No_such_host

let get_inet_addr host =
  let rec aux = function
    | [] -> Lwt.fail No_such_host
    | {Unix.ai_addr=Unix.ADDR_INET (inet_addr, _)}::_ -> Lwt.return inet_addr
    | _::l -> aux l
  in
  Lwt.bind
    (Lwt_lib.getaddrinfo host "" [])
    aux

let getnameinfo ia p =
  try
    Lwt_lib.getnameinfo (Unix.ADDR_INET (ia, p)) [Unix.NI_NAMEREQD] >>= fun r ->
    Lwt.return r.Unix.ni_hostname
  with
  | Not_found ->
      let hs = Unix.string_of_inet_addr ia in
      Lwt.return
        (if String.length hs > 7 && String.sub hs 0 7 = "::ffff:"
        then String.sub hs 7 (String.length hs - 7)
        else if String.contains hs ':'
        then "["^hs^"]"
        else hs)


(************************************************************************)
(* URL parsing *)

(*VVV Ces deux trucs sont dans Neturl version 1.1.2 mais en attendant qu'ils
   soient dans debian, je les mets ici *)
let problem_re = Netstring_pcre.regexp "[ <>\"{}|\\\\^\\[\\]`]"

let fixup_url_string =
  Netstring_pcre.global_substitute
    problem_re
    (fun m s ->
       Printf.sprintf "%%%02x"
        (Char.code s.[Netstring_pcre.match_beginning m]))

(*VVV This is in Netencoding but we have a problem with ~ 
  (not encoded by browsers). Here is a patch that does not encode '~': *)
module MyUrl = struct
  let hex_digits =
    [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
       '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |];;

  let to_hex2 k =
    (* Converts k to a 2-digit hex string *)
    let s = String.create 2 in
    s.[0] <- hex_digits.( (k lsr 4) land 15 );
    s.[1] <- hex_digits.( k land 15 );
    s ;;

  let of_hex1 c =
    match c with
        ('0'..'9') -> Char.code c - Char.code '0'
      | ('A'..'F') -> Char.code c - Char.code 'A' + 10
      | ('a'..'f') -> Char.code c - Char.code 'a' + 10
      | _ ->
        raise Not_found ;;

  let url_encoding_re =
    Netstring_pcre.regexp "[^A-Za-z0-9_.!*-~]";;

  let encode ?(plus = true) s =
    Netstring_pcre.global_substitute
      url_encoding_re
      (fun r _ ->
         match Netstring_pcre.matched_string r s with
             " " when plus -> "+"
           | x ->
               let k = Char.code(x.[0]) in
               "%" ^ to_hex2 k
      )
      s ;;
end


let string_of_url_path ~encode l = 
  if encode
  then
    fixup_url_string (String.concat "/"
                        (List.map (*Netencoding.Url.encode*) MyUrl.encode l))
  else String.concat "/" l (* BYXXX : check illicit characters *)

let parse_url =

  (*SSS Neturl doesn't recognize http://[2002::1]:80. Workaround: we
    chop the http://host:port part and use Neturl for the rest.
    We do not accept http://login:pwd@host:port (should we?). *)
  let url_re = Netstring_pcre.regexp "^([Hh][Tt][Tt][Pp][Ss]?)://([0-9a-zA-Z.-]+|\\[[0-9A-Fa-f:.]+\\])(:([0-9]+))?(/.*)$" in
  let url_relax_re = Netstring_pcre.regexp "^[Hh][Tt][Tt][Pp][Ss]?://[^/]+" in

  fun url ->
    let (https, host, port, url2) =
      try
        let url2 = Neturl.parse_url
          ~base_syntax:(Hashtbl.find Neturl.common_url_syntax "http")
          (fixup_url_string url)
        in
        let https = 
          try (match Neturl.url_scheme url2 with
                 | "http" -> Some false
                 | "https" -> Some true
                 | _ -> None) 
          with Not_found -> None 
        in
        let host = try Some (Neturl.url_host url2) with Not_found -> None in
        let port = try Some (Neturl.url_port url2) with Not_found -> None in
        (https, host, port, url2)
      with Neturl.Malformed_URL ->
        match Netstring_pcre.string_match url_re url 0 with
          | None -> raise Neturl.Malformed_URL
          | Some m ->
              let url2 = Neturl.parse_url
                ~base_syntax:(Hashtbl.find Neturl.common_url_syntax "http")
                (fixup_url_string (Netstring_pcre.matched_group m 5 url)) in
              let https =
                try (match Netstring_pcre.matched_group m 1 url with
                       | "http" -> Some false
                       | "https" -> Some true
                       | _ -> None)
                with Not_found -> None in
              let host =
                try Some (Netstring_pcre.matched_group m 2 url)
                with Not_found -> None in
              let port =
                try Some (int_of_string (Netstring_pcre.matched_group m 4 url))
                with Not_found -> None in
              (https, host, port, url2)
    in

    (* We don't do it before because we don't want [] of IPv6
       addresses to be escaped *)
    let url = fixup_url_string url in

    (* We keep only the path part of the URL *)
    let url = Netstring_pcre.replace_first url_relax_re "" url in

    (* Note that the fragment (string after #) is not sent by browsers *)

    let params =
      try Some (Neturl.url_query ~encoded:true url2)
      with Not_found -> None
    in

    let get_params =
      lazy begin
        let params_string =
          try Neturl.url_query ~encoded:true url2
          with Not_found -> ""
        in try 
          Netencoding.Url.dest_url_encoded_parameters params_string
        with Failure _ -> raise Ocsigen_Bad_Request
      end
    in

    let path =
      remove_dotdot (* and remove "//" *)
        (remove_slash_at_beginning (Neturl.url_path url2))
        (* here we remove .. from paths, as it is dangerous.
           But in some very particular cases, we may want them?
           I prefer forbid that. *)
    in

    (https, host, port, url, url2, path, params, get_params)


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


(************************************************************************)

let basename f =
  let n = String.length f in
  let i = try String.rindex f '\\' + 1 with Not_found -> 0 in
  let j = try String.rindex f '/' + 1 with Not_found -> 0 in
  let k = max i j in
  if k < n then
    String.sub f k (n-k)
  else
    "none"


let extension_no_directory filename =
  try
    let pos = String.rindex filename '.' in
    String.sub filename (pos+1) ((String.length filename) - pos - 1)
  with Not_found ->
    raise Not_found

let extension filename =
  try
    let pos = String.rindex filename '.'
    and slash =
      try String.rindex filename '/'
      with Not_found -> -1
    in
    if pos > slash then
      String.sub filename (pos+1) ((String.length filename) - pos - 1)
    else (* Dot before a directory separator *)
      raise Not_found
  with Not_found -> (* No dot in filename *)
    raise Not_found


(* *)
type ('a, 'b) leftright = Left of 'a | Right of 'b
type yesnomaybe = Yes | No | Maybe

module StringSet = Set.Make(String)

