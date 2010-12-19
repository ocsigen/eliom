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

(* TODO: organize this ... *)

include Ocsigen_lib_cli

exception Input_is_too_large
exception Ocsigen_Bad_Request
exception Ocsigen_Request_too_long


let split_path = Neturl.split_path 
(* there is an implementation in Ocsigen_lib for client side *)


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

let inet6_addr_loopback = 
  fst (parse_ip (Unix.string_of_inet_addr Unix.inet6_addr_loopback))

let network_of_ip ip mask4 (mask61, mask62) = match ip with
  | IPv4 a -> IPv4 (Int32.logand a mask4)
  | IPv6 (a, b) -> IPv6 (Int64.logand a mask61, Int64.logand b mask62)


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

(* Taken from Neturl version 1.1.2 *)
let problem_re1 = Netstring_pcre.regexp "[ <>\"{}|\\\\^\\[\\]`]"

let fixup_url_string1 =
  Netstring_pcre.global_substitute
    problem_re1
    (fun m s ->
       Printf.sprintf "%%%02x"
        (Char.code s.[Netstring_pcre.match_beginning m]))

(* I add this fixup to handle %uxxxx sent by browsers.
   Translated to %xx%xx *)
let problem_re2 = Netstring_pcre.regexp "\\%u(..)(..)"

let fixup_url_string s =
  fixup_url_string1
    (Netstring_pcre.global_substitute
       problem_re2
       (fun m s ->
          String.concat "" ["%"; Netstring_pcre.matched_group m 1 s; 
                            "%"; Netstring_pcre.matched_group m 2 s]
       )
       s)

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

  let url_encoding_re =
    Netstring_pcre.regexp "[^A-Za-z0-9~_.!*\\-]";;

  let encode ?(plus = true) s =
    Netstring_pcre.global_substitute
      url_encoding_re
      (fun r _ ->
         match Netstring_pcre.matched_string r s with
           | " " when plus -> "+"
           | x ->
               let k = Char.code(x.[0]) in
               "%" ^ to_hex2 k
      )
      s

end

let encode = MyUrl.encode
let decode = Netencoding.Url.decode

let mk_url_encoded_parameters = Netencoding.Url.mk_url_encoded_parameters

let string_of_url_path ~encode l =
  if encode
  then
    fixup_url_string (String.concat "/"
                        (List.map (*Netencoding.Url.encode*) 
                           MyUrl.encode l))
  else String.concat "/" l (* BYXXX : check illicit characters *)


let parse_url =

  (* We do not accept http://login:pwd@host:port (should we?). *)
  let url_re = Netstring_pcre.regexp "^([Hh][Tt][Tt][Pp][Ss]?)://([0-9a-zA-Z.-]+|\\[[0-9A-Fa-f:.]+\\])(:([0-9]+))?/([^\\?]*)(\\?(.*))?$" in
  let short_url_re = Netstring_pcre.regexp "^/([^\\?]*)(\\?(.*))?$" in
(*  let url_relax_re = Netstring_pcre.regexp "^[Hh][Tt][Tt][Pp][Ss]?://[^/]+" in
*)
  fun url ->

    let match_re = Netstring_pcre.string_match url_re url 0 in

    let (https, host, port, pathstring, query) =
      match match_re with
        | None ->
          (match Netstring_pcre.string_match short_url_re url 0 with
            | None -> raise Ocsigen_Bad_Request
            | Some m ->
              let path = 
                fixup_url_string (Netstring_pcre.matched_group m 1 url)
              in
              let query =
                try 
                  Some (fixup_url_string (Netstring_pcre.matched_group m 3 url))
                with Not_found -> None 
              in
              (None, None, None, path, query))
        | Some m ->
          let path = fixup_url_string (Netstring_pcre.matched_group m 5 url) in
          let query =
            try Some (fixup_url_string (Netstring_pcre.matched_group m 7 url))
            with Not_found -> None
          in
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
          (https, host, port, path, query)
    in

    (* Note that the fragment (string after #) is not sent by browsers *)

    let get_params =
      lazy begin
        let params_string = match query with None -> "" | Some s -> s in
        try 
          Netencoding.Url.dest_url_encoded_parameters params_string
        with Failure _ -> raise Ocsigen_Bad_Request
      end
    in

    let path = List.map Netencoding.Url.decode (Neturl.split_path pathstring) in

    let path = remove_dotdot path (* and remove "//" *)
        (* here we remove .. from paths, as it is dangerous.
           But in some very particular cases, we may want them?
           I prefer forbid that. *)
    in
    let uri_string = match query with
      | None -> pathstring
      | Some s -> String.concat "?" [pathstring; s]
    in

    (https, host, port, uri_string, path, query, get_params)



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
type yesnomaybe = Yes | No | Maybe

module StringSet = Set.Make(String)



(* circular lists *)
module Clist = 
(struct
  
  type 'a node =
      { content : 'a option;
        mutable prev : 'a node; 
        mutable next : 'a node }

  type 'a t = 'a node
        
  let make' c =
    let rec x = { content = c; prev = x; next = x } in
    x
      
  let make c = make' (Some c)

  let create () = make' None

  let insert p x =
    let n = p.next in
    p.next <- x;
    x.prev <- p;
    x.next <- n;
    n.prev <- x
      
  let remove x =
    let p = x.prev in
    let n = x.next in
    p.next <- n;
    n.prev <- p;
    x.next <- x;
    x.prev <- x
      
  let in_list x = x.next != x
    
  let is_empty set = set.next == set

  let value c = 
    match c.content with 
      | None -> failwith "Clist.value"
      | Some c -> c

  let rec iter f (node : 'a t) = 
    match node.next.content with
      | Some c -> 
          f c;
          iter f node.next
      | None -> ()

  let rec fold_left f a (node : 'a t) = 
    match node.next.content with
      | Some c ->  fold_left f (f a c) node.next
      | None -> a

end : sig
  type 'a t
  type 'a node
  val make : 'a -> 'a node
  val create : unit -> 'a t
  val insert : 'a t -> 'a node -> unit
  val remove : 'a node -> unit
  val value : 'a node -> 'a
  val in_list : 'a node -> bool
  val is_empty : 'a t -> bool
  val iter : ('a -> unit) -> 'a t -> unit
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end)

module Int_Table = Map.Make(struct
  type t = int
  let compare = compare
end)

let make_cryptographic_safe_string =
  let rng = Cryptokit.Random.device_rng "/dev/urandom"
  and to_hex = Cryptokit.Hexa.encode () in
  fun () ->
      let random_part =
          let random_number = Cryptokit.Random.string rng 20 in
          Cryptokit.transform_string to_hex random_number
      and sequential_part =
          Printf.sprintf "%Lx" (Int64.bits_of_float (Unix.gettimeofday ())) in
      random_part ^ sequential_part

(*

The string is produced from the concatenation of two components: a
160-bit random sequence obtained from /dev/urandom, and a 64-bit sequential
component derived from the system clock.  The former is supposed to prevent
session spoofing.  The assumption is that given the high cryptographic quality
of /dev/urandom, it is impossible for an attacker to deduce the sequence of
random numbers produced.  As for the latter component, it exists to prevent
a theoretical (though infinitesimally unlikely) session ID collision if the
server were to be restarted.
*)



let debug = prerr_endline

let to_json ?typ v =
  match typ with
    | Some typ -> Deriving_Json.to_string typ v
    | None -> assert false (* implemented only client side *)

let of_json ?typ s =
  match typ with
    | Some typ -> Deriving_Json.from_string typ s
    | None -> assert false (* implemented only client side *)
