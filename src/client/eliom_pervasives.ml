(* Ocsigen
 * Copyright (C) 2005-2008 Vincent Balat, Stéphane Glondu
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

include Eliom_pervasives_base

exception False

let iter_option f m = match m with
  | Some v -> f v
  | None -> ()

(*****************************************************************************)

module List = struct

  include List
  include Eliom_pervasives_base.List_base

  let rec remove_first_if_any a = function
    |  [] -> []
    | b::l when a = b -> l
    | b::l -> b::(remove_first_if_any a l)

  let rec remove_first_if_any_q a = function
    |  [] -> []
    | b::l when a == b -> l
    | b::l -> b::(remove_first_if_any_q a l)

  let rec remove_first a = function
    |  [] -> raise Not_found
    | b::l when a = b -> l
    | b::l -> b::(remove_first a l)

  let rec remove_first_q a = function
    | [] -> raise Not_found
    | b::l when a == b -> l
    | b::l -> b::(remove_first_q a l)

  let rec remove_all a = function
    | [] -> []
    | b::l when a = b -> remove_all a l
    | b::l -> b::(remove_all a l)

  let rec remove_all_q a = function
    | [] -> []
    | b::l when a == b -> remove_all_q a l
    | b::l -> b::(remove_all_q a l)

  let rec remove_all_assoc a = function
    | [] -> []
    | (b, _)::l when a = b -> remove_all_assoc a l
    | b::l -> b::(remove_all_assoc a l)

  let rec remove_all_assoc_q a = function
    | [] -> []
    | (b,_)::l when a == b -> remove_all_assoc_q a l
    | b::l -> b::(remove_all_assoc_q a l)

  let rec last = function
    |  [] -> raise Not_found
    | [b] -> b
    | _::l -> last l

  let rec assoc_remove a = function
    | [] -> raise Not_found
    | (b, c)::l when a = b -> c, l
    | b::l -> let v, ll = assoc_remove a l in (v, b::ll)

  let rec is_prefix l1 l2 =
    match (l1, l2) with
    | [], _ -> true
    | a::ll1, b::ll2 when a=b -> is_prefix ll1 ll2
    | _ -> false

  let rec is_prefix_skip_end_slash l1 l2 =
    match (l1, l2) with
    | [""], _
    | [], _ -> true
    | a::ll1, b::ll2 when a=b -> is_prefix_skip_end_slash ll1 ll2
    | _ -> false

  let rec chop n xs =
    if n <= 0
    then xs
    else
      match xs with
      | [] -> []
      | x :: xs -> chop (n-1) xs

end

(*****************************************************************************)

module String = struct

  include String

  let index s c =
    let c = (Js.Unsafe.variable "String")##fromCharCode(c) in
    let n = (Js.bytestring s)##indexOf(c) in
    if n = -1 then raise Not_found;
    n

  let index_from s i c =
    let c = (Js.Unsafe.variable "String")##fromCharCode(c) in
    let n = (Js.bytestring s)##indexOf_from(c, i) in
    if n = -1 then raise Not_found;
    n

  (* Returns a copy of the string from beg to endd,
     removing spaces at the beginning and at the end *)
  let remove_spaces s beg endd =
    let rec find_not_space s i step =
      if (i > endd) || (beg > i)
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

(*FIX: one should not catch Invalid_argument exceptions!
  (* Cut a string to the next separator *)
  let basic_sep char s =
    try
      let seppos = index s char in
      ((String.sub s 0 seppos),
       (String.sub s (seppos+1)
          ((String.length s) - seppos - 1)))
    with Invalid_argument _ -> raise Not_found

  (* Cut a string to the next separator, removing spaces.
     Raises Not_found if the separator connot be found.
   *)
  let sep char s =
    let len = String.length s in
    let seppos = index s char in
    ((remove_spaces s 0 (seppos-1)),
     (remove_spaces s (seppos+1) (len-1)))
*)

  (* splits a string, for ex azert,   sdfmlskdf,    dfdsfs *)
  let rec split ?(multisep=false) char s =
    let longueur = String.length s in
    let rec aux deb =
      if deb >= longueur
      then []
      else
	try
          let firstsep = index_from s deb char in
          if multisep && firstsep = deb then
            aux (deb + 1)
          else
            (remove_spaces s deb (firstsep-1))::
            (aux (firstsep+1))
	with Not_found -> [remove_spaces s deb (longueur-1)]
    in
    aux 0

  let may_append s1 ~sep = function
    | "" -> s1
    | s2 -> s1^sep^s2

  let may_concat s1 ~sep s2 = match s1, s2 with
  | _, "" -> s1
  | "", _ -> s2
  | _ -> String.concat sep [s1;s2]


(*FIX: one should not catch Invalid_argument exceptions!
  (* returns the index of the first difference between s1 and s2,
     starting from n and ending at last.
     returns (last + 1) if no difference is found.
   *)
  let rec first_diff s1 s2 n last =
    try
      if s1.[n] = s2.[n]
      then
	if n = last
	then last+1
	else first_diff s1 s2 (n+1) last
      else n
    with Invalid_argument _ -> n
*)

  let eol_re = Regexp.regexp "[\r\n]"

  (* returns a copy of a string without \r and \n *)
  let remove_eols s = Regexp.global_replace eol_re s ""

  module Table = Map.Make(String)
  module Set = Set.Make(String)
  module Map = Map.Make(String)

end

(*****************************************************************************)

module Url = struct

  include Url

  type t = string
  type uri = string
  type path = string list

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


  let remove_dotdot = (* removes "../" *)
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

  let remove_end_slash s =
    try
      if s.[(String.length s) - 1] = '/'
      then String.sub s 0 ((String.length s) - 1)
      else s
    with Invalid_argument _ -> s


  let remove_internal_slash u =
    let rec aux = function
      | [] -> []
      | [a] -> [a]
      | ""::l -> aux l
      | a::l -> a::(aux l)
    in match u with
    | [] -> []
    | a::l -> a::(aux l)

  let change_empty_list = function
    | [] -> [""] (* It is not possible to register an empty URL *)
    | l -> l

  let rec add_end_slash_if_missing = function
    | [] -> [""]
    | [""] as a -> a
    | a::l -> a::(add_end_slash_if_missing l)

  let rec remove_slash_at_end = function
    | []
    | [""] -> []
    | a::l -> a::(remove_slash_at_end l)

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

  let decode = Url.urldecode
  let encode ?plus s = Url.urlencode ?with_plus:plus s

  let make_encoded_parameters = Url.encode_arguments

  let split_path = Url.path_of_path_string

  let split_fragment s =
    try
      let pos = String.index s '#' in
      String.sub s 0 pos,
      Some (String.sub s (pos+1) (String.length s - 1 - pos))
    with Not_found -> s, None

  let ssl_re = Regexp.regexp "^(https?):\\/\\/"
  let get_ssl s =
    map_option
      (fun r -> Regexp.matched_group r 1 = Some "https")
      (Regexp.string_match ssl_re s 0)

end
(*****************************************************************************)
(*
module Ip_address = struct


  type t =
    | IPv4 of int32
    | IPv6 of int64 * int64

  exception Invalid_ip_address of string

  let parse s =
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

  let network_of_ip ~ip ~mask = match ip, mask with
  | IPv4 a, IPv4 mask4 -> IPv4 (Int32.logand a mask4)
  | IPv6 (a, b), IPv6 (mask61, mask62) -> IPv6 (Int64.logand a mask61, Int64.logand b mask62)
  | _ -> invalid_arg "Ip_address.network_of_ip"

end
*)
(*****************************************************************************)

module Printexc = struct

  include Printexc

  let exc_printer = ref (fun _ e -> Printexc.to_string e)

  let rec to_string e = !exc_printer to_string e

  let register_exn_printer p =
    let printer =
      let old = !exc_printer in
      (fun f_rec s ->
        try p f_rec s
        with e -> old f_rec s) in
    exc_printer := printer

end

(*****************************************************************************)

let debug_exn f e =
  Printf.ksprintf (fun s -> Firebug.console##log (Js.string (s^(Printexc.to_string e)))) f
let debug f = Printf.ksprintf (fun s -> Firebug.console##log (Js.string s)) f
let error f = Printf.ksprintf (fun s -> Firebug.console##error (Js.string s); failwith s) f
let jsdebug a = Firebug.console##log (a)
let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert (Js.string s)) f
let jsalert a = Dom_html.window##alert (a)

let debug_var s v = Js.Unsafe.set Dom_html.window (Js.string s) v

let lwt_ignore ?(message="") t = Lwt.on_failure t (fun e -> debug_exn "%s" e message)

(* We do not use the deriving (un)marshaling even if typ is available
   because direct jsn (un)marshaling is very fast client side
*)
let to_json ?typ s = Js.to_string (Json.output s)
let of_json ?typ v = Json.unsafe_input (Js.string v)

(* to marshal data and put it in a form *)
let encode_form_value x = to_json x
(* Url.urlencode ~with_plus:true (Marshal.to_string x [])
    (* I encode the data because it seems that multipart does not
       like \0 character ... *)
*)
let encode_header_value x =
  (* We remove end of lines *)
  String.remove_eols (to_json x)

let unmarshal_js_var s =
  Marshal.from_string (Js.to_bytestring (Js.Unsafe.variable s)) 0

module XML = struct

  include RawXML

  type econtent =
    | Empty
    | Comment of string
    | EncodedPCDATA of string
    | PCDATA of string
    | Entity of string
    | Leaf of ename * attrib list
    | Node of ename * attrib list * elt list
  and node =
    | DomNode of Dom.node Js.t
    | TyXMLNode of econtent
  and elt = {
    (* See Eliom_client.HTML5 for the 'unwrap' function that convert
       the server's tree representation into the client one. *)
    mutable elt : node;
    node_id : node_id;
  }

  let content e =
    match e.elt with
    | DomNode _ -> assert false (* TODO *)
    | TyXMLNode elt -> elt
  let get_node e = e.elt
  let set_dom_node elt node = elt.elt <- DomNode node
  let get_node_id elt = elt.node_id

  let make ?(id = NoId) elt = { elt = TyXMLNode elt; node_id = id; }
  let make_dom ?(id = NoId) node = { elt = DomNode node; node_id = id; }

  let empty () = make Empty

  let comment c = make (Comment c)
  let pcdata d = make (PCDATA d)
  let encodedpcdata d = make (EncodedPCDATA d)
  let entity e = make (Entity e)

  let leaf ?(a = []) name =  make (Leaf (name, a))
  let node ?(a = []) name children = make (Node (name, a, children))
  let lazy_node ?a name children = node ?a name (Eliom_lazy.force children)

  let event_handler_of_function (ev: #Dom_html.event Js.t -> unit) =
    Caml (CE_client_closure (Obj.magic ev))

  (* Deprecated: HTML5.M.a_on* functions are redefinied on the client
     to call event_handler_of_function. *)
  let event_of_function ev = ev

  let end_re = Regexp.regexp_string "]]>"

  let make_node_name =
    let node_id_counter = ref 0 in
    (fun () ->
      incr node_id_counter;
      "client_" ^ (string_of_int !node_id_counter))

  let make_process_node ?(id = make_node_name ()) elt =
    { elt with node_id = ProcessId id }

  let make_request_node elt =
    { elt with
      node_id = RequestId (make_node_name ()) }

  let cdata s =
    let s' =
      "\n<![CDATA[\n" ^ Regexp.global_replace end_re s "" ^ "\n]]>\n" in
    encodedpcdata s'

  let cdata_script s =
    let s' =
      "\n//<![CDATA[\n" ^ Regexp.global_replace end_re s "" ^ "\n//]]>\n" in
    encodedpcdata s'

  let cdata_style s =
    let s' =
      "\n/* <![CDATA[ */\n" ^ Regexp.global_replace end_re s "" ^ "\n/* ]]> */\n" in
    encodedpcdata s'


end

module SVG = struct

  module DOM = SVG_f.Make(struct
    include XML

    let make elt = make_request_node (make elt)
    let make_lazy elt = make_request_node (make (Lazy.force elt))

    let empty () = make Empty

    let comment c = make (Comment c)
    let pcdata d = make (PCDATA d)
    let encodedpcdata d = make (EncodedPCDATA d)
    let entity e = make (Entity e)

    let leaf ?(a = []) name =  make (Leaf (name, a))
    let node ?(a = []) name children = make (Node (name, a, children))
    let lazy_node ?(a = []) name children =
      make (Node (name, a, Eliom_lazy.force children))

  end)

  include DOM

  module M = SVG_f.Make(XML)

  type 'a id = string (* FIXME invariant type parameter ? *)
  let new_global_elt_id: unit -> 'a id = XML.make_node_name
  let create_global_elt ?(id : 'a id option) elt =
    tot (XML.make_process_node ?id (toelt elt))

end

module HTML5 = struct

  module DOM = struct

    include HTML5_f.Make(struct
      include XML

      let make elt = make_request_node (make elt)
      let make_lazy elt = make_request_node (make (Lazy.force elt))

      let empty () = make Empty

      let comment c = make (Comment c)
      let pcdata d = make (PCDATA d)
      let encodedpcdata d = make (EncodedPCDATA d)
      let entity e = make (Entity e)

      let leaf ?(a = []) name =  make (Leaf (name, a))
      let node ?(a = []) name children = make (Node (name, a, children))
      let lazy_node ?(a = []) name children =
        make (Node (name, a, Eliom_lazy.force children))

    end)(SVG.DOM)

    let raw_a_onabort = a_onabort
    let raw_a_onafterprint = a_onafterprint
    let raw_a_onbeforeprint = a_onbeforeprint
    let raw_a_onbeforeunload = a_onbeforeunload
    let raw_a_onblur = a_onblur
    let raw_a_oncanplay = a_oncanplay
    let raw_a_oncanplaythrough = a_oncanplaythrough
    let raw_a_onchange = a_onchange
    let raw_a_onclick = a_onclick
    let raw_a_oncontextmenu = a_oncontextmenu
    let raw_a_ondblclick = a_ondblclick
    let raw_a_ondrag = a_ondrag
    let raw_a_ondragend = a_ondragend
    let raw_a_ondragenter = a_ondragenter
    let raw_a_ondragleave = a_ondragleave
    let raw_a_ondragover = a_ondragover
    let raw_a_ondragstart = a_ondragstart
    let raw_a_ondrop = a_ondrop
    let raw_a_ondurationchange = a_ondurationchange
    let raw_a_onemptied = a_onemptied
    let raw_a_onended = a_onended
    let raw_a_onerror = a_onerror
    let raw_a_onfocus = a_onfocus
    let raw_a_onformchange = a_onformchange
    let raw_a_onforminput = a_onforminput
    let raw_a_onhashchange = a_onhashchange
    let raw_a_oninput = a_oninput
    let raw_a_oninvalid = a_oninvalid
    let raw_a_onmousedown = a_onmousedown
    let raw_a_onmouseup = a_onmouseup
    let raw_a_onmouseover = a_onmouseover
    let raw_a_onmousemove = a_onmousemove
    let raw_a_onmouseout = a_onmouseout
    let raw_a_onmousewheel = a_onmousewheel
    let raw_a_onoffline = a_onoffline
    let raw_a_ononline = a_ononline
    let raw_a_onpause = a_onpause
    let raw_a_onplay = a_onplay
    let raw_a_onplaying = a_onplaying
    let raw_a_onpagehide = a_onpagehide
    let raw_a_onpageshow = a_onpageshow
    let raw_a_onpopstate = a_onpopstate
    let raw_a_onprogress = a_onprogress
    let raw_a_onratechange = a_onratechange
    let raw_a_onreadystatechange = a_onreadystatechange
    let raw_a_onredo = a_onredo
    let raw_a_onresize = a_onresize
    let raw_a_onscroll = a_onscroll
    let raw_a_onseeked = a_onseeked
    let raw_a_onseeking = a_onseeking
    let raw_a_onselect = a_onselect
    let raw_a_onshow = a_onshow
    let raw_a_onstalled = a_onstalled
    let raw_a_onstorage = a_onstorage
    let raw_a_onsubmit = a_onsubmit
    let raw_a_onsuspend = a_onsuspend
    let raw_a_ontimeupdate = a_ontimeupdate
    let raw_a_onundo = a_onundo
    let raw_a_onunload = a_onunload
    let raw_a_onvolumechange = a_onvolumechange
    let raw_a_onwaiting = a_onwaiting
    let raw_a_onkeypress = a_onkeypress
    let raw_a_onkeydown = a_onkeydown
    let raw_a_onkeyup = a_onkeyup
    let raw_a_onload = a_onload
    let raw_a_onloadeddata = a_onloadeddata
    let raw_a_onloadedmetadata = a_onloadedmetadata
    let raw_a_onloadstart = a_onloadstart
    let raw_a_onmessage = a_onmessage

    let a_onabort ev = a_onabort (XML.event_handler_of_function ev)
    let a_onafterprint ev = a_onafterprint (XML.event_handler_of_function ev)
    let a_onbeforeprint ev = a_onbeforeprint (XML.event_handler_of_function ev)
    let a_onbeforeunload ev = a_onbeforeunload (XML.event_handler_of_function ev)
    let a_onblur ev = a_onblur (XML.event_handler_of_function ev)
    let a_oncanplay ev = a_oncanplay (XML.event_handler_of_function ev)
    let a_oncanplaythrough ev = a_oncanplaythrough (XML.event_handler_of_function ev)
    let a_onchange ev = a_onchange (XML.event_handler_of_function ev)
    let a_onclick ev = a_onclick (XML.event_handler_of_function ev)
    let a_oncontextmenu ev = a_oncontextmenu (XML.event_handler_of_function ev)
    let a_ondblclick ev = a_ondblclick (XML.event_handler_of_function ev)
    let a_ondrag ev = a_ondrag (XML.event_handler_of_function ev)
    let a_ondragend ev = a_ondragend (XML.event_handler_of_function ev)
    let a_ondragenter ev = a_ondragenter (XML.event_handler_of_function ev)
    let a_ondragleave ev = a_ondragleave (XML.event_handler_of_function ev)
    let a_ondragover ev = a_ondragover (XML.event_handler_of_function ev)
    let a_ondragstart ev = a_ondragstart (XML.event_handler_of_function ev)
    let a_ondrop ev = a_ondrop (XML.event_handler_of_function ev)
    let a_ondurationchange ev = a_ondurationchange (XML.event_handler_of_function ev)
    let a_onemptied ev = a_onemptied (XML.event_handler_of_function ev)
    let a_onended ev = a_onended (XML.event_handler_of_function ev)
    let a_onerror ev = a_onerror (XML.event_handler_of_function ev)
    let a_onfocus ev = a_onfocus (XML.event_handler_of_function ev)
    let a_onformchange ev = a_onformchange (XML.event_handler_of_function ev)
    let a_onforminput ev = a_onforminput (XML.event_handler_of_function ev)
    let a_onhashchange ev = a_onhashchange (XML.event_handler_of_function ev)
    let a_oninput ev = a_oninput (XML.event_handler_of_function ev)
    let a_oninvalid ev = a_oninvalid (XML.event_handler_of_function ev)
    let a_onmousedown ev = a_onmousedown (XML.event_handler_of_function ev)
    let a_onmouseup ev = a_onmouseup (XML.event_handler_of_function ev)
    let a_onmouseover ev = a_onmouseover (XML.event_handler_of_function ev)
    let a_onmousemove ev = a_onmousemove (XML.event_handler_of_function ev)
    let a_onmouseout ev = a_onmouseout (XML.event_handler_of_function ev)
    let a_onmousewheel ev = a_onmousewheel (XML.event_handler_of_function ev)
    let a_onoffline ev = a_onoffline (XML.event_handler_of_function ev)
    let a_ononline ev = a_ononline (XML.event_handler_of_function ev)
    let a_onpause ev = a_onpause (XML.event_handler_of_function ev)
    let a_onplay ev = a_onplay (XML.event_handler_of_function ev)
    let a_onplaying ev = a_onplaying (XML.event_handler_of_function ev)
    let a_onpagehide ev = a_onpagehide (XML.event_handler_of_function ev)
    let a_onpageshow ev = a_onpageshow (XML.event_handler_of_function ev)
    let a_onpopstate ev = a_onpopstate (XML.event_handler_of_function ev)
    let a_onprogress ev = a_onprogress (XML.event_handler_of_function ev)
    let a_onratechange ev = a_onratechange (XML.event_handler_of_function ev)
    let a_onreadystatechange ev = a_onreadystatechange (XML.event_handler_of_function ev)
    let a_onredo ev = a_onredo (XML.event_handler_of_function ev)
    let a_onresize ev = a_onresize (XML.event_handler_of_function ev)
    let a_onscroll ev = a_onscroll (XML.event_handler_of_function ev)
    let a_onseeked ev = a_onseeked (XML.event_handler_of_function ev)
    let a_onseeking ev = a_onseeking (XML.event_handler_of_function ev)
    let a_onselect ev = a_onselect (XML.event_handler_of_function ev)
    let a_onshow ev = a_onshow (XML.event_handler_of_function ev)
    let a_onstalled ev = a_onstalled (XML.event_handler_of_function ev)
    let a_onstorage ev = a_onstorage (XML.event_handler_of_function ev)
    let a_onsubmit ev = a_onsubmit (XML.event_handler_of_function ev)
    let a_onsuspend ev = a_onsuspend (XML.event_handler_of_function ev)
    let a_ontimeupdate ev = a_ontimeupdate (XML.event_handler_of_function ev)
    let a_onundo ev = a_onundo (XML.event_handler_of_function ev)
    let a_onunload ev = a_onunload (XML.event_handler_of_function ev)
    let a_onvolumechange ev = a_onvolumechange (XML.event_handler_of_function ev)
    let a_onwaiting ev = a_onwaiting (XML.event_handler_of_function ev)
    let a_onkeypress ev = a_onkeypress (XML.event_handler_of_function ev)
    let a_onkeydown ev = a_onkeydown (XML.event_handler_of_function ev)
    let a_onkeyup ev = a_onkeyup (XML.event_handler_of_function ev)
    let a_onload ev = a_onload (XML.event_handler_of_function ev)
    let a_onloadeddata ev = a_onloadeddata (XML.event_handler_of_function ev)
    let a_onloadedmetadata ev = a_onloadedmetadata (XML.event_handler_of_function ev)
    let a_onloadstart ev = a_onloadstart (XML.event_handler_of_function ev)
    let a_onmessage ev = a_onmessage (XML.event_handler_of_function ev)

    type ('a, 'b, 'c) lazy_plus =
        ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elt1 elts =
      tot (XML.lazy_node ~a:(to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
	        (fun () ->
		  toelt (Eliom_lazy.force elt1)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  include DOM

  module M = struct
    include HTML5_f.Make(XML)(SVG.M)

    let a_onabort ev = a_onabort (XML.event_handler_of_function ev)
    let a_onafterprint ev = a_onafterprint (XML.event_handler_of_function ev)
    let a_onbeforeprint ev = a_onbeforeprint (XML.event_handler_of_function ev)
    let a_onbeforeunload ev = a_onbeforeunload (XML.event_handler_of_function ev)
    let a_onblur ev = a_onblur (XML.event_handler_of_function ev)
    let a_oncanplay ev = a_oncanplay (XML.event_handler_of_function ev)
    let a_oncanplaythrough ev = a_oncanplaythrough (XML.event_handler_of_function ev)
    let a_onchange ev = a_onchange (XML.event_handler_of_function ev)
    let a_onclick ev = a_onclick (XML.event_handler_of_function ev)
    let a_oncontextmenu ev = a_oncontextmenu (XML.event_handler_of_function ev)
    let a_ondblclick ev = a_ondblclick (XML.event_handler_of_function ev)
    let a_ondrag ev = a_ondrag (XML.event_handler_of_function ev)
    let a_ondragend ev = a_ondragend (XML.event_handler_of_function ev)
    let a_ondragenter ev = a_ondragenter (XML.event_handler_of_function ev)
    let a_ondragleave ev = a_ondragleave (XML.event_handler_of_function ev)
    let a_ondragover ev = a_ondragover (XML.event_handler_of_function ev)
    let a_ondragstart ev = a_ondragstart (XML.event_handler_of_function ev)
    let a_ondrop ev = a_ondrop (XML.event_handler_of_function ev)
    let a_ondurationchange ev = a_ondurationchange (XML.event_handler_of_function ev)
    let a_onemptied ev = a_onemptied (XML.event_handler_of_function ev)
    let a_onended ev = a_onended (XML.event_handler_of_function ev)
    let a_onerror ev = a_onerror (XML.event_handler_of_function ev)
    let a_onfocus ev = a_onfocus (XML.event_handler_of_function ev)
    let a_onformchange ev = a_onformchange (XML.event_handler_of_function ev)
    let a_onforminput ev = a_onforminput (XML.event_handler_of_function ev)
    let a_onhashchange ev = a_onhashchange (XML.event_handler_of_function ev)
    let a_oninput ev = a_oninput (XML.event_handler_of_function ev)
    let a_oninvalid ev = a_oninvalid (XML.event_handler_of_function ev)
    let a_onmousedown ev = a_onmousedown (XML.event_handler_of_function ev)
    let a_onmouseup ev = a_onmouseup (XML.event_handler_of_function ev)
    let a_onmouseover ev = a_onmouseover (XML.event_handler_of_function ev)
    let a_onmousemove ev = a_onmousemove (XML.event_handler_of_function ev)
    let a_onmouseout ev = a_onmouseout (XML.event_handler_of_function ev)
    let a_onmousewheel ev = a_onmousewheel (XML.event_handler_of_function ev)
    let a_onoffline ev = a_onoffline (XML.event_handler_of_function ev)
    let a_ononline ev = a_ononline (XML.event_handler_of_function ev)
    let a_onpause ev = a_onpause (XML.event_handler_of_function ev)
    let a_onplay ev = a_onplay (XML.event_handler_of_function ev)
    let a_onplaying ev = a_onplaying (XML.event_handler_of_function ev)
    let a_onpagehide ev = a_onpagehide (XML.event_handler_of_function ev)
    let a_onpageshow ev = a_onpageshow (XML.event_handler_of_function ev)
    let a_onpopstate ev = a_onpopstate (XML.event_handler_of_function ev)
    let a_onprogress ev = a_onprogress (XML.event_handler_of_function ev)
    let a_onratechange ev = a_onratechange (XML.event_handler_of_function ev)
    let a_onreadystatechange ev = a_onreadystatechange (XML.event_handler_of_function ev)
    let a_onredo ev = a_onredo (XML.event_handler_of_function ev)
    let a_onresize ev = a_onresize (XML.event_handler_of_function ev)
    let a_onscroll ev = a_onscroll (XML.event_handler_of_function ev)
    let a_onseeked ev = a_onseeked (XML.event_handler_of_function ev)
    let a_onseeking ev = a_onseeking (XML.event_handler_of_function ev)
    let a_onselect ev = a_onselect (XML.event_handler_of_function ev)
    let a_onshow ev = a_onshow (XML.event_handler_of_function ev)
    let a_onstalled ev = a_onstalled (XML.event_handler_of_function ev)
    let a_onstorage ev = a_onstorage (XML.event_handler_of_function ev)
    let a_onsubmit ev = a_onsubmit (XML.event_handler_of_function ev)
    let a_onsuspend ev = a_onsuspend (XML.event_handler_of_function ev)
    let a_ontimeupdate ev = a_ontimeupdate (XML.event_handler_of_function ev)
    let a_onundo ev = a_onundo (XML.event_handler_of_function ev)
    let a_onunload ev = a_onunload (XML.event_handler_of_function ev)
    let a_onvolumechange ev = a_onvolumechange (XML.event_handler_of_function ev)
    let a_onwaiting ev = a_onwaiting (XML.event_handler_of_function ev)
    let a_onkeypress ev = a_onkeypress (XML.event_handler_of_function ev)
    let a_onkeydown ev = a_onkeydown (XML.event_handler_of_function ev)
    let a_onkeyup ev = a_onkeyup (XML.event_handler_of_function ev)
    let a_onload ev = a_onload (XML.event_handler_of_function ev)
    let a_onloadeddata ev = a_onloadeddata (XML.event_handler_of_function ev)
    let a_onloadedmetadata ev = a_onloadedmetadata (XML.event_handler_of_function ev)
    let a_onloadstart ev = a_onloadstart (XML.event_handler_of_function ev)
    let a_onmessage ev = a_onmessage (XML.event_handler_of_function ev)

    type ('a, 'b, 'c) lazy_plus =
        ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elt1 elts =
      tot (XML.lazy_node ~a:(to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
	        (fun () ->
		  toelt (Eliom_lazy.force elt1)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  type 'a id = string (* FIXME invariant type parameter ? *)
  let new_global_elt_id: unit -> 'a id = XML.make_node_name
  let create_global_elt ?(id : 'a id option) elt =
    tot (XML.make_process_node ?id (toelt elt))

  let rebuild_xml (node: 'a Js.t) : 'a elt =
    Obj.magic { XML.elt = XML.DomNode (node :> Dom.node Js.t); node_id = XML.NoId }

  let of_element : Dom_html.element Js.t -> 'a elt = rebuild_xml
  let of_html : Dom_html.htmlElement Js.t -> HTML5_types.html elt = rebuild_xml
  let of_head : Dom_html.headElement Js.t -> HTML5_types.head elt = rebuild_xml
  let of_link : Dom_html.linkElement Js.t -> HTML5_types.link elt = rebuild_xml
  let of_title : Dom_html.titleElement Js.t -> HTML5_types.title elt = rebuild_xml
  let of_meta : Dom_html.metaElement Js.t -> HTML5_types.meta elt = rebuild_xml
  let of_base : Dom_html.baseElement Js.t -> HTML5_types.base elt = rebuild_xml
  let of_style : Dom_html.styleElement Js.t -> HTML5_types.style elt = rebuild_xml
  let of_body : Dom_html.bodyElement Js.t -> HTML5_types.body elt = rebuild_xml
  let of_form : Dom_html.formElement Js.t -> HTML5_types.form elt = rebuild_xml
  let of_optGroup : Dom_html.optGroupElement Js.t -> HTML5_types.optgroup elt = rebuild_xml
  let of_option : Dom_html.optionElement Js.t -> HTML5_types.selectoption elt = rebuild_xml
  let of_select : Dom_html.selectElement Js.t -> HTML5_types.select elt = rebuild_xml
  let of_input : Dom_html.inputElement Js.t -> HTML5_types.input elt = rebuild_xml
  let of_textArea : Dom_html.textAreaElement Js.t -> HTML5_types.textarea elt = rebuild_xml
  let of_button : Dom_html.buttonElement Js.t -> HTML5_types.button elt = rebuild_xml
  let of_label : Dom_html.labelElement Js.t -> HTML5_types.label elt = rebuild_xml
  let of_fieldSet : Dom_html.fieldSetElement Js.t -> HTML5_types.fieldset elt = rebuild_xml
  let of_legend : Dom_html.legendElement Js.t -> HTML5_types.legend elt = rebuild_xml
  let of_uList : Dom_html.uListElement Js.t -> HTML5_types.ul elt = rebuild_xml
  let of_oList : Dom_html.oListElement Js.t -> HTML5_types.ol elt = rebuild_xml
  let of_dList : Dom_html.dListElement Js.t -> [`Dl] elt = rebuild_xml
  let of_li : Dom_html.liElement Js.t -> HTML5_types.li elt = rebuild_xml
  let of_div : Dom_html.divElement Js.t -> HTML5_types.div elt = rebuild_xml
  let of_paragraph : Dom_html.paragraphElement Js.t -> HTML5_types.p elt = rebuild_xml
  let of_heading : Dom_html.headingElement Js.t -> HTML5_types.heading elt = rebuild_xml
  let of_quote : Dom_html.quoteElement Js.t -> HTML5_types.blockquote elt = rebuild_xml
  let of_pre : Dom_html.preElement Js.t -> HTML5_types.pre elt = rebuild_xml
  let of_br : Dom_html.brElement Js.t -> HTML5_types.br elt = rebuild_xml
  let of_hr : Dom_html.hrElement Js.t -> HTML5_types.hr elt = rebuild_xml
  let of_anchor : Dom_html.anchorElement Js.t -> 'a HTML5_types.a elt = rebuild_xml
  let of_image : Dom_html.imageElement Js.t -> [`Img] elt = rebuild_xml
  let of_object : Dom_html.objectElement Js.t -> 'a HTML5_types.object_ elt = rebuild_xml
  let of_param : Dom_html.paramElement Js.t -> HTML5_types.param elt = rebuild_xml
  let of_area : Dom_html.areaElement Js.t -> HTML5_types.area elt = rebuild_xml
  let of_map : Dom_html.mapElement Js.t -> 'a HTML5_types.map elt = rebuild_xml
  let of_script : Dom_html.scriptElement Js.t -> HTML5_types.script elt = rebuild_xml
  let of_tableCell : Dom_html.tableCellElement Js.t -> [ HTML5_types.td | HTML5_types.td ] elt = rebuild_xml
  let of_tableRow : Dom_html.tableRowElement Js.t -> HTML5_types.tr elt = rebuild_xml
  let of_tableCol : Dom_html.tableColElement Js.t -> HTML5_types.col elt = rebuild_xml
  let of_tableSection : Dom_html.tableSectionElement Js.t -> [ HTML5_types.tfoot | HTML5_types.thead | HTML5_types.tbody ] elt = rebuild_xml
  let of_tableCaption : Dom_html.tableCaptionElement Js.t -> HTML5_types.caption elt = rebuild_xml
  let of_table : Dom_html.tableElement Js.t -> HTML5_types.table elt = rebuild_xml
  let of_canvas : Dom_html.canvasElement Js.t -> 'a HTML5_types.canvas elt = rebuild_xml
  let of_iFrame : Dom_html.iFrameElement Js.t -> HTML5_types.iframe elt = rebuild_xml

  let string_of_id x = x

end

(** Empty type (not used on client side, see eliom_parameter_base.ml) *)
type file_info


