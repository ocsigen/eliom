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

  (* Cut a string to the next separator *)
  let basic_sep char s =
    try
      let seppos = String.index s char in
      ((String.sub s 0 seppos),
       (String.sub s (seppos+1)
          ((String.length s) - seppos - 1)))
    with Invalid_argument _ -> raise Not_found

  (* Cut a string to the next separator, removing spaces.
     Raises Not_found if the separator connot be found.
   *)
  let sep char s =
    let len = String.length s in
    let seppos = String.index s char in
    ((remove_spaces s 0 (seppos-1)),
     (remove_spaces s (seppos+1) (len-1)))

  (* splits a string, for ex azert,   sdfmlskdf,    dfdsfs *)
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

  let may_append s1 ~sep = function
    | "" -> s1
    | s2 -> s1^sep^s2

  let may_concat s1 ~sep s2 = match s1, s2 with
  | _, "" -> s1
  | "", _ -> s2
  | _ -> String.concat sep [s1;s2]


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

  (* returns a copy of a string without \r and \n *)
  let remove_eols s =
    let l = String.length s in
    let buf = Buffer.create l in
    let rec aux n =
      if n < l
      then begin
	let c = s.[n] in
	if c <> '\r' && c <> '\n' then Buffer.add_char buf c;
	aux (n+1)
      end
    in
    aux 0;
    Buffer.contents buf


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

let () = Js.Unsafe.eval_string "debug = {}"
let debug_var s v =
  let d = Js.Unsafe.variable "debug" in
  Js.Unsafe.set d (Js.string s) v

let lwt_ignore ?(message="") t = Lwt.on_failure t (fun e -> debug_exn "%s" e message)

(* We do not use the deriving (un)marshaling even if typ is available
   because direct jsn (un)marshaling is very fast client side
*)
let to_json ?typ s = Js.to_string (Json.output ~encoding:`Byte s)
let of_json ?typ v = Json.unsafe_input ~encoding:`Byte (Js.string v)

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

  let event_of_function ev = Caml (CE_client_closure ev)

  let end_re = Regexp.regexp_string "]]>"

  let unique_counter = ref 0

  let make_unique ?copy elt =
    let id = match copy with
      | Some copy -> copy.unique_id
      | None -> Some (incr unique_counter; "client_unique"^(string_of_int !unique_counter)) in
    { elt with unique_id = id }

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
  module M = struct
    include SVG_f.Make(XML)
    let unique ?copy elt =
      tot (XML.make_unique ?copy:(map_option toelt copy) (toelt elt))
  end
end

module HTML5 = struct

  module M = struct

    include HTML5_f.Make(XML)(SVG.M)

    let rebuild_xml _ =
      debug "conversion from DOM to XML tree not implemented yet";
      assert false (* FIXME GRGR *)

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

    let unique ?copy elt =
      tot (XML.make_unique ?copy:(map_option toelt copy) (toelt elt))

    let lazy_uri_attrib name uri =
      XML.lazy_string_attrib name
	(Eliom_lazy.from_fun (fun () -> Uri.string_of_uri (Eliom_lazy.force uri)))

    let lazy_a_href uri = to_attrib (lazy_uri_attrib "href" uri)
    let lazy_a_action uri = to_attrib (lazy_uri_attrib "action" uri)

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elt1 elts =
      tot (XML.lazy_node ~a:(to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt1)
		  :: toeltl (Eliom_lazy.force elts))))

    (* GRGR: Uncomment when ocaml 3.12.1 is released ! See ocaml bug #1441. *)

    (* let a_onabort ev = a_onabort (XML.event_of_function ev) *)
    (* let a_onafterprint ev = a_onafterprint (XML.event_of_function ev) *)
    (* let a_onbeforeprint ev = a_onbeforeprint (XML.event_of_function ev) *)
    (* let a_onbeforeunload ev = a_onbeforeunload (XML.event_of_function ev) *)
    (* let a_onblur ev = a_onblur (XML.event_of_function ev) *)
    (* let a_oncanplay ev = a_oncanplay (XML.event_of_function ev) *)
    (* let a_oncanplaythrough ev = a_oncanplaythrough (XML.event_of_function ev) *)
    (* let a_onchange ev = a_onchange (XML.event_of_function ev) *)
    (* let a_onclick ev = a_onclick (XML.event_of_function ev) *)
    (* let a_oncontextmenu ev = a_oncontextmenu (XML.event_of_function ev) *)
    (* let a_ondblclick ev = a_ondblclick (XML.event_of_function ev) *)
    (* let a_ondrag ev = a_ondrag (XML.event_of_function ev) *)
    (* let a_ondragend ev = a_ondragend (XML.event_of_function ev) *)
    (* let a_ondragenter ev = a_ondragenter (XML.event_of_function ev) *)
    (* let a_ondragleave ev = a_ondragleave (XML.event_of_function ev) *)
    (* let a_ondragover ev = a_ondragover (XML.event_of_function ev) *)
    (* let a_ondragstart ev = a_ondragstart (XML.event_of_function ev) *)
    (* let a_ondrop ev = a_ondrop (XML.event_of_function ev) *)
    (* let a_ondurationchange ev = a_ondurationchange (XML.event_of_function ev) *)
    (* let a_onemptied ev = a_onemptied (XML.event_of_function ev) *)
    (* let a_onended ev = a_onended (XML.event_of_function ev) *)
    (* let a_onerror ev = a_onerror (XML.event_of_function ev) *)
    (* let a_onfocus ev = a_onfocus (XML.event_of_function ev) *)
    (* let a_onformchange ev = a_onformchange (XML.event_of_function ev) *)
    (* let a_onforminput ev = a_onforminput (XML.event_of_function ev) *)
    (* let a_onhashchange ev = a_onhashchange (XML.event_of_function ev) *)
    (* let a_oninput ev = a_oninput (XML.event_of_function ev) *)
    (* let a_oninvalid ev = a_oninvalid (XML.event_of_function ev) *)
    (* let a_onmousedown ev = a_onmousedown (XML.event_of_function ev) *)
    (* let a_onmouseup ev = a_onmouseup (XML.event_of_function ev) *)
    (* let a_onmouseover ev = a_onmouseover (XML.event_of_function ev) *)
    (* let a_onmousemove ev = a_onmousemove (XML.event_of_function ev) *)
    (* let a_onmouseout ev = a_onmouseout (XML.event_of_function ev) *)
    (* let a_onmousewheel ev = a_onmousewheel (XML.event_of_function ev) *)
    (* let a_onoffline ev = a_onoffline (XML.event_of_function ev) *)
    (* let a_ononline ev = a_ononline (XML.event_of_function ev) *)
    (* let a_onpause ev = a_onpause (XML.event_of_function ev) *)
    (* let a_onplay ev = a_onplay (XML.event_of_function ev) *)
    (* let a_onplaying ev = a_onplaying (XML.event_of_function ev) *)
    (* let a_onpagehide ev = a_onpagehide (XML.event_of_function ev) *)
    (* let a_onpageshow ev = a_onpageshow (XML.event_of_function ev) *)
    (* let a_onpopstate ev = a_onpopstate (XML.event_of_function ev) *)
    (* let a_onprogress ev = a_onprogress (XML.event_of_function ev) *)
    (* let a_onratechange ev = a_onratechange (XML.event_of_function ev) *)
    (* let a_onreadystatechange ev = a_onreadystatechange (XML.event_of_function ev) *)
    (* let a_onredo ev = a_onredo (XML.event_of_function ev) *)
    (* let a_onresize ev = a_onresize (XML.event_of_function ev) *)
    (* let a_onscroll ev = a_onscroll (XML.event_of_function ev) *)
    (* let a_onseeked ev = a_onseeked (XML.event_of_function ev) *)
    (* let a_onseeking ev = a_onseeking (XML.event_of_function ev) *)
    (* let a_onselect ev = a_onselect (XML.event_of_function ev) *)
    (* let a_onshow ev = a_onshow (XML.event_of_function ev) *)
    (* let a_onstalled ev = a_onstalled (XML.event_of_function ev) *)
    (* let a_onstorage ev = a_onstorage (XML.event_of_function ev) *)
    (* let a_onsubmit ev = a_onsubmit (XML.event_of_function ev) *)
    (* let a_onsuspend ev = a_onsuspend (XML.event_of_function ev) *)
    (* let a_ontimeupdate ev = a_ontimeupdate (XML.event_of_function ev) *)
    (* let a_onundo ev = a_onundo (XML.event_of_function ev) *)
    (* let a_onunload ev = a_onunload (XML.event_of_function ev) *)
    (* let a_onvolumechange ev = a_onvolumechange (XML.event_of_function ev) *)
    (* let a_onwaiting ev = a_onwaiting (XML.event_of_function ev) *)
    (* let a_onkeypress ev = a_onkeypress (XML.event_of_function ev) *)
    (* let a_onkeydown ev = a_onkeydown (XML.event_of_function ev) *)
    (* let a_onkeyup ev = a_onkeyup (XML.event_of_function ev) *)
    (* let a_onload ev = a_onload (XML.event_of_function ev) *)
    (* let a_onloadeddata ev = a_onloadeddata (XML.event_of_function ev) *)
    (* let a_onloadedmetadata ev = a_onloadedmetadata (XML.event_of_function ev) *)
    (* let a_onloadstart ev = a_onloadstart (XML.event_of_function ev) *)
    (* let a_onmessage ev = a_onmessage (XML.event_of_function ev) *)

  end

end

(*
module Reactive_dom = struct

  module Engine : sig

    val set_timer : float -> unit

    val register : (unit -> unit) -> unit
	(*TODO? make unregisterable *)

  end = struct

    let timer = ref 0.2

    let set_timer f = timer := f

    let registered = ref []

    let register f = registered := f :: !registered

    let rec poll () =
      Lwt_js.sleep !timer >>= fun () ->
	List.iter (fun f -> f ()) !registered;
	poll ()

    let _ = poll ()

  end

  let signalify (cb : (unit -> 'a)) : 'a React.S.t =
    let (s, set_s) = React.S.create (cb ()) in
    ignore (Engine.register (fun () -> set_s (cb ())));
    s

  let eventify_mouse target typ f =
    let (e, push_e) = React.E.create () in
    ignore (Dom_events.listen target typ (fun n e -> push_e (f n e)));
    e

  let eventify_keyboard target typ f =
    let (e, push_e) = React.E.create () in
    ignore (Dom_events.listen target typ (fun n e -> push_e (f n e)));
    e

end
*)
module Regexp = struct

  type t
  type flag =
    | Global_search (* g *)
    | Case_insensitive (* i *)
    | Multi_line (* m *)

  external make : string -> string -> t = "caml_regexp_make"
  external last_index : t -> int = "caml_regexp_last_index"

  let make
      ?(global = false)
      ?(case_insensitive = false)
      ?(multi_line = false)
      expr =
    make expr
      (""
       ^ (if global then "g" else "")
       ^ (if case_insensitive then "i" else "")
       ^ (if multi_line then "m" else ""))

  external test : t -> string -> bool = "caml_regexp_test"

(** executes a match
    the result is an array of substrings corresponding to matched groups
    0 is the whole substring matched by the regexp
    1 is the outermost parenthetised group
    etc.
 *)
  external exec : t -> string -> string array = "caml_regexp_exec"

(** returns the index of the first match of the regexp in the string
    raises Not_found if the string is not matched by the regexp
 *)
  external index : t -> string -> int = "caml_regexp_index"

(** replace [regexp] [substitution] [string]
    special chars (doc from MDC):
    - $$
    Inserts a "$".
    - $&
    Inserts the matched substring.
    - $`
    Inserts the portion of the string that precedes the matched substring.
    - $'
    Inserts the portion of the string that follows the matched substring.
    - $n or $nn  Where n or nn are decimal digits
    Inserts the nth parenthesized submatch string, provided the first argument was a RegExp object.
 *)
  external replace : t -> string -> string -> string = "caml_regexp_replace"

(** replace_fun [regexp] [substitution function] [string]
    the substitution function takes :
    - the offset of the current match
    - an array of matched groups (0 = total curren match, see [exec])
    WARNING: uses callback mechanism which is not "au point"
 *)
  external replace_fun : t -> (int -> string array -> string) -> string -> string = "caml_regexp_replace_fun"

  external split : t -> string -> string array = "caml_regexp_split"

end

(** Empty type (not used on client side, see eliom_parameter_base.ml) *)
type file_info
