(* Ocsigen
 * Copyright (C) 2009 Vincent Balat
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

include Ocsigen_lib_obrowser

let (>>>) x f = f x

let window = JSOO.eval "window"
let document = JSOO.eval "document"
let body = JSOO.eval "document.body"

(* functions from AXO: *)
(* internal only : encode a string according to percent-encoding system *)
let urlencode_string str =
  window >>> JSOO.call_method "escape" [| JSOO.string str |] >>> JSOO.as_string

(* decode a string according to percent encoding system *)
(* we do not use this one because it does not work with marshaled data *)
(*let urldecode_string str =
  window >>> JSOO.call_method "unescape" [| JSOO.string str |]
  >>> JSOO.as_string
*)

let of_hex1 c = match c with
  | ('0'..'9') -> Char.code c - Char.code '0'
  | ('A'..'F') -> Char.code c - Char.code 'A' + 10
  | ('a'..'f') -> Char.code c - Char.code 'a' + 10
  | _ -> failwith "of_hex1"

let urldecode_string s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec aux pos len =
    if len > 0
    then
      let percent = 
        try String.index_from s pos '%'
        with Not_found -> -1
      in
      if percent = -1 
      then Buffer.add_substring buf s pos len
      else begin
        if percent + 3 > pos + len
        then failwith "urldecode_string_"
        else begin
          Buffer.add_substring buf s pos (percent - pos);
          let c1 = s.[percent+1] in
          let c2 = s.[percent+2] in
          let k1 = of_hex1 c1 in
	  let k2 = of_hex1 c2 in
	  Buffer.add_char buf (Char.chr((k1 lsl 4) lor k2));
          aux (percent+3) ((len - (percent - pos)) - 3)
        end
      end
  in
  aux 0 len;
  Buffer.contents buf


let encode ?plus s = urlencode_string s
(* plus has no effect here :-/ *)

let mk_url_encoded_parameters nv_pairs =
    String.concat "&"
      (List.map
	 (fun (name,value) ->
	    let name_encoded = encode name in
	    let value_encoded = encode value in
	    name_encoded ^ "=" ^ value_encoded
	 )
	 nv_pairs
      )

