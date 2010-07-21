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

include Ocsigen_lib_cli

let urldecode_string s = Js.to_bytestring (Js.unescape (Js.bytestring s))

let encode ?plus s = Js.to_bytestring (Js.escape (Js.bytestring s))
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

let rec split_path s =
  try
    let length = String.length s in
    if length = 0 then []
    else
      let pos_slash = String.index s '/' in
      if pos_slash = 0
      then ""::split_path (String.sub s 1 (length-1))
      else
        let prefix = String.sub s 0 pos_slash in
        prefix::(split_path (String.sub s (pos_slash+1) (length - pos_slash - 1)))
  with Not_found -> [s]


let debug a = Firebug.console##log (Js.string a)
let jsdebug a = Firebug.console##log (a)
let alert a = Dom_html.window##alert (Js.string a)
let jsalert a = Dom_html.window##alert (a)

(* to marshal data and put it in a form *)
let encode_form_value v =
  Js.to_string (Js.escape (Js.bytestring (Marshal.to_string v [])))
    (* I encode the data because it seems that multipart does not
       like \0 character ... *)
