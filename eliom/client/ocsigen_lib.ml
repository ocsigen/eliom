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

let urldecode_string = Url.urldecode

let encode ?plus s = Url.urlencode ?with_plus:plus s

let mk_url_encoded_parameters = Url.encode_arguments

let split_path = Url.path_of_path_string


let debug a = Firebug.console##log (Js.string a)
let jsdebug a = Firebug.console##log (a)
let alert a = Dom_html.window##alert (Js.string a)
let jsalert a = Dom_html.window##alert (a)

(* to marshal data and put it in a form *)
let encode_form_value x = Url.urlencode ~with_plus:true (Marshal.to_string x [])
    (* I encode the data because it seems that multipart does not
       like \0 character ... *)
