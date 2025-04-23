(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2007 Vincent Balat
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

open Js_of_ocaml

type param = Form.form_elt
type field = [`String of Js.js_string Js.t | `File of File.file Js.t]

let insert_string s : field = `String (Js.string s)
let insert_file s : field = `File s

let to_string : field -> string = function
  | `File _ -> failwith "Cannot put a file in URL"
  | `String s -> Js.to_string s

let inject_param_list l = List.map (fun (n, v) -> n, insert_string v) l
let get_param_list l = List.map (fun (n, v) -> n, to_string v) l

let inject_param_table t : (string * param) list Eliom_lib.String.Table.t =
  Eliom_lib.String.Table.map (fun v -> inject_param_list v) t

let string_of_param = function
  | `String s -> Js.to_string s
  | `File _ -> failwith "is a file"
