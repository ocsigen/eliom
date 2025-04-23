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

type param = string
type field = string

let insert_string x : field = Ocsigen_lib.id x

(* insert_file is implemented only on client side *)
let insert_file _ : field =
  failwith "Constructing an URL with file parameters not possible"

let to_string : field -> string = fun x -> Ocsigen_lib.id x
let inject_param_list = Ocsigen_lib.id
let get_param_list = Ocsigen_lib.id
let inject_param_table = Ocsigen_lib.id
let string_of_param s = s
