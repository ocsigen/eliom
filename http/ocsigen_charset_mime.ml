(* Ocsigen
 * http://www.ocsigen.org
 * ocsigen_charset_mime.ml Copyright (C) 2008
 * Boris Yakobowski
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

module MapString = Map.Make(String)
type extension = string
type filename = string
type file = string


type 'a assoc_item =
  | Extension of extension * 'a
  | File of filename * 'a
  | Regexp of Netstring_pcre.regexp * 'a
  | Map of 'a MapString.t

type 'a assoc = {
  assoc_list: 'a assoc_item list;
  assoc_default: 'a
}

let find_in_assoc file assoc =
  let filename = Filename.basename file in
  let ext =
    try String.lowercase (Ocsigen_lib.extension_no_directory file)
    with Not_found -> ""
  in
  let rec aux = function
    | [] -> assoc.assoc_default
    | Extension (ext', v) :: q ->
        if ext = ext' then v else aux q
    | File (filename', v) :: q ->
        if filename = filename' then v else aux q
    | Regexp (reg, v) :: q ->
        if Netstring_pcre.string_match reg file 0 <> None then v else aux q
    | Map m :: q ->
        try MapString.find ext m
        with Not_found -> aux q
  in
  aux assoc.assoc_list


let default assoc = assoc.assoc_default

let set_default assoc default = { assoc with assoc_default = default }

let update_ext assoc (ext : extension) v =
  { assoc with assoc_list =
      Extension (String.lowercase ext, v) :: assoc.assoc_list}

let update_file assoc (file : filename) v =
  { assoc with assoc_list = File (file, v) :: assoc.assoc_list}

let update_regexp assoc r v =
  { assoc with assoc_list = Regexp (r, v) :: assoc.assoc_list}


let empty default () = {
  assoc_list = [];
  assoc_default = default
}


(* Handling of charset and mime ; specific values and declarations *)

type charset = string
type mime_type = string

type charset_assoc = charset assoc
type mime_assoc = mime_type assoc

let no_charset : charset = ""
let default_mime_type : mime_type = "application/octet-stream"

let empty_charset_assoc ?(default=no_charset) = empty default
let empty_mime_assoc ?(default=default_mime_type) = empty default

(* Generic functions *)

let default_charset = default
let default_mime = default

let update_charset_ext = update_ext
let update_mime_ext = update_ext

let update_charset_file = update_file
let update_mime_file = update_file

let update_charset_regexp = update_regexp
let update_mime_regexp = update_regexp

let set_default_mime = set_default
let set_default_charset = set_default

let find_charset = find_in_assoc
let find_mime = find_in_assoc


(* Specific handling of content-type *)


let parse_mime_types ~filename : mime_type assoc =
  let rec read_and_split mimemap in_ch =
    try
      let line = input_line in_ch in
      let line_upto =
        try
          let upto = String.index line '#' in
          String.sub line 0 upto
        with Not_found -> line
      in
      let strlist =
        Netstring_pcre.split (Netstring_pcre.regexp "\\s+") line_upto
      in
      match  strlist with
      | [] | [_] -> (* No extension on this line *) read_and_split mimemap in_ch
      | mime :: extensions ->
          let mimemap =
            List.fold_left (fun mimemap ext ->
                              MapString.add ext mime mimemap) mimemap extensions
          in
          read_and_split mimemap in_ch
    with End_of_file -> mimemap
  in
  { assoc_list =
      [ Map(try
              let in_ch = open_in filename in
              let map =
                (try
                   read_and_split MapString.empty in_ch
                 with e -> close_in in_ch; raise e)
              in
              close_in in_ch;
              map
            with Sys_error _ -> MapString.empty
           )];
    assoc_default = default_mime_type;
  }


let default_mime_assoc () =
  let parsed = ref None in
  match !parsed with
    | None ->
        let file = Ocsigen_config.get_mimefile () in
        Ocsigen_messages.debug
          (fun () -> Printf.sprintf "Loading mime types in '%s'" file);
        let map = parse_mime_types file in
        parsed := Some map;
        map
    | Some map -> map
