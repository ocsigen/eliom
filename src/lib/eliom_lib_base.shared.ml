(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2011 Grégoire Henry
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

open Ocsigen_lib_base

exception Eliom_Internal_Error of string

module Lwt_ops = struct
  let ( >>= ) = Lwt.( >>= )
  let ( =<< ) = Lwt.( =<< )
  let ( >|= ) = Lwt.( >|= )
  let ( =|< ) = Lwt.( =|< )
end

type pos = Lexing.position * Lexing.position

let pos_to_string ((start, stop) : pos) =
  let open Lexing in
  let start_col = start.pos_cnum - start.pos_bol in
  let stop_col = stop.pos_cnum - stop.pos_bol in
  if start.pos_lnum = stop.pos_lnum
  then
    if start_col = stop_col
    then Printf.sprintf "%s %d:%d" start.pos_fname start.pos_lnum start_col
    else
      Printf.sprintf "%s %d:%d-%d" start.pos_fname start.pos_lnum start_col
        stop_col
  else
    Printf.sprintf "%s %d:%d-%d:%d" start.pos_fname start.pos_lnum start_col
      stop.pos_lnum stop_col

module type Map_S = sig
  include Map.S

  val from_list : (key * 'a) list -> 'a t
  val to_string : ?sep:string -> ('a -> string) -> 'a t -> string
end

module Map_make (Ord : sig
    include Map.OrderedType

    val to_string : t -> string
  end) =
struct
  include Map.Make (Ord)

  let from_list li = List.fold_right (uncurry add) li empty

  let to_string ?(sep = ", ") value_to_string map =
    String.concat sep
      (List.map
         (fun (key, value) -> Ord.to_string key ^ ":" ^ value_to_string value)
         (bindings map))
end

module Int64_map = Map_make (Int64)

module Int_map = Map_make (struct
    type t = int

    let compare = ( - )
    let to_string = string_of_int
  end)

module String_map = Map_make (struct
    include String

    let to_string x = x
  end)
