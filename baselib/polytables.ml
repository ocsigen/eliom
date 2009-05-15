(* Ocsimore
 * Copyright (C) 2008
 * Laboratoire PPS - Université Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(**
   @author Vincent Balat
   @author Jérôme Vouillon
*)

type 'a key = int * 'a option ref

module T = Map.Make(struct 
                      type t = int
                      let compare = compare
                    end)

type t = (unit -> unit) T.t ref

let create () = ref T.empty

let c = ref (-1)
let make_key () =
  c := !c + 1;
  (!c, ref None)

let set ~(table : t) ~key:((k, r) : 'a key) ~(value : 'a) =
  table := T.add k (fun () -> r := Some value) !table

let get ~(table : t) ~key:((k, r) : 'a key) =
  (T.find k !table) ();
  match !r with
    | Some v -> r:= None; v
    | None -> failwith "Polytable.get"

let clear ~(table : t) =
  table := T.empty

