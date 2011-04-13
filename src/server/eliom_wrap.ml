(* Ocsigen
 * Copyright (C) 2011 Pierre Chambart
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

module AddrType =
struct
  type t = Obj.t
  let hash v =
    let v = Obj.repr v in
    if Obj.is_block v
    then (Obj.obj v:int)
    else failwith ("not a block "^(string_of_int (Obj.obj v))) 
  let equal = (==)
end

module T = Hashtbl.Make(AddrType)

let with_no_heap_move f v =
  let gc_control = Gc.get () in
  (* disable heap compaction *)
  Gc.set { gc_control with Gc.max_overhead = max_int };
  (* promote all remaining parts of v to the major heap *)
  Gc.minor ();
  (* from now on, memory addresses of parts of v won't change *)
  let res =
    try `Data (f v)
    with e -> `Exn e
  in
  (* reset gc settings *)
  Gc.set gc_control;
  match res with
    | `Data v -> v
    | `Exn e -> raise e

module Mark : 
sig
  type t
  val wrap_mark : t
  val do_nothing_mark: t
  val unwrap_mark : t
end =
struct
  type t = string
  let wrap_mark = "wrap_mark"
  let do_nothing_mark = "do_nothing_mark"
  let unwrap_mark = "unwrap_mark"
end

type marked_value =
    { mark : Mark.t;
      f : ( Obj.t -> Obj.t ) option; }

let make_mark f mark =
  { mark; f }

let is_marked (mark:Mark.t) o =

  let is_mark o =
    if (Obj.tag o = 0 && Obj.size o = 2 && Obj.field o 0 == (Obj.repr mark))
    then (let f = (Obj.field o 1) in
	  assert (Obj.tag f = 0); (* The case None should not happen here *)
	  assert (Obj.size f = 1);
	  assert (let tag = Obj.tag (Obj.field f 0) in tag = Obj.infix_tag || tag = Obj.closure_tag);
	  true)
    else false
  in

  if (Obj.tag o = 0 && Obj.size o >= 2)
  (* WARNING: we only allow block values with tag = 0 to be wrapped.
     It is easier: we do not have to do another test to know if the
     value is a function *)
  then
    begin
      let potential_mark = (Obj.field o (Obj.size o - 1)) in
      if is_mark potential_mark
      then Some (Obj.obj potential_mark:marked_value)
      else None
    end
  else None

let rec search_and_replace_ t v =
  try
    if Obj.tag v < Obj.no_scan_tag
    then T.find t v
    else v
  with
    | Not_found ->
      match is_marked Mark.wrap_mark v with
	| Some { f = Some f } ->
	  let new_v = f v in
	  let res = search_and_replace_ t new_v in
	  T.replace t v res;
	  res
	| Some { f = None } -> assert false
	| None ->
	  let tag = Obj.tag v in
	  if tag = Obj.closure_tag || tag = Obj.infix_tag || tag = Obj.lazy_tag || tag = Obj.object_tag
	  then
	    ( if tag = Obj.lazy_tag then failwith "lazy values must be forced before wrapping";
	      if tag = Obj.object_tag then failwith "cannot wrap object values";
	      if tag = Obj.closure_tag then failwith "cannot wrap functionnal values";
	      failwith "cannot wrap functionnal values: infix tag" )
	  else
	    begin
	      let size = Obj.size v in
	      let new_v = Obj.new_block tag size in
	      T.add t v new_v;
	      (* It is ok to do this because tag < no_scan_tag and it is
		 not a closure ( either infix, normal or lazy ) *)
	      for i = 0 to size - 1 do
		Obj.set_field new_v i (search_and_replace_ t (Obj.field v i));
	      done;
	      new_v
	    end

let search_and_replace v =
  let t = T.create 1 in
  with_no_heap_move (search_and_replace_ t) v

type +'a wrapper = marked_value

let create_wrapper (f: 'a -> 'b) : 'a wrapper =
  make_mark (Some (fun x -> Obj.repr (f (Obj.obj x)))) Mark.wrap_mark

let empty_wrapper : 'a wrapper =
  make_mark None Mark.do_nothing_mark

type unwrap_id = int

let id_of_int x = x

type unwrapper =
    (* WARNING Must be the same as Eliom_unwrap.unwrapper *)
    { id : unwrap_id;
      umark : Mark.t; }

let create_unwrapper id =
  { id = id;
    umark = Mark.unwrap_mark }

let empty_unwrapper =
  { id = -1;
    umark = Mark.do_nothing_mark }

let wrap v =
  Obj.magic Mark.unwrap_mark, Obj.obj (search_and_replace (Obj.repr v))

