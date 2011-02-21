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

module Mark : sig
  type t
  val t : t
end
=
struct
  type t = string
  let t = "wrap_mark"
end

type 'a t =
    { wrap : Obj.t -> Obj.t;
      mark : Mark.t; }

let make_table v =
  let t = Obj_table.make v in
  Obj_table.no_copy t (Obj.repr Mark.t);
  let wrappers =
    List.map (fun (_,v) -> (Obj.obj v:'a t)) (Obj_table.find_ancessors t (Obj.repr Mark.t)) in
  List.iter (fun wrapper ->
    Obj_table.no_copy t (Obj.repr wrapper);
    Obj_table.no_copy t (Obj.repr wrapper.wrap))
    wrappers;
  t

let rec replace_marked t =
  if not (Obj_table.mem t (Obj.repr Mark.t))
  then Obj_table.root t
  else
    begin
      let t = Obj_table.copy t in
      match Obj_table.find_ancessors t (Obj.repr Mark.t) with
	| [] -> assert false
	(* since [Obj_table.mem t Mark.t] is true this list can't be empty *)
	| (_,wrapper)::_ ->
	  match Obj_table.find_ancessors t wrapper with
	    | [] -> assert false
	      (* a wrapper can't be the root: it must have ancessors *)
	    | (_,father)::_ ->
	      let wrapper = (Obj.obj wrapper:'a t) in
	      let v = wrapper.wrap father in
	      Obj_table.replace t father v;
	      replace_marked (make_table (Obj_table.root t))
    end

let wrap v =
  let v = Obj.repr v in
  let t = make_table v in
  let v = replace_marked t in
  Marshal.to_string v []

let create (f:'a -> 'b) : 'a t =
  { wrap = (fun x -> Obj.repr (f (Obj.obj x)));
    mark = Mark.t; }

