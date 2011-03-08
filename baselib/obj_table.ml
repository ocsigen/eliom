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

(* functions suffixed by _ must be called within protected, otherwise
   there is no guaranty that the hashtable is still valid *)

(* XXX do no use Array of Obj.t: it is dangerous ( if the first element is a
   float, it may become an float array ) *)

external infix_to_closure : Obj.t -> Obj.t = "caml_infix_to_closure"
(** [infix_to_closure infix] returns the closure of a value tagged as infix *)
external infix_of_closure : Obj.t -> Obj.t -> Obj.t = "caml_infix_of_closure"
(** [infix_of_closure closure infix] returns the infix value from [closure] with offset given by [infix]:
    [infix_of_closure (Obj.dup (infix_to_closure infix)) infix] is a copy of infix *)

exception Obj_not_found

let addr x = Printf.sprintf "%x" (Obj.magic x:int)

(* blocks that should be traversed: i.e. blocks that are not
   out_of_heap, unaligned or tagged as no_scan *)
let is_good_block v =
  let tag = Obj.tag v in
  ( tag < Obj.int_tag ) && ( tag <> Obj.no_scan_tag )

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

let compactions () = (Gc.stat ()).Gc.compactions

let with_no_heap_move f =
  (*Printf.printf "(%!";*)
  let gc_control = Gc.get () in
  (* disable heap compaction *)
  Gc.set { gc_control with Gc.max_overhead = max_int };
  (* promote all remaining parts of v to the major heap *)
  Gc.minor ();
  (* from now on, memory addresses of parts of v won't change *)
  let res =
    try `Data (f ())
    with e -> `Exn e
  in
  (* reset gc settings *)
  Gc.set gc_control;
  (*Printf.printf ")\n%!";*)
  match res with
    | `Data v -> v
    | `Exn e -> raise e

let ( -- ) x y =
  let rec aux y x acc =
    if x > y then acc else aux (y-1) x (y::acc)
  in
  aux y x []

let obj_error = ref None

let pos_sons_of_obj o =
  match Obj.tag o with
    | tag when tag = Obj.double_tag -> []
    | tag when tag = Obj.double_array_tag -> []
    | tag when tag = Obj.string_tag -> []
    | tag when tag = Obj.closure_tag -> []
    (* | tag when tag = Obj.closure_tag -> (1 -- (Obj.size o - 1))
       If this were to change, see remarks about copying closures this in the
       copy_ function *)
    | tag when tag = Obj.object_tag -> failwith "do not hande objects"
    | tag when tag = Obj.int_tag -> []
    | tag when tag = Obj.custom_tag -> []
    | tag when tag = Obj.infix_tag -> failwith "pos_sons_of_obj: infix_tax"
    (* This case shouldn't happen, in case of infix_tag, call
       pos_sons_of_obj on [infix_closure o] instead of [o]*)
    | tag when tag = Obj.forward_tag -> failwith "forward tag: TODO"
    | tag when tag = Obj.lazy_tag -> failwith "lazy tag: TODO"
    | tag when tag < Obj.lazy_tag -> (0 -- (Obj.size o - 1)) (* classic block values *)
(*
    | tag when tag = Obj.unaligned_tag -> []
    | tag when tag = Obj.out_of_heap_tag -> []
    | tag when tag = Obj.no_scan_tag -> []
*)
    | t -> failwith (Printf.sprintf "tag not handled %i" t)

let real_obj o =
  if Obj.tag o = Obj.infix_tag
  then infix_to_closure o
  else o

let sons_of_obj o =
  let o = real_obj o in
  List.map (fun i -> Obj.field o i) (pos_sons_of_obj o)

let sons o =
  let o = real_obj o in
  List.map (fun i -> i,Obj.field o i) (pos_sons_of_obj o)

module T = Hashtbl.Make(AddrType)

type entry =
    { v: Obj.t;
      mutable no_copy: bool;
      id : int; }

let new_id = let r = ref 0 in fun () -> incr r; !r

type table =
    { mutable table: entry T.t;
      mutable compactions: int;
      mutable root: Obj.t; }

(* XXX to do some optimisations it is possible to structure values
   according to depth from the root. -> it would permit to copy and
   scan only a part of the table when replacing values. *)

let restore_table_ table =
  let new_t = T.create (T.length table) in
  let add _ v = T.add new_t v.v v in
  T.iter add table;
  new_t

let restore_ t =
  t.table <- restore_table_ t.table;
  t.compactions <- compactions ()

let check_and_restore_ t =
  if compactions () <> t.compactions
  then restore_ t

let protect f t v =
  let aux () =
    check_and_restore_ t;
    f t v
  in
  try
    with_no_heap_move aux
  with
    | Not_found -> raise Obj_not_found

let restore t = protect (fun t () -> restore_ t) t ()

let rec tail_rec_add_ t = function
  | [] -> ()
  | v::acc ->
    let acc =
      if is_good_block v
      then
	(if not ( T.mem t.table v )
	 then
	    begin
	      let entry =
		{ v = v;
		  no_copy = false;
		  id = new_id (); }
	      in
	      T.add t.table v entry;
	      (sons_of_obj v)@acc
	    end
	 else acc)
      else acc
    in
    tail_rec_add_ t acc

let add_ t v = tail_rec_add_ t [v]

let add t v = protect add_ t v

let make_  v =
  let table = T.create 0 in
  let t = { table = table;
	    compactions = compactions ();
	    root = v; } in
  add_ t v;
  t

let make v =
  let f () = make_ v in
  with_no_heap_move f

let add_from_ t_old t v =
  let rec aux = function
    | [] -> ()
    | v::acc ->
      let acc =
	if is_good_block v
	then
	  (if not ( T.mem t.table v )
	   then
	      begin
		let entry =
		  try
		    T.find t_old.table v
		  with
		    | Not_found ->
		      { v = v;
			no_copy = false;
			id = new_id (); }
		in
		T.add t.table v entry;
		(sons_of_obj v)@acc
	      end
	   else acc)
	else acc
      in
      aux acc
  in
  aux [v]

let make_from_table_ t_old =
  let v = t_old.root in
  let table = T.create 0 in
  let t = { table = table;
	    compactions = compactions ();
	    root = v; } in
  add_from_ t_old t v;
  t

let copy_ t' =
  (*Printf.printf "copy_%!";*)
  let t = make_from_table_ t' in
  (* create a new table with the same "no_copy" marks, but without
     unreacable values *)
  let new_t = T.create (T.length t.table) in
  (* in new_t, values will be stored with the old value as key *)
  let copy key v =
    let new_v =
      if v.no_copy || (let tag = Obj.tag v.v in tag = Obj.infix_tag || tag = Obj.closure_tag)
      (* It is far easier to never copy closures. I don't see any use
	 for now to copy closure ( we do not browse their sons, so
	 they will never be modified, hence do not need to be
	 copied ).
	 To allow copy of closure, it is needed to take into account the case of infix_tag:
	 to copy an infix, we must in fact copy the closure then get the infix back with infix_of_closure.
	 We also need to do some caching to ensure that we copy the closure only one time:
	 we must not do a copy for each infix of a closure and for the closure itself, we must
	 verify before the copy if there is already a copy of this function. *)
      then { v with id = v.id } (* ensure that v is copied *)
      else { v with v = Obj.dup v.v } (* we keep the no_copy mark *)
    (* Obj.dup v.v is a newly created value: it is in the minor heap:
       it must be pushed to the major heap (with Gc.minor) before
       using it as a key in the table: its address would change. *)
    in
    T.add new_t key new_v
  in
  T.iter copy t.table;
  let restore_sons old_v new_entry =
    let old_v = real_obj old_v in
    let new_entry_v = real_obj new_entry.v in
    List.iter (fun i ->
      let old_son = (Obj.field old_v i) in
      if is_good_block old_son
      then
	let new_son = T.find new_t old_son in
	Obj.set_field new_entry_v i new_son.v)
      (pos_sons_of_obj new_entry_v)
  in
  (* after copying, every field in copied value [v] points to old values [o]:
     we make it point to the copy of [o]. *)
  T.iter restore_sons new_t;
  let t = { table = new_t;
	    root = (T.find new_t t.root).v;
	    compactions = compactions (); }
  in
  Gc.minor ();
  (* we need all newly created (copied) values to be in the major heap
     to ensure that the table is consistent ( values correspond to
     their key if the number of compaction didn't change *)
  restore_ t;
  (* we now restore t such that values in new_t are now stored with
     their own value as key. *)
  t

let copy t = protect (fun t () -> copy_ t) t ()

let mem_ t v = T.mem t.table v

let mem t v = protect mem_ t v

let find_ t v = T.find t.table v

let find t v = protect find_ t v

let find_ancessors_ t v =
  let acc = ref [] in
  T.iter (fun _ elt -> 
    List.iter
      (fun (i,son) -> if son == v then acc := (i,elt.v)::!acc)
      (sons elt.v)) t.table;
  !acc

let find_ancessors t v = protect find_ancessors_ t v

let root t = t.root

let replace_ t (old_v,new_v) =
  if t.root == old_v
  then t.root <- new_v;
  add_ t new_v;
  List.iter (fun (i,father) -> Obj.set_field father i new_v)
    (find_ancessors_ t old_v)

let replace t a b = protect replace_ t (a,b)

let no_copy t v =
  try
    (T.find t.table v).no_copy <- true
  with
    | Not_found -> ()

type data =
  | Long of int
  | Double of entry * float
  | Double_array of entry * float array
  | Block of entry * data array
  | Closure of entry * data array
  | Infix of entry * data array
  | String of entry * string
  | Object of entry
  | Out_of_heap of Obj.t
  | No_scan of Obj.t
  | Other of entry

let sons_empty_array o =
  Array.create (List.length (sons_of_obj o)) (Long (-1))

let data_of_obj_ t o =
  (*Printf.printf "debug_%!";*)
  let cache = T.create 0 in
  let rec aux = function
    | [] -> ()
    | (o,i,arr)::acc ->
      let make f =
	let a = sons_empty_array o in
	let res = f (find_ t o) a in
	T.add cache o res;
	res, List.map (fun (i,o) -> o,i,a) (sons o)
      in
      let res,next =
	try
	  if Obj.is_int o
	  then Long (Obj.obj o),[]
	  else T.find cache o,[]
	with
	  | Not_found ->
	    try
	      let res,next =
		match Obj.tag o with
		  | tag when ( tag = Obj.unaligned_tag ) || ( tag = Obj.out_of_heap_tag )
		      -> Out_of_heap o,[]
		  | tag when tag = Obj.no_scan_tag -> No_scan o,[]
		  | tag when tag = Obj.double_tag -> Double (find_ t o,Obj.obj o),[]
		  | tag when tag = Obj.double_array_tag -> Double_array (find_ t o,Obj.obj o),[]
		  | tag when tag = Obj.string_tag -> String (find_ t o,Obj.obj o),[]
		  | tag when tag = Obj.closure_tag -> make (fun o a -> Closure (o,a))
		  | tag when tag = Obj.object_tag -> Object (find_ t o),[]
		  | tag when tag = Obj.int_tag -> assert false (* can't happend here *)
		  | tag when tag = Obj.out_of_heap_tag -> Other (find_ t o),[]
		  | tag when tag = Obj.custom_tag -> Other (find_ t o),[]
		  | tag when tag = Obj.infix_tag -> make (fun o a -> Infix (o,a))
		  | tag when tag < Obj.lazy_tag -> make (fun o a -> Block (o,a))
		  | _ -> Other (find_ t o),[]
	      in
	      T.add cache o res;
	      res,next
	    with Not_found ->
	      obj_error := Some o;
	      failwith "debug: inconsistent table"
      in
      arr.(i) <- res;
      aux (next@acc)
  in
  let a = Array.create 1 (Long (-1)) in
  aux [(o,0,a)];
  a.(0)

let debug t = protect data_of_obj_ t t.root

let size t = T.length t.table

let elts t = T.fold (fun _ e l -> e::l) t.table [] 

let iter_ t f = 
  (*Printf.printf "iter_%!";*)
  T.iter f t.table

let iter t f = protect iter_ t f
