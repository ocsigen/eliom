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

let section = Lwt_log.Section.make "eliom:wrap"

type poly

external to_poly : 'a -> poly = "%identity"

type 'a wrapped_value = poly * 'a

let with_no_heap_compaction f v =
  let gc_control = Gc.get () in
  (* disable heap compaction *)
  Gc.set {gc_control with Gc.max_overhead = max_int};
  match f v with
  | v ->
      (* reset gc settings *)
      Gc.set gc_control; v
  | exception e ->
      (* reset gc settings *)
      Gc.set gc_control; raise e

module Mark : sig
  type t

  val wrap_mark : t
  val do_nothing_mark : t
  val unwrap_mark : t
end = struct
  type t = string

  let wrap_mark = "wrap_mark"
  let do_nothing_mark = "do_nothing_mark"
  let unwrap_mark = "unwrap_mark"
end

type marked_value = {mark : Mark.t; f : (Obj.t -> Obj.t) option}
[@@warning "-69"]

let make_mark f mark = {mark; f}

let is_marked o =
  let is_mark o =
    if
      Obj.tag o = 0
      && Obj.size o = 2
      && Obj.field o 0 == Obj.repr Mark.wrap_mark
    then (
      let f = Obj.field o 1 in
      assert (Obj.tag f = 0);
      (* The case None should not happen here *)
      assert (Obj.size f = 1);
      assert (
        let tag = Obj.tag (Obj.field f 0) in
        tag = Obj.infix_tag || tag = Obj.closure_tag);
      true)
    else false
  in
  if
    Obj.tag o = 0 && Obj.size o >= 2
    (* WARNING: we only allow block values with tag = 0 to be wrapped.
     It is easier: we do not have to do another test to know if the
     value is a function *)
  then
    let potential_mark = Obj.field o (Obj.size o - 1) in
    is_mark potential_mark
  else false

let wrap_locally o =
  let mark : marked_value = Obj.obj (Obj.field o (Obj.size o - 1)) in
  match mark.f with Some f -> f o | None -> assert false

let bits = 8
(* We use a hash-table with open addressing (which minimize
   allocations) and resizable arrays. The initial size of the hash
   table is 2 ** bits; the initial size of arrays is half this *)

let none = Obj.repr 0 (* Unallocated entry in an array or in a hash-table *)

module DynArray = struct
  let rec check_size a i =
    let len = Array.length !a in
    if i > len
    then (
      let old_a = !a in
      a := Array.make (2 * len) none;
      Array.blit old_a 0 !a 0 len;
      check_size a i)

  let make () = ref (Array.make (1 lsl (bits - 1)) none)
  let get a i = !a.(i)
  let set a i v = !a.(i) <- v
end

let resize_count = ref 0
let rehash_count = ref 0

(* Hash-table associating an integer to a block.
   As the block may be moved (once) during a minor garbage collection,
   we may allocate more than once index for a block. But thereafter a
   look-up will always return the second index. *)
module Tbl = struct
  type t =
    { mutable size : int
    ; (* Size of the hash table *)
      mutable shift : int
    ; (* For hashing *)
      mutable occupancy : int
    ; (* How many elements have been inserted *)
      mutable obj : Obj.t array
    ; (* Inserted blocks *)
      mutable idx : int array
    ; (* Corresponding indices *)
      mutable gc : int
    ; (* Last minor GC cycle where the
                                    table was accurate *)
      on_resize : (int -> unit) list }
  (* Functions called on resize *)

  let cst =
    (* Fibonacci hash: 2 ^ Sys.int_size / phi *)
    Int64.to_int (Int64.shift_right 0x4F1BBCDCBFA53E09L (63 - Sys.int_size))

  let hash tbl x = (Obj.magic x * cst) lsr tbl.shift
  let gc_count () = Gc.((quick_stat ()).minor_collections)

  (* Rehash the hash-table, possibly after resizing it *)
  let reallocate resize tbl =
    let old_size = tbl.size in
    let old_obj = tbl.obj in
    let old_idx = tbl.idx in
    if resize
    then (
      tbl.size <- 2 * old_size;
      tbl.shift <- tbl.shift - 1;
      List.iter (fun f -> f (tbl.size lsr 1)) tbl.on_resize);
    tbl.obj <- Array.make tbl.size none;
    tbl.idx <- Array.make tbl.size (-1);
    tbl.gc <- gc_count ();
    let rec insert tbl h x idx =
      let y = tbl.obj.(h) in
      if y == none
      then (
        tbl.obj.(h) <- x;
        tbl.idx.(h) <- idx)
      else if y == x
      then tbl.idx.(h) <- max idx tbl.idx.(h) (* Keep largest index *)
      else insert tbl ((h + 1) land (tbl.size - 1)) x idx
    in
    for i = 0 to old_size - 1 do
      let x = old_obj.(i) in
      if x != none then insert tbl (hash tbl x) x old_idx.(i)
    done

  let resize tbl = incr resize_count; reallocate true tbl
  let rehash tbl = incr rehash_count; reallocate false tbl

  let make tbls =
    let size = 1 lsl bits in
    let obj = Array.make size none in
    let idx = Array.make size (-1) in
    let on_resize = List.map DynArray.check_size tbls in
    let gc = gc_count () in
    {size; shift = Sys.int_size - bits; occupancy = 0; obj; idx; gc; on_resize}

  let rec allocate_rec tbl x i =
    if tbl.obj.(i) == x
    then tbl.idx.(i)
    else if tbl.obj.(i) == none
    then (
      tbl.obj.(i) <- x;
      let idx = tbl.occupancy in
      tbl.idx.(i) <- idx;
      tbl.occupancy <- idx + 1;
      if tbl.occupancy * 2 >= tbl.size then resize tbl;
      idx)
    else allocate_rec tbl x ((i + 1) land (tbl.size - 1))

  (* Insert a block into the hash-table. This may return a new index
     if the block was moved. *)
  let allocate_index tbl x = allocate_rec tbl x (hash tbl x)

  let rec get_rec tbl x i =
    let y = tbl.obj.(i) in
    if y == x
    then tbl.idx.(i)
    else if y == none
    then -1 (* Not found *)
    else get_rec tbl x ((i + 1) land (tbl.size - 1))

  (* This may fail if a GC occurred *)
  let get_index_no_retry tbl x = get_rec tbl x (hash tbl x)

  (* Get the index associated to a block already in the hash-table.
     If allocate_index is not invoked in-between, this always returns
     the same index for a given block. Indeed, a look-up always return
     the largest index of a block; this property is preserved both
     when invoking allocate_index (though this may allocate a larger
     index) and by rehashing. *)
  let get_index tbl x =
    let idx = get_index_no_retry tbl x in
    if idx <> -1
    then idx
    else (
      rehash tbl;
      let idx = get_index_no_retry tbl x in
      if idx = -1
      then (
        for i = 0 to Array.length tbl.obj - 1 do
          assert (tbl.obj.(i) != x)
        done;
        Format.eprintf "%b@." (is_marked x));
      assert (idx <> -1);
      idx)

  (* We can check whether the table is up to date, but this has a very
     slight chance to perform an allocation; in which case, the table
     will no longer be up to date... *)
  let was_up_to_date tbl = tbl.gc = gc_count ()
end

let obj_kind v =
  if not (Obj.is_block v)
  then `Opaque
  else
    let tag = Obj.tag v in
    if tag >= Obj.no_scan_tag
    then `Opaque
    else if tag <= Obj.last_non_constant_constructor_tag
    then `Scannable
    else if tag = Obj.forward_tag
    then
      let tag' = Obj.tag (Obj.field v 0) in
      if tag' = Obj.forward_tag || tag' = Obj.double_tag
      then `Scannable
      else (* Forward pointer that may be optimized away by the GC *)
        `Forward
    else (
      if tag = Obj.lazy_tag
      then failwith "lazy values must be forced before wrapping";
      if tag = Obj.object_tag then failwith "cannot wrap object values";
      if tag = Obj.closure_tag then failwith "cannot wrap functional values";
      if tag = Obj.infix_tag
      then failwith "cannot wrap functional values: infix tag";
      (* Should not happen (in case a new kind of value is added) *)
      failwith (Printf.sprintf "cannot wrap value (unexpected tag %d)" tag))

let unchanged =
  (* This block and its descendants can be left unchanged *)
  Obj.repr 1

let modified =
  (* This block or its descendants may need to be modified *)
  Obj.repr 2

let iteration_count = ref 0
let wrap_count = ref 0

(* First step: we traverse the value and find which parts need to be
   replaced. We also compute which parts can be clearly left
   unchanged. We may traverse some values twice if a minor GC occurs,
   but this is harmless. *)
(* TODO: shall we use an explicit stack to avoid stack overflows? *)
let rec find_substs tbl subst_tbl v =
  incr iteration_count;
  match obj_kind v with
  | `Opaque ->
      (* Opaque values don't need to be copied *)
      unchanged
  | `Forward ->
      (* Follow the forward pointers that may disappear due to GC
       (our code might get confused if we stored them in the
       hash-table) *)
      find_substs tbl subst_tbl (Obj.field v 0)
  | `Scannable ->
      let idx = Tbl.allocate_index tbl v in
      let v' = DynArray.get subst_tbl idx in
      if v' == none (* Not visited yet *)
      then
        if is_marked v
        then
          if not (Tbl.was_up_to_date tbl)
          then (
            (* v may have been visited already, so we rehash and try
             again. Indeed, we don't want to call the wrapping
             function twice on the same value. *)
            Tbl.rehash tbl;
            find_substs tbl subst_tbl v)
          else (
            incr wrap_count;
            let v' = wrap_locally v in
            DynArray.set subst_tbl idx v';
            ignore (find_substs tbl subst_tbl v');
            modified)
        else (
          (* We don't know yet whether v needs to be copied.
           We conservatively assume so for now. *)
          DynArray.set subst_tbl idx modified;
          let size = Obj.size v in
          let is_unchanged = ref true in
          for i = 0 to size - 1 do
            let status = find_substs tbl subst_tbl (Obj.field v i) in
            is_unchanged := !is_unchanged && status == unchanged
          done;
          let res = if !is_unchanged then unchanged else modified in
          DynArray.set subst_tbl idx res;
          res)
      else v'

let copy_count = ref 0

let rec duplicate tbl subst_tbl copy_tbl orig =
  match obj_kind orig with
  | `Opaque ->
      (* Opaque values are not copied *)
      orig
  | `Forward ->
      (* Follow forward pointers that may disappear due to GC *)
      duplicate tbl subst_tbl copy_tbl (Obj.field orig 0)
  | `Scannable ->
      let idx = Tbl.get_index tbl orig in
      let subst = DynArray.get subst_tbl idx in
      if subst == unchanged
      then (* This block does not need to be copied *)
        orig
      else if subst != modified
      then
        (* This block is replaced by another value *)
        duplicate tbl subst_tbl copy_tbl subst
      else
        let copy = DynArray.get copy_tbl idx in
        if copy != none
        then (* Since we have already copied the block; return the copy *)
          copy
        else (
          incr copy_count;
          let copy = Obj.dup orig in
          DynArray.set copy_tbl idx copy;
          let size = Obj.size orig in
          for i = 0 to size - 1 do
            let child = Obj.field orig i in
            let child_copy = duplicate tbl subst_tbl copy_tbl child in
            if child_copy != child then Obj.set_field copy i child_copy
          done;
          copy)

let perform_wrap =
  with_no_heap_compaction @@ fun v ->
  iteration_count := 0;
  copy_count := 0;
  wrap_count := 0;
  resize_count := 0;
  rehash_count := 0;
  (* TODO: maybe we should use globally allocated tables by default,
     with temporary allocations only for really large values? *)
  let subst_tbl = DynArray.make () in
  let copy_tbl = DynArray.make () in
  let tbl = Tbl.make [subst_tbl; copy_tbl] in
  ignore (find_substs tbl subst_tbl v);
  let w = duplicate tbl subst_tbl copy_tbl v in
  Lwt_log.ign_debug_f ~section
    "Wrap stats: %d visited (%d blocks), %d wrapped, %d copied, %d resizes, %d rehashes"
    !iteration_count tbl.occupancy !wrap_count !copy_count !resize_count
    !rehash_count;
  w

type +'a wrapper = marked_value

let create_wrapper (f : 'a -> 'b) : 'a wrapper =
  make_mark (Some (fun x -> Obj.repr (f (Obj.obj x)))) Mark.wrap_mark

let empty_wrapper : 'a wrapper = make_mark None Mark.do_nothing_mark

type unwrap_id = int

let id_of_int x = x

type unwrapper =
  { (* WARNING Must be the same as Eliom_unwrap.unwrapper *)
    id : unwrap_id
  ; umark : Mark.t }
[@@warning "-69"]

let create_unwrapper id = {id; umark = Mark.unwrap_mark}
let empty_unwrapper = {id = -1; umark = Mark.do_nothing_mark}
let wrap v = to_poly Mark.unwrap_mark, Obj.obj (perform_wrap (Obj.repr v))
