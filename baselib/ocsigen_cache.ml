(* Ocsigen
 * Copyright (C) 2008-2009
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
Cache.

@author Vincent Balat
*)

let (>>=) = Lwt.bind

module Dlist = (struct

  type 'a node =
      { mutable value : 'a;
        mutable succ : 'a node option;
        mutable prev : 'a node option}

  (* Doubly-linked list with maximum size. The field [list] is
     the beginning of the list. The field [first] is the first
     element that must be removed if the list becomes too long *)
  type 'a t =
      {mutable list : 'a node option (* None = empty *);
       mutable first : 'a node option;
       mutable size : int;
       maxsize : int}

  let length c =
    let rec aux i = function
      | Some {prev=p} -> aux (i + 1) p
      | None -> i
    in aux 0 c.list

  let correct_node n =
    (match n.succ with
       | None -> true
       | Some n' -> n'.prev == Some n) &&
     (match n.prev with
        | None -> true
        | Some n' -> n'.succ == Some n)


  (* Check that a list is correct. To be completed
     1) by adding a check on nodes,
     2) by verifying that last can be reached from first and respectively *)
  let correct_list l =
    (l.size <= l.maxsize) &&
    (length l = l.size) &&
    (match l.first with
       | None -> true
       | Some n -> n.prev = None) &&
    (match l.list with
       | None -> true
       | Some n -> n.succ = None)


  let create_one a = { value = a; succ = None; prev = None}

  let create size = {list = None; first = None; size = 0; maxsize = size}

  (* Add a node to the list. The fields [succ] and [prev] are overridden.
     The function returns the value that is being removed from the list
     if it is too long. The node added becomes the element [list] of the list *)
  let add_node node r =
    match r.list with
      | None ->
          node.succ <- None;
          node.prev <- None;
          r.list <- Some node;
          r.first <- r.list;
          r.size <- 1;
          None
      | Some rl ->
          node.succ <- None;
          node.prev <- r.list;
          rl.succ <- Some node;
          r.list <- Some node;
          if r.size >= r.maxsize
          then (
            match r.first with
              | None -> assert false
              | Some a ->
                  (match a.succ with
                     | None -> assert false
                     | Some b -> b.prev <- None);
                  r.first <- a.succ;
                  Some a.value)
          else (
            r.size <- r.size + 1;
            None
          )

  let add x = add_node (create_one x)

  (* Remove an element that is supposed to be in the list *)
  let remove l node =
    let first =
      match l.first with
        | Some n when node == n -> node.succ
        | _ -> l.first
    in
    let last =
      match l.list with
        | Some n when node == n -> node.prev
        | _ -> l.list
    in
    (match node.succ with
       | None -> ()
       | Some s -> s.prev <- node.prev);
    (match node.prev with
       | None -> ()
       | Some s -> s.succ <- node.succ);
    l.first <- first;
    l.list <- last;
    l.size <- l.size - 1

  let last a = a.list
  let first a = a.first
  let size c = c.size
  let maxsize c = c.maxsize

  let value n = n.value

  let up l node =
    match l.list with
      | Some n when node == n -> ()
      | _ ->
          remove l node;
          ignore (add_node node l)
          (* we must not change the physical address => use add_node *)

end : sig
  type 'a t
  type 'a node
  val create : int -> 'a t
  val add : 'a -> 'a t -> 'a option
  val last : 'a t -> 'a node option
  val first : 'a t -> 'a node option
  val remove : 'a t -> 'a node -> unit
  val up : 'a t -> 'a node -> unit
  val size : 'a t -> int
  val maxsize : 'a t -> int
  val length : 'a t -> int
  val value : 'a node -> 'a
end)


module Weak =  Weak.Make(struct type t = unit -> unit
                                let hash = Hashtbl.hash
                                let equal = (==)
                         end)

let clear_all = Weak.create 17

module Make =
  functor (A: sig
             type key
             type value
           end) ->
struct

  type data = A.key

  module H = Hashtbl.Make(
    struct
      type t = A.key
      let equal a a' = a = a'
      let hash = Hashtbl.hash
    end)

  type t =
      { mutable pointers : A.key Dlist.t;
        mutable table : (A.value * A.key Dlist.node) H.t;
        finder : A.key -> A.value Lwt.t;
        clear: unit -> unit (* This function clears the cache. It is put inside the
          cache structure so that it is garbage-collected only when the cache
          is no longer referenced, as the functions themselves are put inside
          a weak hash table *)
      }

  let clear cache =
    let size = Dlist.maxsize cache.pointers in
    cache.pointers <- Dlist.create size;
    cache.table <- H.create size

  let create f size =
    let rec cache = {pointers = Dlist.create size;
                 table = H.create size;
                 finder = f;
                 clear = f_clear;
                }
    and f_clear = (fun () -> clear cache)
    in
    Weak.add clear_all f_clear;
    cache

  let poke r node =
    Dlist.up r.pointers node

  let find_in_cache cache k =
    let (v, node) = H.find cache.table k in
    poke cache node;
    v

  let remove cache k =
    try
      let (_v, node) = H.find cache.table k in
      H.remove cache.table k;
      Dlist.remove cache.pointers node
    with Not_found -> ()

  (* Add in a cache, under the hypothesis that the value is
     not already in the cache *)
  let add_no_remove cache k v =
    (match Dlist.add k cache.pointers with
      | None -> ()
      | Some v -> H.remove cache.table v
    );
    match Dlist.last cache.pointers with
      | None -> assert false
      | Some n -> H.add cache.table k (v, n)

  let add cache k v =
    remove cache k;
    add_no_remove cache k v

  let size c =
    Dlist.length c.pointers

  let find cache k =
    (try
       Lwt.return (find_in_cache cache k)
     with Not_found ->
       cache.finder k >>= fun r ->
       add_no_remove cache k r;
       Lwt.return r)

  class cache f size_c =
    let c = create f size_c in
  object
    method clear () = clear c
    method find = find c
    method add = add c
    method size = size c
    method find_in_cache = find_in_cache c
    method remove = remove c
  end

end

let clear_all_caches () = Weak.iter (fun f -> f ()) clear_all
