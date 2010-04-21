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
        mutable succ : 'a node option; (* the node added just after *)
        mutable prev : 'a node option; (* the node added just before *)
        mutable mylist : 'a t option; (* the list to which it belongs *)
      }

  (* Doubly-linked list with maximum size.
     The field [oldest] is the first
     element that must be removed if the list becomes too long.
  *)
  and 'a t =
      {mutable newest : 'a node option (* None = empty *);
       mutable oldest : 'a node option;
       mutable size : int;
       mutable maxsize : int;
       mutable finaliser : 'a node -> unit;
      }

(* Checks (by BY):

  let compute_length c =
    let rec aux i = function
      | Some {prev=p} -> aux (i + 1) p
      | None -> i
    in aux 0 c.newest

  let correct_node n =
    (match n.succ with
       | None -> true
       | Some n' -> n'.prev == Some n) &&
     (match n.prev with
        | None -> true
        | Some n' -> n'.succ == Some n)

  (* Check that a list is correct. To be completed
     1. by adding a check on nodes,
     2. by verifying that newest can be reached from oldest and respectively *)
  let correct_list l =
    (l.size <= l.maxsize) &&
    (compute_length l = l.size) &&
    (match l.oldest with
       | None -> true
       | Some n -> n.prev = None) &&
    (match l.newest with
       | None -> true
       | Some n -> n.succ = None)
*)

  let create size = 
    {newest = None;
     oldest = None; 
     size = 0;
     maxsize = size; 
     finaliser = fun _ -> ()}

  (* Remove an element from its list - don't finalise *)
  let remove' node l =
    (* assertion (node.mylist = Some l' with l' == l); *)
    let oldest =
      match l.oldest with
        | Some n when node == n -> node.succ
        | _ -> l.oldest
    in
    let newest =
      match l.newest with
        | Some n when node == n -> node.prev
        | _ -> l.newest
    in
    (match node.succ with
       | None -> ()
       | Some s -> s.prev <- node.prev);
    (match node.prev with
       | None -> ()
       | Some s -> s.succ <- node.succ);
    l.oldest <- oldest;
    l.newest <- newest;
    node.mylist <- None;
    l.size <- l.size - 1

  (* Remove an element from its list - and finalise *)
  let remove node =
    match node.mylist with
      | None -> ()
      | Some l as a ->
          try
            l.finaliser node;
            assert (node.mylist == a);
            remove' node l
          with e ->
            remove' node l;
            raise e

  (* Add a node that do not belong to any list to a list.
     The fields [succ] and [prev] are overridden.
     If the list is too long, the function returns the oldest value.
     The node added becomes the element [list] of the list *)
  (* do not finalise *)
  (* not exported *)
  let add_node node r =
    assert (node.mylist = None);
    node.mylist <- Some r;
    match r.newest with
      | None ->
          node.succ <- None;
          node.prev <- None;
          r.newest <- Some node;
          r.oldest <- r.newest;
          r.size <- 1;
          None
      | Some rl ->
          node.succ <- None;
          node.prev <- r.newest;
          rl.succ <- Some node;
          r.newest <- Some node;
          r.size <- r.size + 1;
          if r.size > r.maxsize
          then r.oldest
          else None

  let add x l = 
    let create_one a = { value = a; succ = None; prev = None; mylist = None;} in
    (* create_one not exported *)
    match add_node (create_one x) l with
      | None -> None
      | Some v -> remove v; Some v.value

  let newest a = a.newest
  let oldest a = a.oldest
  let size c = c.size
  let maxsize c = c.maxsize

  let value n = n.value
  let list_of n = n.mylist

  let up node =
    match node.mylist with
      | None -> ()
      | Some l -> 
          match l.newest with
            | Some n when node == n -> ()
            | _ ->
                remove' node l;
                ignore (add_node node l) (* assertion: = None *)
                  (* we must not change the physical address => use add_node *)

  let rec remove_n_oldest l n = (* remove the n oldest values 
                                   (or less if the list is not long enough) ;
                                   returns the list of removed values *)
    if n <= 0 
    then []
    else
      match l.oldest with
        | None -> []
        | Some node -> 
            let v = node.value in
            remove node; (* and finalise! *)
            v::remove_n_oldest l (n-1)

  let set_maxsize l m =
    let size = l.size in
    if m >= size
    then (l.maxsize <- m; [])
    else if m <= 0
    then failwith "Dlist.set_maxsize"
    else 
      let ll = remove_n_oldest l (size - m) in
      l.maxsize <- m;
      ll

  let set_finaliser f l = l.finaliser <- f

  let get_finaliser l = l.finaliser

end : sig
  type 'a t
  type 'a node
  val create : int -> 'a t
  val add : 'a -> 'a t -> 'a option
  val newest : 'a t -> 'a node option
  val oldest : 'a t -> 'a node option

  (** Removes an element from its list.
      If the element is not in a list, it does nothing.
      If it is in a list, it calls the finaliser, then removes the element.
      If the finaliser fails with an exception, 
      the element is removed and the exception is raised again.
  *)
  val remove : 'a node -> unit

  (** Removes the element from its list without finalising, 
      then adds it as newest. *)
  val up : 'a node -> unit

  val size : 'a t -> int
  val maxsize : 'a t -> int

  val value : 'a node -> 'a
  val list_of : 'a node -> 'a t option

  (** remove the n oldest values ; 
      returns the list of removed values *)
  val remove_n_oldest : 'a t -> int -> 'a list

  (** change the maximum size ;
      returns the list of removed values, if any. *)
  val set_maxsize : 'a t -> int -> 'a list

  (** set a function to be called automatically on a piece of data
      when it disappears from the list
      (either by explicit removal or because the maximum size is exceeded) *)
  val set_finaliser : ('a node -> unit) -> 'a t -> unit
  val get_finaliser : 'a t -> ('a node -> unit)
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

  (* not exported *)
  let poke cache node =
    assert (match Dlist.list_of node with
              | None -> false | Some l -> cache.pointers == l);
    Dlist.up node

  let find_in_cache cache k =
    let (v, node) = H.find cache.table k in
    poke cache node;
    v

  let remove cache k =
    try
      let (_v, node) = H.find cache.table k in
      H.remove cache.table k;
      assert (match Dlist.list_of node with
                | None -> false | Some l -> cache.pointers == l);
      Dlist.remove node
    with Not_found -> ()

  (* Add in a cache, under the hypothesis that the value is
     not already in the cache *)
  let add_no_remove cache k v =
    (match Dlist.add k cache.pointers with
      | None -> ()
      | Some v -> H.remove cache.table v
    );
    match Dlist.newest cache.pointers with
      | None -> assert false
      | Some n -> H.add cache.table k (v, n)

  let add cache k v =
    remove cache k;
    add_no_remove cache k v

  let size c =
    Dlist.size c.pointers

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
