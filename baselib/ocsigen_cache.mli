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
Cache.
Association tables (from any kind of database)
that keep the most recently used values in memory.

It is based on a structure of doubly linked lists with maximum size,
that keeps only the mostly recently used values first, if you call the [up]
function each time you use a value.
(Insertion, remove and "up" in time 1).
This structure is exported, so that it can be used in other cases.

Not (preemptive) thread safe.

@author Vincent Balat
*)

module Make :
  functor (A : sig type key type value end) ->
    sig

      class cache : (A.key -> A.value Lwt.t) -> int ->
      object

      (** Find the cached value associated to the key, or binds this
         value in the cache using the function passed as argument
         to [create], and returns this value *)
      method find : A.key -> A.value Lwt.t

      (** Find the cached value associated to the key. Raises [Not_found]
         if the key is not present in the cache *)
      method find_in_cache : A.key -> A.value

      method remove : A.key -> unit
      method add : A.key -> A.value -> unit
      method clear : unit -> unit
      method size : int
    end
 end


(** Clear the contents of all the existing caches *)
val clear_all_caches : unit -> unit



(** Doubly-linked lists with maximum size. *)
module Dlist : sig
  type 'a t
  type 'a node
  val create : int -> 'a t

  (** Adds an element to the list, 
      and possibly returns the element that has been removed if the maximum
      size was exceeded. *)
  val add : 'a -> 'a t -> 'a option

  (** Removes an element from its list. *)
  val remove : 'a node -> unit

  (** Removes the element from its list without finalising, 
      then adds it as newest. *)
  val up : 'a node -> unit

  val newest : 'a t -> 'a node option
  val oldest : 'a t -> 'a node option

  val size : 'a t -> int
  val maxsize : 'a t -> int
  val value : 'a node -> 'a

  (** The list to which the node belongs *)
  val list_of : 'a node -> 'a t option

  (** remove the n oldest values (or less if the list is not long enough) ;
      returns the list of removed values *)
  val remove_n_oldest : 'a t -> int -> 'a list

  (** change the maximum size ;
      returns the list of removed values, if any.
  *)
  val set_maxsize : 'a t -> int -> 'a list

  (** set a function to be called automatically on a piece of data
      when it disappears from the list
      (either by explicit removal or because the maximum size is exceeded) *)
  val set_finaliser : ('a node -> unit) -> 'a t -> unit
  val get_finaliser : 'a t -> ('a node -> unit)
end
