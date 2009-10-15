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
Keeps the most recently used values in memory.

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
