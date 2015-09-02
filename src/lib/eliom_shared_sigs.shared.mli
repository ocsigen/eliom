(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015
 * Vasilis Papavasileiou
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

module type S = sig

  type 'a t

  val const : 'a -> 'a t
  val value : 'a t -> 'a Eliom_lib.shared_value

  val map :
    ?eq:('b -> 'b -> bool) Eliom_lib.shared_value ->
    ('a -> 'b) Eliom_lib.shared_value ->
    'a t -> 'b t
  val merge :
    ?eq:('a -> 'a -> bool) ->
    ('a -> 'b -> 'a) Eliom_lib.shared_value ->
    'a -> 'b t list -> 'a t
  val l2 :
    ?eq:('c -> 'c -> bool) Eliom_lib.shared_value ->
    ('a -> 'b -> 'c) Eliom_lib.shared_value ->
    'a t -> 'b t -> 'c t
  val l3 :
    ?eq:('d -> 'd -> bool) Eliom_lib.shared_value ->
    ('a -> 'b -> 'c -> 'd) Eliom_lib.shared_value ->
    'a t -> 'b t -> 'c t -> 'd t
  val switch :
    ?eq:('a -> 'a -> bool) Eliom_lib.shared_value ->
    'a t t -> 'a t

  module Infix : sig
    val (>|=) : 'a t -> ('a -> 'b) Eliom_lib.shared_value -> 'b t
    val (=|<) : ('a -> 'b) Eliom_lib.shared_value -> 'a t -> 'b t
  end

  module Lwt : sig
    val map_s :
      ?eq:('b -> 'b -> bool) Eliom_lib.shared_value ->
      ('a -> 'b Lwt.t) Eliom_lib.shared_value ->
      'a t -> 'b t Lwt.t
    val l2_s :
      ?eq:('c -> 'c -> bool) Eliom_lib.shared_value ->
      ('a -> 'b -> 'c Lwt.t) Eliom_lib.shared_value ->
      'a t -> 'b t -> 'c t Lwt.t
    val merge_s :
      ?eq:('a -> 'a -> bool) Eliom_lib.shared_value ->
      ('a -> 'b -> 'a Lwt.t) Eliom_lib.shared_value ->
      'a -> 'b t list -> 'a t Lwt.t
  end

end

module type RLIST = sig

  type 'a t
  type 'a handle
  type 'a signal

  val make :
    ?default:('a t Eliom_lib.client_value *
              'a handle Eliom_lib.client_value) ->
    ?reset_default:bool ->
    'a list ->
    'a t * 'a handle

  val concat : 'a t -> 'a t -> 'a t
  val from_signal : 'a list signal -> 'a t
  val value : 'a t -> 'a list Eliom_lib.shared_value
  val value_s : 'a t -> 'a list signal
  val singleton_s : 'a signal -> 'a t
  val map : ('a -> 'b) Eliom_lib.shared_value -> 'a t -> 'b t
  val make_from_s : 'a list signal -> 'a t

  module Lwt : sig
    val map_p :
      ('a -> 'b Lwt.t) Eliom_lib.shared_value ->
      'a t -> 'b t Lwt.t
  end

end
