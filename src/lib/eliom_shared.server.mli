(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2014
 * Vincent Balat
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

(** This module implements shared (i.e., client-server) versions of
    the React and ReactiveData libraries.

    On the server side, the reactive signals and data structures are
    comprised of a server-side version and a client-side version. The
    server-side signals (and data structures) are evaluated only once.

    All operations on signals and data need to be provided in the form
    of shared functions, i.e., functions that have both a client-side
    and a server-side implementation. *)

(** [to_signal ~init s] converts the Lwt-wrapped signal [s] into a
    regular signal with initial value [init]. *)
val to_signal : init:'a -> 'a React.S.t Lwt.t -> 'a React.S.t

(** Accessing shared values *)
module Value : Eliom_shared_sigs.VALUE

(** Shared implementation of React *)
module React : sig

  module S : sig

    include Eliom_shared_sigs.S

    (** [create ?default ?reset_default x] produces a pair [s, f],
        where [s] is a (shared) reactive signal, and [f] is a shared
        function for updating the signal.

        The initial value of the signal is [x], unless [default] is
        provided.  [default], if provided, is used as the client-side
        signal. [reset_default], if set to true (default: false),
        resets the value of [default] to [x].

        The behavior of [f] is undefined on the server side. On the
        client side, [f] behaves just like the standard React-provided
        update functions. *)
    val create :
      ?default :
        ('a t Eliom_lib.client_value *
         (?step:React.step -> 'a -> unit) Eliom_lib.client_value) ->
      ?reset_default:bool ->
      'a ->
      'a t * (?step:React.step -> 'a -> unit) Eliom_lib.shared_value

    (** If [synced s] is true, then the server-side and client-side
        values of [s] are equal. This means that the client-side code
        can initially rely on the server-provided value, and defer
        updates until the first client-side update of [s]. *)
    val synced : 'a t -> bool

  end

end

(** Shared implementation of ReactiveData *)
module ReactiveData : sig

  module RList : sig

    include Eliom_shared_sigs.RLIST
      with type 'a signal := 'a React.S.t

    (** If [synced l] is true, then the server-side and client-side
        values of [l] are equal. This means that the client-side code
        can initially rely on the server-provided value, and defer any
        updates until the first client-side update of [l]. *)
    val synced : 'a t -> bool

  end

end
