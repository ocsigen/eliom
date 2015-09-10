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

    Client-side signals and data are type-wise and behavior-wise
    equivalent to those provided by the underlying React and
    ReactiveData libraries. Thus, all the operations from React and
    ReactiveData apply. We provide extended versions of these
    libraries. *)

(** [to_signal ~init s] converts the Lwt-wrapped signal [s] into a
    regular signal with initial value [init]. *)
val to_signal : init:'a -> 'a React.S.t Lwt.t -> 'a React.S.t

(** Accessing shared values *)
module Value : Eliom_shared_sigs.VALUE

(** Shared implementation of React; client-side behavior is like
    standard React *)
module React : sig

  module S : sig

    include module type of React.S

    include Eliom_shared_sigs.S with type 'a t := 'a t

    (** [create ?eq ?default ?reset_default x] produces a pair [s, f],
        where [s] is a reactive signal, and [f] is a function for
        updating the signal.

        The initial value of the signal is [x], unless [default] is
        provided.  [default], if provided, is used as the
        signal. [reset_default], if set to true (default: false),
        resets the value of [default] to [x]. *)
    val create :
      ?eq:('a -> 'a -> bool) ->
      ?default:(('a t * (?step:React.step -> 'a -> unit)) option) ->
      ?reset_default:bool ->
      'a -> 'a React.signal * (?step:React.step -> 'a -> unit)

  end

  module E : module type of React.E

end

(** This is a dummy ReactiveData module that allows us to refer to
    client-side ReactiveData types on the server side, without
    actually linking against ReactiveData. *)
module FakeReactiveData : sig
  module RList : sig
    type 'a t = 'a ReactiveData.RList.t
    type 'a handle = 'a ReactiveData.RList.handle
  end
end

(** Shared implementation of ReactiveData; client-side behavior is
    like standard ReactiveData *)
module ReactiveData : sig

  module RList : sig

    include module type of ReactiveData.RList
    with type 'a t = 'a ReactiveData.RList.t
     and type 'a handle = 'a ReactiveData.RList.handle

    include Eliom_shared_sigs.RLIST
      with type 'a t := 'a t
       and type 'a handle := 'a handle
       and type 'a signal := 'a React.S.t
       and type 'a ct := 'a ReactiveData.RList.t
       and type 'a chandle := 'a ReactiveData.RList.handle

  end

end
