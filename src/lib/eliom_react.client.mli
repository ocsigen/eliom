(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2011
 * Raphaël Proust
 * Pierre Chambart
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

(** Client side type declarations for React event propagation. This
    module must be linked for events to work properly. *)

module Down : sig
  type 'a t = 'a React.E.t

  val set_handle_react_exn_function : (?exn:exn -> unit -> unit) -> unit
  (** Makes possible to customize the function called when comet fails
      in Eliom_react, for example because a channel is full or closed.
      It is called for each exception.
  *)
end

module Up : sig
  type 'a t = 'a -> unit
end

module S : sig
  module Down : sig
    type 'a t = 'a React.S.t
  end
end

(**/**)

val force_link : unit
val section : Logs.src
