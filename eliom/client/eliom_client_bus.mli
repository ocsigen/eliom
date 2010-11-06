(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010
 * Raphaël Proust
 * Laboratoire PPS - CNRS Université Paris Diderot
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

type 'a t
(** The type of bus's with values of type ['a]. *)

val write : 'a t -> 'a -> unit Lwt.t
(** [write b x] sends the message [x] on the bus [b]. *)

val set_handler : 'a t -> ('a -> unit Lwt.t) -> unit
(** [set_handler b handler] sets the handler for bus [b]. Following messages on
    the bus [b] will be treated with function [handler]. *)

val unwrap :
    (  ('a Eliom_common_comet.buffered_chan_id)
     * (unit,
        'a,
        [< Eliom_services.service_kind ],
        [< `WithSuffix | `WithoutSuffix ],
        'b,
        'c,
        [< Eliom_services.registrable ],
        'd) Eliom_services.service
    ) Eliom_client_types.data_key
  -> 'a t

