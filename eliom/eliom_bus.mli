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
(** The type of bus's carrying values of type ['a]. Bus's are values that can be
    easily shared among clients. Each of these clients along with the server can
    send a value on the bus. Values are handled by each of the participant using
    a custom handler. Note that no effort is put to order message receptions on
    the different participants. *)

val create :
     ?scope:Eliom_common.scope -> ?name:string
  -> ('a -> unit Lwt.t)
  -> 'a t
(** [create ?name handler] makes a fresh bus. The [name] optional parameter can
  * be used to make persistent (as in server restart persistent) bus's. Names
  * must be unique, and can't even be shared with a channel. The function
  * [handler] is associated to [b] on the server at creation. *)

val write : 'a t -> 'a -> unit
(** [write b x] sends the value [x] on the bus [b]. Every participant will
  * execute the handler it set for [b] automaticcaly. *)

val set_handler : 'a t -> ('a -> unit Lwt.t) -> unit

val wrap :
     'a t
  -> (  ('a Eliom_common_comet.buffered_chan_id)
     * (unit,
        'a,
        [ `Nonattached of [ `Post ] Eliom_services.na_s ],
        [ `WithoutSuffix ],
        unit,
        [ `One of 'a Eliom_parameters.caml ] Eliom_parameters.param_name,
        [ `Registrable ],
        Eliom_output.Action.return
       ) Eliom_services.service
    ) Eliom_client_types.data_key
(** [wrap b] wraps the bus [b] so that it can be transmitted to the client. *)
