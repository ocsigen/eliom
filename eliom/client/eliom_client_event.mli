(* Ocsigen
 * http://www.ocsigen.org
 * Module server.ml
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


(** This module is the client part of the shared event scheme used in Eliom. It
    is to be used with [Eliom_event] as it defines an unwrapping function for
    each possible wrap in the dual module. *)


module Down :
sig

  val unwrap :
       'a Eliom_common_comet.buffered_chan_id Eliom_client_types.data_key
    -> 'a React.E.t

end

module Up :
sig

  val unwrap :
    (* WAS (but didn't type) :
    (unit,
     'a,
     [ `Nonattached of [ `Post ] Eliom_services.na_s ],
     [ `WithoutSuffix ],
     unit,
     [`One of 'a] Eliom_parameters.param_name,
     [ `Registrable ], Eliom_services.http)
       Eliom_services.service
     *)
     sp:Eliom_client_types.server_params
  -> (unit,
     'a,
     [< Eliom_services.service_kind ],
     [< `WithSuffix | `WithoutSuffix ],
     'b,
     'c,
     [< Eliom_services.registrable ],
     'd)
        Eliom_services.service Eliom_client_types.data_key
  -> ('a -> string Lwt.t)

end
