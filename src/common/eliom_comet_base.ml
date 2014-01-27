(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010-2011
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

type 'a chan_id = string

external string_of_chan_id : 'a chan_id -> string = "%identity"
external chan_id_of_string : string -> 'a chan_id = "%identity"

type position =
  | Newest of int
  | After of int
  | Last of int option (* None means this is a 'newest channel' *)
deriving (Json)

type comet_stateless_request = (string*position) array
deriving (Json)

type command =
  | Register of string
  | Close of string
deriving (Json)

type comet_stateful_request =
  | Request_data of int
  | Commands of command array
deriving (Json)

type comet_request =
  | Stateless of comet_stateless_request
  | Stateful of comet_stateful_request
deriving (Json)

let comet_request_param =
  Eliom_parameter.ocaml "comet_request" Json.t<comet_request>

type 'a channel_data =
  | Data of 'a
  | Full
  | Closed
deriving (Json)

type answer =
  | Stateless_messages of (string * (string * int) channel_data) array
  | Stateful_messages of (string * string channel_data) array
  | Timeout
  | Process_closed
  | Comet_error of string
deriving (Json)

type comet_service =
    (unit, comet_request,
     Eliom_service.service_kind,
     [ `WithoutSuffix ], unit,
     [ `One of comet_request Eliom_parameter.ocaml ] Eliom_parameter.param_name,
     Eliom_service.registrable,
     Eliom_registration.http_service )
      Eliom_service.service

type internal_comet_service =
    (unit, comet_request,
     Eliom_service.internal_service_kind,
     [ `WithoutSuffix ], unit,
     [ `One of comet_request Eliom_parameter.ocaml ] Eliom_parameter.param_name,
     [ `Registrable ],
     Eliom_registration.http_service )
      Eliom_service.service

type stateless_kind =
  | After_kind of int
  | Newest_kind of int
  | Last_kind of int option

type 'a wrapped_channel =
  | Stateful_channel of (comet_service * 'a chan_id)
  | Stateless_channel of (comet_service * 'a chan_id * stateless_kind)


type 'a bus_send_service =
    (unit,
     'a list,
     [ `Nonattached of [ `Post ] Eliom_service.na_s ],
     [ `WithoutSuffix ],
     unit,
     [ `One of 'a list Eliom_parameter.ocaml ] Eliom_parameter.param_name,
     [ `Registrable ],
     Eliom_registration.http_service
    ) Eliom_service.service

type ('a, 'b) wrapped_bus = 'b wrapped_channel * 'a bus_send_service
