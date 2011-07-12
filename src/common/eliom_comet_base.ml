7(* Ocsigen
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

type postion =
  | Newest of int
  | After of int
deriving (Json)

type comet_stateless_request = (string*postion) list
deriving (Json)

type command =
  | Register of string
  | Close of string
deriving (Json)

type comet_statefull_request =
  | Request_data of int
  | Commands of command list
deriving (Json)

type comet_request =
  | Stateless of comet_stateless_request
  | Statefull of comet_statefull_request
deriving (Json)

let comet_request_param =
  Eliom_parameters.caml "comet_request" Json.t<comet_request>

type 'a channel_data =
  | Data of 'a
  | Full
deriving (Json)

type answer =
  | Stateless_messages of ( string * (string * int) channel_data ) list
  | Statefull_messages of ( string * string channel_data ) list
  | Timeout
  | Process_closed
  | Comet_error of string
deriving (Json)

type comet_service =
    (unit, comet_request,
     Eliom_services.internal_service_kind,
     [ `WithoutSuffix ], unit,
     [ `One of comet_request Eliom_parameters.caml ] Eliom_parameters.param_name, [ `Registrable ],
     Eliom_output.http_service )
      Eliom_services.service

type stateless_kind =
  | After_kind of int
  | Newest_kind of int

type 'a wrapped_channel =
  | Statefull_channel of (comet_service * 'a chan_id)
  | Stateless_channel of (comet_service * 'a chan_id * stateless_kind)


type 'a bus_send_service =
    (unit,
     'a list,
     [ `Nonattached of [ `Post ] Eliom_services.na_s ],
     [ `WithoutSuffix ],
     unit,
            [ `One of 'a list Eliom_parameters.caml ] Eliom_parameters.param_name,
     [ `Registrable ],
     Eliom_output.http_service
    ) Eliom_services.service

type 'a wrapped_bus =
    ( 'a wrapped_channel )
    * ( 'a bus_send_service )
