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

type 'a chan_id

val string_of_chan_id : 'a chan_id -> string
val chan_id_of_string : string -> 'a chan_id

type position =
  | Newest of int
  | After of int
  | Last of int option  (** None means 'newest channel' *)
[@@deriving json]

type comet_stateless_request = (string * position) array [@@deriving json]
type command = Register of string | Close of string [@@deriving json]

type comet_stateful_request = Request_data of int | Commands of command array
[@@deriving json]

type comet_request =
  | Stateless of comet_stateless_request
  | Stateful of comet_stateful_request
[@@deriving json]

val comet_request_param :
  ( comet_request
    , [`WithoutSuffix]
    , [`One of comet_request Parameter.ocaml] Parameter.param_name )
    Parameter.params_type

type 'a channel_data = Data of 'a | Full | Closed [@@deriving json]

type answer =
  | Stateless_messages of (string * (string * int) channel_data) array
  | Stateful_messages of (string * string channel_data) array
  | Timeout
  | State_closed
  | Comet_error of string
[@@deriving json]

type comet_service =
  | Comet_service :
      ( unit
        , bool * comet_request
        , Service.post
        , Service.att
        , _
        , _
        , _
        , [`WithoutSuffix]
        , unit
        , [`One of bool] Parameter.param_name
          * [`One of comet_request Parameter.ocaml] Parameter.param_name
        , Service.non_ocaml )
        Service.t
      * command list ref
      -> comet_service

type internal_comet_service =
  | Internal_comet_service :
      ( unit
        , bool * comet_request
        , Service.post
        , Service.att
        , _
        , Service.non_ext
        , Service.reg
        , [`WithoutSuffix]
        , unit
        , [`One of bool] Parameter.param_name
          * [`One of comet_request Parameter.ocaml] Parameter.param_name
        , Service.non_ocaml )
        Service.t
      * command list ref
      -> internal_comet_service

type stateless_kind =
  | After_kind of int
  | Newest_kind of int
  | Last_kind of int option

type 'a wrapped_channel =
  | Stateful_channel of (comet_service * 'a chan_id)
  | Stateless_channel of (comet_service * 'a chan_id * stateless_kind)

type 'a bus_send_service =
  | Bus_send_service :
      ( unit
        , 'a list
        , Service.post
        , Service.non_att
        , Service.co
        , Service.non_ext
        , Service.reg
        , [`WithoutSuffix]
        , unit
        , [`One of 'a list Parameter.ocaml] Parameter.param_name
        , Service.non_ocaml )
        Service.t
      -> 'a bus_send_service

type ('a, 'b) wrapped_bus = 'b wrapped_channel * 'a bus_send_service
