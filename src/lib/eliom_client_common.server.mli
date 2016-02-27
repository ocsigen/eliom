(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) CNRS Univ Paris Diderot
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

include module type of Eliom_client_common_base
  with type escaped_value = Eliom_lib_base.escaped_value
  with type +'a Client_value_server_repr.t = 'a Eliom_lib_base.Client_value_server_repr.t
  with type client_value_datum = Eliom_lib_base.client_value_datum
  with type injection_datum = Eliom_lib_base.injection_datum
  with type compilation_unit_global_data = Eliom_lib_base.compilation_unit_global_data
  with type global_data := Eliom_lib_base.global_data
  with type request_data = Eliom_lib_base.request_data

(** {2 Client and shared values}

    See the {% <<a_manual chapter="clientserver-language"|manual>> %}. *)

(** Client values on the server are created by the syntax [{typ{ expr }}]
    in the server section (cf. {% <<a_manual chapter="clientserver-language"
    fragment="clientvalues"|the manual>> %}).  They are abstract, but
    become concrete once sent to the client. See also {% <<a_api
    subproject="client" text="the concrete representation on the client"
    |type Eliom_client_common.client_value>> %}. *)
type +'a client_value
type +'a shared_value

(** Raised if a client value of the given closure ID is created at a
    point in time where it is neither global (i.e. during the
    initialization of the server program), nor request (i.e. during
    the processing of a request).
*)
exception Client_value_creation_invalid_context of string


(**/**)

val create_client_value :
  ?loc:pos -> instance_id:int -> _ Client_value_server_repr.t
val client_value_from_server_repr :
  'a Client_value_server_repr.t -> 'a client_value
val client_value_datum :
  closure_id:string -> args:poly -> value:'a client_value -> client_value_datum
val create_shared_value : 'a -> 'a client_value -> 'a shared_value
val shared_value_server_repr : 'a shared_value -> 'a * 'a client_value

val escaped_value : 'a -> escaped_value (* * Eliom_wrap.unwrapper *)

val string_escape : string -> string

type global_data = Eliom_lib_base.global_data * Eliom_wrap.unwrapper

val global_data_unwrapper : Eliom_wrap.unwrapper

(**/**)
