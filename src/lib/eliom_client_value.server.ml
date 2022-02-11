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

type +'a t = 'a Eliom_runtime.Client_value_server_repr.t

let client_value_unwrapper =
  Eliom_wrap.create_unwrapper
    (Eliom_wrap.id_of_int Eliom_runtime.client_value_unwrap_id_int)

let create_client_value ?loc ~instance_id =
  Eliom_runtime.Client_value_server_repr.create ?loc ~instance_id
    ~unwrapper:client_value_unwrapper

let client_value_from_server_repr cv = cv

let client_value_datum ~closure_id ~args ~value =
  { Eliom_runtime.closure_id
  ; args
  ; value = Eliom_runtime.Client_value_server_repr.to_poly value }

exception Client_value_creation_invalid_context of string

let escaped_value value
    : Eliom_runtime.escaped_value (* * Eliom_wrap.unwrapper *)
  =
  Ocsigen_lib.to_poly value
