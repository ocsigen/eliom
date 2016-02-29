(* Ocsigen
 * Copyright (C) 2005-2008 Vincent Balat, Stéphane Glondu
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

include (Eliom_client_common_base :
           module type of Eliom_client_common_base
         with type 'a Client_value_server_repr.t =
                        'a Eliom_client_common_base.Client_value_server_repr.t
         with type client_value_datum = Eliom_client_common_base.client_value_datum
         with type injection_datum := Eliom_client_common_base.injection_datum
         with type compilation_unit_global_data =
           Eliom_client_common_base.compilation_unit_global_data
         with type global_data := Eliom_client_common_base.global_data
         with type request_data = Eliom_client_common_base.request_data)

exception False

exception Exception_on_server of string

type 'a client_value = 'a
type 'a shared_value = 'a

let create_shared_value (_ : 'a) (c : 'a client_value) = c

(*****************************************************************************)
type injection_datum = Eliom_client_common_base.injection_datum

type global_data2
