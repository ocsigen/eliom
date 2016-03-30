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

let escaped_value_escaped_value = fst

type +'a t = 'a Eliom_runtime.Client_value_server_repr.t

type 'a fragment = 'a t

let client_value_unwrapper =
  Eliom_wrap.create_unwrapper
    (Eliom_wrap.id_of_int Eliom_runtime.client_value_unwrap_id_int)

let create_client_value ?loc ~instance_id =
  Eliom_runtime.Client_value_server_repr.create
    ?loc ~instance_id ~unwrapper:client_value_unwrapper

let client_value_from_server_repr cv = cv

let client_value_datum ~closure_id ~args ~value = {
  Eliom_runtime.closure_id;
  args;
  value = Eliom_runtime.Client_value_server_repr.to_poly value
}

type +'a shared_value = {
  sh_server : 'a;
  sh_client : 'a t;
  sh_mark : 'a shared_value Eliom_wrap.wrapper
}

let internal_wrap (x : 'a shared_value) : 'a t = x.sh_client

let shared_value_mark () : 'a shared_value Eliom_wrap.wrapper =
  Eliom_wrap.create_wrapper internal_wrap

let create_shared_value
    (v : 'a) (c : 'a t) : 'a shared_value = {
  sh_server = v;
  sh_client = c;
  sh_mark = shared_value_mark ()
}

let shared_value_server_repr x = x.sh_server,x.sh_client

exception Client_value_creation_invalid_context of string

let escaped_value value :
  Eliom_runtime.escaped_value (* * Eliom_wrap.unwrapper *) =
  Ocsigen_lib.to_poly value
