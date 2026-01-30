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

(** {2 Client and shared values}

    See the {% <<a_manual chapter="clientserver-language"|manual>>
    %}. *)

type +'a t
(** Client values on the server are created by the syntax [{typ{ expr
    }}] in the server section (cf. {% <<a_manual
    chapter="clientserver-language" fragment="clientvalues"|the
    manual>> %}).  They are abstract, but become concrete once sent to
    the client. See also {% <<a_api subproject="client" text="the
    concrete representation on the client" |type
    Eliom_client_value.t>> %}. *)

exception Client_value_creation_invalid_context of string
(** Raised if a client value of the given closure ID is created at a
    point in time where it is neither global (i.e. during the
    initialization of the server program), nor request (i.e. during
    the processing of a request). *)

(**/**)

val create_client_value :
   loc:Eliom_lib.pos option
  -> instance_id:int
  -> _ Eliom_runtime.Client_value_server_repr.t

val client_value_from_server_repr :
   'a Eliom_runtime.Client_value_server_repr.t
  -> 'a t

val client_value_datum :
   closure_id:string
  -> args:Ocsigen_lib.poly
  -> value:'a t
  -> Eliom_runtime.client_value_datum

val escaped_value : 'a -> Eliom_runtime.escaped_value
(* * Eliom_wrap.unwrapper *)
