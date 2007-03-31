(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod.mli
 * Copyright (C) 2005 Vincent Balat
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

open Extensions

exception Eliom_Wrong_parameter
exception Eliom_duplicate_registering of string
exception Eliom_register_for_session_outside_session
exception Eliom_page_erasing of string
exception Eliom_service_created_outside_site_loading
exception Eliom_there_are_unregistered_services of string
exception Eliom_error_while_loading_site of string
exception Eliom_Typing_Error of (string * exn) list

type internal_state = int

type tables
type cookiestable
type pages_tree =
    tables (* global tables of services *)
      * cookiestable (* session tables *)

type 'a server_params1 = 
    request_info * (current_dir * 'a ref * (string * string) list * url_path)
type server_params = tables server_params1

type page_table_key =
    {suffix:bool;
     state: (internal_state option * internal_state option)}

val gen :
    pages_tree ->
      string option -> 
        request_info -> answer Lwt.t

val empty_tables : unit -> tables

val add_service :
    tables ->
      url_path ->
        bool ->
          string list ->
            Predefined_senders.create_sender_type option ->
              page_table_key *
                (int * (server_params -> 
                  Predefined_senders.send_page_type Lwt.t)) ->
                    unit

val add_anservice :
    tables -> 
      current_dir ->
	bool -> 
	  (string option * string option) -> 
            Predefined_senders.create_sender_type option ->
	      (server_params -> 
		Predefined_senders.send_page_type Lwt.t) -> unit


val get_state_param_name : string
val post_state_param_name : string
val eliom_suffix_name : string
val anservice_prefix : string
val anservice_name : string
val co_param_prefix : string
val na_co_param_prefix : string

val config : 
    Simplexmlparser.ExprOrPatt.texprpatt Simplexmlparser.ExprOrPatt.tlist ref

(** Profiling *)
(* val number_of_sessions : unit -> int *)


(** internal functions: *)
val get_current_hostdir : unit -> pages_tree * url_path
val end_current_hostdir : unit -> unit
val verify_all_registered : unit -> unit
val add_unregistered : string list option * int -> unit
val remove_unregistered : string list option * int -> unit
val global_register_allowed : unit -> bool

