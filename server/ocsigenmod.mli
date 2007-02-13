(* Ocsigen
 * http://www.ocsigen.org
 * Module pagesearch.mli
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

exception Ocsigen_Wrong_parameter
exception Ocsigen_duplicate_registering of string
exception Ocsigen_register_for_session_outside_session
exception Ocsigen_page_erasing of string
exception Ocsigen_service_or_action_created_outside_site_loading
exception Ocsigen_there_are_unregistered_services of string
exception Ocsigen_error_while_loading_site of string
exception Ocsigen_Typing_Error of (string * exn) list

type internal_state = int

type tables
type cookiestable
type pages_tree =
    tables (* global tables of continuations/actions *)
      * cookiestable (* session tables *)

type 'a server_params1 = 
    request_info * current_dir * 'a ref
type 'a server_params2 = url_path * 'a server_params1
type server_params = tables server_params1

type page_table_key =
    {prefix:bool;
     state: internal_state option}

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
            Predefined_senders.create_sender_type ->
              page_table_key *
                (int * (tables server_params2 -> 
                  Predefined_senders.send_page_type Lwt.t)) ->
                    unit

val add_action :
    tables -> current_dir
      -> string -> (tables server_params1 -> unit Lwt.t) -> unit

val state_param_name : string
val ocsigen_suffix_name : string
val action_prefix : string
val action_name : string
val action_reload : string

val config : 
    Simplexmlparser.ExprOrPatt.texprpatt Simplexmlparser.ExprOrPatt.tlist ref

(** Profiling *)
(* val number_of_sessions : unit -> int *)


(** internal functions: *)
val get_current_hostdir : unit -> pages_tree * url_path
val end_current_hostdir : unit -> unit
val verify_all_registered : unit -> unit
val add_unregistered : string list * int -> unit
val remove_unregistered : string list * int -> unit
val global_register_allowed : unit -> bool

