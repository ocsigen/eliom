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
exception Eliom_Link_too_old
exception Eliom_Session_expired
exception Eliom_Typing_Error of (string * exn) list

exception Eliom_duplicate_registering of string
exception Eliom_page_erasing of string
exception Eliom_function_forbidden_outside_site_loading
exception Eliom_there_are_unregistered_services of string
exception Eliom_error_while_loading_site of string

type internal_state = int

type tables
type cookiestable
type pages_tree = 
    tables (* global tables of continuations/naservices *)
      * cookiestable (* session tables *)
      * (string -> unit) ref (* remove_session_data *)

type sess_info =
    {si_other_get_params: (string * string) list;
     si_cookie: string option ref;
     si_persistent_cookie: (string * int64) option ref;
     si_nonatt_info: (string option * string option);
     si_state_info: (internal_state option * internal_state option);
     si_exn: exn list;
     si_config_file_charset: string option}

module Cookies : Hashtbl.S with type key = string

type 'a server_params1 = 
    request_info * sess_info * 
      (current_dir (* main directory of the site *) *
         ('a (* global table *) * 
            ('a * float option * float option option ref)
            Cookies.t (* cookies table *) * 
            (string -> unit) ref) * (* remove_session_data *)
         'a ref (* session table ref *) * 
         (float option option ref * float option ref *
            float option option ref * float option ref) 
         (* user timeout for this site (None -> see global config)
            and expiration date for the cookie (None -> browser) 
            then the same for persistent session
          *) *
         url_path (* suffix *))
      
      
type server_params = tables server_params1

type result_to_send = 
    EliomResult of Extensions.result
  | EliomExn of (exn list * cookieslist)

type page_table_key =
    {state: (internal_state option * internal_state option)}

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
            page_table_key *
              (int * int ref option *
                 (server_params -> result_to_send Lwt.t)) ->
                        unit

val add_naservice :
    tables -> 
      current_dir ->
	bool -> 
	  (string option * string option) -> 
            (int ref option *
	       (server_params -> result_to_send Lwt.t))
            -> unit


val get_state_param_name : string
val post_state_param_name : string
val eliom_suffix_name : string
val eliom_suffix_internal_name : string
val naservice_prefix : string
val naservice_name : string
val co_param_prefix : string
val na_co_param_prefix : string

val config : Simplexmlparser.xml list ref


val set_global_timeout : url_path -> float option -> unit
val find_global_timeout : url_path -> float option
val get_default_timeout : unit -> float option
val set_global_persistent_timeout : url_path -> float option -> unit
val find_global_persistent_timeout : url_path -> float option
val get_default_persistent_timeout : unit -> float option

val create_persistent_cookie : server_params -> (string * int64) Lwt.t
val create_cookie : server_params -> string
val remove_session_table : server_params -> string option -> unit
val remove_session_data : server_params -> string option -> unit
val create_table : unit -> 'a Cookies.t
val create_table_during_session : server_params -> 'a Cookies.t

(** Profiling *)
val number_of_sessions : server_params -> int
val number_of_tables : unit -> int
val number_of_table_elements : unit -> int list
val number_of_persistent_sessions : unit -> int Lwt.t



(** internal functions: *)
val end_current_hostdir : unit -> unit
val verify_all_registered : unit -> unit
val add_unregistered : string list option * int -> unit
val remove_unregistered : string list option * int -> unit
val global_register_allowed : unit -> 
  ((unit -> pages_tree * url_path) option)


