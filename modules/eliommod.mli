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

(** This module contains low level functions for Eliom (undocumented) and
   not available for the user. It also defines some exceptions you may
   want to catch.
 *)

open Extensions


exception Eliom_Wrong_parameter (** Service called with wrong parameter names *)
exception Eliom_Link_too_old (** The coservice does not exist any more *)
exception Eliom_Session_expired (** The cookie does not exist any more *)
exception Eliom_Typing_Error of (string * exn) list
    (** The service (GET or POST) parameters do not match expected type *)


exception Eliom_function_forbidden_outside_site_loading of string (** That function cannot be used like that outside the initialisation phase. For some functions, you must add the [~sp] parameter during a session. *)

(**/**)
exception Eliom_duplicate_registration of string (** The service has been registered twice*)
exception Eliom_page_erasing of string (** The location where you want to register something already exists *)
exception Eliom_there_are_unregistered_services of string (** Some services have not been registered *)
exception Eliom_error_while_loading_site of string


type internal_state = string

type anon_params_type = int

type tables

type cookiestable
type pages_tree = 
    tables (* global tables of continuations/naservices *)
      * cookiestable (* session tables *)
      * ((string -> unit) ref (* remove_session_data *) *
           (string -> bool) ref (* not_bound_in_tables *))

type sess_info =
    {si_other_get_params: (string * string) list;
     si_all_get_params: (string * string) list;
     si_all_post_params: (string * string) list;
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
            ('a * string list * float option * float option option ref)
            Cookies.t (* cookies table *) * 
            ((string -> unit) ref * (* remove_session_data *)
             (string -> bool) ref)) * (* are_empty_session_tables *)
         'a ref (* session table ref *) * 
         (float option option ref * float option ref *
            float option option ref * float option ref) 
         (* user timeout for this site (None -> see global config)
            and expiration date for the cookie (None -> browser) 
            then the same for persistent session
          *) *
         url_path (* suffix *))


      
(**/**)
(** The type to send if you want to create your own modules for generating
   pages
 *)
type result_to_send = 
  | EliomResult of Extensions.result
  | EliomExn of (exn list * cookieslist)

(** Type of server parameters. This is the type of the first parameter of
   service handlers. It is abstract but you can get a lot of information 
   about the session or the request by using the functions defined in
   this module.
 *)
type server_params = tables server_params1
(**/**)

type page_table_key =
    {key_state: (internal_state option * internal_state option);
     key_kind: Http_frame.Http_header.http_method}


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
              ((anon_params_type * anon_params_type) * 
                 int ref option *
                 (float * float ref) option *
                 (server_params -> result_to_send Lwt.t)) ->
                        unit

val add_naservice :
    tables -> 
      current_dir ->
	bool -> 
	  (string option * string option) -> 
            (int ref option *
               (float * float ref) option *
	       (server_params -> result_to_send Lwt.t))
            -> unit


val get_state_param_name : string
val post_state_param_name : string
val eliom_suffix_name : string
val eliom_suffix_internal_name : string
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
val remove_session : server_params -> unit
val create_table : unit -> 'a Cookies.t
val create_table_during_session : server_params -> 'a Cookies.t
val create_persistent_table : string -> 'a Ocsipersist.table
val remove_from_all_persistent_tables : string -> unit Lwt.t

val set_site_handler : url_path -> 
  (server_params -> exn -> result_to_send Lwt.t) -> unit

(** Profiling *)
val number_of_sessions : sp:server_params -> int
val number_of_tables : unit -> int
val number_of_table_elements : unit -> int list
val number_of_persistent_sessions : unit -> int Lwt.t
val number_of_persistent_tables : unit -> int
(** Number of persistent tables opened *)
val number_of_persistent_table_elements : unit -> (string * int) list Lwt.t
(** Whole number of elements in all persistent tables, table by table *)



(** internal functions: *)
val end_current_hostdir : unit -> unit
val verify_all_registered : unit -> unit
val add_unregistered : string list option -> unit
val remove_unregistered : string list option -> unit
val global_register_allowed : unit -> 
  ((unit -> pages_tree * url_path) option)


