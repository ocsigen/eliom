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
exception Eliom_Session_expired of (string list * string list)
    (** The cookie does not exist any more.
       The string lists are the list of names of expired sessions
       (persistent sessions for the first one,
       in memory sessions for the second list).
     *)
exception Eliom_Typing_Error of (string * exn) list
    (** The service (GET or POST) parameters do not match expected type *)


exception Eliom_function_forbidden_outside_site_loading of string (** That function cannot be used like that outside the initialisation phase. For some functions, you must add the [~sp] parameter during a session. *)

(**/**)
exception Eliom_duplicate_registration of string (** The service has been registered twice*)
exception Eliom_page_erasing of string (** The location where you want to register something already exists *)
exception Eliom_there_are_unregistered_services of string list (** Some services have not been registered *)
exception Eliom_error_while_loading_site of string


type internal_state = string

type anon_params_type = int

type tables

type 'a cookiestable
type pages_tree = 
    tables (* global tables of continuations/naservices *)
      * tables cookiestable (* session tables *)
      * ((string -> unit) ref (* remove_session_data *) *
           (string -> bool) ref (* not_bound_in_tables *))

type sess_info =
    {si_other_get_params: (string * string) list;
     si_all_get_params: (string * string) list;
     si_all_post_params: (string * string) list;

     si_session_cookies: (string (* cookie name (or site dir) *) * 
                    string (* value *)) list;
     (* the session cookies sent by the request *)

     si_persistent_session_cookies: (string (* cookie name (or site dir) *) *
                               string (* value *)) list;
     (* the persistent session cookies sent by the request *)

     si_nonatt_info: (string option * string option);
     si_state_info: (internal_state option * internal_state option);
     si_exn: exn list;
     si_config_file_charset: string option}


module Cookies : Hashtbl.S with type key = string



type 'a one_cookie_info =
    (* in memory sessions: *)
    (string                   (* current value *) *
     'a ref                   (* service session table
                                 ref towards cookie table
                               *) * 
     float option option ref  (* user timeout - 
                                 None = see global config
                                 Some None = no timeout
                                 ref towards cookie table
                               *) * 
     float option ref         (* expiration date ref (server side) - 
                                 None = never
                                 ref towards cookie table
                               *) * 
     float option option ref  (* cookie expiration date to set
                                 None = nothing to set
                                 Some None = set expiration = browser close
                                 Some Some = send expiration date
                               *)
    )

type one_persistent_cookie_info =
     (string                   (* current value *) *
      int64                    (* key (here to ensure concistancy of tables
                                  and privacy of data (reuse of old cookie).
                                  Actually not needed 
                                  with the cookie we use). *) *
      float option option ref  (* user timeout - 
                                  None = see global config
                                  Some None = no timeout
                                *) * 
      float option option ref  (* cookie expiration date to set
                                  None = nothing to set
                                  Some None = set expiration = browser close
                                  Some Some = send expiration date
                                *)

     )


type 'a cookie_info =
    (* in memory sessions: *)
    (string                    (* cookie name *) 
       * 

     (string option            (* value sent by the browser *)
                               (* None = new cookie 
                                   (not sent by the browser) *)
        *

      'a one_cookie_info option ref)
       (* None = the cookie has been removed in the table.
          Ask the browser to remove the cookie *)
    )
      list ref *
      
      (* persistent sessions: *)
    (string                    (* cookie name *) 
       *

     ((string                  (* value sent by the browser *) *
       float option option     (* timeout at the beginning of the request *) *
       float option            (* (server side) expdate 
                                  at the beginning of the request
                                  None = no exp *))
        option
                               (* None = new cookie 
                                   (not sent by the browser) *)
       *

       one_persistent_cookie_info option ref)
       (* None = the cookie has been removed in the table.
          Ask the browser to remove the cookie *)
       
    )
      list ref





type 'a server_params1 = 
    {sp_ri:request_info;
     sp_si:sess_info;
     sp_site_dir:url_path (* main directory of the site *);
     sp_site_dir_string:string (* the same, but string *);
     sp_global_table:'a (* global table *);
     sp_cookie_table: 'a cookiestable (* cookies table for volatile sessions *);
     sp_remove_sess_data:(string -> unit) ref (* remove_session_data *);
     sp_are_empty_tables:(string -> bool) ref (* are_empty_session_tables *);
     sp_cookie_info:'a cookie_info;
     sp_suffix:url_path (* suffix *);
     sp_fullsessname:string option (* the name of the session to which belong the service that answered (if it is a session service) *)}

      



      
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
      url_path ->
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


val set_global_timeout :
    session_name:string option ->
    recompute_expdates:bool ->
    Extensions.url_path ->
    (Cookies.key -> unit) ->
    tables cookiestable ->
    float option -> unit Lwt.t

val get_global_timeout : session_name:string option -> url_path -> float option

val set_global_persistent_timeout :
    session_name:string option ->
    recompute_expdates:bool ->
    Extensions.url_path -> float option -> unit Lwt.t

val get_global_persistent_timeout : session_name:string option ->
  url_path -> float option

val get_default_timeout : unit -> float option

val set_default_timeout : float option -> unit

val get_default_persistent_timeout : unit -> float option

val set_default_persistent_timeout : float option -> unit


val create_table : unit -> 'a Cookies.t
val create_table_during_session : server_params -> 'a Cookies.t
val create_persistent_table : string -> 'a Ocsipersist.table
val remove_from_all_persistent_tables : string -> unit Lwt.t

val set_site_handler : url_path -> 
  (server_params -> exn -> result_to_send Lwt.t) -> unit

val find_or_create_cookie : 
    ?session_name:string -> sp:server_params -> unit -> tables one_cookie_info

val find_cookie_only : 
    ?session_name:string -> sp:server_params -> unit -> tables one_cookie_info

val find_or_create_persistent_cookie : 
    ?session_name:string -> sp:server_params -> unit 
      -> one_persistent_cookie_info Lwt.t

val find_persistent_cookie_only : 
    ?session_name:string -> sp:server_params -> unit -> one_persistent_cookie_info



val close_persistent_session :
    ?session_name:string -> sp:server_params -> unit -> unit Lwt.t

val close_volatile_session :
    ?session_name:string -> sp:server_params -> unit -> unit

val close_all_persistent_sessions :
    ?session_name:string -> url_path -> unit Lwt.t

val close_all_volatile_sessions :
    ?session_name:string ->
      (string -> unit) ->
        tables cookiestable ->
          url_path -> unit Lwt.t



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


