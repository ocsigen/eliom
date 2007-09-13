(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomsessions.mli
 * Copyright (C) 2007 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(** This module contains the functions you need to get (or set)
   information about the request or the session.
 *)

open Extensions





(*****************************************************************************)
(** {2 Getting information about the request} *)

(** returns the name of the user agent that did the request
   (usually the name of the browser). *)
val get_user_agent : sp:Eliommod.server_params -> string

(** returns the full URL as a string *)
val get_full_url : sp:Eliommod.server_params -> string

(** returns the internet address of the client as a string *)
val get_ip : sp:Eliommod.server_params -> string

(** returns the internet address of the client, 
   using the type [Unix.inet_addr] (defined in OCaml's standard library). *)
val get_inet_addr : sp:Eliommod.server_params -> Unix.inet_addr

(** returns the path of the URL as a string *)
val get_current_path_string : sp:Eliommod.server_params -> string

(** returns the path of the URL using the type {!Extensions.url_path} *)
val get_current_path : sp:Eliommod.server_params -> url_path

(** returns the hostname that has been sent by the user agent, if any.
   This is usefull if your server has several hostnames, but that
   piece of information is not mandatory for HTTP/1.0.
 *)
val get_hostname : sp:Eliommod.server_params -> string option

(** returns the port on which the request has been done. *)
val get_port : sp:Eliommod.server_params -> int

(** returns the suffix of the current URL *)
val get_suffix : sp:Eliommod.server_params -> url_path

(** returns the cookies sent by the browser *)
val get_cookies : sp:Eliommod.server_params -> (string * string) list



(*****************************************************************************)
(** {2  Getting and setting information about the session} *)

(** {3 Session timeouts} *)

(** sets the timeout for volatile session (server side) for one user, 
   in seconds. *)
val set_user_timeout : sp:Eliommod.server_params -> float option -> unit

(** remove the (volatile) session timeout for one user
   (and turn back to the default). *)
val unset_user_timeout : sp:Eliommod.server_params -> unit

(** returns the timeout for (volatile) session for one user. *)
val get_user_timeout : sp:Eliommod.server_params -> float option


(** sets the timeout for persistent session (server side) for one user,
   in seconds. *)
val set_user_persistent_timeout : sp:Eliommod.server_params -> float option -> unit

(** remove the persistent session timeout for one user
   (and turn back to the default). *)
val unset_user_persistent_timeout : sp:Eliommod.server_params -> unit

(** returns the persistent session timeout for one user. *)
val get_user_persistent_timeout : sp:Eliommod.server_params -> float option


(** sets the timeout for sessions (server side). 
    The sessions will be closed after this amount of time of inactivity 
    from the user. 
    {e Warning: If you use this function after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliommod.Eliom_function_forbidden_outside_site_loading}.}
*)
val set_global_timeout : ?sp:Eliommod.server_params -> float option -> unit

(** returns the timeout for sessions (server side).
    {e Warning: If you use this function after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliommod.Eliom_function_forbidden_outside_site_loading}.}   
 *)
val get_global_timeout : ?sp:Eliommod.server_params -> unit -> float option

(** returns the default timeout for sessions (server side). *)
val get_default_timeout : unit -> float option

(** sets the timeout for persistent sessions (server side). 
    The sessions will be closed after this amount of time of inactivity 
    from the user. 
    {e Warning: If you use this function after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliommod.Eliom_function_forbidden_outside_site_loading}.}
*)
val set_global_persistent_timeout : ?sp:Eliommod.server_params -> float option -> unit

(** returns the timeout for persistent sessions (server side). 
    {e Warning: If you use this function after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliommod.Eliom_function_forbidden_outside_site_loading}.}
 *)
val get_global_persistent_timeout : ?sp:Eliommod.server_params -> unit -> float option

(** returns the default timeout for persistent sessions (server side). *)
val get_default_persistent_timeout : unit -> float option


(** {3 Cookies expiration} *)

(** sets the cookie expiration date for the session. 
   [None] means the cookie will expire when the browser is closed. 
 *)
val set_user_expdate : sp:Eliommod.server_params -> float option -> unit

(** returns the cookie expiration date for the session. *)
val get_user_expdate : sp:Eliommod.server_params -> float option


(** sets the cookie expiration date for the persistent session. 
   [None] means the cookie will expire when the browser is closed. 
 *)
val set_user_persistent_expdate : sp:Eliommod.server_params -> float option -> unit

(** returns the cookie expiration date for the persistent session. *)
val get_user_persistent_expdate : sp:Eliommod.server_params -> float option



(*****************************************************************************)
(** {2 Getting information about files uploaded} *)

(** Warning: The files uploaded are automatically erased by Ocsigen 
   just after the request has been fulfilled.
   If you want to keep them, create a new hard link yourself during
   the service (or make a copy).
 *)

(** returns the filename used by Ocsigen for the uploaded file. *)
val get_tmp_filename : file_info -> string

(** returns the size of the file. *)
val get_filesize : file_info -> int64

(** returns the name the file had on the client when it has been sent. *)
val get_original_filename : file_info -> string



(*****************************************************************************)
(** {2 Using your own error pages} *)


(** allows to use your own error pages
   (404, or any exception during page generation).
    {e Warning: If you use this function after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliommod.Eliom_function_forbidden_outside_site_loading}.}
 *)
val set_exn_handler : 
    ?sp:Eliommod.server_params ->
      (Eliommod.server_params -> exn -> Eliommod.result_to_send Lwt.t) -> unit

(** returns the exceptions that have been sent by the previous services
   to their fallback, if any. Keep an eye on these exception to know what
   succeeded before that service was called (failed connection, timeout ...)
 *)
val get_exn : sp:Eliommod.server_params -> exn list




(*****************************************************************************)
(** {2 Getting information from the configuration file} *)

(** returns the information of the configuration file concerning that site
   (between [<site>] and [</site>]).
   {e Warning: You must call that function during the initialisation of
   your module (not during a Lwt thread or a service).
   If you use that function after, 
   you must give the [~sp] parameter, otherwise it will raise the exception
   {!Eliommod.Eliom_function_forbidden_outside_site_loading}.}
 *)
val get_config : unit -> Simplexmlparser.xml list

(** returns the root of the site. *)
val get_working_dir : sp:Eliommod.server_params -> url_path

(** returns the charset for this site (from the configuration file) *)
val get_config_file_charset : sp:Eliommod.server_params -> string option



(*****************************************************************************)
(** {2 Session data} *)

(** {3 Session data in memory} *)

(** The type of (volatile) session data tables. *)
type 'a table

(** creates a table in memory where you can store the session data for 
   all users. 
   {e Warning: If you use that function after the initialization phase, 
   you must give the [~sp] parameter, otherwise it will raise the exception
   {!Eliommod.Eliom_function_forbidden_outside_site_loading}.}
 *)
val create_table : ?sp:Eliommod.server_params -> unit -> 'a table

(** gets session data for the current session (if any). *)
val get_session_data : 
    table:'a table -> sp:Eliommod.server_params -> 'a option

(** sets session data for the current session. *)
val set_session_data : 
    table:'a table -> sp:Eliommod.server_params -> value:'a -> unit

(** removes session data for the current session 
   (but does not close the session). *)
val remove_session_data : 
    table:'a table -> sp:Eliommod.server_params -> unit


(** {3 Persistent sessions} *)

(** The type of persistent session data tables. *)
type 'a persistent_table

(** creates a table on hard disk where you can store the session data for 
   all users. It uses {!Ocsipersist}. *)
val create_persistent_table : string -> 'a persistent_table

(** gets persistent session data for the current persistent session (if any) *)
val get_persistent_data : 
    table:'a persistent_table -> sp:Eliommod.server_params -> 'a option Lwt.t

(** sets persistent session data for the current persistent session *)
val set_persistent_data : 
    table:'a persistent_table -> sp:Eliommod.server_params -> value:'a -> unit Lwt.t

(** removes session data for the current persistent session 
   (but does not close the session). *)
val remove_persistent_data : 
    table:'a persistent_table -> sp:Eliommod.server_params -> unit Lwt.t


(** {3 Closing sessions} *)

(** Close both volatile and persistent sessions *)
val close_session : sp:Eliommod.server_params -> unit Lwt.t

(** Close the persistent session
   (destroying all persistent data for that user) *)
val close_persistent_session : sp:Eliommod.server_params -> unit Lwt.t

(** Close Eliom's volatile session
   (destroying all session data for that user) *)
val close_volatile_session : sp:Eliommod.server_params -> unit


(*****************************************************************************)
(** {2 Getting parameters (low level)} *)

(** The usual way to get parameters with Eliom is to use the second
   and third parameters of the service handlers.
   These are low level functions you may need for more advanced use.
 *)

(** returns the parameters of the URL (GET parameters) 
   that concern the running service. 
   For example in the case of a non-attached coservice called from
   a page with GET parameters, only the parameters of that non-attached
   coservice are returned (even if the other are still in the URL).
 *)
val get_get_params : sp:Eliommod.server_params -> (string * string) list

(** returns all parameters of the URL (GET parameters)
   (even those that are for another service) *)
val get_all_get_params : sp:Eliommod.server_params -> (string * string) list

(** returns the parameters of the URL (GET parameters) 
   that do not concern the running service. *)
val get_other_get_params : sp:Eliommod.server_params -> (string * string) list

(** returns the parameters in the body of the HTTP request (POST parameters)
   that concern the running service *)
val get_post_params : sp:Eliommod.server_params -> (string * string) list Lwt.t

(** returns all parameters in the body of the HTTP request (POST parameters)
   (even those that are for another service) *)
val get_all_post_params : sp:Eliommod.server_params -> (string * string) list


(*****************************************************************************)
(** {2 Other low level functions} *)

(** You probably don't need these functions. *)

(** returns all the information about the request. *)
val get_ri : sp:Eliommod.server_params -> request_info

(** returns the value of the cookie for persistent session. *)
val get_persistent_cookie : sp:Eliommod.server_params -> (string * int64) option

(** returns the value of the cookie for non persistent session. *)
val get_cookie : sp:Eliommod.server_params -> string option





(**/**)
(*****************************************************************************)
val number_of_sessions : sp:Eliommod.server_params -> int

val number_of_tables : unit -> int

val number_of_table_elements : unit -> int list

val number_of_persistent_sessions : unit -> int Lwt.t

val number_of_persistent_tables : unit -> int

val number_of_persistent_table_elements : unit -> (string * int) list Lwt.t
(* Because of Dbm implementation, the result may be less thann the expected
   result in some case (with a version of ocsipersist based on Dbm) *)


val get_global_tables : sp:Eliommod.server_params -> Eliommod.tables
val get_session_tables : sp:Eliommod.server_params -> Eliommod.tables ref
