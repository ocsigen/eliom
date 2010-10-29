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

open Ocsigen_extensions


(** There are three kinds of sessions, all using different cookies:
   - service sessions (used to register services in a table of session),
   - volatile data sessions (used to save session data in tables in memory),
   - persistent sessions (used to save session data on hard disk).

   For all these sessions, you may set a timeout (global or individual for one
   user) or set an expiration date for the cookie.
   "Volatile" denotes both service and in memory data sessions.

   Be very carefull if you use several sessions concurrently, as they may have
   different duration (one may be closed while the other are not).
   Duration of service sessions is sometimes shorter than
   volatile data sessions, which is usually shorter than
   persistent sessions.

   If you want several sessions of the same type for one site,
   you can choose a personalized session name by giving the optional
   parameter [?state_name].

   It is highly recommended to put all the sessions for one user in one
   {e session group}. Thus, it will be possible to implement features
   like "close all opened sessions" for one user, or limitation of
   the number of sessions one user can open concurrently, or setting
   data for one group of sessions.

   The default duration of session may be set in Ocsigen's configuration file,
   as options for the Eliom module. Each Eliom site can override these options
   using functions of this module.

   Setting sessions timeout in the configuration file. Example:
    [
      <extension findlib-package="ocsigen.ext.eliom">
        <datatimeout value="3600"> (* in memory data sessions *)
        <persistenttimeout value="infinity"> (* persistent session data *)
        <servicetimeout value="3600"> (* session services *)
        <volatiletimeout value="3600"> (* both session services and
                                          in memory data sessions *)
      </extension>
    ]


  *)







(*****************************************************************************)
(** {2  Getting and setting information about the current session} *)

(** {3 State status} *)

(** The following function return the current state of the state for a given
    scope:
    - [Alive_state] means that data has been recorded for this scope
    - [Empty_state] means that there is no data for this scope
    - [Expired_state] means that data for this scope has been removed
    because the timeout has been reached.

    The default scope is [`Session].
*)

type state_status = Alive_state | Empty_state | Expired_state

val service_state_status :
  ?state_name:string -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit -> state_status

val volatile_data_state_status :
  ?state_name:string -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit -> state_status

val persistent_data_state_status :
  ?state_name:string -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit -> state_status Lwt.t


(** {3 Global configuration of session timeouts} *)

(** The following functions set the timeout for sessions, for the
    different kinds of session.  The sessions will be closed after
    this amount of time of inactivity from the user. [None] means no
    timeout.

    The optional parameter [?recompute_expdates] is [false] by
    default.  If you set it to [true], the expiration dates for all
    sessions in the table will be recomputed with the new timeout.
    That is, the difference between the new timeout and the old one
    will be added to their expiration dates (by another Lwt thread).
    Sessions whose timeout has been set individually with
    {!Eliom_state.set_volatile_state_timeout} won't be affected.

    If [~state_name] is not present, it is the default for all session names,
    and in that case [recompute_expdates] is ignored. [~state_name:None]
    means the default session name.

    If [~override_configfile] is [true] (default ([false]),
    then the function will set the timeout even if it has been
    modified in the configuration file.
    It means that by default, these functions have no effect
    if there is a value in the configuration file.
    This gives the ability to override the values choosen by the module
    in the configuration file.
    Use [~override_configfile:true] for example if your
    Eliom module wants to change the values afterwards
    (for example in the site configuration Web interface).

    {e Warning: If you use one of these functions after the
    initialisation phase, you must give the [~sp] parameter, otherwise
    it will raise the exception
    {!Eliom_common.Eliom_function_forbidden_outside_site_loading}.
   If you are using static linking, you must delay the call to these functions
   until the configuration file is read, using
   {!Eliom_services.register_eliom_module}. Otherwise you will also get 
   this exception.
   This remark also applies to most [get_*] functions.}
*)

(** Sets the timeout for volatile (= "in memory") sessions (both
    service session and volatile data session) (server side).
*)
val set_global_volatile_state_timeout :
  ?state_name:string option -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?sp:Eliom_request_info.server_params ->
  ?recompute_expdates:bool -> 
  ?override_configfile:bool ->
  float option -> unit

(** Sets the timeout for service states (server side).
*)
val set_global_service_state_timeout :
  ?state_name:string option -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?sp:Eliom_request_info.server_params ->
  ?recompute_expdates:bool -> 
  ?override_configfile:bool ->
  float option -> unit

(** Sets the timeout for volatile (= "in memory") data states (server side).
*)
val set_global_volatile_data_state_timeout :
  ?state_name:string option -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?sp:Eliom_request_info.server_params ->
  ?recompute_expdates:bool -> 
  ?override_configfile:bool ->
  float option -> unit

(** Sets the timeout for persistent states (server side).
*)
val set_global_persistent_data_state_timeout :
  ?state_name:string option ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?sp:Eliom_request_info.server_params -> 
  ?recompute_expdates:bool ->
  ?override_configfile:bool ->
  float option -> unit






(** Returns the timeout for service states (server side).
*)
val get_global_service_state_timeout :
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?sp:Eliom_request_info.server_params -> unit -> float option

(** Returns the timeout for "volatile data" states (server side).
*)
val get_global_volatile_data_state_timeout :
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?sp:Eliom_request_info.server_params -> unit -> float option

(** Returns the timeout for persistent states (server side).
*)
val get_global_persistent_data_state_timeout :
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?sp:Eliom_request_info.server_params -> unit -> float option



(** {3 Personalizing state timeouts} *)

(** sets the timeout for service state (server side) for one user,
   in seconds. [None] = no timeout *)
val set_service_state_timeout :
  ?state_name:string -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> float option -> unit

(** remove the service state timeout for one user
   (and turn back to the default). *)
val unset_service_state_timeout :
  ?state_name:string -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> unit -> unit

(** returns the timeout for current service state.
    [None] = no timeout
 *)
val get_service_state_timeout :
  ?state_name:string -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> unit -> float option




(** sets the timeout for volatile data state (server side) for one user,
   in seconds. [None] = no timeout *)
val set_volatile_data_state_timeout :
  ?state_name:string -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> float option -> unit

(** remove the "volatile data" state timeout for one user
   (and turn back to the default). *)
val unset_volatile_data_state_timeout :
  ?state_name:string -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> unit -> unit

(** returns the timeout for current volatile data state.
    [None] = no timeout
 *)
val get_volatile_data_state_timeout :
  ?state_name:string -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> unit -> float option







(** sets the timeout for persistent state (server side) for one user,
   in seconds. [None] = no timeout *)
val set_persistent_data_state_timeout : 
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> float option -> unit Lwt.t

(** remove the persistent state timeout for one user
   (and turn back to the default). *)
val unset_persistent_data_state_timeout : 
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> unit -> unit Lwt.t

(** returns the persistent state timeout for one user. [None] = no timeout *)
val get_persistent_data_state_timeout : 
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> unit -> float option Lwt.t


(** {3 Session groups} *)

type 'a session_data =
  | No_data
  | Data_session_expired
  | Data of 'a

(** Session groups may be used    for example to limit
    the number of sessions one user can open at the same time, or to implement
    a "close all sessions" feature.
    Usually, the group is the user name.
*)

(** sets the group to which belong the service session.
    If the optional [?set_max] parameter is present, also sets the maximum
    number of sessions in the group. [None] means "no limitation".
    If [~secure] is false when the protocol is https, it will affect
    the unsecure session, otherwise, il will affect the secure session in 
    https, the unsecure one in http.

    It is possibe to set the groupe only for regular browser sessions.
    Tab sessions are automatically put in a group which corresponds
    to the browser session.
*)
val set_service_session_group :
  ?set_max: int ->
  ?state_name:string ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  string ->
  unit

(** Remove the session from its group *)
val unset_service_session_group :
  ?set_max: int ->
  ?state_name:string ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit ->
  unit

(** returns the group to which belong the service session.
    If the session does not belong to any group, or if no session is opened,
    return [None].
*)
val get_service_session_group :
  ?state_name:string ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit ->
  string session_data

(** sets the group to which belong the volatile data session.
    If the optional [?set_max] parameter is present, also sets the maximum
    number of sessions in the group. [None] means "no limitation".
*)
val set_volatile_data_session_group :
  ?set_max: int ->
  ?state_name:string ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  string ->
  unit

(** Remove the session from its group *)
val unset_volatile_data_session_group :
  ?set_max: int ->
  ?state_name:string ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit ->
  unit

(** returns the group to which belong the data session.
    If the session does not belong to any group, or if no session is opened,
    return [None].
*)
val get_volatile_data_session_group :
  ?state_name:string ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit ->
  string session_data

(** sets the group to which belong the persistent session.
    If the optional [?set_max] parameter is present, also sets the maximum
    number of sessions in the group. [None] means "no limitation".
*)
val set_persistent_data_session_group :
  ?set_max: int option ->
  ?state_name:string ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  string ->
  unit Lwt.t

(** Remove the session from its group *)
val unset_persistent_data_session_group :
  ?state_name:string ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit ->
  unit Lwt.t

(** returns the group to which belong the persistent session.
    If the session does not belong to any group, or if no session is opened,
    return [None].
*)
val get_persistent_data_session_group :
  ?state_name:string ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit ->
  string session_data Lwt.t

(** The following functions of this section set the maximum number of
    sessions in a session group, for the different kinds of session.
    [None] means "no limit". This won't modify existing groups. That
    value will be used only as default value if you do not specify the
    optional parameter [?set_max] of function
    {!Eliom_state.set_volatile_data_session_group}.

    If there is no group, the number of sessions is limitated by sub network
    (which can be a problem for example if the server is behind a
    reverse proxy).
    It is highly recommended to use session groups!
*)

(** Sets the maximum number of service sessions in a session group
    (see above).
*)
val set_default_max_service_sessions_per_group :
  ?sp:Eliom_request_info.server_params -> ?override_configfile:bool -> int -> unit

(** Sets the maximum number of volatile data sessions in a session
    group (see above).
*)
val set_default_max_volatile_data_sessions_per_group :
  ?sp:Eliom_request_info.server_params -> ?override_configfile:bool -> int -> unit

(** Sets the maximum number of persistent data sessions in a session
    group (see above).
*)
val set_default_max_persistent_data_sessions_per_group :
  ?sp:Eliom_request_info.server_params -> ?override_configfile:bool -> int option -> unit

(** Sets the maximum number of volatile sessions (data and service) in a session
    group (see above).
*)
val set_default_max_volatile_sessions_per_group :
  ?sp:Eliom_request_info.server_params -> ?override_configfile:bool -> int -> unit

(** Sets the maximum number of service sessions in a subnet (see above).
*)
val set_default_max_service_sessions_per_subnet :
  ?sp:Eliom_request_info.server_params -> ?override_configfile:bool -> int -> unit

(** Sets the maximum number of volatile data sessions in a subnet (see above).
*)
val set_default_max_volatile_data_sessions_per_subnet :
  ?sp:Eliom_request_info.server_params -> ?override_configfile:bool -> int -> unit

(** Sets the maximum number of volatile sessions (data and service) 
    in a subnet (see above).
*)
val set_default_max_volatile_sessions_per_subnet :
  ?sp:Eliom_request_info.server_params -> ?override_configfile:bool -> int -> unit


(** Sets the maximum number of tab service sessions in a session group
    (see above).
*)
val set_default_max_service_tab_sessions_per_group :
  ?sp:Eliom_request_info.server_params -> ?override_configfile:bool -> int -> unit

(** Sets the maximum number of volatile data tab sessions in a session
    group (see above).
*)
val set_default_max_volatile_data_tab_sessions_per_group :
  ?sp:Eliom_request_info.server_params -> ?override_configfile:bool -> int -> unit

(** Sets the maximum number of persistent data tab sessions in a session
    group (see above).
*)
val set_default_max_persistent_data_tab_sessions_per_group :
  ?sp:Eliom_request_info.server_params -> ?override_configfile:bool -> int option -> unit

(** Sets the maximum number of volatile tab sessions (data and service)
    in a session group (see above).
*)
val set_default_max_volatile_tab_sessions_per_group :
  ?sp:Eliom_request_info.server_params -> ?override_configfile:bool -> int -> unit



(** Sets the mask for subnet (IPV4). *)
val set_ipv4_subnet_mask :
  ?sp:Eliom_request_info.server_params -> ?override_configfile:bool -> int32 -> unit

(** Sets the mask for subnet (IPV6). *)
val set_ipv6_subnet_mask :
  ?sp:Eliom_request_info.server_params -> ?override_configfile:bool -> int64 * int64 -> unit



(** Sets the maximum number of service sessions in the current session
    group (or for the client sub network, if there is no group).
*)
val set_max_service_sessions_for_group_or_subnet :
  ?state_name:string ->
  ?scope:Eliom_common.user_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  int ->
  unit
(*VVV renommer! *)


(** Sets the maximum number of volatile data sessions in the current session
    group (or for the client sub network, if there is no group).
*)
val set_max_volatile_data_sessions_for_group_or_subnet :
  ?state_name:string ->
  ?scope:Eliom_common.user_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  int ->
  unit
(*VVV renommer! *)

(** Sets the maximum number of volatile sessions 
    (both data and service sessions) in the current 
    group (or for the client sub network, if there is no group).
*)
val set_max_volatile_sessions_for_group_or_subnet :
  ?state_name:string ->
  ?scope:Eliom_common.user_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  int ->
  unit
(*VVV renommer! *)


(** {3 Session cookies} *)

(** The functions in this section ask the browser to set the cookie
    expiration date, for the different kinds of session, in seconds,
    since the 1st of January 1970. [None] means the cookie will expire
    when the browser is closed. Note: there is no way to set cookies
    for an infinite time on browsers. *)

(** Sets the cookie expiration date for the current service session
    (see above).
*)
val set_service_cookie_exp_date : 
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> 
  float option -> 
  unit
(*VVV renommer! *)

(** Sets the cookie expiration date for the current data session (see
    above).
*)
val set_volatile_data_cookie_exp_date :
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> 
  float option -> 
  unit
(*VVV renommer! *)


(** Sets the cookie expiration date for the persistent session (see
    above).
*)
val set_persistent_data_cookie_exp_date :
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> 
  float option -> 
  unit Lwt.t
(*VVV renommer! *)




(*****************************************************************************)
(** {2 Server side state data: Eliom references} *)

(** Eliom references are some kind of references with limited scope.
    You define the reference with an initial value and a scope
    (group of sessions, session or client process).
    When you change the value, it actually changes only for the scope
    you specified.

    Eliom references are used for example to store session data,
    or server side data for a client process.
*)

module Eref : sig
  (** The type of Eliom references. *)
  type 'a eref

  (** Create an Eliom reference for the given scope (default: [`Session]).

      Use the optional parameter [?persistent] if you want the data to survive
      after relaunching the server. You must give an unique name to the
      table in which it will be stored on the hard disk (using Ocsipersist).
      Be very careful to use unique names, and to change the name if
      you change the type of the data.

      Use the optional parameter [?secure] if you want the data to be available
      only using HTTPS (default: false).

      Use the optional parameter [?state_name] if you want to distinguish
      between several server side states for the same scope.

      If you create the eref during a request, do not forget to give
      to [~sp] parameter.
  *)
  val eref :
    ?state_name:string ->
    ?scope:Eliom_common.user_scope ->
    ?secure:bool ->
    ?persistent:string ->
    ?sp:Eliom_request_info.server_params -> 'a -> 'a eref

  (** Get the value of an Eliom reference. *)
  val get : sp:Eliom_request_info.server_params -> 'a eref -> 'a Lwt.t

  (** Change the value of an Eliom reference. *)
  val set : sp:Eliom_request_info.server_params -> 'a eref -> 'a -> unit Lwt.t

  (** Turn back to the default value 
      (by removing the entry in the server side table) *)
  val unset : sp:Eliom_request_info.server_params -> 'a eref -> unit Lwt.t
end

(*****************************************************************************)
(** {2 Closing sessions, removing state data and services} *)

(** Delete server side state data for a session, a group of sessions or
    a client process. Default scope: [`Session].

    Use that function to close a session (using scope [`Session]).

    Shortcut for {!Eliom_state.discard_services} followed by
    {!Eliom_state.discard_data}.

    By default will remove both secure and unsecure data and services, but
    if [~secure] is present.

    Warning: you may also want to remove some data from the polymorphic
    request data table when closing a session 
    (See {!Eliom_state.get_request_cache}).
*)
val discard :
  ?state_name:string ->
  ?scope:Eliom_common.user_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit ->
  unit Lwt.t

(** close_session is a synonymous for [discard ~scope:`Session] *)
val close_session :
  ?state_name:string ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit ->
  unit Lwt.t

(** close_group is a synonymous for [discard ~scope:`Session_group] *)
val close_group :
  ?state_name:string ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit ->
  unit Lwt.t

(** Remove current state data.

    If the optional parameter [?persistent] is not present, will
    remove both volatile and persistent data. Otherwise only volatile
    or persistent data.
 *)
val discard_data :
  ?persistent:bool ->
  ?state_name:string ->
  ?scope:Eliom_common.user_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit ->
  unit Lwt.t

(** Remove all services registered for the given scope (the default beeing
    [`Session]). *)
val discard_services :
  ?state_name:string ->
  ?scope:Eliom_common.user_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit ->
  unit



(*****************************************************************************)
(** {2 User cookies} *)

val set_cookie :
  sp:Eliom_request_info.server_params ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?path:string list ->
  ?exp:float -> name:string -> value:string -> ?secure:bool -> unit -> unit

val unset_cookie :
  sp:Eliom_request_info.server_params ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?path:string list ->
  name:string -> unit -> unit



(*****************************************************************************)
(** {2 Session data (deprecated interface)} *)


(** {3 In memory session data} *)

(** The type of (volatile) session data tables. *)
type 'a volatile_table

(** creates a table in memory where you can store the session data for
   all users. (deprecated)

   {e Warning: If you use that function after the initialization phase,
   you must give the [~sp] parameter, otherwise it will raise the exception
   {!Eliom_common.Eliom_function_forbidden_outside_site_loading}.
   If you are using static linking, you must delay the call to this function
   until the configuration file is read, using
   {!Eliom_services.register_eliom_module}. Otherwise you will also get 
   this exception.}
 *)
val create_volatile_table :
  ?state_name:string ->
  ?scope:Eliom_common.user_scope ->
  ?secure:bool ->
  ?sp:Eliom_request_info.server_params -> unit -> 'a volatile_table

(** gets session data for the current session (if any).  (deprecated) *)
val get_volatile_data : 
  table:'a volatile_table -> 
  sp:Eliom_request_info.server_params -> 
  unit ->
  'a session_data

(** sets session data for the current session.  (deprecated) *)
val set_volatile_data : 
  table:'a volatile_table -> 
  sp:Eliom_request_info.server_params -> 
  'a ->
  unit

(** removes session data for the current session
   (but does not close the session).
   If the session does not exist, does nothing.
 (deprecated)
 *)
val remove_volatile_data : 
  table:'a volatile_table -> 
  sp:Eliom_request_info.server_params -> 
  unit -> 
  unit


(** {3 Persistent state} *)

(** The type of persistent session data tables. *)
type 'a persistent_table

(** creates a table on hard disk where you can store the session data for
   all users. It uses {!Ocsipersist}.  (deprecated) *)
val create_persistent_table :
  ?state_name:string ->
  ?scope:Eliom_common.user_scope ->
  ?secure:bool ->
  string -> 'a persistent_table

(** gets persistent session data for the current persistent session (if any).
 (deprecated) *)
val get_persistent_data : 
  table:'a persistent_table -> 
  sp:Eliom_request_info.server_params ->
  unit -> 
  'a session_data Lwt.t

(** sets persistent session data for the current persistent session.
 (deprecated) *)
val set_persistent_data : 
  table:'a persistent_table -> 
  sp:Eliom_request_info.server_params -> 
  'a -> 
  unit Lwt.t

(** removes session data for the current persistent session
   (but does not close the session).
   If the session does not exist, does nothing.
 (deprecated)
 *)
val remove_persistent_data : 
  table:'a persistent_table -> 
  sp:Eliom_request_info.server_params -> 
  unit -> 
  unit Lwt.t



(*****************************************************************************)
(** {2 Administrating server side state} *)

(** Discard all services and persistent and volatile data for one state name.
    If the optional parameter [?state_name] is not present,
    the default name will be used.

    {e Warning: If you use this function after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliom_common.Eliom_function_forbidden_outside_site_loading}.
   If you are using static linking, you must delay the call to this function
   until the configuration file is read, using
   {!Eliom_services.register_eliom_module}. Otherwise you will also get 
   this exception.}
 *)
val discard_all :
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?sp:Eliom_request_info.server_params ->
  unit ->
  unit Lwt.t
(*VVV missing: scope group *)
(*VVV missing ~secure? *)

(** Discard server side data for all clients, for the given state name
    and scope.
    If the optional parameter [?state_name] is not present,
    the default name will be used.

    If the optional parameter [?persistent] is not present,
    both the persistent and volatile data will be removed.

    {e Warning: If you use this function after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliom_common.Eliom_function_forbidden_outside_site_loading}.
   If you are using static linking, you must delay the call to this function
   until the configuration file is read, using
   {!Eliom_services.register_eliom_module}. Otherwise you will also get 
   this exception.}
 *)
val discard_all_data :
  ?persistent:bool ->
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?sp:Eliom_request_info.server_params ->
  unit ->
  unit Lwt.t
(*VVV missing: scope group *)
(*VVV missing ~secure? *)


(** Remove all services registered for clients for the given state name
    and scope.
    If the optional parameter [?state_name] is not present,
    the default name is used.

    {e Warning: If you use this function after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliom_common.Eliom_function_forbidden_outside_site_loading}.
   If you are using static linking, you must delay the call to this function
   until the configuration file is read, using
   {!Eliom_services.register_eliom_module}. Otherwise you will also get 
   this exception.}
 *)
val discard_all_services :
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?sp:Eliom_request_info.server_params ->
  unit ->
  unit Lwt.t
(*VVV missing: scope group *)
(*VVV missing ~secure? *)


module Session_admin : sig

  (** Type used to describe session timeouts *)
  type timeout =
    | TGlobal (** see global setting *)
    | TNone   (** explicitely set no timeout *)
    | TSome of float (** timeout duration in seconds *)


  type service_session
  type data_session
  type persistent_session

  val close_service_session :
    ?close_group:bool ->
    session:service_session ->
    unit

  val close_volatile_data_session :
    ?close_group:bool ->
    session:data_session ->
    unit

  val close_persistent_data_session :
    ?close_group:bool ->
    session:persistent_session ->
    unit Lwt.t

  (** Raises [Not_found] if no data in the table for the session. *)
  val get_volatile_session_data :
    session:data_session ->
    table:'a volatile_table ->
    'a

  (** Fails with lwt exception [Not_found]
     if no data in the table for the session. *)
  val get_persistent_session_data :
    session:persistent_session ->
    table:'a persistent_table ->
    'a Lwt.t

  val remove_volatile_session_data :
      session:data_session -> table:'a volatile_table -> unit
  val remove_persistent_session_data :
      session:persistent_session -> table:'a persistent_table -> unit Lwt.t

  (** [None] means default session name *)
  val get_service_state_name :
    session:service_session -> string option

  (** [None] means default session name *)
  val get_volatile_data_state_name : session:data_session -> 
    string option

  (** [None] means default session name *)
  val get_persistent_data_state_name :
      session:persistent_session -> string option

  val get_service_session_cookie_scope :
    session:service_session -> Eliom_common.cookie_scope
  val get_volatile_data_session_cookie_scope : session:data_session -> 
    Eliom_common.cookie_scope
  val get_persistent_data_session_cookie_scope :
    session:persistent_session -> Eliom_common.cookie_scope

  val set_service_session_timeout :
      session:service_session -> float option -> unit
  val set_volatile_data_session_timeout :
      session:data_session -> float option -> unit
  val set_persistent_data_session_timeout :
      session:persistent_session -> float option -> unit Lwt.t

  val get_service_session_timeout :
      session:service_session -> timeout

  val get_volatile_data_session_timeout :
      session:data_session -> timeout

  val get_persistent_data_session_timeout :
      session:persistent_session -> timeout

  val unset_service_session_timeout :
      session:service_session -> unit
  val unset_volatile_data_session_timeout :
      session:data_session -> unit
  val unset_persistent_data_session_timeout :
      session:persistent_session -> unit Lwt.t

  (** Iterator on service sessions. [Lwt_unix.yield] is called automatically
     after each iteration.

    {e Warning: If you use this function after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliom_common.Eliom_function_forbidden_outside_site_loading}.
   If you are using static linking, you must delay the call to this function
   until the configuration file is read, using
   {!Eliom_services.register_eliom_module}. Otherwise you will also get 
   this exception.}
   *)
  val iter_service_sessions :
      ?sp:Eliom_request_info.server_params ->
        (service_session -> unit Lwt.t) -> unit Lwt.t

  (** Iterator on data sessions. [Lwt_unix.yield] is called automatically
     after each iteration.

    {e Warning: If you use this function after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliom_common.Eliom_function_forbidden_outside_site_loading}.
   If you are using static linking, you must delay the call to this function
   until the configuration file is read, using
   {!Eliom_services.register_eliom_module}. Otherwise you will also get 
   this exception.}
   *)
  val iter_volatile_data_sessions :
      ?sp:Eliom_request_info.server_params ->
        (data_session -> unit Lwt.t) -> unit Lwt.t

  (** Iterator on persistent sessions. [Lwt_unix.yield] is called automatically
     after each iteration. *)
  val iter_persistent_data_sessions :
    (persistent_session -> unit Lwt.t) -> unit Lwt.t

  (** Iterator on service sessions. [Lwt_unix.yield] is called automatically
     after each iteration.

    {e Warning: If you use this function after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliom_common.Eliom_function_forbidden_outside_site_loading}.
   If you are using static linking, you must delay the call to this function
   until the configuration file is read, using
   {!Eliom_services.register_eliom_module}. Otherwise you will also get 
   this exception.}
   *)
  val fold_service_sessions :
      ?sp:Eliom_request_info.server_params ->
        (service_session -> 'b -> 'b Lwt.t) -> 'b -> 'b Lwt.t

  (** Iterator on data sessions. [Lwt_unix.yield] is called automatically
     after each iteration.

    {e Warning: If you use this function after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliom_common.Eliom_function_forbidden_outside_site_loading}.
   If you are using static linking, you must delay the call to this function
   until the configuration file is read, using
   {!Eliom_services.register_eliom_module}. Otherwise you will also get 
   this exception.}
   *)
  val fold_volatile_data_sessions :
      ?sp:Eliom_request_info.server_params ->
        (data_session -> 'b -> 'b Lwt.t) -> 'b  -> 'b Lwt.t

  (** Iterator on persistent sessions. [Lwt_unix.yield] is called automatically
     after each iteration. *)
  val fold_persistent_data_sessions :
    (persistent_session -> 'b -> 'b Lwt.t) -> 'b -> 'b Lwt.t

end



(*****************************************************************************)
(** {2 Getting information about the URL of the client side process}

    Warning: it is different from the URL to which the request has been made.
    To get information about the current request, see module {Eliom_request_info}.

   {3 General information}
 *)

(** returns the full path of the URL where the process is running *)
val get_original_full_path : sp:Eliom_request_info.server_params -> Ocsigen_lib.url_path

(** returns the hostname used for absolute links, computed
    when launching the client side process for the first time.
    If there is no client side process, same as {!Eliom_request_info.get_hostname}
    It is either the [Host] header sent by the browser or the default hostname
    set in the configuration file, depending on server configuration
    ([<usedefaulthostname/>] option).
 *)
val get_hostname : sp:Eliom_request_info.server_params -> string

(** returns the port of the server, used when launching the client side process
    (not the current request). It corresponds to the port in the URL of 
    the browser.
    If there is no client side process, same as
    {!Eliom_request_info.get_server_port}.
*)
val get_server_port : sp:Eliom_request_info.server_params -> int

(** returns true if https is used in the URL of the browser, false if http.
    If there is no client side process, same as {!Eliom_request_info.get_ssl}.
*)
val get_ssl : sp:Eliom_request_info.server_params -> bool




(**/**)
(*
(** {2 Default timeouts} *)

(** returns the default timeout for service sessions (server side).
    The default timeout is common for all sessions for which no other value
    has been set. At the beginning of the server, it is taken from the
    configuration file, (or set to default value).
    [None] = no timeout.
    *)
val get_default_service_session_timeout : unit -> float option

(** returns the default timeout for "volatile data" sessions (server side).
    The default timeout is common for all sessions for which no other value
    has been set. At the beginning of the server, it is taken from the
    configuration file, (or set to default value).
    [None] = no timeout.
    *)
val get_default_volatile_data_session_timeout : unit -> float option

(** returns the default timeout for sessions (server side).
    The default timeout is common for all sessions for which no other value
    has been set. At the beginning of the server, it is taken from the
    configuration file, (or set to default value).
    [None] = no timeout.
    *)
val get_default_persistent_data_session_timeout : unit -> float option

(** sets the default timeout for volatile (= "in memory")
   sessions (i.e. both service session and volatile data session)
   (server side).
   [None] = no timeout.

   Warning: this function sets the default for all sites. You should
   probably use [set_global_volatile_session_timeout] instead.
    *)
val set_default_volatile_session_timeout : float option -> unit

(** sets the default timeout for service sessions.
    [None] = no timeout.

    Warning: this function sets the default for all sites. You should
    probably use [set_global_service_session_timeout] instead.
    *)
val set_default_service_session_timeout : float option -> unit

(** sets the default timeout for "volatile data" sessions (server side).
    [None] = no timeout.

    Warning: this function sets the default for all sites. You should
    probably use [set_global_volatile_data_session_timeout] instead.
    *)
val set_default_volatile_data_session_timeout : float option -> unit

(** sets the default timeout for sessions (server side).
    [None] = no timeout.

    Warning: this function sets the default for all sites. You should
    probably use [set_global_persistent_data_session_timeout] instead.
    *)
val set_default_persistent_data_session_timeout : float option -> unit
*)
(**/**)


(*****************************************************************************)
(** {2 Other low level functions} *)

(** You probably don't need these functions. *)


(** returns the value of the Eliom's cookies for one persistent session.
   Returns [None] is no session is active.
 *)
val get_persistent_data_cookie : 
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> unit -> string option Lwt.t

(** returns the value of Eliom's cookies for one service session.
   Returns [None] is no session is active.
 *)
val get_service_cookie :
  ?state_name:string -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit -> string option

(** returns the value of Eliom's cookies for one "volatile data" session.
   Returns [None] is no session is active.
 *)
val get_volatile_data_cookie :
  ?state_name:string -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params ->
  unit -> string option







(**/**)
(*****************************************************************************)
val number_of_service_sessions : sp:Eliom_request_info.server_params -> int

val number_of_volatile_data_sessions : sp:Eliom_request_info.server_params -> int

val number_of_tables : unit -> int

val number_of_table_elements : unit -> int list

val number_of_persistent_data_sessions : unit -> int Lwt.t

val number_of_persistent_tables : unit -> int

val number_of_persistent_table_elements : unit -> (string * int) list Lwt.t
(* Because of Dbm implementation, the result may be less than the expected
   result in some case (with a version of ocsipersist based on Dbm) *)


val get_global_table : ?sp:Eliom_request_info.server_params -> unit -> Eliom_common.tables

val get_session_service_table :
  ?state_name:string -> 
  ?scope:Eliom_common.user_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> 
  unit ->
  Eliom_common.tables ref

val get_session_service_table_if_exists :
  ?state_name:string -> 
  ?scope:Eliom_common.user_scope ->
  ?secure:bool ->
  sp:Eliom_request_info.server_params -> 
  unit ->
  Eliom_common.tables ref




val make_server_params :
  Eliom_common.sitedata ->
  Eliom_common.info ->
  Ocsigen_lib.url_path option -> 
  Eliom_common.fullsessionname option -> Eliom_common.server_params


val create_volatile_table_during_session_ :
  scope:Eliom_common.user_scope ->
  state_name:string option ->
  secure:bool ->
  Eliom_common.sitedata -> 
  'a volatile_table
