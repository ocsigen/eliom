(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2007 Vincent Balat
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

(** Storing server-side values for your applications or sessions. *)

(** {b Please read the
    {% <<a_manual chapter="server-state" | Eliom manual >>%}
    before this page to learn how server side state works. }

{% <<outline| <<header| **Table of contents** >> >>%}

*)

(*****************************************************************************)
(** {2 Managing the state of an application} *)

(** {3 Closing sessions, removing state data and services} *)

val discard
  :  scope:[< Eliom_common.user_scope | Eliom_common.request_scope]
  -> ?secure:bool
  -> unit
  -> unit Lwt.t
(** Delete server-side (volatile and persistent) state data and services
    for a session,
    a group of sessions, a client process or a request.

    Use that function to close a session
    (using scope [Eliom_common.default_session_scope]).

    Closing a group of sessions will close all sessions in the group.

    By default will remove both secure and unsecure data and services, unless
    [~secure] is present.

    {e Warning: you may also want to unset some request-scoped Eliom references
    when discarding a state.}
*)

(* Discard services and (volatile and persistent) data
   for all user and request scopes *)
val discard_all_scopes : ?secure:bool -> unit -> unit Lwt.t

val discard_data
  :  ?persistent:bool
  -> scope:[< Eliom_common.user_scope | Eliom_common.request_scope]
  -> ?secure:bool
  -> unit
  -> unit Lwt.t
(** Remove current state data.

    If the optional parameter [?persistent] is not present, will
    remove both volatile and persistent data. Otherwise only volatile
    or persistent data.
 *)

val discard_services
  :  scope:[< Eliom_common.user_scope]
  -> ?secure:bool
  -> unit
  -> unit
(** Remove all services registered for the given scope (the default being
    [`Session]). *)

(*****************************************************************************)
(** {3 State status} *)

(** The following functions return the current state of the state for a given
    scope:
    - [Alive_state] means that data has been recorded for this scope
    - [Empty_state] means that there is no data for this scope
    - [Expired_state] means that data for this scope has been removed
    because the timeout has been reached.

    The default scope is [`Session].
*)

type state_status = Alive_state | Empty_state | Expired_state

val service_state_status
  :  scope:[< Eliom_common.user_scope]
  -> ?secure:bool
  -> unit
  -> state_status

val volatile_data_state_status
  :  scope:[< Eliom_common.user_scope]
  -> ?secure:bool
  -> unit
  -> state_status

val persistent_data_state_status
  :  scope:[< Eliom_common.user_scope]
  -> ?secure:bool
  -> unit
  -> state_status Lwt.t

(*****************************************************************************)
(** {3 User cookies}

    If you want to store a client-side state, and ask the browser to
    send it back with each request, you can set manually your own cookies.
    Usual cookies correspond to scope [`Session] (that is, one browser).
    The browser send them with each request to the same Web site.
    But Eliom also implements client-side process cookies
    (scope [`Client_process]), that behave in the same way,
    but for one instance of the client-side Eliom program (if there is one).

    Cookies can be limited to a subsite using the [?path] optional
    parameter. This path is relative to the main path of your Web site.
    (It is not possible to set a cookie for a subsite larger than your current
    Web site).

    Cookies can have an expiration date, specified (in seconds
    since the 1st of January 1970) in the optional parameter [?exp]
    (see how to set default expiration dates below).

    Secure cookies are sent by the browser only with HTTPS (default: [false]).
*)

val set_cookie
  :  ?cookie_level:Eliom_common.cookie_level
  -> ?path:string list
  -> ?exp:float
  -> ?secure:bool
  -> name:string
  -> value:string
  -> unit
  -> unit
(** Ask the browser to record a cookie. *)

val unset_cookie
  :  ?cookie_level:Eliom_common.cookie_level
  -> ?path:string list
  -> name:string
  -> unit
  -> unit
(** Ask the browser to remove a cookie. *)

(*****************************************************************************)
(** {2 Session groups} *)

(** If your Web site has users,
    it is a good idea to group together all the sessions for one user.
    Otherwise, you may want to group sessions according to another
    criterion.

    Session groups may be used for example to limit
    the number of sessions one user can open at the same time, or to implement
    a "close all your sessions" feature.
    Usually, the group is the user name.
*)

(** {3 Putting a session in a group, removing a session from a group} *)

val set_service_session_group
  :  ?set_max:int
  -> ?scope:Eliom_common.session_scope
  -> ?secure:bool
  -> string
  -> unit
(** sets the group to which belong the service session.

    If the optional [?set_max] parameter is present, also sets the
    maximum number of sessions in the group. Default: follow current
    configuration for the group or default configuration if the group
    does not exist.

    If [~secure] is true, it will affect the secure session (secure cookies),
    otherwise (default), the unsecure one (behavior change in Eliom 4).
*)

val unset_service_session_group
  :  ?set_max:int
  -> ?scope:Eliom_common.session_scope
  -> ?secure:bool
  -> unit
  -> unit
(** Remove the session from its group.
    Will not close the session if it contains data. *)

val get_service_session_group
  :  ?scope:Eliom_common.session_scope
  -> ?secure:bool
  -> unit
  -> string option
(** returns the group to which belong the service session.
    If the session does not belong to any group,
    or if no session is opened, return [None].
*)

val get_service_session_group_size
  :  ?scope:Eliom_common.session_scope
  -> ?secure:bool
  -> unit
  -> int option
(** returns the number of sessions in the group. If he session does not
    belong to any group or if no session is opened, returns [None] *)

val set_volatile_data_session_group
  :  ?set_max:int
  -> ?scope:Eliom_common.session_scope
  -> ?secure:bool
  -> string
  -> unit
(** sets the group to which belong the volatile data session.

    If the optional [?set_max] parameter is present, also sets the maximum
    number of sessions in the group.
    Default: follow current configuration for the group
    or default configuration if the group does not exist.
*)

val unset_volatile_data_session_group
  :  ?set_max:int
  -> ?scope:Eliom_common.session_scope
  -> ?secure:bool
  -> unit
  -> unit
(** Remove the session from its group.
    Will not close the session if it contains data. *)

val get_volatile_data_session_group
  :  ?scope:Eliom_common.session_scope
  -> ?secure:bool
  -> unit
  -> string option
(** returns the group to which belong the data session.
    If the session does not belong to any group, or if no session is opened,
    return [None].
*)

val get_volatile_data_session_group_size
  :  ?scope:Eliom_common.session_scope
  -> ?secure:bool
  -> unit
  -> int option
(** returns the number of sessions in the group. If he session does not
    belong to any group or if no session is opened, returns [None] *)

val set_persistent_data_session_group
  :  ?set_max:int option
  -> ?scope:Eliom_common.session_scope
  -> ?secure:bool
  -> string
  -> unit Lwt.t
(** sets the group to which belong the persistent session.

    If the optional [?set_max] parameter is present, also sets the
    maximum number of sessions in the group. When [~set_max:None] is
    present, the number of session is unlimited. Default: follow
    current configuration for the group or default configuration if
    the group does not exist.
*)

val unset_persistent_data_session_group
  :  ?scope:Eliom_common.session_scope
  -> ?secure:bool
  -> unit
  -> unit Lwt.t
(** Remove the session from its group.
    Will not close the session if it contains data. *)

val get_persistent_data_session_group
  :  ?scope:Eliom_common.session_scope
  -> ?secure:bool
  -> unit
  -> string option Lwt.t
(** returns the group to which belong the persistent session.
    If the session does not belong to any group, or if no session is opened,
    return [None].
*)

(** {3 Maximum group size} *)

(** The following functions of this section set the maximum number of
    sessions in a session group, for the different kinds of session.
    This won't modify existing groups.
    That value will be used only as default value if you do not specify the
    optional parameter [?set_max] of function
    {!Eliom_state.set_volatile_data_session_group}.

    If there is no group, the number of sessions is limitated by sub network
    (which can be a problem for example if the server is behind a
    reverse proxy).
    It is highly recommended to use session groups!

    - Default number of sessions in a group: 5
    - Default number of sessions in a sub network: 1000000
    - Default IPV4 sub network: /16
    - Default IPV6 sub network: /56

    These default can be changed from configuration file and/or
    using these functions.

    If [~override_configfile] is [true] (default ([false]),
    then the function will set the value even if it has been
    modified in the configuration file.
    It means that by default, these functions have no effect
    if there is a value in the configuration file.
    This gives the ability to override the values chosen by the module
    in the configuration file.
    Use [~override_configfile:true] for example if your
    Eliom module wants to change the values afterwards
    (for example in the site configuration Web interface).
*)

val set_default_max_service_sessions_per_group
  :  ?override_configfile:bool
  -> int
  -> unit
(** Sets the maximum number of service sessions in a session group
    (see above).
*)

val set_default_max_volatile_data_sessions_per_group
  :  ?override_configfile:bool
  -> int
  -> unit
(** Sets the maximum number of volatile data sessions in a session
    group (see above).
*)

val set_default_max_persistent_data_sessions_per_group
  :  ?override_configfile:bool
  -> int option
  -> unit
(** Sets the maximum number of persistent data sessions in a session
    group (see above). [None] means "no limitation".
*)

val set_default_max_volatile_sessions_per_group
  :  ?override_configfile:bool
  -> int
  -> unit
(** Sets the maximum number of volatile sessions (data and service) in a session
    group (see above).
*)

val set_default_max_service_sessions_per_subnet
  :  ?override_configfile:bool
  -> int
  -> unit
(** Sets the maximum number of service sessions in a subnet (see above).
*)

val set_default_max_volatile_data_sessions_per_subnet
  :  ?override_configfile:bool
  -> int
  -> unit
(** Sets the maximum number of volatile data sessions in a subnet (see above).
*)

val set_default_max_volatile_sessions_per_subnet
  :  ?override_configfile:bool
  -> int
  -> unit
(** Sets the maximum number of volatile sessions (data and service)
    in a subnet (see above).
*)

val set_default_max_service_tab_sessions_per_group
  :  ?override_configfile:bool
  -> int
  -> unit
(** Sets the maximum number of tab service sessions in a session group
    (see above).
*)

val set_default_max_volatile_data_tab_sessions_per_group
  :  ?override_configfile:bool
  -> int
  -> unit
(** Sets the maximum number of volatile data tab sessions in a session
    group (see above).
*)

val set_default_max_persistent_data_tab_sessions_per_group
  :  ?override_configfile:bool
  -> int option
  -> unit
(** Sets the maximum number of persistent data tab sessions in a session
    group (see above).
*)

val set_default_max_volatile_tab_sessions_per_group
  :  ?override_configfile:bool
  -> int
  -> unit
(** Sets the maximum number of volatile tab sessions (data and service)
    in a session group (see above).
*)

val set_ipv4_subnet_mask : ?override_configfile:bool -> int -> unit
(** Sets the mask for subnet (IPV4). *)

val set_ipv6_subnet_mask : ?override_configfile:bool -> int -> unit
(** Sets the mask for subnet (IPV6). *)

val set_max_service_states_for_group_or_subnet
  :  scope:Eliom_common.user_scope
  -> ?secure:bool
  -> int
  -> unit
(** Sets the maximum number of service sessions in the current session
    group (or for the client sub network, if there is no group).
*)

val set_max_volatile_data_states_for_group_or_subnet
  :  scope:Eliom_common.user_scope
  -> ?secure:bool
  -> int
  -> unit
(** Sets the maximum number of volatile data sessions in the current session
    group (or for the client sub network, if there is no group).
*)

val set_max_volatile_states_for_group_or_subnet
  :  scope:Eliom_common.user_scope
  -> ?secure:bool
  -> int
  -> unit
(** Sets the maximum number of volatile sessions
    (both data and service sessions) in the current
    group (or for the client sub network, if there is no group).
*)

(** {2 Expiration of cookies and timeouts} *)

(** {3 Cookie expiration} *)

(** The functions in this section ask the browser to set the state cookie
    expiration date, for the different kinds of session, in seconds,
    since the 1st of January 1970. [None] means the cookie will expire
    when the browser is closed. There is no way to set cookies
    for an infinite time on browsers.
    By default, Eliom sets default expiration date to
    10 years after opening the session.

    By default, it will affect regular browser cookies (sessions).
    But if you set [~cookie_level:`Client_process],
    it will only affect the client-side Eliom process (if there is one),
    which simulates some kind of "tab cookies".
*)

val set_service_cookie_exp_date
  :  cookie_scope:Eliom_common.cookie_scope
  -> ?secure:bool
  -> float option
  -> unit
(** Sets the cookie expiration date for the current service state
    (see above).
*)

val set_volatile_data_cookie_exp_date
  :  cookie_scope:Eliom_common.cookie_scope
  -> ?secure:bool
  -> float option
  -> unit
(** Sets the cookie expiration date for the current data state (see
    above).
*)

val set_persistent_data_cookie_exp_date
  :  cookie_scope:Eliom_common.cookie_scope
  -> ?secure:bool
  -> float option
  -> unit Lwt.t
(** Sets the cookie expiration date for the persistent state (see
    above).
*)

(** {3 Global configuration of state timeouts} *)

(** The following functions set the timeout for states, for the
    different kinds of states.  States will be closed after
    this amount of time of inactivity from the user. [None] means no
    timeout.

    The optional parameter [?recompute_expdates] is [false] by
    default.  If you set it to [true], the expiration dates for all
    states in the table will be recomputed with the new timeout.
    That is, the difference between the new timeout and the old one
    will be added to their expiration dates (asynchronously,
    by another Lwt thread, as this can take a long time).
    States whose timeout has been set individually with
    functions like
    {!Eliom_state.set_volatile_data_state_timeout} won't be affected.

    If [~scope_hierarchy] is not present,
    it is the default for all scope hierarchies,
    and in that case [recompute_expdates] is ignored. [~scope_hierarchy:None]
    means the default scope hierarchy.

    If [~override_configfile] is [true] (default ([false]),
    then the function will set the timeout even if it has been
    modified in the configuration file.
    It means that by default, these functions have no effect
    if there is a value in the configuration file.
    This gives the ability to override the values chosen by the module
    in the configuration file.
    Use [~override_configfile:true] for example if your
    Eliom module wants to change the values afterwards
    (for example in the site configuration Web interface).
*)

val set_global_volatile_state_timeout
  :  cookie_scope:[< Eliom_common.cookie_scope]
  -> ?secure:bool
  -> ?recompute_expdates:bool
  -> ?override_configfile:bool
  -> float option
  -> unit
(** Sets the (server side) timeout for volatile (= "in memory") sessions (both
    service session and volatile data session).
*)

val set_global_service_state_timeout
  :  cookie_scope:[< Eliom_common.cookie_scope]
  -> ?secure:bool
  -> ?recompute_expdates:bool
  -> ?override_configfile:bool
  -> float option
  -> unit
(** Sets the (server side) timeout for service states. *)

val set_default_global_service_state_timeout
  :  cookie_level:[< Eliom_common.cookie_level]
  -> ?override_configfile:bool
  -> float option
  -> unit

val set_global_volatile_data_state_timeout
  :  cookie_scope:[< Eliom_common.cookie_scope]
  -> ?secure:bool
  -> ?recompute_expdates:bool
  -> ?override_configfile:bool
  -> float option
  -> unit
(** Sets the (server side) timeout for volatile (= "in memory") data states.
*)

val set_default_global_volatile_data_state_timeout
  :  cookie_level:[< Eliom_common.cookie_level]
  -> ?override_configfile:bool
  -> float option
  -> unit

val set_global_persistent_data_state_timeout
  :  cookie_scope:[< Eliom_common.cookie_scope]
  -> ?secure:bool
  -> ?recompute_expdates:bool
  -> ?override_configfile:bool
  -> float option
  -> unit
(** Sets the (server side) timeout for persistent states.
*)

val set_default_global_persistent_data_state_timeout
  :  cookie_level:[< Eliom_common.cookie_level]
  -> ?override_configfile:bool
  -> float option
  -> unit

val get_global_service_state_timeout
  :  ?secure:bool
  -> cookie_scope:[< Eliom_common.cookie_scope]
  -> unit
  -> float option
(** Returns the (server side) timeout for service states.
*)

val get_global_volatile_data_state_timeout
  :  ?secure:bool
  -> cookie_scope:[< Eliom_common.cookie_scope]
  -> unit
  -> float option
(** Returns the (server side) timeout for "volatile data" states.
*)

val get_global_persistent_data_state_timeout
  :  ?secure:bool
  -> cookie_scope:[< Eliom_common.cookie_scope]
  -> unit
  -> float option
(** Returns the (server side) timeout for persistent states.
*)

(** {3 Personalizing timeouts for current state} *)

val set_service_state_timeout
  :  cookie_scope:Eliom_common.cookie_scope
  -> ?secure:bool
  -> float option
  -> unit
(** sets the timeout for service state (server side) for current user,
   in seconds. [None] = no timeout *)

val unset_service_state_timeout
  :  cookie_scope:[< Eliom_common.cookie_scope]
  -> ?secure:bool
  -> unit
  -> unit
(** remove the service state timeout for current user
   (and turn back to the default). *)

val get_service_state_timeout
  :  cookie_scope:[< Eliom_common.cookie_scope]
  -> ?secure:bool
  -> unit
  -> float option
(** returns the timeout for current service state.
    [None] = no timeout
 *)

val set_volatile_data_state_timeout
  :  cookie_scope:[< Eliom_common.cookie_scope]
  -> ?secure:bool
  -> float option
  -> unit
(** sets the (server side) timeout for volatile data state for current user,
   in seconds. [None] = no timeout *)

val unset_volatile_data_state_timeout
  :  cookie_scope:[< Eliom_common.cookie_scope]
  -> ?secure:bool
  -> unit
  -> unit
(** remove the "volatile data" state timeout for current user
   (and turn back to the default). *)

val get_volatile_data_state_timeout
  :  cookie_scope:[< Eliom_common.cookie_scope]
  -> ?secure:bool
  -> unit
  -> float option
(** returns the timeout for current volatile data state.
    [None] = no timeout
 *)

val set_persistent_data_state_timeout
  :  cookie_scope:[< Eliom_common.cookie_scope]
  -> ?secure:bool
  -> float option
  -> unit Lwt.t
(** sets the (server side) timeout for persistent state for current user,
   in seconds. [None] = no timeout *)

val unset_persistent_data_state_timeout
  :  cookie_scope:[< Eliom_common.cookie_scope]
  -> ?secure:bool
  -> unit
  -> unit Lwt.t
(** remove the persistent state timeout for current user
   (and turn back to the default). *)

val get_persistent_data_state_timeout
  :  cookie_scope:[< Eliom_common.cookie_scope]
  -> ?secure:bool
  -> unit
  -> float option Lwt.t
(** returns the persistent state timeout for current user.
    [None] = no timeout *)

(*****************************************************************************)
(** {2 Administrating server side state} *)

(** {e Warning: Most these functions must be called when the site
    information is available, that is, either
    during a request or during the initialisation phase of the site.
    Otherwise, it will raise the exception
    {!Eliom_common.Eliom_site_information_not_available}.
    If you are using static linking, you must delay the call to this function
    until the configuration file is read, using
    {!Eliom_service.register_eliom_module}. Otherwise you will also get
    this exception.}
 *)

type 'a volatile_table
(** The type of (volatile) state data tables. *)

type 'a persistent_table
(** The type of persistent state data tables. *)

val discard_everything : unit -> unit Lwt.t
(** Discard all services and persistent and volatile data for every scopes. *)

(*CCC missing ~secure? *)

val discard_all
  :  scope:Eliom_common.user_scope
  -> ?secure:bool
  -> unit
  -> unit Lwt.t
(** Discard all services and persistent and volatile data for one scope. *)

(*VVV missing: scope group *)

val discard_all_data
  :  ?persistent:bool
  -> scope:Eliom_common.user_scope
  -> ?secure:bool
  -> unit
  -> unit Lwt.t
(** Discard server side data for all clients, for the given scope.

    If the optional parameter [?persistent] is not present,
    both the persistent and volatile data will be removed.
 *)

(*VVV missing: scope group *)
(*VVV missing ~secure? *)

val discard_all_services
  :  scope:Eliom_common.user_scope
  -> ?secure:bool
  -> unit
  -> unit Lwt.t
(** Remove all services registered for clients for the given scope. *)

(*VVV missing: scope group *)
(*VVV missing ~secure? *)

module Ext : sig
  exception Wrong_scope
  (** Exception raised when you try to access a reference
      belonging to a scope different to the state's scope *)

  (** Type used to describe session timeouts *)
  type timeout =
    | TGlobal  (** see global setting *)
    | TNone  (** explicitly set no timeout *)
    | TSome of float  (** timeout duration in seconds *)

  (** These types are used to get or set information about browser
      or process cookies (like timeouts). *)

  type service_cookie_info
  type data_cookie_info
  type persistent_cookie_info

  type (+'a, +'b) state
  (** The type of states. The first parameter corresponds to the scope level
      and the second one to the kind of state (volatile or persistent data,
      or service state) *)

  val volatile_data_group_state
    :  ?scope:Eliom_common.session_group_scope
    -> string
    -> ([> `Session_group], [> `Data]) state
  (** [volatile_data_group_state ~scope n] returns the state corresponding to
      the group named [n] in scope [scope]. *)

  val persistent_data_group_state
    :  ?scope:Eliom_common.session_group_scope
    -> string
    -> ([> `Session_group], [> `Pers]) state
  (** Same for persistent data *)

  val service_group_state
    :  ?scope:Eliom_common.session_group_scope
    -> string
    -> ([> `Session_group], [> `Service]) state
  (** Same for services *)

  val current_volatile_data_state
    :  ?secure:bool
    -> ?scope:Eliom_common.user_scope
    -> unit
    -> ([< Eliom_common.user_level], [< `Data]) state
  (** [current_volatile_data_state ~scope] returns the state corresponding
      to scope [scope].
      Raises [Not_found] if not connected or if no session group is set,
      or [Eliom_common.Eliom_Session_expired] if a cookie was present but
      expired.
  *)

  val current_persistent_data_state
    :  ?secure:bool
    -> ?scope:Eliom_common.user_scope
    -> unit
    -> ([< Eliom_common.user_level], [< `Pers]) state Lwt.t
  (** Same for persistent data *)

  val current_service_state
    :  ?secure:bool
    -> ?scope:Eliom_common.user_scope
    -> unit
    -> ([< Eliom_common.user_level], [< `Service]) state
  (** Same for services *)

  val discard_state
    :  ?sitedata:Eliom_common.sitedata
    -> state:('a, 'b) state
    -> unit
    -> unit Lwt.t
  (** Discard external states.
      See {!fold_volatile_sub_states} for explanation about the [?sitedata]
      parameter.
  *)

  val fold_volatile_sub_states
    :  ?sitedata:Eliom_common.sitedata
    -> state:([< `Session_group | `Session], ([< `Data | `Service] as 'k)) state
    -> ('a -> ([< `Session | `Client_process], 'k) state -> 'a)
    -> 'a
    -> 'a
  (** Fold all sessions in a groups, or all client processes in a session.
      If you do not call the function during
      a request or during the initialisation phase of the Eliom module,
      you must provide the extra parameter [?sitedata],
      that you can get by calling {!Eliom_request_info.get_sitedata}
      during the initialisation phase of the Eliom module.
  *)

  val iter_volatile_sub_states
    :  ?sitedata:Eliom_common.sitedata
    -> state:([< `Session_group | `Session], ([< `Data | `Service] as 'k)) state
    -> (([< `Session | `Client_process], 'k) state -> unit)
    -> unit
  (** Iter on all sessions in a groups, or all client processes in a session.
      See {!fold_volatile_sub_states} for explanation about the [?sitedata]
      parameter.
  *)

  val fold_sub_states
    :  ?sitedata:Eliom_common.sitedata
    -> state:
         ( [< `Session_group | `Session]
         , ([< `Data | `Pers | `Service] as 'k) )
         state
    -> ('a -> ([< `Session | `Client_process], 'k) state -> 'a Lwt.t)
    -> 'a
    -> 'a Lwt.t
  (** Fold all sessions in a groups, or all client processes in a session
      (volatile and persistent).
      See {!fold_volatile_sub_states} for explanation about the [?sitedata]
      parameter.
  *)

  val iter_sub_states
    :  ?sitedata:Eliom_common.sitedata
    -> state:([< `Session_group | `Session], 'k) state
    -> (([< `Session | `Client_process], 'k) state -> unit Lwt.t)
    -> unit Lwt.t
  (** Iter on all sessions in a groups, or all client processes in a session
      (volatile and persistent).
      See {!fold_volatile_sub_states} for explanation about the [?sitedata]
      parameter.
  *)

  module Low_level : sig
    (** Functions to access table data.
        Prefer using Eliom references. *)

    val get_volatile_data
      :  state:([< `Session_group | `Session | `Client_process], [< `Data]) state
      -> table:'a volatile_table
      -> 'a
    (** Raises [Not_found] if no data in the table for the cookie. *)

    val get_persistent_data
      :  state:([< `Session_group | `Session | `Client_process], [< `Pers]) state
      -> table:'a persistent_table
      -> 'a Lwt.t
    (** Fails with lwt exception [Not_found]
        if no data in the table for the cookie. *)

    val set_volatile_data
      :  state:([< `Session_group | `Session | `Client_process], [< `Data]) state
      -> table:'a volatile_table
      -> 'a
      -> unit

    val set_persistent_data
      :  state:([< `Session_group | `Session | `Client_process], [< `Pers]) state
      -> table:'a persistent_table
      -> 'a
      -> unit Lwt.t
    (** Fails with lwt exception [Not_found]
        if no data in the table for the cookie. *)

    val remove_volatile_data
      :  state:([< `Session_group | `Session | `Client_process], [< `Data]) state
      -> table:'a volatile_table
      -> unit

    val remove_persistent_data
      :  state:([< `Session_group | `Session | `Client_process], [< `Pers]) state
      -> table:'a persistent_table
      -> unit Lwt.t
  end

  val get_service_cookie_info
    :  ?sitedata:Eliom_common.sitedata
    -> ([< Eliom_common.cookie_level], [`Service]) state
    -> service_cookie_info
  (** Get the infomration about cookies (timeouts, etc.).
      See {!fold_volatile_sub_states} for explanation about the [?sitedata]
      parameter.
  *)

  val get_volatile_data_cookie_info
    :  ?sitedata:Eliom_common.sitedata
    -> ([< Eliom_common.cookie_level], [`Data]) state
    -> data_cookie_info
  (** See {!fold_volatile_sub_states} for explanation about the [?sitedata]
      parameter.
  *)

  val get_persistent_cookie_info
    :  ([< Eliom_common.cookie_level], [`Pers]) state
    -> persistent_cookie_info Lwt.t

  val get_service_cookie_scope
    :  cookie:service_cookie_info
    -> Eliom_common.user_scope

  val get_volatile_data_cookie_scope
    :  cookie:data_cookie_info
    -> Eliom_common.user_scope

  val get_persistent_data_cookie_scope
    :  cookie:persistent_cookie_info
    -> Eliom_common.user_scope

  val set_service_cookie_timeout
    :  cookie:service_cookie_info
    -> float option
    -> unit

  val set_volatile_data_cookie_timeout
    :  cookie:data_cookie_info
    -> float option
    -> unit

  val set_persistent_data_cookie_timeout
    :  cookie:persistent_cookie_info
    -> float option
    -> unit Lwt.t

  val get_service_cookie_timeout : cookie:service_cookie_info -> timeout
  val get_volatile_data_cookie_timeout : cookie:data_cookie_info -> timeout

  val get_persistent_data_cookie_timeout
    :  cookie:persistent_cookie_info
    -> timeout

  val unset_service_cookie_timeout : cookie:service_cookie_info -> unit
  val unset_volatile_data_cookie_timeout : cookie:data_cookie_info -> unit

  val unset_persistent_data_cookie_timeout
    :  cookie:persistent_cookie_info
    -> unit Lwt.t

  val get_session_group_list : unit -> string list
  (** Returns a list containing the names of all session group
      that are available for this site. *)

  val iter_service_cookies : (service_cookie_info -> unit Lwt.t) -> unit Lwt.t
  (** Iterator on all active service cookies.
      [Lwt.pause] is called automatically after each iteration.
   *)

  val iter_volatile_data_cookies
    :  (data_cookie_info -> unit Lwt.t)
    -> unit Lwt.t
  (** Iterator on data cookies. [Lwt.pause] is called automatically
      after each iteration.
   *)

  val iter_persistent_data_cookies
    :  (persistent_cookie_info -> unit Lwt.t)
    -> unit Lwt.t
  (** Iterator on persistent cookies. [Lwt.pause] is called automatically
      after each iteration. *)

  val fold_service_cookies
    :  (service_cookie_info -> 'b -> 'b Lwt.t)
    -> 'b
    -> 'b Lwt.t
  (** Iterator on service cookies. [Lwt.pause] is called automatically
      after each iteration.
  *)

  val fold_volatile_data_cookies
    :  (data_cookie_info -> 'b -> 'b Lwt.t)
    -> 'b
    -> 'b Lwt.t
  (** Iterator on data cookies. [Lwt.pause] is called automatically
     after each iteration.
   *)

  val fold_persistent_data_cookies
    :  (persistent_cookie_info -> 'b -> 'b Lwt.t)
    -> 'b
    -> 'b Lwt.t
  (** Iterator on persistent cookies. [Lwt.pause] is called automatically
     after each iteration. *)

  (**/**)

  val untype_state : ('a, 'b) state -> ('c, 'd) state
end

(*****************************************************************************)
(**/**)

(** {3 Session data (deprecated interface)} *)

(** This is the low level interface (deprecated). Use now Eliom references. *)

(** The type used for getting data from a state. *)
type 'a state_data = No_data | Data_session_expired | Data of 'a

(** {4 In memory state data} *)

val create_volatile_table
  :  scope:Eliom_common.user_scope
  -> ?secure:bool
  -> unit
  -> 'a volatile_table
(** creates a table in memory where you can store the session data for
    all users. (low level)

    {e Warning: This functions must be called when the site
    information is available, that is, either
    during a request or during the initialisation phase of the site.
    Otherwise, it will raise the exception
    {!Eliom_common.Eliom_site_information_not_available}.
    If you are using static linking, you must delay the call to this function
    until the configuration file is read, using
    {!Eliom_service.register_eliom_module}. Otherwise you will also get
    this exception.}
 *)

val get_volatile_data : table:'a volatile_table -> unit -> 'a state_data
(** gets session data for the current session (if any).  (low level) *)

val set_volatile_data : table:'a volatile_table -> 'a -> unit
(** sets session data for the current session.  (low level) *)

val remove_volatile_data : table:'a volatile_table -> unit -> unit
(** removes session data for the current session
    (but does not close the session).
    If the session does not exist, does nothing.
    (low level)
 *)

(**/**)

(**/**)

(** {4 Persistent state data} *)

val create_persistent_table
  :  scope:Eliom_common.user_scope
  -> ?secure:bool
  -> string
  -> 'a persistent_table Lwt.t
(** creates a table on hard disk where you can store the session data
    for all users. It uses {!Ocsipersist}.  (low level) *)

val get_persistent_data
  :  table:'a persistent_table
  -> unit
  -> 'a state_data Lwt.t
(** gets persistent session data for the current persistent session (if any).
    (low level) *)

val set_persistent_data : table:'a persistent_table -> 'a -> unit Lwt.t
(** sets persistent session data for the current persistent session.
    (low level) *)

val remove_persistent_data : table:'a persistent_table -> unit -> unit Lwt.t
(** removes session data for the current persistent session
    (but does not close the session).
    If the session does not exist, does nothing.
    (low level)
 *)

(**/**)

(*
(** {3 Default timeouts} *)

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

(*****************************************************************************)

(**/**)

(** {3 Other low level functions}
    You probably don't need these functions. *)

val get_persistent_data_cookie
  :  cookie_scope:Eliom_common.cookie_scope
  -> ?secure:bool
  -> unit
  -> Eliom_common.Hashed_cookies.t option Lwt.t
(** returns the hashed value of the Eliom's cookies for one persistent session.
    Returns [None] is no session is active.
 *)

val get_service_cookie
  :  cookie_scope:Eliom_common.cookie_scope
  -> ?secure:bool
  -> unit
  -> Eliom_common.Hashed_cookies.t option
(** returns the value of Eliom's cookies for one service session.
    Returns [None] is no session is active.
 *)

val get_volatile_data_cookie
  :  cookie_scope:Eliom_common.cookie_scope
  -> ?secure:bool
  -> unit
  -> Eliom_common.Hashed_cookies.t option
(** returns the value of Eliom's cookies for one "volatile data" session.
    Returns [None] is no session is active.
 *)

(**/**)

(**/**)

(*****************************************************************************)
val number_of_service_cookies : unit -> int
val number_of_volatile_data_cookies : unit -> int
val number_of_tables : unit -> int
val number_of_table_elements : unit -> int list
val number_of_persistent_data_cookies : unit -> int Lwt.t
val number_of_persistent_tables : unit -> int
val number_of_persistent_table_elements : unit -> (string * int) list Lwt.t
(* Because of Dbm implementation, the result may be less than the expected
   result in some case (with a version of ocsipersist based on Dbm) *)

val get_global_table : unit -> Eliom_common.tables

val get_session_service_table
  :  sp:Eliom_common.server_params
  -> scope:Eliom_common.user_scope
  -> ?secure:bool
  -> unit
  -> Eliom_common.tables ref

val get_session_service_table_if_exists
  :  sp:Eliom_common.server_params
  -> scope:Eliom_common.user_scope
  -> ?secure:bool
  -> unit
  -> Eliom_common.tables ref

val create_volatile_table_during_session_
  :  scope:Eliom_common.user_scope
  -> secure:bool
  -> Eliom_common.sitedata
  -> 'a volatile_table
