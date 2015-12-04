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

(** {b Please read the {% <<a_manual chapter="server-state" | Eliom
    manual >>%} before this page to learn how server side state
    works. }

    {% <<outline| <<header| **Table of contents** >> >>%}

*)

(** {2 Managing the state of an application} *)

(** {3 Closing sessions, removing state data and services} *)


(** {3 State status} *)

(** The following functions return the current state of the state for
    a given scope:

    - [Alive_state] means that data has been recorded for this scope
    - [Empty_state] means that there is no data for this scope
    - [Expired_state] means that data for this scope has been removed
      because the timeout has been reached.

    The default scope is [`Session].
*)

type state_status = Alive_state | Empty_state | Expired_state

val service_state_status :
  scope:[< Eliom_common.user_scope ] ->
  ?secure:bool ->
  unit -> state_status

val volatile_data_state_status :
  scope:[< Eliom_common.user_scope ] ->
  ?secure:bool ->
  unit -> state_status

val persistent_data_state_status :
  scope:[< Eliom_common.user_scope ] ->
  ?secure:bool ->
  unit -> state_status Lwt.t

(** {3 User cookies}

    If you want to store a client-side state, and ask the browser to
    send it back with each request, you can set manually your own
    cookies.  Usual cookies correspond to scope [`Session] (that is,
    one browser).  The browser send them with each request to the same
    Web site.  But Eliom also implements client-side process cookies
    (scope [`Client_process]), that behave in the same way, but for
    one instance of the client-side Eliom program (if there is one).

    Cookies can be limited to a subsite using the [?path] optional
    parameter. This path is relative to the main path of your Web
    site.  (It is not possible to set a cookie for a subsite larger
    than your current Web site).

    Cookies can have an expiration date, specified (in seconds since
    the 1st of January 1970) in the optional parameter [?exp] (see how
    to set default expiration dates below).

    Secure cookies are sent by the browser only with HTTPS (default:
    [false]). *)

(** Ask the browser to record a cookie. *)
val set_cookie :
  ?cookie_level:Eliom_common.cookie_level ->
  ?path:string list ->
  ?exp:float ->
  ?secure:bool -> name:string -> value:string -> unit -> unit

(** Ask the browser to remove a cookie. *)
val unset_cookie :
  ?cookie_level:Eliom_common.cookie_level ->
  ?path:string list ->
  name:string -> unit -> unit

(** {2 Session groups} *)
module Group : Eliom_state_sigs.GROUP

(** Sets the mask for subnet (IPV4). *)
val set_ipv4_subnet_mask :
  ?override_configfile:bool -> int -> unit

(** Sets the mask for subnet (IPV6). *)
val set_ipv6_subnet_mask :
  ?override_configfile:bool -> int -> unit

(** {2 Expiration of cookies and timeouts} *)

(** {3 Cookie expiration} *)
module Expire : Eliom_state_sigs.EXPIRE

(** {3 Global configuration of state timeouts} *)
module Timeout : Eliom_state_sigs.TIMEOUT

(** {2 Administrating server side state} *)

(** {e Warning: Most these functions must be called when the site
    information is available, that is, either during a request or
    during the initialisation phase of the site.  Otherwise, it will
    raise the exception
    {!Eliom_common.Eliom_site_information_not_available}.  If you are
    using static linking, you must delay the call to this function
    until the configuration file is read, using
    {!Eliom_service.register_eliom_module}. Otherwise you will also
    get this exception.}  *)

(** The type of (volatile) state data tables. *)
type 'a volatile_table
(** The type of persistent state data tables. *)
type 'a persistent_table

module Discard : Eliom_state_sigs.DISCARD

module Ext : sig

  include Eliom_state_sigs.EXT

  module Low_level : sig
    (** Functions to access table data. Prefer using Eliom
        references. *)

    (** Raises [Not_found] if no data in the table for the cookie. *)
    val get_volatile_data :
      state:([< `Session_group | `Session | `Client_process ],
             [< `Data ]) state ->
      table:'a volatile_table ->
      'a

    (** Fails with lwt exception [Not_found] if no data in the table
        for the cookie. *)
    val get_persistent_data :
      state:([< `Session_group | `Session | `Client_process ],
             [< `Pers ]) state ->
      table:'a persistent_table ->
      'a Lwt.t

    val set_volatile_data :
      state:([< `Session_group | `Session | `Client_process ],
             [< `Data ]) state ->
      table:'a volatile_table ->
      'a -> unit

    (** Fails with lwt exception [Not_found] if no data in the table
        for the cookie. *)
    val set_persistent_data :
      state:([< `Session_group | `Session | `Client_process ],
             [< `Pers ]) state ->
      table:'a persistent_table ->
      'a -> unit Lwt.t

    val remove_volatile_data :
      state:([< `Session_group | `Session | `Client_process ],
             [< `Data ]) state ->
      table:'a volatile_table -> unit

    val remove_persistent_data :
      state:([< `Session_group | `Session | `Client_process ],
             [< `Pers ]) state ->
      table:'a persistent_table -> unit Lwt.t

  end

end

(**/**)
(** {3 Session data (deprecated interface)} *)

(** This is the low level interface (deprecated). Use now Eliom references. *)

(** The type used for getting data from a state. *)
type 'a state_data =
  | No_data
  | Data_session_expired
  | Data of 'a

(** {4 In memory state data} *)

(** creates a table in memory where you can store the session data for
    all users. (low level)

    {e Warning: This functions must be called when the site
    information is available, that is, either during a request or
    during the initialisation phase of the site.  Otherwise, it will
    raise the exception
    {!Eliom_common.Eliom_site_information_not_available}.  If you are
    using static linking, you must delay the call to this function
    until the configuration file is read, using
    {!Eliom_service.register_eliom_module}. Otherwise you will also
    get this exception.}  *)
val create_volatile_table :
  scope:Eliom_common.user_scope ->
  ?secure:bool ->
  unit -> 'a volatile_table

(** gets session data for the current session (if any).  (low
    level) *)
val get_volatile_data :
  table:'a volatile_table ->
  unit ->
  'a state_data

(** sets session data for the current session.  (low level) *)
val set_volatile_data :
  table:'a volatile_table ->
  'a ->
  unit

(** Removes session data for the current session (but does not close
    the session). If the session does not exist, does nothing. (low
    level) *)
val remove_volatile_data :
  table:'a volatile_table ->
  unit ->
  unit
(**/**)


(**/**)

(** {4 Persistent state data} *)
(** Creates a table on hard disk where you can store the session data
    for all users. It uses {!Ocsipersist}. (low level) *)
val create_persistent_table :
  scope:Eliom_common.user_scope ->
  ?secure:bool ->
  string -> 'a persistent_table

(** Gets persistent session data for the current persistent session
    (if any). (low level) *)
val get_persistent_data :
  table:'a persistent_table ->
  unit ->
  'a state_data Lwt.t

(** Sets persistent session data for the current persistent session.
    (low level) *)
val set_persistent_data :
  table:'a persistent_table ->
  'a ->
  unit Lwt.t

(** Removes session data for the current persistent session (but does
    not close the session). If the session does not exist, does
    nothing. (low level) *)
val remove_persistent_data :
  table:'a persistent_table ->
  unit ->
  unit Lwt.t
(**/**)

(**/**)
(** {3 Other low level functions}
    You probably don't need these functions. *)

(** Returns the value of the Eliom's cookies for one persistent
    session. Returns [None] is no session is active. *)
val get_persistent_data_cookie :
  cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  unit -> string option Lwt.t

(** Returns the value of Eliom's cookies for one service session.
    Returns [None] is no session is active. *)
val get_service_cookie :
  cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  unit -> string option

(** Returns the value of Eliom's cookies for one "volatile data"
    session. Returns [None] is no session is active. *)
val get_volatile_data_cookie :
  cookie_scope:Eliom_common.cookie_scope ->
  ?secure:bool ->
  unit -> string option
(**/**)

(**/**)

val number_of_service_cookies : unit -> int

val number_of_volatile_data_cookies : unit -> int

val number_of_tables : unit -> int

val number_of_table_elements : unit -> int list

val number_of_persistent_data_cookies : unit -> int Lwt.t

val number_of_persistent_tables : unit -> int

val number_of_persistent_table_elements : unit -> (string * int) list Lwt.t

(* Because of Dbm implementation, the result may be less than the
   expected result in some case (with a version of ocsipersist based
   on Dbm) *)
val get_global_table : unit -> Eliom_common.tables

val get_session_service_table :
  sp:Eliom_common.server_params ->
  scope:Eliom_common.user_scope ->
  ?secure:bool ->
  unit ->
  Eliom_common.tables ref

val get_session_service_table_if_exists :
  sp:Eliom_common.server_params ->
  scope:Eliom_common.user_scope ->
  ?secure:bool ->
  unit ->
  Eliom_common.tables ref

val create_volatile_table_during_session_ :
  scope:Eliom_common.user_scope ->
  secure:bool ->
  Eliom_common.sitedata ->
  'a volatile_table
