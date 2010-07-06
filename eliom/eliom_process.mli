(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
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


(** {3 Application and process id} *)

(** returns the name of the application running client side, if any. *)
val get_application_name : sp:Eliom_sessions.server_params -> string option

(** returns the unique identifier of the application instance running
    client side, if any. *)
val get_process_id : sp:Eliom_sessions.server_params -> string option

(*
(** {3 Timeouts} *)

(** Sets the timeout for processes (server side) *)
&val set_global_process_timeout :
  ?sp:server_params ->
  ?recompute_expdates:bool -> 
  ?override_configfile:bool ->
  float option -> unit

(** sets the timeout for processes (server side) for one user,
   in seconds. [None] = no timeout *)
&val set_process_timeout : sp:server_params -> float option -> unit

(** remove the process timeout for one user (and turn back to the default). *)
&val unset_process_timeout : sp:server_params -> unit -> unit

(** Sets the maximum number of processes per session. *)
&val set_default_max_processes_per_session :
  ?sp:server_params -> ?override_configfile:bool -> int -> unit


(** {3 Volatile process tables} *)

type 'a process_data =
  | PNo_data
  | PProcess_closed
  | PData of 'a

(* The type of volatile (in memory) process tables
   where you can store data for each process. *)
type 'a process_table

(** creates a volatile process table. *)
&val create_process_table : ?sp:server_params -> unit -> 'a process_table

(** gets data for the current process (if any). *)
&val get_process_data : 
  table:'a process_table -> 
  sp:server_params -> 
  unit -> 
  'a process_data

(** sets data for the current process. *)
&val set_process_data : 
  table:'a process_table -> 
  sp:server_params -> 
  'a ->
  unit

(** removes data for the current process.
   If the process does not exist, does nothing.
 *)
&val remove_process_data : 
  table:'a process_table -> 
  sp:server_params -> 
  unit


(** {3 Persistent process tables} *)

(** The type of persistent process data tables. *)
&type 'a persistent_process_table

(** creates a table on hard disk where you can store the data for
   all processes. It uses {!Ocsipersist}. *)
&val create_persistent_process_table : string -> 'a persistent_process_table

(** gets persistent data for the current process (if any) *)
&val get_persistent_process_data : 
  table:'a persistent_process_table ->
  sp:server_params ->
  unit ->
  'a process_data Lwt.t

(** sets persistent data for the current process *)
&val set_persistent_process_data :
  table:'a persistent_process_table ->
  sp:server_params ->
  'a ->
  unit Lwt.t

(** removes persistent data for the current process.
   If the session does not exist, does nothing.
 *)
&val remove_persistent_process_data :
  table:'a persistent_process_table ->
  sp:server_params ->
  unit ->
  unit Lwt.t


(** {3 Closing processes on server side} *)

(** Close current volatile process (if any) on server side by removing
    all server side volatile data for that process.
 *)
&val close_process :
  sp:server_params ->
  unit

(** Close current persistent processes (if any) on server side by
   removing all server side persistent data for that process. *)
&val close_persistent_process :
  sp:server_params ->
  unit Lwt.t

&Fermer les sessions doit fermer les processus associés ?

&Administration des processus et des sessions :
 - Faut-il la même chose que pour les session ?
 - Est-ce que utiliser les trucs des sessions fonctionne aussi avec les
   processus ?

*)


(**/**)
val get_content_only : sp:Eliom_sessions.server_params -> bool

val appl_name_key : string option Polytables.key
val process_key : string option Polytables.key
