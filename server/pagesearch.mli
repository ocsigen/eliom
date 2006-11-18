(* Ocsigen
 * http://www.ocsigen.org
 * Module pagesearch.mli
 * Copyright (C) 2005 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

exception Ocsigen_Wrong_parameter
exception Ocsigen_404
exception Ocsigen_duplicate_registering of string
exception Ocsigen_register_for_session_outside_session
exception Ocsigen_page_erasing of string
exception Ocsigen_Is_a_directory
exception Ocsigen_malformed_url
exception Ocsigen_service_or_action_created_outside_site_loading
exception Ocsigen_there_are_unregistered_services of string
exception Ocsigen_error_while_loading of string
exception Ocsigen_Typing_Error of (string * exn) list
exception Ocsigen_Internal_Error of string

val ocsigen_suffix_name : string

(*****************************************************************************)
(*****************************************************************************)
(* Tables of services (global and session tables)                            *)
(* Store and load pages, static or dynamic                                   *)
(* Module loading                                                            *)
(*****************************************************************************)
(*****************************************************************************)

(** type of URL, without parameter *)
type url_path = string list
type current_url = string list
type current_dir = string list

type fileinfo = {tmp_filename: string;
                 filesize: int64;
                 original_filename: string}

type 'a server_params1 = {full_url: string;
                          hostname: string option;
                          user_agent: string;
                          ip: Unix.inet_addr;
                          get_params: (string * string) list;
                          post_params: (string * string) list;
                          files: (string * fileinfo) list;
                          current_url: current_url;
                          current_dir: current_dir;
                          session_table: 'a ref
                        }
      
type 'a server_params2 = url_path * 'a server_params1
      
(** state is a parameter to differenciate
   several instances of the same URL.
   (for internal use)
 *)
type internal_state = int


(*****************************************************************************)
(*
type tables
type cookiestable
type pages_tree =
    Ocsimisc.static_dir ref (* static pages *)
      * tables (* global tables of continuations/actions *)
      * cookiestable (* session tables *)


      
val make_server_params :
    current_dir -> tables ref -> current_url -> string ->
      (string * string) list -> (string * string) list -> 
        string -> Unix.inet_addr -> tables server_params1

            
val are_empty_tables : tables -> bool
val find_service :
    tables ->
      tables ref * 
        current_url * internal_state option * (string * string) list *
        (string * string) list * string * Unix.inet_addr * string -> 
          (Sender_helpers.send_page_type * 
             Sender_helpers.create_sender_type * url_path) Lwt.t
val find_action :
    tables -> string -> (tables server_params1 -> unit Lwt.t) * url_path
*)



(*****************************************************************************)
type tables
type cookiestable
type pages_tree = 
    Ocsimisc.static_dir ref (* static pages *)
      * tables (* global tables of continuations/actions *)
      * cookiestable (* session tables *)
type session_table = tables

type page_table_key =
    {prefix:bool;
     state: internal_state option}

val empty_tables : unit -> tables

val add_service : tables ->
  current_dir -> bool -> url_path ->
    Sender_helpers.create_sender_type ->
      page_table_key * (int * (tables server_params2 -> 
        Sender_helpers.send_page_type Lwt.t)) -> unit

val add_action :
    tables -> current_dir
      -> string -> (tables server_params1 -> unit Lwt.t) -> unit

(** Type of http parameters *)
type server_params = session_table server_params1


(*****************************************************************************)
(** return a page from a service and parameters *)
val get_page :
    string * string * internal_state option *
    (current_url * string option * 
       (string * string) list *
       (string * string) list * 
       (string * fileinfo) list * string) ->
         int ->
         Unix.sockaddr -> string option -> 
           ((string option * 
               Sender_helpers.send_page_type *
               Sender_helpers.create_sender_type * string) * 
              float option * Http_frame.etag option) Lwt.t

val make_action :
    string ->
      (string * string) list ->
        string * string * internal_state option * 
          (current_url * string option *
             (string * string) list *
             (string * string) list * 
             (string * fileinfo) list * string) ->
               Unix.sockaddr -> string option -> 
                 (string option * string) Lwt.t


val state_param_name : string
val action_prefix : string
val action_name : string
val action_reload : string

(** Profiling *)
val number_of_sessions : unit -> int
val get_number_of_connected : unit -> int

val get_number_of_connected : unit -> int


(** Server internal functions: *)
(** loads a module in the server *)
val load_ocsigen_module : Ocsimisc.virtual_hosts -> 
  (url_path * (string list * string option)) list -> unit
val incr_connected : unit -> unit
val decr_connected : unit -> unit

val get_current_hostdir : unit -> pages_tree * url_path
val add_unregistered : string list * int -> unit
val remove_unregistered : string list * int -> unit

val global_register_allowed : unit -> bool
val during_initialisation : unit -> bool
val end_initialisation : unit -> unit
