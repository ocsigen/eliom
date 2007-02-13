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
(*****************************************************************************)
(*****************************************************************************)
(* Tables of services (global and session tables)                            *)
(* Store and load dynamic pages                                              *)
(*****************************************************************************)
(*****************************************************************************)

open Lwt
open Ocsimisc

exception Ocsigen_404
exception Ocsigen_Is_a_directory
exception Ocsigen_malformed_url
exception Ocsigen_Internal_Error of string

exception Bad_config_tag_for_extension of string
exception Error_in_config_file of string

(*****************************************************************************)
(** type of URLs, without parameter *)
type url_path = string list
type current_url = string list
type current_dir = string list

(** The files sent in the request *)
type file_info = {tmp_filename: string; (** Where the file is stored on the server*)
                  filesize: int64; (** Size, in bytes *)
                  original_filename: string (** Original file name *) }
(** Note that the files are cancelled once the request has been fulfilled *)

(** The request *)
type request_info = 
    {ri_path_string: string; (** path of the URL *)
     ri_path: string list;   (** path of the URL *)
     ri_params: string;      (** string containing parameters *)
     ri_host: string option; (** Host field of the request (if any) *)
     ri_get_params: (string * string) list;  (** Association list of get parameters*)
     ri_post_params: (string * string) list; (** Association list of post parameters*)
     ri_files: (string * file_info) list; (** Files sent in the request *)
     ri_inet_addr: Unix.inet_addr;        (** IP of the client *)
     ri_ip: string;            (** IP of the client *)
     ri_port: int;             (** Port of the request *)
     ri_user_agent: string;    (** User_agent of the browser *)
     ri_cookies: (string * string) list; (** Cookie sent by the browser *)
     ri_ifmodifiedsince: float option;   (** if-modified-since field *)
     ri_http_frame: Predefined_senders.Stream_http_frame.http_frame (** The full http_frame *)}

(** The result of a page generation *)
type result =
    {res_cookies: (string * string) list; (** The cookies to set *)
     res_path: string;                    (** The path for the cookies *)
     res_lastmodified: float option;      (** Last modified date *)
     res_etag: Http_frame.etag option;    (** ETag for the page *)
     res_code: int option;                (** HTTP code to send, if not 200 *)
     res_send_page: Predefined_senders.send_page_type; (** A function from {{:Predefined_senders.html}[Predefined_senders]}, for example [Predefined_senders.send_xhtml_page] *)
     res_create_sender: Predefined_senders.create_sender_type (** A function from {{:Predefined_senders.html}[Predefined_senders]}, for example [Predefined_senders.create_xhtml_sender] *);
     res_charset: string option
   }


(** Charset for each directory *)
type charset_tree_type

(** The result given by the extension (filter or page generation) *)
type answer =
    Ext_found of result (** OK stop! I found the page *)
  | Ext_not_found (** Page not found. Try next extension. *)
  | Ext_continue_with of request_info (** Used to modify the request 
                               before giving it to next extension (filter) *)

(** We register for each extension four functions:
   - a function that will be called for each
   virtual server, generating two functions:
     - one that will be called to generate the pages
     - one to parse the configuration file
   - a function that will be called at the beginning 
   of the initialisation phase 
   - a function that will be called at the end of the initialisation phase 
   of the server
   - a function that will create an error message from the exceptions
   that may be raised during the initialisation phase, and raise again
   all other exceptions
 *)
val register_extension :
    (virtual_hosts -> 
      (string option -> request_info -> answer Lwt.t) * 
	(string list ->
          Simplexmlparser.ExprOrPatt.texprpatt -> 
            unit)) *
    (unit -> unit) * 
    (unit -> unit) *
    (exn -> string) -> unit

(**/**)
val create_virthost : 
    Ocsimisc.virtual_hosts ->
      ((request_info -> answer Lwt.t) * 
	 (string list ->
           Simplexmlparser.ExprOrPatt.texprpatt ->
             unit)) * 
        (string option -> string list -> unit)

val set_virthosts : (Ocsimisc.virtual_hosts * 
                       (request_info -> answer Lwt.t)) list -> unit

val get_virthosts : unit -> (Ocsimisc.virtual_hosts * 
                               (request_info -> answer Lwt.t)) list

val add_virthost : (Ocsimisc.virtual_hosts * 
                      (request_info -> answer Lwt.t)) -> unit

val do_for_host_matching : 
    string option ->
      int ->
        ((Ocsimisc.virtual_host_part list * int option) list *
           ('a -> answer Lwt.t))
          list -> 'a -> result Lwt.t

(** Profiling *)
val get_number_of_connected : unit -> int
val get_number_of_connected : unit -> int


(** Server internal functions: *)
val incr_connected : unit -> unit
val decr_connected : unit -> unit

val during_initialisation : unit -> bool
val start_initialisation : unit -> unit
val end_initialisation : unit -> unit

val get_init_exn_handler : unit -> exn -> string
