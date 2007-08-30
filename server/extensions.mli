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
exception Ocsigen_403
exception Ocsigen_Is_a_directory
exception Ocsigen_malformed_url
exception Ocsigen_Internal_Error of string

exception Bad_config_tag_for_extension of string (* Try next extension *)
exception Error_in_config_file of string (* Stop with an error message *)

(*****************************************************************************)
(** type of URLs, without parameter *)
type url_path = string list
type current_url = string list
type current_dir = string list

(** Type used for cookies to set. The url_path option is for the path,
   The float option is the timestamp for the expiration date. 
*)
type cookies = 
  | Set of string list option * float option * (string * string) list
  | Unset of (string list option * string list)

type cookieslist = cookies list

val change_cookie : cookies -> 
  string list option * float option * (string * string) list

(* virtual hosts: *)
type virtual_host_part = Text of string * int | Wildcard
type virtual_hosts = ((virtual_host_part list) * int option) list

(** The files sent in the request *)
type file_info = {tmp_filename: string; (** Where the file is stored on the server*)
                  filesize: int64; (** Size, in bytes *)
                  original_filename: string (** Original file name *) }
(** Note that the files are cancelled once the request has been fulfilled *)

(** The request *)
type request_info = 
    {ri_url_string: string; (** full URL *)
     ri_url: Neturl.url;
     ri_method: Http_frame.Http_header.http_method; (** GET, POST, HEAD... *)
     ri_path_string: string; (** path of the URL *)
     ri_path: string list;   (** path of the URL *)
     ri_get_params_string: string option; (** string containing GET parameters *)
     ri_host: string option; (** Host field of the request (if any) *)
     ri_get_params: (string * string) list Lazy.t;  (** Association list of get parameters*)
     ri_post_params: (string * string) list Lwt.t Lazy.t; (** Association list of post parameters*)
     ri_files: (string * file_info) list Lwt.t Lazy.t; (** Files sent in the request *)
     ri_inet_addr: Unix.inet_addr;        (** IP of the client *)
     ri_ip: string;            (** IP of the client *)
     ri_remote_port: int;      (** Port used by the client *)
     ri_port: int;             (** Port of the request (server) *)
     ri_user_agent: string;    (** User_agent of the browser *)
     ri_cookies_string: string option Lazy.t; (** Cookies sent by the browser *)
     ri_cookies: (string * string) list Lazy.t; (** Cookies sent by the browser *)
     ri_ifmodifiedsince: float option;   (** if-modified-since field *)
     ri_ifunmodifiedsince: float option;   (** if-unmodified-since field *)
     ri_ifnonematch: string list;   (** if-none-match field ( * and weak entity tags not implemented) *)
     ri_ifmatch: string list option;   (** if-match field ( * not implemented) *)
     ri_content_type: string option; (** Content-Type HTTP header *)
     ri_content_length: int64 option; (** Content-Length HTTP header *)
     ri_referer: string option Lazy.t; (** Referer HTTP header *)
(* Not implemented. See if we can use Ocamlnet?
     ri_accept: ... list Lazy.t; (** Accept HTTP header *)
     ri_accept_charset: ... list Lazy.t; (** Accept-Charset HTTP header *)
     ri_accept_encoding: ... list Lazy.t; (** Accept-Encoding HTTP header *)
     ri_accept_language: ... list Lazy.t; (** Accept-Language HTTP header *)
*)
     ri_http_frame: Predefined_senders.Stream_http_frame.http_frame; (** The full http_frame *)
   }

(** If you force [ri_files] or [ri_post_params], the request is fully read,
   so it is not possible any more to read it from [ri_http_frame]
   (and vice versa).
 *)

(** The result of a page generation *)
type result =
    {res_cookies: cookieslist; (** The cookies to set (with optional paths) *)
     res_lastmodified: float option;      (** Last modified date *)
     res_etag: Http_frame.etag option;    (** ETag for the page *)
     res_code: int option;                (** HTTP code to send, if not 200 *)
     res_send_page: Predefined_senders.send_page_type; (** A function to send the content. Some are predefined in {{:Predefined_senders.html}[Predefined_senders]}, for example [Predefined_senders.send_xhtml_page]. *)
     res_headers: (string * string) list (** The HTTP headers you want to add. For example {!Predefined_senders.nocache_headers} if you don't want the page to be cached (dynamic pages). *);
     res_charset: string option;          (** Charset used by the page *)
   }


(** Charset for each directory *)
type charset_tree_type

(** The result given by the extension (filter or page generation) *)
type answer =
  | Ext_found of result  (** OK stop! I found the page *)
  | Ext_not_found of exn (** Page not found. Try next extension.
                            The exception is usally Ocsigen_404, 
                            but may be for ex Ocsigen_403 (forbidden)
                            if you want another extension to try after a 403
                          *)
  | Ext_continue_with of request_info * cookieslist
        (** Used to modify the request before giving it to next extension ;
           The extension may want to set cookies ; in that case, put the new
           cookies in the list (and possibly the path in the string list
           option of cookieslist), 
           and possibly in the ri_cookies field
           of request_info if you want them to be seen by the following
           extension. *)
  | Ext_retry_with of request_info * cookieslist
        (** Used to retry all the extensions with a new request_info ;
           May set cookies (idem) *)

(** We register for each extension four functions:
   - a function that will be called for each
   virtual server, generating two functions:
     - one that will be called to generate the pages
       (from charset (string option) and request_info)
     - one to parse the configuration file
   - a function that will be called at the beginning 
   of the initialisation phase 
   - a function that will be called at the end of the initialisation phase 
   of the server
   - a function that will create an error message from the exceptions
   that may be raised during the initialisation phase, and raise again
   all other exceptions
 *)
module R : sig
  val register_extension :
      (virtual_hosts -> 
        (string option -> request_info -> answer Lwt.t) * 
	  (string list ->
            Simplexmlparser.xml -> 
              unit)) *
      (unit -> unit) * 
      (unit -> unit) *
      (exn -> string) -> unit
end


(** While loading an extension, 
    get the configuration tree between <dynlink></dynlink>*)
val get_config : unit -> Simplexmlparser.xml list


(** Parsing URLs. 
   This allows to modify the URL in the request_info.
   (to be used for example with Ext_retry_with or Ext_continue_with)
 *)
val ri_of_url : string -> request_info -> request_info


(**/**)

val parse_url : string ->
  string * Neturl.url * string list * string option *
    (string * string) list Lazy.t

val create_virthost : 
    virtual_hosts ->
      ((request_info -> 
        (answer * cookieslist) Lwt.t) * 
	 (string list ->
           Simplexmlparser.xml ->
             unit)) * 
        (string option -> string list -> unit)

val set_virthosts : (virtual_hosts * 
                       (request_info -> 
                         (answer * cookieslist) Lwt.t)) list -> unit

val get_virthosts : unit -> (virtual_hosts * 
                               (request_info -> 
                                 (answer * cookieslist) Lwt.t)) list

val add_virthost : (virtual_hosts * 
                      (request_info -> 
                        (answer * cookieslist) Lwt.t)) -> unit

val do_for_host_matching : 
    string option ->
      int ->
        ((virtual_host_part list * int option) list *
           (request_info -> 
             (answer * cookieslist) Lwt.t))
          list -> request_info -> (result * cookieslist) Lwt.t

(** Profiling *)
val get_number_of_connected : unit -> int
val get_number_of_connected : unit -> int


(** Server internal functions: *)
val incr_connected : unit -> unit
val decr_connected : unit -> unit

val during_initialisation : unit -> bool
val start_initialisation : unit -> unit
val end_initialisation : unit -> unit
val get_numberofreloads : unit -> int

val get_init_exn_handler : unit -> exn -> string

val set_config : Simplexmlparser.xml list -> unit
