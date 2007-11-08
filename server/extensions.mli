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

(** Writing extensions for Ocsigen                                           *)

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
(** The type of URL paths. [["plop";"plip"]] corresponds to [plop/plip]. *)
type url_path = string list

val string_of_url_path : url_path -> string


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
     ri_sub_path: string list;   (** path of the URL (only part concerning the site) *)
     ri_full_path: string list;   (** full path of the URL *)
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
     ri_cookies: string Http_frame.Cookievalues.t Lazy.t;  (** Cookies sent by the browser *)
     ri_ifmodifiedsince: float option;   (** if-modified-since field *)
     ri_ifunmodifiedsince: float option;   (** if-unmodified-since field *)
     ri_ifnonematch: string list;   (** if-none-match field ( * and weak entity tags not implemented) *)
     ri_ifmatch: string list option;   (** if-match field ( * not implemented) *)
     ri_content_type: string option; (** Content-Type HTTP header *)
     ri_content_length: int64 option; (** Content-Length HTTP header *)
     ri_referer: string option Lazy.t; (** Referer HTTP header *)

     ri_accept: ((string option * string option) * float option * (string * string) list) list Lazy.t; (** Accept HTTP header. For example [(Some "text", None)] means ["text/*"]. The float is the "quality" value, if any. The last association list is for other extensions. *)
     ri_accept_charset: (string option * float option) list Lazy.t; (** Accept-Charset HTTP header. [None] for the first value means "*". The float is the "quality" value, if any. *)
     ri_accept_encoding: (string option * float option) list Lazy.t; (** Accept-Encoding HTTP header. [None] for the first value means "*". The float is the "quality" value, if any. *)
     ri_accept_language: (string * float option) list Lazy.t; (** Accept-Language HTTP header. The float is the "quality" value, if any. *)


     ri_http_frame: Http_frame.t; (** The full http_frame *)
   }

(** If you force [ri_files] or [ri_post_params], the request is fully read,
   so it is not possible any more to read it from [ri_http_frame]
   (and vice versa).
 *)



(** The result given by the extension (filter or page generation) *)
type answer =
  | Ext_found of Http_frame.result  (** OK stop! I found the page. *)
  | Ext_not_found of exn (** Page not found. Try next extension.
                            The exception is usally Ocsigen_404, 
                            but may be for ex Ocsigen_403 (forbidden)
                            if you want another extension to try after a 403
                          *)
  | Ext_stop of exn      (** Page forbidden. Do not try next extension, but
                            try next site. If you do not want to try next site
                            send an Ext_found with an error code.
                            The exception is usally Ocsigen_403.
                          *)
  | Ext_continue_with of request_info
        (** Used to modify the request before giving it to next extension *)
  | Ext_retry_with of request_info
        (** Used to retry all the extensions with a new request_info *)


type extension =
  | Page_gen of (string -> request_info -> answer Lwt.t)
  | Filter of (string -> request_info -> Http_frame.result -> answer Lwt.t)
(** For each <site> tag in the configuration file, 
    you can set the extensions you want. They take a charset (type [string]),
    a [request_info]. If it is a filter, it takes the result of the previous
    extension. And they all return an [answer].
 *)


(** 
   For each extension generating pages, we register four functions:
   - a function taking 
   {ul
     {- the name of the virtual <host>}}
     that will be called for each <host>, 
     and that will generate a function taking:
   {ul
     {- the path attribute of a <site> tag
     that will be called for each <site>, 
     and that will generate a function taking:}}
   {ul
     {- an item of the config file
     that will be called on each tag inside <site> and:}
   {ul
     {- raise [Bad_config_tag_for_extension] if it does not recognize that tag}
     {- return something of type [extension] (filter or page generator)}
   - a function that will be called at the beginning 
   of the initialisation phase (each time the config file is reloaded)
   (Note that the extensions are not reloaded)
   - a function that will be called at the end of the initialisation phase 
   of the server
   - a function that will create an error message from the exceptions
   that may be raised during the initialisation phase, and raise again
   all other exceptions
 *)
val register_extension :
    (virtual_hosts -> url_path -> string option -> 
      Simplexmlparser.xml -> extension) *
    (unit -> unit) * (unit -> unit) * (exn -> string) -> unit
        

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

val parse_site : virtual_hosts -> 
  url_path -> string option -> Simplexmlparser.xml list -> extension list

val set_sites : (virtual_hosts * url_path * string option * extension list) list
  -> unit
                        
val get_sites : unit -> 
  (virtual_hosts * url_path * string option * extension list) list

val add_site : (virtual_hosts * url_path * string option * extension list) -> unit

val do_for_site_matching :
    string option ->
    int ->
    request_info -> Http_frame.result Lwt.t

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
