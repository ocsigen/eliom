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
open Ocsigen_lib

exception Ocsigen_http_error of (Ocsigen_http_frame.cookieset * int)

exception Bad_config_tag_for_extension of string (** Try next extension *)
exception Error_in_config_file of string (** Stop with an error message *)

exception Ocsigen_Internal_Error of string

(** Error in an user config file. Stop with an error message *)
exception Error_in_user_config_file of string

(** A configuration option cannot be overriden in userconf files *)
exception Not_overridable of string

val badconfig : ('a, unit, string, 'b) format4 -> 'a
(** Convenient function for raising Error_in_config_file exceptions with
    a sprintf-formatted argument. *)

(*****************************************************************************)
(** The type of URL paths. [["plop";"plip"]] corresponds to [plop/plip]. *)
type url_path = string list

val string_of_url_path : encode:bool -> url_path -> string


(* virtual hosts: *)
type virtual_host_part = Text of string * int | Wildcard
type virtual_hosts = ((virtual_host_part list) * int option) list


(*****************************************************************************)
(** Handling of charset *)

type charset = string

(** Association between extensions and charset *)
type charset_assoc

val empty_charset_assoc: charset_assoc

val find_charset:
  ?default:charset -> charset_assoc:charset_assoc -> extension:string -> charset

val find_charset_file:
  ?default:charset -> charset_assoc:charset_assoc -> filename:string -> charset

val update_charset:
  charset_assoc:charset_assoc -> extension:string -> charset:charset -> charset_assoc


(*****************************************************************************)

(** Configuration options, passed to (and modified by) extensions *)
type config_info = {
  default_hostname: string;
  default_httpport: int;
  default_httpsport: int;

  mime_assoc: Mime.mime_assoc;
  default_mime_type: Mime.mime_type;

  charset_assoc : charset_assoc;
  default_charset : charset;

  (** Default name to use as index file when a directory is requested.
      Use [None] if no index should be tried. The various indexes
      are tried in the given order. If no index is specified,
      or the index does not exists, the content of the directory
      might be listed, according to [list_directry_content] *)
  default_directory_index : string list;

  (** Should the list of files in a directory be displayed
      if there is no index in this directory ? *)
  list_directory_content : bool;

  (** Should symlinks be followed when accessign a local file? *)
  follow_symlinks: follow_symlink;

  do_not_serve_404: string list;
  do_not_serve_403: string list;

}
and follow_symlink =
  | DoNotFollowSymlinks (** Never follow a symlink *)
  | FollowSymlinksIfOwnerMatch (** Follow a symlink if the symlink and its
                          target have the same owner *)
  | AlwaysFollowSymlinks (** Always follow symlinks *)


(*****************************************************)


(** The files sent in the request *)
type file_info = {tmp_filename: string; (** Where the file is stored on the server*)
                  filesize: int64; (** Size, in bytes *)
                  raw_original_filename: string;
                  (** Original file name, as given by the client. *)
                  original_basename: string (** Original file name *) }
(** Note that the files are cancelled once the request has been fulfilled *)

(** Only IE is known to make [raw_original_filename] and
    [original_basename] differ, as it sends the full original path
    of uploaded files.  In all cases, [original_basename] is the
    basename of the file. More precisely, it is the part of the
    filename after the last [/] or [\ ], if any, or ["none"] if one of
    these characters is the last one. You should probably never use
    [raw_original_filename]. *)

type client
(** A value of this type represents the client who did the request. *)

val client_id : client -> int
(** Returns the id number of the connection *)

val client_connection : client -> Ocsigen_http_com.connection
(** Returns the connection *)

(** The request *)
type request_info =
    {ri_url_string: string; (** full URL *)
     ri_url: Neturl.url;
     ri_method: Ocsigen_http_frame.Http_header.http_method; (** GET, POST, HEAD... *)
     ri_protocol: Ocsigen_http_frame.Http_header.proto; (** HTTP protocol used by client (1.0 or 1.1) *)
     ri_ssl: bool; (** true if HTTPS, false if HTTP *)
     ri_full_path_string: string; (** full path of the URL *)
     ri_full_path: string list;   (** full path of the URL *)
     ri_original_full_path_string: string;   (** full path of the URL, as first sent by the client. Should not be changed by extensions, even rewritemod. It is used to create relative links. *)
     ri_original_full_path: string list;   (** full path of the URL, as first sent by the client. See below. *)
     ri_sub_path: string list;   (** path of the URL (only part concerning the site) *)
     ri_sub_path_string: string;   (** path of the URL (only part concerning the site) *)
     ri_get_params_string: string option; (** string containing GET parameters *)
     ri_host: string option; (** Host field of the request (if any) *)
     ri_get_params: (string * string) list Lazy.t;  (** Association list of get parameters *)
     ri_initial_get_params: (string * string) list Lazy.t;  (** Association list of get parameters, as sent by the browser (must not be modified by extensions) *)
     ri_post_params: (string * string) list Lwt.t Lazy.t; (** Association list of post parameters *)
     ri_files: (string * file_info) list Lwt.t Lazy.t; (** Files sent in the request *)
     ri_remote_inet_addr: Unix.inet_addr; (** IP of the client *)
     ri_remote_ip: string;            (** IP of the client *)
     ri_remote_ip_parsed: ip_address Lazy.t;    (** IP of the client, parsed *)
     ri_remote_port: int;      (** Port used by the client *)
     ri_server_port: int;      (** Port of the request (server) *)
     ri_user_agent: string;    (** User_agent of the browser *)
     ri_cookies_string: string option Lazy.t; (** Cookies sent by the browser *)
     ri_cookies: string Ocsigen_http_frame.Cookievalues.t Lazy.t;  (** Cookies sent by the browser *)
     ri_ifmodifiedsince: float option;   (** if-modified-since field *)
     ri_ifunmodifiedsince: float option;   (** if-unmodified-since field *)
     ri_ifnonematch: string list;   (** if-none-match field ( * and weak entity tags not implemented) *)
     ri_ifmatch: string list option;   (** if-match field ( * not implemented) *)
     ri_content_type: ((string * string) * (string * string) list) option; (** Content-Type HTTP header *)
     ri_content_type_string: string option; (** Content-Type HTTP header *)
     ri_content_length: int64 option; (** Content-Length HTTP header *)
     ri_referer: string option Lazy.t; (** Referer HTTP header *)

     ri_accept: ((string option * string option) * float option * (string * string) list) list Lazy.t; (** Accept HTTP header. For example [(Some "text", None)] means ["text/*"]. The float is the "quality" value, if any. The last association list is for other extensions. *)
     ri_accept_charset: (string option * float option) list Lazy.t; (** Accept-Charset HTTP header. [None] for the first value means "*". The float is the "quality" value, if any. *)
     ri_accept_encoding: (string option * float option) list Lazy.t; (** Accept-Encoding HTTP header. [None] for the first value means "*". The float is the "quality" value, if any. *)
     ri_accept_language: (string * float option) list Lazy.t; (** Accept-Language HTTP header. The float is the "quality" value, if any. *)


     ri_http_frame: Ocsigen_http_frame.t; (** The full http_frame *)
     ri_extension_info: exn list; (** Use this to put anything you want,
                                      for example, information for subsequent
                                      extensions
                                   *)
     ri_client: client; (** The request connection *)
   }
(** If you force [ri_files] or [ri_post_params], the request is fully read,
   so it is not possible any more to read it from [ri_http_frame]
   (and vice versa).
 *)
and request = {
  request_info: request_info;
  request_config: config_info;
}

exception Ocsigen_Is_a_directory of request


type answer =
  | Ext_do_nothing
      (** Do nothing *)
  | Ext_found of (unit -> Ocsigen_http_frame.result Lwt.t)
      (** "OK stop! I will take the page.
          You can start the following request of the same pipelined connection.
          Here is the function to generate the page".
          The extension must return Ext_found as soon as possible
          when it is sure it is safe to start next request.
          Usually as soon as you know tha the result will be Ext_found.
          But in some case, for example proxies, you don't want the request of
          one connection to be handled in different order.
          In that case, wait to be sure that the new request will not
          overtake this one.
      *)
  | Ext_found_stop of (unit -> Ocsigen_http_frame.result Lwt.t)
      (** Found but do not try next extensions *)
  | Ext_next of int (** Page not found. Try next extension.
                        The integer is the HTTP error code.
                        It is usally 404, but may be for ex 403 (forbidden)
                        if you want another extension to try after a 403.
                        Same as Ext_continue_with but does not change
                        the request.
                    *)
  | Ext_stop_site of (Ocsigen_http_frame.cookieset * int)
                    (** Error. Do not try next extension, but
                        try next site.
                        The integer is the HTTP error code, usally 403.
                     *)
  | Ext_stop_host of (Ocsigen_http_frame.cookieset * int)
                    (** Error. Do not try next extension,
                        do not try next site,
                        but try next host.
                        The integer is the HTTP error code, usally 403.
                     *)
  | Ext_stop_all of (Ocsigen_http_frame.cookieset * int)
                    (** Error. Do not try next extension (even filters),
                        do not try next site,
                        do not try next host,
                        do not .
                        The integer is the HTTP error code, usally 403.
                     *)
  | Ext_continue_with of (request * Ocsigen_http_frame.cookieset * int)
        (** Used to modify the request before giving it to next extension.
            The extension returns the request_info (possibly modified)
            and a set of cookies if it wants to set or cookies
            ([!Ocsigen_http_frame.Cookies.empty] for no cookies).
            You must add these cookies yourself in request_info if you
            want them to be seen by subsequent extensions,
            for example using {!Ocsigen_http_frame.compute_new_ri_cookies}.
            The integer is usually equal to the error code received
            from preceding extension (but you may want to modify it).
         *)
  | Ext_retry_with of request * Ocsigen_http_frame.cookieset
        (** Used to retry all the extensions with a new request_info.
            The extension returns the request_info (possibly modified)
            and a set of cookies if it wants to set or cookies
            ([!Ocsigen_http_frame.Cookies.empty] for no cookies).
            You must add these cookies yourself in request_info if you
            want them to be seen by subsequent extensions,
            for example using {!Ocsigen_http_frame.compute_new_ri_cookies}.
         *)
  | Ext_sub_result of extension2
        (** Used if your extension want to define option that may contain
            other options from other extensions.
            In that case, while parsing the configuration file, call
            the parsing function (of type [parse_fun]),
            that will return something of type [extension2].
        *)
  | Ext_found_continue_with of 
      (unit -> (Ocsigen_http_frame.result * request) Lwt.t)
        (** Same as [Ext_found] but may modify the request. *)
  | Ext_found_continue_with' of (Ocsigen_http_frame.result * request)
        (** Same as [Ext_found_continue_with] but does not allow to delay
            the computation of the page. You should probably not use it,
            but for output filters.
        *)

and request_state =
  | Req_not_found of (int * request)
  | Req_found of (request * Ocsigen_http_frame.result)

and extension2 =
  (unit -> unit) ->
  Ocsigen_http_frame.cookieset ->
  request_state ->
  (answer * Ocsigen_http_frame.cookieset) Lwt.t

type extension = request_state -> answer Lwt.t
(** For each <site> tag in the configuration file,
    you can set the extensions you want.
    Each extension is implemented as a function, taking
    the charset found in configuration file,
    the current state of the request,
    and returning an answer.
    If no page has been generated so far ([Req_not_found]), it receive
    the error code given by the previous extension (default 404),
    and the request information.
    If a page has been generated by previous extensions (case [Req_found]),
    the extension may want to modify the result (filters).
 *)

type parse_fun = Simplexmlparser.xml list -> extension2

(** Type of the functions parsing the content of a <host> tag *)
type parse_host


(** Information received by extensions accepting userconf files.

   The parameter [localfiles_root] is an absolute path to the
   directory that the user is allowed to serve. This is used
   by staticmod, to disallow the user from allowing access to
   outside of this directory
*)
type userconf_info = {
  localfiles_root : string;
}

(** [parse_config] is the type of the functions parsing a <site> tag
    (and returning an extension).  Those are functions taking
   {ul
     {- the name of the virtual <host>}}
     that will be called for each <host>,
     and that will generate a function taking:
   {ul
     {- the path attribute of a <site> tag}}
     that will be called for each <site>,
     and that will generate a function taking:
   {ul
     {- an item of the config file}}
     that will be called on each tag inside <site> and:
   {ul
     {- raise [Bad_config_tag_for_extension] if it does not recognize that tag}
     {- return something of type [extension] (filter or page generator)}}

    [parse_config_user] is the type of functions parsing a site tag
    inside an userconf file. They take one more parameter, of type userconf_info
*)
type parse_config =
  virtual_hosts -> parse_config_aux
and parse_config_user =
    userconf_info -> parse_config
and parse_config_aux =
    url_path -> parse_host ->
      (parse_fun -> Simplexmlparser.xml ->
         extension
      )


(** BYXXX : update this documentation
   For each extension generating pages, we register five functions:
   - a function of type parse_config, parsing the configuration for
    the server
   - a function of type parse_config type, that will be called every time user
    configuration  files are parsed (if userconf is enabled).
    It must define only safe options, for example it is not
    safe to allow such options to load a cmo specified by a user, or to
    execute a program, as this program will be executed by ocsigen's user.
    Note that function will be called for every request, whereas the first one
    is called only when starting or reloading the server.
    If you do not want to allow users to use your extension,
    use the predefined function [void_extension] (defines no option).
   - a function that will be called at the beginning
   of the initialisation phase (each time the config file is reloaded)
   (Note that the extensions are not reloaded)
   - a function that will be called at the end of the initialisation phase
   of the server
   - a function that will create an error message from the exceptions
   that may be raised during the initialisation phase, and raise again
   all other exceptions

   If the optional parameter [?respect_pipeline] is [true], the extension
   will ask the server to respect the order of the pipeline. That means that
   it will wait to be sure that the previous request from the same connection
   has been taken by an extension before giving a request to an extension.
   Use this to write proxies extensions, when you want to be able to pipeline
   the requests you to another server. It is false by default.
 *)
val register_extension :
  fun_site:parse_config ->
  ?user_fun_site:parse_config_user ->
  ?begin_init:(unit -> unit) ->
  ?end_init:(unit -> unit) ->
  ?exn_handler:(exn -> string) ->
  ?respect_pipeline:bool ->
  unit -> unit

(** A predefined function to be passed to {!Ocsigen_extensions.register_extension}
    that defines no option.
 *)
val extension_void_fun_site : parse_config_user


(** While loading an extension,
    get the configuration tree between <dynlink></dynlink>*)
val get_config : unit -> Simplexmlparser.xml list

(** Returns the hostname to be used for absolute links or redirections.
    It is either the Host header or the hostname set in 
    the configuration file. *)
val get_hostname : request -> string


(** Parsing URLs.
   This allows to modify the URL in the request_info.
   (to be used for example with Ext_retry_with or Ext_continue_with)
 *)
val ri_of_url : string -> request_info -> request_info


(** User directories *)

(** Exception raised when an non-existing user is found *)

exception NoSuchUser

(** The type for string that may contain a $u(...) *)
type ud_string

val parse_user_dir : string -> ud_string

val replace_user_dir : Netstring_pcre.regexp -> ud_string -> string -> string
(** raises [Not_found] is the directory does not exist *)

(**/**)

val make_parse_config : url_path -> parse_config_aux -> parse_fun

val parse_config_item : parse_config
val parse_user_site_item : parse_config_user

val set_hosts : (virtual_hosts * config_info * extension2) list -> unit

val get_hosts : unit -> (virtual_hosts * config_info * extension2) list

val do_for_site_matching :
    string option ->
    int ->
    request_info -> Ocsigen_http_frame.result Lwt.t

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

val client_of_connection : Ocsigen_http_com.connection -> client

