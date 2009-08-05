(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_sessions.ml
 * Copyright (C) 2009 Vincent Balat
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

(*VVV sp should probably be mostly reconstructed client side
  instead of serialising it to the client
*)


(* types form Ocsigen_http_frame: *)
type http_method =
  | GET
  | POST
  | HEAD
  | PUT
  | DELETE
  | TRACE
  | OPTIONS
  | CONNECT
  | LINK
  | UNLINK
  | PATCH

(** type of ocsigen_http_frame mode. The int is the HTTP answer code *)
type http_mode =
  | Query of (http_method * string)
  | Answer of int
  | Nofirstline
      
type proto = HTTP10 | HTTP11

(* the types sitedata, config_info, request and server_params
   are different on client side: *)
type sitedata =
  {site_dir: Ocsigen_lib.url_path;
   site_dir_string: string;
  }

type config_info = {
  default_hostname: string;
  default_httpport: int;
  default_httpsport: int;
  usedefaulthostname: bool;
}

type request_info =
    {ri_url_string: string; (** full URL *)
(*     ri_url: Neturl.url; *)
     ri_method: http_method; (** GET, POST, HEAD... *)
     ri_protocol: proto; (** HTTP protocol used by client (1.0 or 1.1) *)
     ri_ssl: bool; (** true if HTTPS, false if HTTP *)
     ri_full_path_string: string; (** full path of the URL *)
     ri_full_path: string list;   (** full path of the URL *)
     ri_original_full_path_string: string;   (** full path of the URL, as first sent by the client. Should not be changed by extensions, even rewritemod. It is used to create relative links. *)
     ri_original_full_path: string list;   (** full path of the URL, as first sent by the client. See below. *)
     ri_sub_path: string list;   (** path of the URL (only part concerning the site) *)
     ri_sub_path_string: string;   (** path of the URL (only part concerning the site) *)
     ri_get_params_string: string option; (** string containing GET parameters *)
     ri_host: string option; (** Host field of the request (if any), without port *)
     ri_port_from_host_field: int option; (** Port in the host field of the request (if any) *)
     ri_get_params: (string * string) list Lazy.t;  (** Association list of get parameters *)
     ri_initial_get_params: (string * string) list Lazy.t;  (** Association list of get parameters, as sent by the browser (must not be modified by extensions) *)
     ri_post_params: (string * string) list Lwt.t Lazy.t; (** Association list of post parameters *)
     ri_files: (string * Ocsigen_lib.file_info) list Lwt.t Lazy.t; (** Files sent in the request *)
(*     ri_remote_inet_addr: Unix.inet_addr; (** IP of the client *) *)
     ri_remote_ip: string;            (** IP of the client *)
     ri_remote_ip_parsed: Ocsigen_lib.ip_address Lazy.t;    (** IP of the client, parsed *)
     ri_remote_port: int;      (** Port used by the client *)
     ri_server_port: int;      (** Port of the request (server) *)
     ri_user_agent: string;    (** User_agent of the browser *)
     ri_cookies_string: string option Lazy.t; (** Cookies sent by the browser *)
     ri_cookies: string Ocsigen_lib.String_Table.t Lazy.t;  (** Cookies sent by the browser *)
     ri_ifmodifiedsince: float option;   (** if-modified-since field *)
     ri_ifunmodifiedsince: float option;   (** if-unmodified-since field *)
     ri_ifnonematch: string list option;   (** if-none-match field ( * and weak entity tags not implemented) *)
     ri_ifmatch: string list option;   (** if-match field ( * not implemented) *)
     ri_content_type: ((string * string) * (string * string) list) option; (** Content-Type HTTP header *)
     ri_content_type_string: string option; (** Content-Type HTTP header *)
     ri_content_length: int64 option; (** Content-Length HTTP header *)
     ri_referer: string option Lazy.t; (** Referer HTTP header *)

     ri_accept: ((string option * string option) * float option * (string * string) list) list Lazy.t; (** Accept HTTP header. For example [(Some "text", None)] means ["text/*"]. The float is the "quality" value, if any. The last association list is for other extensions. *)
     ri_accept_charset: (string option * float option) list Lazy.t; (** Accept-Charset HTTP header. [None] for the first value means "*". The float is the "quality" value, if any. *)
     ri_accept_encoding: (string option * float option) list Lazy.t; (** Accept-Encoding HTTP header. [None] for the first value means "*". The float is the "quality" value, if any. *)
     ri_accept_language: (string * float option) list Lazy.t; (** Accept-Language HTTP header. The float is the "quality" value, if any. *)


(*     ri_http_frame: Ocsigen_http_frame.t; (** The full http_frame *) *)
     mutable ri_request_cache: Polytables.t;
     (** Use this to put anything you want,
         for example, information for subsequent
         extensions
     *)
(*      ri_client: client; (** The request connection *)*)
(*     ri_range: ((int64 * int64) list * int64 option * ifrange) option Lazy.t; 
     (** Range HTTP header. [None] means all the document. 
         List of intervals + possibly from an index to the end of the document.
*) *)
(*     mutable ri_nb_tries: int; (** For internal use: 
                                   used to prevent loops of requests *) *)
   }
(** If you force [ri_files] or [ri_post_params], the request is fully read,
   so it is not possible any more to read it from [ri_http_frame]
   (and vice versa).
 *)
and request = {
  request_info: request_info;
  request_config: config_info;
}

type server_params =
    {sp_request: request;
     sp_si: Eliom_common.sess_info;
     sp_sitedata: sitedata (* data for the whole site *);
(*     sp_cookie_info: tables cookie_info; *)
     sp_suffix: Ocsigen_lib.url_path option (* suffix *);
     sp_fullsessname: string option (* the name of the session
                                       to which belong the service
                                       that answered
                                       (if it is a session service) *)}

let get_user_agent ~sp =
  sp.sp_request.request_info.ri_user_agent
let get_full_url ~sp =
  sp.sp_request.request_info.ri_url_string
let get_remote_ip ~sp =
  sp.sp_request.request_info.ri_remote_ip
(*let get_remote_inet_addr ~sp =
  sp.sp_request.request_info.ri_remote_inet_addr *)
let get_get_params ~sp =
  Lazy.force sp.sp_request.request_info.ri_get_params
let get_all_current_get_params ~sp =
  sp.sp_si.Eliom_common.si_all_get_params
let get_initial_get_params ~sp =
  Lazy.force sp.sp_request.request_info.ri_initial_get_params
let get_get_params_string ~sp =
  sp.sp_request.request_info.ri_get_params_string
let get_post_params ~sp =
  Lazy.force sp.sp_request.request_info.ri_post_params
let get_all_post_params ~sp =
  sp.sp_si.Eliom_common.si_all_post_params
let get_original_full_path_string ~sp =
  sp.sp_request.request_info.ri_original_full_path_string
let get_original_full_path ~sp =
  sp.sp_request.request_info.ri_original_full_path
let get_current_full_path ~sp =
  sp.sp_request.request_info.ri_full_path
let get_current_full_path_string ~sp =
  sp.sp_request.request_info.ri_full_path_string
let get_current_sub_path ~sp =
  sp.sp_request.request_info.ri_sub_path
let get_current_sub_path_string ~sp =
  sp.sp_request.request_info.ri_sub_path_string
let get_header_hostname ~sp =
  sp.sp_request.request_info.ri_host
 let get_default_hostname ~sp =
  sp.sp_request.request_config.default_hostname

let get_hostname ~sp =
  let req = sp.sp_request in
  if req.request_config.usedefaulthostname
  then req.request_config.default_hostname
  else match req.request_info.ri_host with
    | None -> req.request_config.default_hostname
    | Some host -> host

let get_default_port ~sp =
  sp.sp_request.request_config.default_httpport
let get_default_sslport ~sp =
  sp.sp_request.request_config.default_httpsport

let get_server_port ~sp =
  let req = sp.sp_request in
  if req.request_config.usedefaulthostname
  then (if req.request_info.ri_ssl
        then req.request_config.default_httpsport
        else req.request_config.default_httpport)
  else match req.request_info.ri_port_from_host_field with
    | None -> req.request_info.ri_server_port
    | Some p -> p

let get_ssl ~sp =
  sp.sp_request.request_info.ri_ssl
let get_other_get_params ~sp =
  sp.sp_si.Eliom_common.si_other_get_params
let get_nl_get_params ~sp =
  sp.sp_si.Eliom_common.si_nl_get_params
let get_persistent_nl_get_params ~sp =
  Lazy.force sp.sp_si.Eliom_common.si_persistent_nl_get_params
let get_nl_post_params ~sp =
  sp.sp_si.Eliom_common.si_nl_post_params
let get_suffix ~sp =
  sp.sp_suffix
let get_session_name ~sp =
  sp.sp_fullsessname
let get_request_cache ~sp =
  sp.sp_request.request_info.ri_request_cache
let clean_request_cache ~sp =
  sp.sp_request.request_info.ri_request_cache <- 
    Polytables.create ()
let get_link_too_old ~sp =
  try
    Polytables.get
      ~table:sp.sp_request.request_info.ri_request_cache
      ~key:Eliom_common.eliom_link_too_old
  with Not_found -> false
let get_expired_service_sessions ~sp =
  try
    Polytables.get
      ~table:sp.sp_request.request_info.ri_request_cache
      ~key:Eliom_common.eliom_service_session_expired
  with Not_found -> []
(* let get_config_default_charset ~sp =
  Ocsigen_charset_mime.default_charset
    sp.sp_request.request_config.charset_assoc *)
let get_cookies ~sp =
  Lazy.force sp.sp_request.request_info.ri_cookies
let get_data_cookies ~sp =
  sp.sp_si.Eliom_common.si_data_session_cookies
let get_persistent_cookies ~sp =
  sp.sp_si.Eliom_common.si_persistent_session_cookies
let get_previous_extension_error_code ~sp =
  sp.sp_si.Eliom_common.si_previous_extension_error
let get_si ~sp =
  sp.sp_si

let get_request ~sp = sp.sp_request
let get_ri ~sp = sp.sp_request.request_info
(* let get_config_info ~sp = sp.sp_request.request_config *)



let get_site_dir ~sp = sp.sp_sitedata.site_dir
let get_site_dir_string ~sp =
  sp.sp_sitedata.site_dir_string
