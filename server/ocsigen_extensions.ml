(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsigen_extensions.ml
 * Copyright (C) 2005 2007 Vincent Balat
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
(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)

(** Writing extensions for Ocsigen                                           *)

(* TODO

   - awake must be called after each Ext_found or Ext_found_continue_with
   or Ext_found_stop sent by an extension. It is perhaps called too often.

*)

open Lwt
open Ocsigen_lib

exception Ocsigen_http_error of (Ocsigen_http_frame.cookieset * int)
exception Ocsigen_Internal_Error of string
exception Ocsigen_Looping_request


(** Xml tag not recognized by an extension (usually not a real error) *)
exception Bad_config_tag_for_extension of string

(** Error in a <site> tag inside the main ocsigen.conf file *)
exception Error_in_config_file of string

(** Option incorrect in a userconf file *)
exception Error_in_user_config_file of string


let badconfig fmt = Printf.ksprintf (fun s -> raise (Error_in_config_file s)) fmt

(*****************************************************************************)
(** type of URL, without parameter *)
type url_path = string list

let string_of_url_path = Ocsigen_lib.string_of_url_path


type file_info = {tmp_filename: string;
                  filesize: int64;
                  raw_original_filename: string;
                  original_basename: string}


(* virtual hosts: *)
type virtual_host_part = Text of string * int | Wildcard
type virtual_hosts = ((virtual_host_part list) * int option) list

type client = Ocsigen_http_com.connection

let client_id = Ocsigen_http_com.connection_id
let client_connection x = x
let client_of_connection x = x


(*****************************************************************************)

(* Server configuration, for local files that must not be sent *)

type do_not_serve = {
  do_not_serve_regexps: string list;
  do_not_serve_files: string list;
  do_not_serve_extensions: string list;
}

(* BY TODO : Use unbalanced trees instead *)
let join_do_not_serve d1 d2 = {
  do_not_serve_regexps = d1.do_not_serve_regexps @ d2.do_not_serve_regexps;
  do_not_serve_files = d1.do_not_serve_files @ d2.do_not_serve_files;
  do_not_serve_extensions = d1.do_not_serve_extensions @ d2.do_not_serve_extensions;
}

let hash_consed_do_not_serve = Hashtbl.create 17

exception IncorrectRegexpes of do_not_serve

let do_not_serve_to_regexp d =
  try Hashtbl.find hash_consed_do_not_serve d
  with Not_found ->
    let wrap l = if l = [] then None else Some l
    and bind f = function None -> None | Some v -> Some (f v)
    in
    let files, extensions, regexps =
      wrap d.do_not_serve_files,
      wrap d.do_not_serve_extensions,
      wrap d.do_not_serve_regexps
    in
    let paren_quote l =
      String.concat "|" (List.map (fun s -> Printf.sprintf "(%s)"
                                     (Netstring_pcre.quote s)) l)
    and paren l =
      String.concat "|" (List.map (fun s -> Printf.sprintf "(%s)"  s) l)
    in
    let files = bind paren_quote files
    and extensions = bind paren_quote extensions
    and regexps = bind paren regexps
    in
    let files = bind (Printf.sprintf ".*/(%s)") files
    and extensions = bind (Printf.sprintf ".*\\.(%s)") extensions
    in
    let l = List.fold_left (fun r -> function None -> r | Some v -> v :: r)
      [] [files;extensions;regexps]
    in
    let regexp =
      if l = [] then
        (* This regexp should not never match *) "$^"
      else
        Printf.sprintf "^(%s)$" (paren l)
    in
    (try
       Ocsigen_messages.debug (fun () -> Printf.sprintf
                                 "Compiling exclusion regexp %s" regexp);
       let r = Netstring_pcre.regexp regexp in
       Hashtbl.add hash_consed_do_not_serve d r;
       r
     with _ -> raise (IncorrectRegexpes d)
    )


(*****************************************************************************)

(* Main server configuration *)


type config_info = {
  default_hostname: string;
  default_httpport: int;
  default_httpsport: int;

  mime_assoc: Ocsigen_charset_mime.mime_assoc;

  charset_assoc : Ocsigen_charset_mime.charset_assoc;

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

  do_not_serve_404: do_not_serve;
  do_not_serve_403: do_not_serve;
}
and follow_symlink =
  | DoNotFollowSymlinks (** Never follow a symlink *)
  | FollowSymlinksIfOwnerMatch (** Follow a symlink if the symlink and its
                          target have the same owner *)
  | AlwaysFollowSymlinks (** Always follow symlinks *)



(* Requests *)
type ifrange = IR_No | IR_Ifunmodsince of float | IR_ifmatch of string

type request_info =
    {ri_url_string: string;
     ri_url: Neturl.url;
     ri_method: Ocsigen_http_frame.Http_header.http_method;
     ri_protocol: Ocsigen_http_frame.Http_header.proto; (** HTTP protocol used by client *)
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

     ri_http_frame: Ocsigen_http_frame.t; (** The full http_frame *)
     mutable ri_request_cache: Polytables.t;
     (** Use this to put anything you want,
         for example, information for subsequent
         extensions
     *)
     ri_client: client; (** The request connection *)
     ri_range: ((int64 * int64) list * int64 option * ifrange) option Lazy.t; 
     (** Range HTTP header. [None] means all the document. 
         List of intervals + possibly from an index to the end of the document.
     *)
     mutable ri_nb_tries: int; (** For internal use: 
                                   used to prevent loops of requests *)
   }
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
          Usually immediately. But in some case, for example proxies,
          you don't want the request of one connection to be handled in
          different order. (for example revproxy.ml starts its requests
          to another server before returning Ext_found, to ensure that all
          requests are done in same order).
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
                    (** Error. Do not try next extension,
                        do not try next site,
                        do not try next host.
                        The integer is the HTTP error code, usally 403.
                     *)
  | Ext_continue_with of (request * Ocsigen_http_frame.cookieset * int)
        (** Used to modify the request before giving it to next extension.
            The extension returns the request (possibly modified)
            and a set of cookies if it wants to set or cookies
            ({!Ocsigen_http_frame.Cookies.empty} for no cookies).
            You must add these cookies yourself in request if you
            want them to be seen by subsequent extensions,
            for example using {!Ocsigen_http_frame.compute_new_ri_cookies}.
            The integer is usually equal to the error code received
            from preceding extension (but you may want to modify it).
         *)
  | Ext_retry_with of request * Ocsigen_http_frame.cookieset
        (** Used to retry all the extensions with a new request.
            The extension returns the request (possibly modified)
            and a set of cookies if it wants to set or cookies
            ({!Ocsigen_http_frame.Cookies.empty} for no cookies).
            You must add these cookies yourself in request if you
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


type parse_fun = Simplexmlparser.xml list -> extension2


type parse_host =
    Parse_host of
      (url_path ->
         parse_host -> parse_fun -> Simplexmlparser.xml -> extension)

let (hosts : (virtual_hosts * config_info * extension2) list ref) =
  ref []



let set_hosts v = hosts := v
let get_hosts () = !hosts



(*****************************************************************************)
(* To give parameters to extensions: *)
let dynlinkconfig = ref ([] : Simplexmlparser.xml list)
let set_config s = dynlinkconfig := s
let get_config () = !dynlinkconfig


(*****************************************************************************)
let site_match request site_path url =
  (* We are sure that there is no / at the end or beginning of site_path *)
  (* and no / at the beginning of url *)
  (* and no // or ../ inside both of them *)
  (* We return the subpath without / at beginning *)
  let rec aux site_path url =
    match site_path, url with
      | [], [] -> raise (Ocsigen_Is_a_directory request)
      | [], p -> Some p
      | a::l, aa::ll when a = aa -> aux l ll
      | _ -> None
  in
  match site_path, url with
    | [], [] -> Some []
    | _ -> aux site_path url




let add_to_res_cookies res cookies_to_set =
  if cookies_to_set = Ocsigen_http_frame.Cookies.empty then
    res
  else
    {res with
     Ocsigen_http_frame.res_cookies =
     Ocsigen_http_frame.add_cookies res.Ocsigen_http_frame.res_cookies cookies_to_set}

let make_ext awake cookies_to_set req_state (genfun : extension) (genfun2 : extension2) =
  genfun req_state
  >>= fun res ->
  let rec aux cookies_to_set = function
    | Ext_do_nothing -> genfun2 awake cookies_to_set req_state
    | Ext_found r ->
        awake ();
        r () >>= fun r' ->
        let ri = match req_state with
          | Req_found (ri, _) -> ri
          | Req_not_found (_, ri) -> ri
        in
        genfun2
          Ocsigen_lib.id (* already awoken *)
          Ocsigen_http_frame.Cookies.empty
          (Req_found (ri, add_to_res_cookies r' cookies_to_set))
    | Ext_found_continue_with r ->
        awake ();
        r () >>= fun (r', req) ->
        genfun2
          Ocsigen_lib.id (* already awoken *)
          Ocsigen_http_frame.Cookies.empty
          (Req_found (req, add_to_res_cookies r' cookies_to_set))
    | Ext_found_continue_with' (r', req) ->
        awake ();
        genfun2
          Ocsigen_lib.id (* already awoken *)
          Ocsigen_http_frame.Cookies.empty
          (Req_found (req, add_to_res_cookies r' cookies_to_set))
    | Ext_next e ->
        let ri = match req_state with
          | Req_found (ri, _) -> ri
          | Req_not_found (_, ri) -> ri
        in
        genfun2 awake cookies_to_set (Req_not_found (e, ri))
    | Ext_continue_with (ri, cook, e) ->
        genfun2
          awake
          (Ocsigen_http_frame.add_cookies cook cookies_to_set)
          (Req_not_found (e, ri))
    | Ext_found_stop _
    | Ext_stop_site _
    | Ext_stop_host _
    | Ext_stop_all _
    | Ext_retry_with _ as res ->
        Lwt.return (res, cookies_to_set)
    | Ext_sub_result sr ->
        sr awake cookies_to_set req_state
        >>= fun (res, cookies_to_set) ->
        aux cookies_to_set res
  in
  aux cookies_to_set res


(*****************************************************************************)
let fun_beg = ref (fun () -> ())
let fun_end = ref (fun () -> ())
let fun_exn = ref (fun exn -> (raise exn : string))

let rec default_parse_config
    (host : virtual_hosts)
    prevpath
    (Parse_host parse_host)
    (parse_fun : parse_fun) = function
  | Simplexmlparser.Element ("site", atts, l) ->
      let rec parse_site_attrs (enc,dir) = function
        | [] -> (match dir with
          | None ->
              raise (Ocsigen_config.Config_file_error
                       ("Missing dir attribute in <site>"))
          | Some s -> (enc, s))
        | ("path", s)::suite
        | ("dir", s)::suite ->
            (match dir with
            | None -> parse_site_attrs (enc, Some s) suite
            | _ -> raise (Ocsigen_config.Config_file_error
                            ("Duplicate attribute dir in <site>")))
        | ("charset", s)::suite ->
            (match enc with
            | None -> parse_site_attrs ((Some s), dir) suite
            | _ -> raise (Ocsigen_config.Config_file_error
                            ("Duplicate attribute charset in <site>")))
        | (s, _)::_ ->
            raise
              (Ocsigen_config.Config_file_error ("Wrong attribute for <site>: "^s))
      in
      let charset, dir = parse_site_attrs (None, None) atts in
      let path =
        prevpath@
        Ocsigen_lib.remove_slash_at_end
          (Ocsigen_lib.remove_slash_at_beginning
             (Ocsigen_lib.remove_dotdot (Neturl.split_path dir)))
      in
      let parse_config = make_parse_config path parse_host l in
      let ext awake cookies_to_set =
        function
          | Req_found (ri, res) ->
              Lwt.return (Ext_found_continue_with' (res, ri), cookies_to_set)
          | Req_not_found (e, oldri) ->
              let oldri = match charset with
                | None -> oldri
                | Some charset ->
                    { oldri with request_config =
                        { oldri.request_config with charset_assoc =
                            Ocsigen_charset_mime.set_default_charset
                              oldri.request_config.charset_assoc charset } }
              in
              match site_match oldri path oldri.request_info.ri_full_path with
              | None ->
                  Ocsigen_messages.debug (fun () ->
                    "site \""^
                    (Ocsigen_lib.string_of_url_path ~encode:true path)^
                    "\" does not match url \""^
                    (Ocsigen_lib.string_of_url_path ~encode:true
                       oldri.request_info.ri_full_path)^
                    "\".");
                  Lwt.return (Ext_next e, cookies_to_set)
              | Some sub_path ->
                  Ocsigen_messages.debug (fun () ->
                    "-------- site found: url \""^
                    (Ocsigen_lib.string_of_url_path ~encode:true
                       oldri.request_info.ri_full_path)^
                    "\" matches \""^
                    (Ocsigen_lib.string_of_url_path ~encode:true path)^"\".");
                  let ri = {oldri with
                              request_info =
                                { oldri.request_info with
                                    ri_sub_path = sub_path;
                                    ri_sub_path_string =
                                    Ocsigen_lib.string_of_url_path
                                      ~encode:true sub_path} }
                  in
                  parse_config awake cookies_to_set (Req_not_found (e, ri))
                  >>= function
                      (* After a site, we turn back to old ri *)
                    | (Ext_stop_site (cs, err), cookies_to_set)
                    | (Ext_continue_with (_, cs, err), cookies_to_set) ->
                        Lwt.return
                          (Ext_continue_with (oldri, cs, err), cookies_to_set)
                    | (Ext_found_continue_with r, cookies_to_set) ->
                        awake ();
                        r () >>= fun (r', req) ->
                        Lwt.return
                          (Ext_found_continue_with' (r', oldri), cookies_to_set)
                    | (Ext_found_continue_with' (r, req), cookies_to_set) ->
                        Lwt.return
                          (Ext_found_continue_with' (r, oldri), cookies_to_set)
                    | (Ext_do_nothing, cookies_to_set) ->
                        Lwt.return 
                          (Ext_continue_with (oldri, 
                                              Ocsigen_http_frame.Cookies.empty,
                                              e), cookies_to_set)
                    | r -> Lwt.return r
      in
      (function
        | Req_found (ri, r) ->
            Lwt.return (Ext_found_continue_with' (r, ri))
        | Req_not_found (err, ri) ->
            Lwt.return (Ext_sub_result ext))
  | Simplexmlparser.Element (tag,_,_) ->
      raise (Bad_config_tag_for_extension tag)
  | _ -> raise (Ocsigen_config.Config_file_error
                  ("Unexpected content inside <host>"))

and make_parse_config path parse_host l : extension2 =
  let f = parse_host path (Parse_host parse_host) in
  (* creates all site data, if any *)
  let rec parse_config : _ -> extension2 = function
    | [] ->
        (fun (awake : unit -> unit) cookies_to_set -> function
          | Req_found (ri, res) ->
              Lwt.return (Ext_found_continue_with' (res, ri), cookies_to_set)
          | Req_not_found (e, ri) ->
              Lwt.return
                (Ext_continue_with
                   (ri, Ocsigen_http_frame.Cookies.empty, e), cookies_to_set))
(* was Lwt.return (Ext_next e, cookies_to_set))
   but to use make_parse_site with userconf,
   we need to know current ri after parsing the sub-configuration.
*)
    | xmltag::ll ->
        try
          let genfun = f parse_config xmltag in
          let genfun2 = parse_config ll in
          fun awake cookies_to_set req_state ->
            make_ext awake cookies_to_set req_state genfun genfun2
        with
        | Bad_config_tag_for_extension t ->
            (* This case happens only if no extension has recognized the
               tag at all *)
            badconfig
              "Unexpected tag <%s> inside <site dir=\"%s\">" t
              (Ocsigen_lib.string_of_url_path ~encode:true path)
        | Error_in_config_file _ as e -> raise e
        | e ->
            badconfig
              "Error while parsing configuration file: %s"
              (try !fun_exn e
               with e -> Ocsigen_lib.string_of_exn e)
  in
  !fun_beg ();
  let r =
    try
      parse_config l
    with e -> !fun_end (); raise e
(*VVV May be we should avoid calling fun_end after parinf user config files
  (with extension userconf) ... See eliommod.ml
*)
  in
  !fun_end ();
  r

(*****************************************************************************)

type userconf_info = {
  localfiles_root : string;
}

type parse_config = virtual_hosts -> parse_config_aux
and parse_config_user = userconf_info -> parse_config
and parse_config_aux =
    url_path -> parse_host ->
      (parse_fun -> Simplexmlparser.xml ->
         extension
      )



let user_extension_void_fun_site : parse_config_user = fun _ _ _ _ _ -> function
  | Simplexmlparser.Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "Unexpected data in config file")

let extension_void_fun_site : parse_config = fun _ _ _ _ -> function
  | Simplexmlparser.Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "Unexpected data in config file")


let register_extension, parse_config_item, parse_user_site_item, get_beg_init, get_end_init, get_init_exn_handler =
  let ref_fun_site = ref default_parse_config in
  let ref_user_fun_site = ref (fun (_ : userconf_info) -> default_parse_config) in

  ((* ********* register_extension ********* *)
    (fun
         ?fun_site
         ?user_fun_site
         ?begin_init
         ?end_init
         ?(exn_handler=raise)
         ?(respect_pipeline=false)
       ()
       ->

       if respect_pipeline then Ocsigen_config.set_respect_pipeline ();

       (match fun_site with
         | None -> ()
         | Some fun_site ->
             let old_fun_site = !ref_fun_site in
             ref_fun_site :=
               (fun host ->
                  let oldf = old_fun_site host in
                  let newf = fun_site host in
                  fun path parse_host ->
                    let oldf = oldf path parse_host in
                    let newf = newf path parse_host in
                    fun parse_config config_tag ->
                      try
                        oldf parse_config config_tag
                      with
                        | Bad_config_tag_for_extension c -> 
                            newf parse_config config_tag
               ));

       (match user_fun_site with
         | None -> ()
         | Some user_fun_site ->
             let old_fun_site = !ref_user_fun_site in
             ref_user_fun_site :=
               (fun path host ->
                  let oldf = old_fun_site path host in
                  let newf = user_fun_site path host in
                  fun path parse_host ->
                    let oldf = oldf path parse_host in
                    let newf = newf path parse_host in
                    fun parse_config config_tag ->
                      try
                        oldf parse_config config_tag
                      with
                        | Bad_config_tag_for_extension c -> 
                            newf parse_config config_tag
               ));


       (match begin_init with
         | Some begin_init -> fun_beg := comp begin_init !fun_beg
         | None -> ());
       (match end_init with
         | Some end_init -> fun_end := comp end_init !fun_end;
         | None -> ());
       let curexnfun = !fun_exn in
       fun_exn := fun e -> try curexnfun e with e -> exn_handler e),


   (* ********* parse_config_item ********* *)
   (fun host -> !ref_fun_site host),

   (* ********* parse_user_site_item ********* *)
   (fun host -> !ref_user_fun_site host),

   (* ********* get_beg_init ********* *)
   (fun () -> !fun_beg),

   (* ********* get_end_init ********* *)
   (fun () -> !fun_end),

   (* ********* get_init_exn_handler ********* *)
   (fun () -> !fun_exn)
  )

let default_parse_extension ext_name = function
  | [] -> ()
  | _ -> raise (Error_in_config_file
                  (Printf.sprintf "Unexpected content found in configuration of extension %s: %s does not accept options" ext_name ext_name))

let register_extension 
    ~name
    ?fun_site
    ?user_fun_site
    ?begin_init
    ?end_init
    ?init_fun
    ?exn_handler
    ?respect_pipeline
    () =
  Ocsigen_loader.set_module_init_function name
    (fun () ->
       (match init_fun with
          | None -> default_parse_extension name (get_config ())
          | Some f -> f (get_config ()));
       register_extension ?fun_site ?user_fun_site ?begin_init ?end_init
         ?exn_handler ?respect_pipeline ())

(*****************************************************************************)
let start_initialisation, during_initialisation,
  end_initialisation, get_numberofreloads =
  let init = ref true in
  let nb = ref (-1) in
   ((fun () ->
     init := true;
     nb := !nb+1;
    ),
    (fun () -> !init),
    (fun () ->
      init := false;
    ),
    (fun () -> !nb))

(********)


let host_match host port =
  let port_match = function
    | None -> true
    | Some p -> p = port
  in
  let rec aux host =
    let hostlen = String.length host in
    let rec host_match1 beg =
      let rec aux1 t len l p0 =
        try
          let (p,_) =
            Netstring_str.search_forward (Netstring_str.regexp t) host p0 in
          let beg2 = p + len in
          (host_match1 beg2 l) || (aux1 t len l (p+1))
        with Not_found -> false
      in
      function
        | [] -> beg = hostlen
        | [Wildcard] -> true
        | (Wildcard)::(Wildcard)::l ->
            host_match1 beg ((Wildcard)::l)
        | (Wildcard)::(Text (t,len))::l -> aux1 t len l beg
        | (Text (t,len))::l ->
            try
              (t = String.sub host beg len) && (host_match1 (beg+len) l)
            with Invalid_argument _ -> false
    in
    function
      | [] -> false
      | (a, p)::l -> ((port_match p) && (host_match1 0 a)) || aux host l
  in match host with
    | None -> List.exists (fun (_, p) -> port_match p)
        (*VVV Warning! For HTTP/1.0, when host is absent,
           we take the first one, even if it doesn't match!
        *)
    | Some host -> aux host


let string_of_host h =
  let aux1 (hh, port) =
    let p = match port with
    | None -> ""
    | Some a -> ":"^(string_of_int a)
    in
    let rec aux2 = function
      | [] -> ""
      | Wildcard::l -> "*"^(aux2 l)
      | (Text (t,_))::l -> t^(aux2 l)
    in (aux2 hh)^p
  in List.fold_left (fun d hh -> d^(aux1 hh)^" ") "" h



let serve_request
    ?(previous_cookies = Ocsigen_http_frame.Cookies.empty)
    ?(awake_next_request = false) ri =

  let host = ri.ri_host in
  let port = ri.ri_server_port in

  let conn = client_connection ri.ri_client in
  let awake =
    if awake_next_request
    then
      (let tobeawoken = ref true in
       (* must be awoken once and only once *)
       fun () ->
         if !tobeawoken then begin
           tobeawoken := false;
           Ocsigen_http_com.wakeup_next_request conn
         end)
    else Ocsigen_lib.id
  in

  let rec do2 sites cookies_to_set ri =
    ri.ri_nb_tries <- ri.ri_nb_tries + 1;
    if ri.ri_nb_tries > Ocsigen_config.get_maxretries ()
    then fail Ocsigen_Looping_request
    else
    let string_of_host_option = function
      | None -> "<no host>:"^(string_of_int port)
      | Some h -> h^":"^(string_of_int port)
    in
    let rec aux_host ri prev_err cookies_to_set = function
      | [] -> fail (Ocsigen_http_error (cookies_to_set, prev_err))
      | (h, conf_info, host_function)::l when host_match host port h ->
          Ocsigen_messages.debug (fun () ->
            "-------- host found! "^
            (string_of_host_option host)^
            " matches "^(string_of_host h));
          host_function
            awake
            cookies_to_set
            (Req_not_found (prev_err, { request_info = ri;
                                        request_config = conf_info }))
          >>= fun (res_ext, cookies_to_set) ->
          (match res_ext with
          | Ext_found r
          | Ext_found_stop r ->
              awake ();
              r () >>= fun r' ->
              Lwt.return (add_to_res_cookies r' cookies_to_set)
          | Ext_do_nothing ->
              aux_host ri prev_err cookies_to_set l
          | Ext_found_continue_with r ->
              awake ();
              r () >>= fun (r', _) ->
              return (add_to_res_cookies r' cookies_to_set)
          | Ext_found_continue_with' (r, _) ->
              awake ();
              return (add_to_res_cookies r cookies_to_set)
          | Ext_next e ->
              aux_host ri e cookies_to_set l
                (* try next site *)
          | Ext_stop_host (cook, e)
          | Ext_stop_site (cook, e) ->
              aux_host ri e (Ocsigen_http_frame.add_cookies cook cookies_to_set) l
                (* try next site *)
          | Ext_stop_all (cook, e) ->
              fail (Ocsigen_http_error (cookies_to_set, e))
          | Ext_continue_with (_, cook, e) ->
              aux_host ri e
                (Ocsigen_http_frame.add_cookies cook cookies_to_set) l
          | Ext_retry_with (request2, cook) ->
              do2
                (get_hosts ())
                (Ocsigen_http_frame.add_cookies cook cookies_to_set)
                request2.request_info
                (* retry all *)
          | Ext_sub_result sr ->
              assert false
          )
      | (h, _, _)::l ->
          Ocsigen_messages.debug (fun () ->
            "-------- host = "^
            (string_of_host_option host)^
            " does not match "^(string_of_host h));
          aux_host ri prev_err cookies_to_set l
    in aux_host ri 404 cookies_to_set sites
  in
  Lwt.finalize
    (fun () ->
      do2 (get_hosts ()) previous_cookies ri
    )
    (fun () ->
       awake ();
       Lwt.return ()
    )


(*****************************************************************************)



(* used to modify the url in ri (for example for retrying after rewrite) *)
let ri_of_url url ri =
  let (_, host, _, url, url2, path, params, get_params) = parse_url url in
  let host = match host with
    | Some h -> host
    | None -> ri.ri_host
  in
  {ri with
   ri_url_string = url;
   ri_url = url2;
   ri_host = host;
   ri_full_path_string = string_of_url_path ~encode:true path;
   ri_full_path = path;
     (* ri_original_full_path is not changed *)
   ri_sub_path = path;
   ri_get_params_string = params;
   ri_get_params = get_params;
  }



(*****************************************************************************)
(* This is used by server.ml.
   I put that here because I need it to be accessible for profiling. *)
let sockets = ref []
let sslsockets = ref []

let get_number_of_connected,
  incr_connected,
  decr_connected,
  wait_fewer_connected =
  let connected = ref 0 in
  let maxr = ref (-1000) in
  let mvar = Lwt_mvar.create_empty () in
  ((fun () -> !connected),
   (fun n -> connected := !connected + n),
   (fun () -> 
      let c = !connected in
      connected := c - 1;
      if !connected <= 0 && !sockets = [] && !sslsockets = []
      then exit 0;
      if c = !maxr
      then begin
        Ocsigen_messages.warning "Number of connections now ok";
        maxr := -1000;
        Lwt_mvar.put mvar ()
      end
      else Lwt.return ()
   ),
   (fun max -> 
      maxr := max;
      Lwt_mvar.take mvar)
  )






(*****************************************************************************)
(* Default hostname is either the Host header or the hostname set in 
   the configuration file. *)
let get_hostname req =
  if Ocsigen_config.get_usedefaulthostname ()
  then req.request_config.default_hostname
  else match req.request_info.ri_host with
    | None -> req.request_config.default_hostname
    | Some host -> host


(*****************************************************************************)
(* Default port is either
   - the port the server is listening at
   - or the port in the Host header
   - or the default port set in the configuration file. *)
let get_port req =
  if Ocsigen_config.get_usedefaulthostname ()
  then (if req.request_info.ri_ssl
        then req.request_config.default_httpsport
        else req.request_config.default_httpport)
  else match req.request_info.ri_port_from_host_field with
    | None -> req.request_info.ri_server_port
    | Some p -> p


(*****************************************************************************)
(* user directories *)

exception NoSuchUser

type ud_string = Nodir of string | Withdir of string * string * string

let user_dir_regexp = Netstring_pcre.regexp "(.*)\\$u\\(([^\\)]*)\\)(.*)"

let parse_user_dir s =
  match Netstring_pcre.full_split user_dir_regexp s with
  | [ Netstring_pcre.Delim _;
      Netstring_pcre.Group (1, s1);
      Netstring_pcre.Group (2, u);
      Netstring_pcre.Group (3, s2)] ->
        Withdir (s1, u, s2)
  | _ -> Nodir s


let replace_user_dir regexp dest pathstring =
  match dest with
  | Nodir dest ->
      Netstring_pcre.global_replace regexp dest pathstring
  | Withdir (s1, u, s2) ->
      try
        let s1 = Netstring_pcre.global_replace regexp s1 pathstring in
        let u = Netstring_pcre.global_replace regexp u pathstring in
        let s2 = Netstring_pcre.global_replace regexp s2 pathstring in
        let userdir = (Unix.getpwnam u).Unix.pw_dir in
        Ocsigen_messages.debug (fun () -> "User " ^ u);
        s1^userdir^s2
      with Not_found ->
        Ocsigen_messages.debug (fun () -> "No such user " ^ u);
        raise NoSuchUser


(*****************************************************************************)
(* Finding redirections *)

exception Not_concerned

let find_redirection regexp full_url dest
    https host port 
    get_params_string
    sub_path_string
    full_path_string
    =
  if full_url
  then
    match host with
      | None -> raise Not_concerned
      | Some host ->
          let path =
            match get_params_string with
              | None -> full_path_string
              | Some g -> full_path_string ^ "?" ^ g
          in
          let path =
            Ocsigen_lib.make_absolute_url https host port ("/"^path)
          in
          (match Netstring_pcre.string_match regexp path 0 with
             | None -> raise Not_concerned
             | Some _ -> (* Matching regexp found! *)
                 Netstring_pcre.global_replace regexp dest path
          )
  else
    let path =
      match get_params_string with
        | None -> sub_path_string
        | Some g -> sub_path_string ^ "?" ^ g
    in
    match Netstring_pcre.string_match regexp path 0 with
      | None -> raise Not_concerned
      | Some _ -> (* Matching regexp found! *)
          Netstring_pcre.global_replace regexp dest path


(******************************************************************)
(* Extending commands *)
exception Unknown_command

let register_command_function, get_command_function =
  let command_function = ref (fun ?prefix _ _ -> raise Unknown_command) in
  ((fun ?prefix f -> 
      let prefix' = prefix in
      let old_command_function = !command_function in
      command_function := 
        (fun ?prefix s c -> 
           try old_command_function ?prefix s c
           with Unknown_command -> 
             if prefix = prefix'
             then f s c
             else raise Unknown_command)),
   (fun () -> !command_function))
