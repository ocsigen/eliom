(* Ocsigen
 * http://www.ocsigen.org
 * Module extensions.ml
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


open Lwt
open Ocsimisc

exception Ocsigen_http_error of int
exception Ocsigen_Is_a_directory
exception Ocsigen_malformed_url
exception Ocsigen_Internal_Error of string
exception Ocsigen_Looping_request

exception Bad_config_tag_for_extension of string
exception Error_in_config_file of string

(*****************************************************************************)
(** type of URL, without parameter *)
type url_path = string list

let string_of_url_path = Ocsimisc.string_of_url_path


type file_info = {tmp_filename: string;
                  filesize: int64;
                  original_filename: string}


(* virtual hosts: *)
type virtual_host_part = Text of string * int | Wildcard
type virtual_hosts = ((virtual_host_part list) * int option) list

(* Requests *)
type request_info = 
    {ri_url_string: string;
     ri_url: Neturl.url;
     ri_method: Http_frame.Http_header.http_method;
     ri_protocol: Http_frame.Http_header.proto; (** HTTP protocol used by client *)
     ri_path_string: string; (** full path of the URL *)
     ri_full_path: string list;   (** full path of the URL *)
     ri_sub_path: string list;   (** path of the URL (only part concerning the site) *)
     ri_sub_path_string: string Lazy.t;   (** path of the URL (only part concerning the site) *)
     ri_get_params_string: string option; (** string containing GET parameters *)
     ri_host: string option; (** Host field of the request (if any) *)
     ri_get_params: (string * string) list Lazy.t;  (** Association list of get parameters*)
     ri_post_params: (string * string) list Lwt.t Lazy.t; (** Association list of post parameters*)
     ri_files: (string * file_info) list Lwt.t Lazy.t; (** Files sent in the request *)
     ri_inet_addr: Unix.inet_addr;        (** IP of the client *)
     ri_ip: string;            (** IP of the client *)
     ri_ip32: int32 Lazy.t;    (** IP of the client, as a 32 bits integer *)
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
     ri_extension_info: exn list; (** Use this to put anything you want, 
                                      for example, information for subsequent
                                      extensions 
                                   *)
   }

   
type answer =
  | Ext_found of Http_frame.result  (** OK stop! I found the page. *)
  | Ext_not_found of int (** Page not found. Try next extension.
                            The integer is the HTTP error code.
                            It is usally 404, but may be for ex 403 (forbidden)
                            if you want another extension to try after a 403
                          *)
(*VVV give the possibility to set cookies here??? *)
  | Ext_stop of int      (** Error. Do not try next extension, but
                            try next site. If you do not want to try next site
                            send an Ext_found with an error code.
                            The integer is the HTTP error code, usally 403.
                          *)
(*VVV give the possibility to set cookies here??? *)
  | Ext_continue_with of request_info * Http_frame.cookieset * int
        (** Used to modify the request before giving it to next extension.
            The extension returns the request_info (possibly modified)
            and a set of cookies if it wants to set or cookies
            ({!Http_frame.Cookies.empty} for no cookies).
            You must add these cookies yourself in request_info if you
            want them to be seen by subsequent extensions,
            for example using {!Http_frame.compute_new_ri_cookies}.
            The integer is usually equal to the error code received 
            from preceding extension (but you may want to modify it).
         *)
  | Ext_retry_with of request_info * Http_frame.cookieset
        (** Used to retry all the extensions with a new request_info.
            The extension returns the request_info (possibly modified)
            and a set of cookies if it wants to set or cookies
            ({!Http_frame.Cookies.empty} for no cookies).
            You must add these cookies yourself in request_info if you
            want them to be seen by subsequent extensions,
            for example using {!Http_frame.compute_new_ri_cookies}.
         *)



type extension =
  | Page_gen of (int -> string -> request_info -> answer Lwt.t)
  | Filter of (string -> request_info -> Http_frame.result -> answer Lwt.t)

let (sites : (virtual_hosts * url_path * string option (* charset *) * 
                extension list) list ref) = 
  ref []

let set_sites v = sites := v
let get_sites () = !sites
let add_site v = sites := !sites@[v] (* at the end *)
   
(*****************************************************************************)
let register_extension,
  parse_site_item, 
  get_beg_init, 
  get_end_init, 
  get_init_exn_handler =
  let fun_site = ref 
      (fun host path charset xmltag -> 
        raise (Bad_config_tag_for_extension "<no extension loaded>"))
  in
  let fun_beg = ref (fun () -> ()) in
  let fun_end = ref (fun () -> ()) in
  let fun_exn = ref (fun exn -> raise exn) in

  ((* ********* register_extension ********* *)
     (fun (new_fun_site, begin_init, end_init, handle_exn) ->
       
       let old_fun_site = !fun_site in
       fun_site := 
         (fun host ->
           let oldf = old_fun_site host in
           let newf = new_fun_site host in
           fun path charset ->
             let oldf = oldf path charset in
             let newf = newf path charset in
             fun config_tag ->
               try
                 oldf config_tag
               with
               | Bad_config_tag_for_extension c -> newf config_tag
         );
       
       fun_beg := comp begin_init !fun_beg;
       fun_end := comp end_init !fun_end;
       let curexnfun = !fun_exn in
       fun_exn := fun e -> try curexnfun e with e -> handle_exn e),
   

   (* ********* parse_site_item ********* *)
   (fun host -> !fun_site host),

   (* ********* get_beg_init ********* *)
   (fun () -> !fun_beg),

   (* ********* get_end_init ********* *)
   (fun () -> !fun_end),

   (* ********* get_init_exn_handler ********* *)
   (fun () -> !fun_exn)
  )


let parse_site host =
  let f = parse_site_item host in (* creates all host data, if any *)
  fun path charset -> 
    let f = f path charset in (* creates all site data, if any *)
    let rec aux = function
      | [] -> []
      | xmltag::ll -> 
          try
            let a = f xmltag in
            a::aux ll
          with
          | Bad_config_tag_for_extension t -> 
              ignore
                (Messages.warning
                   ("Unexpected tag <"^t^"> inside <site dir=\""^
	            (Ocsimisc.string_of_url_path path)^"\"> (ignored)"));
              aux ll
          | Ocsiconfig.Config_file_error t
          | Error_in_config_file t -> 
              ignore
                (Messages.warning
                   ("Error while parsing configuration file: "^
                    t^" (ignored)"));
              aux ll
          | e -> 
              ignore
                (Messages.warning
                   ("Error while parsing configuration file: "^
                    (Ocsimisc.string_of_exn e)^
	            " (ignored)"));
              aux ll
    in aux
    



(*****************************************************************************)
let start_initialisation, during_initialisation, 
  end_initialisation, get_numberofreloads =
  let init = ref true in
  let nb = ref (-1) in
   ((fun () -> 
     init := true;
     nb := !nb+1;
     get_beg_init () ()
    ),
    (fun () -> !init), 
    (fun () -> 
      init := false;
      get_end_init () ()
    ),
    (fun () -> !nb))
    
(********)


let host_match host port =
  let port_match = function
      None -> true
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
      (* Warning! For HTTP/1.0 we take the first one,
         even if it doesn't match! 
         To be changed! *)
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


let rec site_match site_path url = 
  match site_path, url with
  | [], [] -> Some [""]
  | [], p -> Some p
  | [""], (_::_ as p) -> Some p
  | a::l, aa::ll when a = aa -> site_match l ll 
  | _ -> None


let add_to_res_cookies cookies_to_set res =
  if cookies_to_set = Http_frame.Cookies.empty then
    res
  else 
    {res with 
     Http_frame.res_cookies = 
     Http_frame.add_cookies cookies_to_set res.Http_frame.res_cookies}


let do_for_site_matching host port ri =
  let rec do2 sites cookies_to_set ri =
    let string_of_host_option = function
      | None -> "<no host>:"^(string_of_int port)
      | Some h -> h^":"^(string_of_int port)
    in
    let rec aux ri prev_err cookies_to_set = function
      | [] -> fail (Ocsigen_http_error prev_err)
      | (h, path, charset, extlist)::l when host_match host port h ->
          (match site_match path ri.ri_full_path with
          | None ->
              Messages.debug (fun () ->
                "-------- host=\""^
                (string_of_host_option host)^"\" site=\""^
                (Ocsimisc.string_of_url_path path)^
                "\" does not match host=\""^(string_of_host h)^"\" site=\"/"^
                (Ocsimisc.string_of_url_path ri.ri_full_path)^
                "\".");
              aux ri prev_err cookies_to_set l
          | Some sub_path ->
              Messages.debug (fun () -> 
                "-------- site found: \""^(string_of_host_option host)^
                "\" matches \""^(string_of_host h)^"\" and \"/"^
                (Ocsimisc.string_of_url_path ri.ri_full_path)^
                "\" matches \"/"^
                (Ocsimisc.string_of_url_path path)^"\".");
              let ri = {ri with
                        ri_sub_path = ""::sub_path; 
                        ri_sub_path_string = 
                        lazy ("/"^Ocsimisc.string_of_url_path sub_path)}
              in
              let charset = match charset with 
              | None -> Ocsiconfig.get_default_charset ()
              | _ -> charset
              in
              let charset = match charset with 
              | None -> "utf-8"
              | Some charset -> charset
              in
              List.fold_left
                (fun res ext -> 
                  res >>= fun ((res_ext, cookies_to_set) as res) ->
                    match (res_ext, ext) with
                    | (Ext_found r, Page_gen _) -> return res
                    | (Ext_found r, Filter f) -> 
                        f charset ri (add_to_res_cookies cookies_to_set r)
                        >>= fun r -> return (r, Http_frame.Cookies.empty)
                    | (Ext_not_found e, Page_gen f) ->
                        f e charset ri >>= fun r -> return (r, cookies_to_set)
                    | (Ext_not_found e, Filter _) -> return res
                    | (Ext_continue_with (ri, cook, e), Page_gen f) -> 
                        f e charset ri >>= fun r ->
                        return (r, Http_frame.add_cookies cook cookies_to_set)
                    | (Ext_continue_with _, Filter _) -> return res
                    | (Ext_stop _, _) -> return res
                    | (Ext_retry_with _, _) -> return res
                )
                (return (Ext_not_found prev_err, cookies_to_set))
                extlist
                >>= fun (res_ext, cookies_to_set) ->
                  (match res_ext with
                  | Ext_found r -> 
                      return (add_to_res_cookies cookies_to_set r)
                  | Ext_not_found e
                  | Ext_stop e -> aux ri e cookies_to_set l (* try next site *)
                  | Ext_continue_with (_, cook, e) -> 
                      aux ri e
                        (Http_frame.add_cookies cook cookies_to_set) l
                  | Ext_retry_with (ri2, cook) -> 
                      (*VVV not enough to detect loops *)
                      if ri != ri2 then
                        do2 
                          (get_sites ())
                          (Http_frame.add_cookies cook cookies_to_set)
                          ri2
                        (* retry all *)
                      else
                        fail Ocsigen_Looping_request
                  )
          )
      | (h, p, _, _)::l ->
          Messages.debug (fun () ->
            "-------- host = "^
            (string_of_host_option host)^
            " does not match "^(string_of_host h));
          aux ri prev_err cookies_to_set l
    in aux ri 404 cookies_to_set sites
  in do2 (get_sites ()) Http_frame.Cookies.empty ri
(*****************************************************************************)


(* Ces deux trucs sont dans Neturl version 1.1.2 mais en attendant qu'ils
   soient dans debian, je les mets ici *)
let problem_re = Pcre.regexp "[ <>\"{}|\\\\^\\[\\]`]"

let fixup_url_string =
  Netstring_pcre.global_substitute
    problem_re
    (fun m s ->
       Printf.sprintf "%%%02x" 
        (Char.code s.[Netstring_pcre.match_beginning m]))
;;

let parse_url url =
  
  let url = fixup_url_string url in
  
  let url2 = 
    (Neturl.parse_url 
       ~base_syntax:(Hashtbl.find Neturl.common_url_syntax "http")
       (* ~accept_8bits:true *)
       (* Neturl.fixup_url_string url *)
       url)
  in

  (* Note that the fragment (string after #) is not sent by browsers *)
  
(*    let path = 
   (Neturl.string_of_url
   (Neturl.remove_from_url 
   ~param:true
   ~query:true 
   ~fragment:true 
   url2)) in *)
  
  let params = 
    try
      Some (Neturl.url_query ~encoded:true url2)
    with Not_found -> None
        (* Neturl.string_of_url
           (Neturl.remove_from_url
           ~user:true
           ~user_param:true
           ~password:true
           ~host:true
           ~port:true
           ~path:true
           ~other:true
           url2) *) 
  in
  
  let get_params = 
    lazy 
      (let params_string = 
        try
          Neturl.url_query ~encoded:true url2
        with Not_found -> ""
      in
      Netencoding.Url.dest_url_encoded_parameters params_string)
  in

  let path =
    (Ocsimisc.remove_dotdot 
       (Ocsimisc.remove_slash_at_beginning 
          (Neturl.url_path url2)))
      (* here we remove .. from paths, at it is dangerous.
         But in some very particular cases, we may want them?
         I prefer forbid that.
       *)
  in

  (url, url2, path, params, get_params)
    

let ri_of_url url ri =
  let (url, url2, path, params, get_params) = parse_url url in
  {ri with
   ri_url_string = url;
   ri_url = url2;
   ri_path_string = string_of_url_path path;
   ri_full_path = path;
   ri_sub_path = path;
   ri_get_params_string = params;
   ri_get_params = get_params;
 }


(*****************************************************************************)
(* This is used by server.ml. 
   I put that here because I need it to be accessible for profiling. *)
let get_number_of_connected, 
  incr_connected, 
  decr_connected =
  let connected = ref 0 in
  ((fun () -> !connected),
   (fun () -> connected := !connected + 1),
   (fun () -> connected := !connected - 1))




(*****************************************************************************)
(* To give parameters to extensions: *)
let dynlinkconfig = ref []
let set_config s = dynlinkconfig := s
let get_config () = !dynlinkconfig


