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

type client = Http_com.connection

let client_id = Http_com.connection_id
let client_connection x = x
let client_of_connection x = x

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
     ri_client: client; (** The request connection *)
   }

   
type answer =
  | Ext_found of (unit -> Http_frame.result Lwt.t)
      (** "OK stop! I will take the page.
          You can start the following request of the same pipelined connection.
          Here is the function to generate the page". 
          The extension must return Ext_found as soon as possible
          when it is sure it is safe to start next request.
          Usually immediately. But in some case, for example proxies, 
          you don't want the request of one connection to be handled in
          different order.
      *)
  | Ext_next of int (** Page not found. Try next extension.
                        The integer is the HTTP error code.
                        It is usally 404, but may be for ex 403 (forbidden)
                        if you want another extension to try after a 403.
                        Same as Ext_continue_with but does not change
                        the request.
                    *)
  | Ext_stop_site of int   (** Error. Do not try next extension, but
                            try next site. 
                            The integer is the HTTP error code, usally 403.
                          *)
  | Ext_stop_all of int      (** Error. Do not try next extension, do not
                            try next site. It is equivalent to
                            send an Ext_found with an error code
                            but you can not personnalize the page.
                            The integer is the HTTP error code, usally 403.
                          *)
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
  | Ext_sub_result of extension2


and request_state =
  | Req_not_found of (int * request_info)
  | Req_found of (request_info * (unit -> Http_frame.result Lwt.t))

and extension2 =
    (unit -> unit) ->
      Http_frame.cookie Http_frame.Cookievalues.t Http_frame.Cookies.t ->
      string ->
      request_state ->
      (answer * 
         Http_frame.cookie Http_frame.Cookievalues.t Http_frame.Cookies.t)
        Lwt.t

type extension = string -> request_state -> answer Lwt.t


type parse_fun = Simplexmlparser.xml list -> extension2

let (sites : (virtual_hosts * url_path * string option (* charset *) * 
                extension2) list ref) = 
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
      (fun host path charset (parse_site : parse_fun) (xml : Simplexmlparser.xml) -> 
        (raise (Bad_config_tag_for_extension "<no extension loaded>")
           : extension))
  in
  let fun_beg = ref (fun () -> ()) in
  let fun_end = ref (fun () -> ()) in
  let fun_exn = ref (fun exn -> raise exn) in

  ((* ********* register_extension ********* *)
     (fun ?(respect_pipeline=false)
         new_fun_site
         begin_init
         end_init
         handle_exn ->
       
       if respect_pipeline then Ocsiconfig.set_respect_pipeline ();

       let old_fun_site = !fun_site in
       fun_site := 
         (fun host ->
           let oldf = old_fun_site host in
           let newf = new_fun_site host in
           fun path charset ->
             let oldf = oldf path charset in
             let newf = newf path charset in
             fun parse_site config_tag ->
               try
                 oldf parse_site config_tag
               with
               | Bad_config_tag_for_extension c -> newf parse_site config_tag
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




let add_to_res_cookies res cookies_to_set =
  if cookies_to_set = Http_frame.Cookies.empty then
    res
  else 
    {res with 
     Http_frame.res_cookies = 
     Http_frame.add_cookies res.Http_frame.res_cookies cookies_to_set}

let rec make_ext awake cookies_to_set charset req_state genfun f =
  let rec aux cookies_to_set = function
    | Ext_found r -> 
        awake ();
        r () >>= fun r' ->
        let ri = match req_state with
          | Req_found (ri, _) -> ri
          | Req_not_found (_, ri) -> ri
        in
        f 
          awake
          Http_frame.Cookies.empty
          charset
          (Req_found (ri, 
                      fun () -> 
                        Lwt.return (add_to_res_cookies r' cookies_to_set)))
    | Ext_next e ->
        let ri = match req_state with
          | Req_found (ri, _) -> ri
          | Req_not_found (_, ri) -> ri
        in
        f awake cookies_to_set charset (Req_not_found (e, ri))
    | Ext_continue_with (ri, cook, e) -> 
        f 
          awake
          (Http_frame.add_cookies cook cookies_to_set)
          charset
          (Req_not_found (e, ri))
    | Ext_stop_site _
    | Ext_stop_all _
    | Ext_retry_with _ as res -> Lwt.return (res, cookies_to_set)
    | Ext_sub_result sr ->
        sr awake cookies_to_set charset req_state 
        >>= fun (res, cookies_to_set) ->
        aux cookies_to_set res
  in
  genfun charset req_state >>= aux cookies_to_set


let parse_site host =
  let f = parse_site_item host in (* creates all host data, if any *)
  fun path charset -> 
    let f = f path charset in (* creates all site data, if any *)
    let rec aux = function
      | [] ->
          (fun (awake : unit -> unit) cookies_to_set charset -> function
            | Req_found (ri, res) -> 
                Lwt.return (Ext_found res,
                            cookies_to_set)
            | Req_not_found (e, ri) -> 
                Lwt.return (Ext_next e, cookies_to_set))
      | xmltag::ll -> 
          try
            let genfun = 
              f 
                aux
                xmltag 
            in
            let genfun2 = aux ll in
            fun awake cookies_to_set charset req_state -> 
              make_ext awake cookies_to_set charset req_state genfun genfun2
          with
          | Bad_config_tag_for_extension t -> 
              ignore
                (Messages.errlog
                   ("Unexpected tag <"^t^"> inside <site dir=\""^
	            (Ocsimisc.string_of_url_path path)^"\"> (ignored)"));
              aux ll
          | Ocsiconfig.Config_file_error t
          | Error_in_config_file t -> 
              ignore
                (Messages.errlog
                   ("Error while parsing configuration file: "^
                    t^" (ignored)"));
              aux ll
          | e -> 
              ignore
                (Messages.errlog
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


let site_match site_path url = 
  (* We are sure that there is no / at the end or beginning of site_path *)
  (* and no / at the beginning of url *)
  (* and no // or ../ inside both of them *)
  (* We return the subpath without / at beginning *)
  let rec aux site_path url = 
    match site_path, url with
      | [], [] -> raise Ocsigen_Is_a_directory
      | [], p -> Some p
      | a::l, aa::ll when a = aa -> aux l ll 
      | _ -> None
  in
  match site_path, url with
    | [], [] -> Some []
    | _ -> aux site_path url



let do_for_site_matching host port ri =

  let conn = client_connection ri.ri_client in
  let awake =
    let tobeawoken = ref true in
    (* must be awoken once and only once *)
    fun () ->
      if !tobeawoken then begin
        tobeawoken := false;
        Http_com.awake_next_request conn
      end
  in

  let rec do2 sites cookies_to_set ri =
    let string_of_host_option = function
      | None -> "<no host>:"^(string_of_int port)
      | Some h -> h^":"^(string_of_int port)
    in
    let rec aux ri prev_err cookies_to_set = function
      | [] -> fail (Ocsigen_http_error prev_err)
      | (h, path, charset, genfun)::l when host_match host port h ->
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
                        ri_sub_path = sub_path; 
                        ri_sub_path_string = 
                        lazy (Ocsimisc.string_of_url_path sub_path)}
              in
              let charset = match charset with 
              | None -> Ocsiconfig.get_default_charset ()
              | _ -> charset
              in
              let charset = match charset with 
              | None -> "utf-8"
              | Some charset -> charset
              in
              genfun
                awake
                cookies_to_set
                charset
                (Req_not_found (prev_err, ri))
              >>= fun (res_ext, cookies_to_set) ->
              (match res_ext with
                 | Ext_found r -> 
                     awake ();
                     r () >>= fun r' -> 
                     return (add_to_res_cookies r' cookies_to_set)
                 | Ext_next e
                 | Ext_stop_site e -> 
                     aux ri e cookies_to_set l (* try next site *)
                 | Ext_stop_all e -> 
                     fail (Ocsigen_http_error e)
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
                 | Ext_sub_result sr -> 
                     assert false
              )
          )
      | (h, p, _, _)::l ->
          Messages.debug (fun () ->
            "-------- host = "^
            (string_of_host_option host)^
            " does not match "^(string_of_host h));
          aux ri prev_err cookies_to_set l
    in aux ri 404 cookies_to_set sites
  in 
  Lwt.finalize
    (fun () ->
       do2 (get_sites ()) Http_frame.Cookies.empty ri
    )
    (fun () ->
       awake ();
       Lwt.return ()
    )

    
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
    (Ocsimisc.remove_dotdot (* and remove "//" *)
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



