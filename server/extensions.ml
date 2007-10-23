(* Ocsigen
 * http://www.ocsigen.org
 * Module pagegen.ml
 * Copyright (C) 2005 Vincent Balat
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
(** Writing extensions for Ocsigen                                           *)
(*****************************************************************************)
(*****************************************************************************)


open Lwt
open Ocsimisc

exception Ocsigen_404
exception Ocsigen_403
exception Ocsigen_Is_a_directory
exception Ocsigen_malformed_url
exception Ocsigen_Internal_Error of string

exception Bad_config_tag_for_extension of string
exception Error_in_config_file of string

(*****************************************************************************)
(** type of URL, without parameter *)
type url_path = string list

let string_of_url_path = Ocsimisc.string_of_url_path


type file_info = {tmp_filename: string;
                  filesize: int64;
                  original_filename: string}


type cookies = 
  | Set of string list option * float option * (string * string) list
  | Unset of (string list option * string list)

type cookieslist = cookies list

let change_cookie = function
  | Set (a, b, c) -> (a, b, c)
  | Unset (a, b) -> (a, (Some 0.), (List.map (fun v -> (v,"")) b))

(* virtual hosts: *)
type virtual_host_part = Text of string * int | Wildcard
type virtual_hosts = ((virtual_host_part list) * int option) list

(* Requests *)
type request_info = 
    {ri_url_string: string;
     ri_url: Neturl.url;
     ri_method: Http_frame.Http_header.http_method;
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

     ri_accept: ((string option * string option) * float option * (string * string) list) list Lazy.t; (** Accept HTTP header. For example [(Some "text", None)] means ["text/*"]. The float is the "quality" value, if any. The last association list is for other extensions. *)
     ri_accept_charset: (string option * float option) list Lazy.t; (** Accept-Charset HTTP header. [None] for the first value means "*". The float is the "quality" value, if any. *)
     ri_accept_encoding: (string option * float option) list Lazy.t; (** Accept-Encoding HTTP header. [None] for the first value means "*". The float is the "quality" value, if any. *)
     ri_accept_language: (string * float option) list Lazy.t; (** Accept-Language HTTP header. The float is the "quality" value, if any. *)

     ri_http_frame: Http_frame.t; (** The full http_frame *)
   }

type result =
    {res_cookies: cookieslist; (** cookies to set (with optional path) *)
     res_lastmodified: float option;
     res_etag: Http_frame.etag option;
     res_code: int option; (* HTTP code, if not 200 *)
     res_send_page: Predefined_senders.send_page_type;
     res_headers: Http_headers.t;
     res_charset: string option;
     res_filter: Predefined_senders.stream_filter_type option
   }
   
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
           extensions. *)
  | Ext_retry_with of request_info * cookieslist
        (** Used to retry all the extensions with a new request_info ;
           May set cookies (idem) *)

let (virthosts : 
       (virtual_hosts * 
          (request_info -> (answer * cookieslist) Lwt.t) *
          (request_info -> result -> result Lwt.t)
       ) list ref) = 
  ref []

let set_virthosts v = virthosts := v
let get_virthosts () = !virthosts
let add_virthost v = virthosts := !virthosts@[v]
   
(*****************************************************************************)
(** Tree of charsets *)
type charset_tree_type = 
    Charset_tree of (string option * (string * charset_tree_type) list)
        
let new_charset_tree () = 
  Charset_tree ((Ocsiconfig.get_default_charset ()), [])

let add_charset charset path (Charset_tree charset_tree) =
  let add_charset2 charset =
    let rec make_tree = function
      | [] -> (charset, [])
      | a::l -> (None, [(a, Charset_tree (make_tree l))])
    in
    let rec aux path charset_tree = 
      match path, charset_tree with
      | [], (enc, l2) -> (charset, l2)
      | (a::l), (enc, l2) ->
          try
            let (Charset_tree ct2),l3 = Ocsimisc.list_assoc_remove a l2 in
            (enc, (a, Charset_tree (aux l ct2))::l3)
          with Not_found -> 
            (enc, (a, Charset_tree (make_tree l))::l2)
    in
    Charset_tree (aux path charset_tree)
  in
  let charset = match charset with 
  | None -> Ocsiconfig.get_default_charset ()
  | _ -> charset
  in
  add_charset2 charset
        
let find_charset (Charset_tree charset_tree) path =
  let rec aux current path tree =
    let get_enc = function
      | None -> current
      | enc -> enc
    in
    match path, tree with
    | [], (enc,_) -> get_enc enc
    | (a::l), (enc, l2) ->
        try
          let Charset_tree ct2 = List.assoc a l2 in
          aux (get_enc enc) l ct2
        with Not_found -> get_enc enc
  in aux None path charset_tree

(*****************************************************************************)
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
module R = struct
  (* in a sub module to make possible Dynlink.prohibit ["Extensions.R"] *)


  let register_extension,
    register_output_filter,
    create_virthost, 
    get_beg_init, 
    get_end_init, 
    get_init_exn_handler =
    let defaultparseconfig path xml =
      raise (Bad_config_tag_for_extension "No extension loaded")
    in
    let fun_create_virthost =
      ref (fun hostpattern -> 
        let charset_tree = ref (new_charset_tree ()) in
        ((fun cs ri -> return ((Ext_not_found Ocsigen_404),[])),
         (fun ri res -> return res) (* default output filter *),
         defaultparseconfig,
         charset_tree))
    in
    let fun_beg = ref (fun () -> ()) in
    let fun_end = ref (fun () -> ()) in
    let fun_exn = ref (fun exn -> raise exn) in

    ((* ********* register_extension ********* *)
     (fun (fun_virthost, begin_init, end_init, handle_exn) ->
      let cur_fun = !fun_create_virthost in
      (* The function to create a new virtual host is created from
         the previous one (cur_fun) and fun_virthost
       *)
      fun_create_virthost := 
        (fun hostpattern -> 
          (* For each host, we create: *)
          let (g1, fil1, p1, charset_tree) = cur_fun hostpattern in
          let (g2, p2) = fun_virthost hostpattern in

          ((* ***** a function that will handle the requests: *)
           (fun charset ri ->
	     g1 charset ri >>=
             fun (ext_res, cookieslist) ->
               match ext_res with
               | Ext_not_found _ -> g2 charset ri >>= 
                   fun r -> return (r, cookieslist)
               | Ext_continue_with (ri', cookies_to_set) -> 
                   g2 (find_charset !charset_tree ri'.ri_path) ri' >>= 
                   fun r -> return (r, cookies_to_set@cookieslist)
               | r -> return (r, cookieslist)
           ),
           
           (* ***** a function that will filter the output stream: *)
           fil1,

           (* ***** A function to parse the config file for each site: *)
	   (fun path xml -> 
             try
               p1 path xml
             with 
             | Error_in_config_file _ as e -> raise e
             | Bad_config_tag_for_extension _ -> p2 path xml),
           
           (* ***** a tree of charsets to be used for each directory: *)
           charset_tree));
      
      fun_beg := comp begin_init !fun_beg;
      fun_end := comp end_init !fun_end;
      let curexnfun = !fun_exn in
      fun_exn := fun e -> try curexnfun e with e -> handle_exn e),
     
     (* ********* register_output_filter ********* *)
     (fun (fun_virthost, begin_init, end_init, handle_exn) ->
      let cur_fun = !fun_create_virthost in
      (* The function to create a new virtual host is created from
         the previous one (cur_fun) and fun_virthost
       *)
      fun_create_virthost := 
        (fun hostpattern -> 
          (* For each host, we create: *)
          let (g1, fil1, p1, charset_tree) = cur_fun hostpattern in
          let (fil2, p2) = fun_virthost hostpattern in

          ((* ***** a function that will handle the requests: *)
           g1,

           (* ***** a function that will filter the output stream: *)
           (fun ri res -> fil1 ri res >>= fil2 ri),

            (* ***** a function to parse the config file for each site: *)
	   (fun path xml -> 
             try
               p1 path xml
             with 
             | Error_in_config_file _ as e -> raise e
             | Bad_config_tag_for_extension _ -> p2 path xml),

            (* ***** a tree of charsets to be used for each directory: *)
           charset_tree));

      fun_beg := comp begin_init !fun_beg;
      fun_end := comp end_init !fun_end;
      let curexnfun = !fun_exn in
      fun_exn := fun e -> try curexnfun e with e -> handle_exn e),

     (* ********* create_virthost ********* *)
     (fun h ->
       let (f, fil, g, charset_tree) = !fun_create_virthost h in
       (((fun ri -> f (find_charset !charset_tree ri.ri_path) ri), fil, g),
        (fun charset path -> 
          charset_tree := add_charset charset path !charset_tree))),

     (* ********* get_beg_init ********* *)
     (fun () -> !fun_beg),

     (* ********* get_end_init ********* *)
     (fun () -> !fun_end),

     (* ********* get_init_exn_handler ********* *)
     (fun () -> !fun_exn)
    )


end    



let create_virthost = R.create_virthost
let get_beg_init = R.get_beg_init
let get_end_init = R.get_end_init
let get_init_exn_handler = R.get_init_exn_handler

(*****************************************************************************)
(* locks *)
(*
let synchronize =
  let lock = Mutex.create () in
  fun f ->
    Mutex.lock lock;
    let r = f () in
    Mutex.unlock lock;
    r
*)


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
        with _ -> false
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
            with _ -> false
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

exception Serv_no_host_match
let do_for_host_matching host port virthosts ri =
  let string_of_host_option = function
    | None -> "<no host>:"^(string_of_int port)
    | Some h -> h^":"^(string_of_int port)
  in
  let rec aux ri e = function
    | [] -> fail e
    | (h, f, output_filter)::l as ll when host_match host port h ->
        Messages.debug ("---- host found: "^(string_of_host_option host)^
                        " matches "^(string_of_host h));
        (f ri >>=
         fun (res, cookieslist) ->
           match res with
           | Ext_found r ->
               output_filter ri r >>= fun r ->
               return (r, cookieslist)
           | Ext_not_found e -> aux ri e l
           | Ext_continue_with _ -> aux ri Ocsigen_404 l
           | Ext_retry_with (ri', cookies_to_set) ->
               aux ri' e ll >>= fun (ext_res, cookieslist) ->
               return (ext_res, cookies_to_set@cookieslist)
        )

    | (h, _, _)::l ->
        Messages.debug ("---- host = "^(string_of_host_option host)^
                        " does not match "^(string_of_host h));
        aux ri e l
  in aux ri Serv_no_host_match virthosts

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
    with _ -> None
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
   ri_path = path;
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


