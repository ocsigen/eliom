(* Ocsigen
 * http://www.ocsigen.org
 * Module pagegen.ml
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
(** type of URL, without parameter *)
type url_path = string list
type current_url = string list
type current_dir = string list

type file_info = {tmp_filename: string;
                  filesize: int64;
                  original_filename: string}

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
     ri_cookies: (string * string) list; (** Cookies sent by the browser *)
     ri_ifmodifiedsince: float option;   (** if-modified-since field *)
     ri_http_frame: Predefined_senders.Stream_http_frame.http_frame} (** The full http_frame *)

type result =
    {res_cookies: (string * string) list;
     res_path: string;
     res_lastmodified: float option;
     res_etag: Http_frame.etag option;
     res_code: int option; (* HTTP code, if not 200 *)
     res_send_page: Predefined_senders.send_page_type;
     res_create_sender: Predefined_senders.create_sender_type
   }

type answer =
    Ext_found of result  (** OK stop! I found the page *)
  | Ext_not_found        (** Page not found. Try next extension. *)
  | Ext_continue_with of request_info (** Used to modify the request 
                                          before giving it to next extension *)


let (virthosts : (virtual_hosts * (request_info -> 
  answer Lwt.t)) list ref) = ref []

let set_virthosts v = virthosts := v
let get_virthosts () = !virthosts
let add_virthost v = virthosts := v::!virthosts

(*****************************************************************************)
(** We register for each extension three functions:
   - a function that will be called for each
   virtual server, generating two functions:
     -- one that will be called to generate the pages
     -- one to parse the configuration file
   - a function that will be called at the beginning 
   of the initialisation phase 
   - a function that will be called at the end of the initialisation phase 
   of the server
 *)

let register_extension, create_virthost, get_beg_init, get_end_init, 
  get_init_exn_handler =
  let fun_create_virthost =
    ref (fun hostpattern -> 
      ((fun ri -> return Ext_not_found), 
       (fun path xml -> raise (Error_in_config_file "No extension loaded"))))
  in
  let fun_beg = ref (fun () -> ()) in
  let fun_end = ref (fun () -> ()) in
  let fun_exn = ref (fun exn -> raise exn) in
  ((fun (fun_virthost,begin_init,end_init,handle_exn) ->
    let cur_fun = !fun_create_virthost in
    fun_create_virthost := 
      (fun hostpattern -> 
        let (g1,p1) = cur_fun hostpattern in
        let (g2,p2) = fun_virthost hostpattern in
        ((fun ri ->
	  g1 ri >>=
          function
            | (Ext_found _) as r -> return r
            | Ext_not_found -> g2 ri
            | Ext_continue_with ri' -> g2 ri'
         ),
	 (fun path xml -> 
           try
             p1 path xml
           with 
             Error_in_config_file _
           | Bad_config_tag_for_extension _ -> p2 path xml)));
    fun_beg := comp begin_init !fun_beg;
    fun_end := comp end_init !fun_end;
    fun_exn := fun e -> try !fun_exn e with e -> handle_exn e),
   (fun h -> !fun_create_virthost h),
   (fun () -> !fun_beg),
   (fun () -> !fun_end),
   (fun () -> !fun_exn)
  )
    


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
let start_initialisation, during_initialisation, end_initialisation =
  let init = ref true in
   ((fun () -> 
     init := true;
     get_beg_init () ()
    ),
    (fun () -> !init), 
    (fun () -> 
      init := false;
      get_end_init () ()
    ))
    
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
          [] -> beg = hostlen
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
        [] -> false
      | (a, p)::l -> ((port_match p) && (host_match1 0 a)) || aux host l
  in match host with
    None -> List.exists (fun (_, p) -> port_match p)
      (* Warning! For HTTP/1.0 we take the first one,
         even if it doesn't match! 
         To be changed! *)
  | Some host -> aux host


let string_of_host h = 
  let aux1 (hh, port) = 
    let p = match port with
      None -> ""
    | Some a -> ":"^(string_of_int a)
    in
    let rec aux2 = function
        [] -> ""
      | Wildcard::l -> "*"^(aux2 l)
      | (Text (t,_))::l -> t^(aux2 l)
    in (aux2 hh)^p
  in List.fold_left (fun d hh -> d^(aux1 hh)^" ") "" h

exception Serv_no_host_match
let do_for_host_matching host port virthosts ri =
  let string_of_host_option = function
    None -> "<no host>:"^(string_of_int port)
  | Some h -> h^":"^(string_of_int port)
  in
  let rec aux e = function
      [] -> fail e
    | (h, f)::l when host_match host port h ->
        Messages.debug ("---- host found: "^(string_of_host_option host)^
                        " matches "^(string_of_host h));
        (f ri >>=
         function
           | Ext_found r -> return r
           | Ext_not_found
           | Ext_continue_with _ -> aux Ocsigen_404 l)

    | (h,_)::l ->
        Messages.debug ("---- host = "^(string_of_host_option host)^
                        " does not match "^(string_of_host h));
        aux e l
  in aux Serv_no_host_match virthosts


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





