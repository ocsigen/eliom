(* Ocsigen
 * http://www.ocsigen.org
 * Module server.ml
 * Copyright (C) 2005 Vincent Balat and Denis Berthod
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Lwt
open Messages
open Ocsigen
open Http_frame
open Http_com
open Sender_helpers

exception Ocsigen_Timeout

(******************************************************************)
(* Config file parsing *)

open Simplexmlparser
open ExpoOrPatt
open Ocsiconfig

(* I put the parser here and not in config.ml because of cyclic dependancies *)

(* My xml parser is not really adapted to this.
   It is the parser for the syntax extension.
   But it works.
 *)

type modules = Cmo of string | Mod of string list * string

let rec parser_config = 
  let rec verify_empty = function
      PLEmpty -> ()
    | PLCons ((EPcomment _), l) -> verify_empty l
    | PLCons ((EPwhitespace _), l) -> verify_empty l
    | _ -> raise (Config_file_error "Don't know what to do with tailing data")
  in
  let rec parse_string = function
      PLEmpty -> ""
    | PLCons ((EPpcdata s), l) -> s^(parse_string l)
    | PLCons ((EPwhitespace s), l) -> s^(parse_string l)
    | PLCons ((EPcomment _), l) -> parse_string l
    | _ -> raise (Config_file_error "string expected")
  in let rec parse_site2 (cmo,stat) = function
      PLCons ((EPanytag ("module", PLEmpty, s)), l) -> 
	(match cmo with
	  None -> parse_site2 (Some (parse_string s),stat) l
	| _ -> raise 
	      (Config_file_error "Only one <module> tag allowed inside <url>"))
    | PLCons ((EPanytag ("staticdir", PLEmpty, s)), l) -> 
	(match stat with
	  None -> parse_site2 (cmo, Some (parse_string s)) l
	| _ -> raise 
	      (Config_file_error 
		 "Only one <staticdir> tag allowed inside <url>"))
    | PLCons ((EPcomment _), l) -> parse_site2 (cmo,stat) l
    | PLCons ((EPwhitespace _), l) -> parse_site2 (cmo,stat) l
    | PLEmpty -> 
	(match (cmo,stat) with
	  None, None -> raise (Config_file_error "<module> or <staticdir> tag expected inside <site>")
	| _ -> (cmo,stat))
    | _ -> raise 
	  (Config_file_error "Only <module> or <staticdir> tag expected inside <site>")
  in
  let rec parse_site = function
      PLCons ((EPanytag ("url", PLEmpty, s)), l) -> 
	let path = Neturl.split_path (parse_string s) in
	let cmo,static = parse_site2 (None, None) l in
	(match static with
	  None -> ()
	| Some s -> Ocsiconfig.set_static_dir s path);
	(match cmo with
	  None -> []
	| Some cmo -> [Mod (path,cmo)])
    | PLCons ((EPcomment _), l) -> parse_site l
    | PLCons ((EPwhitespace _), l) -> parse_site l
    | _ -> raise (Config_file_error "<url> tag expected inside <site>")
  in
  let rec parse_ocsigen = function
      PLEmpty -> []
    | PLCons ((EPanytag ("port", PLEmpty, p)), ll) -> 
	set_port (int_of_string (parse_string p));
	parse_ocsigen ll
    | PLCons ((EPanytag ("logdir", PLEmpty, p)), ll) -> 
	set_logdir (parse_string p);
	parse_ocsigen ll
    | PLCons ((EPanytag ("staticdir", PLEmpty, p)), ll) -> 
	set_staticpages (parse_string p);
	parse_ocsigen ll
    | PLCons ((EPanytag ("user", PLEmpty, p)), ll) -> 
	set_user (parse_string p);
	parse_ocsigen ll
    | PLCons ((EPanytag ("group", PLEmpty, p)), ll) -> 
	set_group (parse_string p);
	parse_ocsigen ll
    | PLCons ((EPanytag ("maxconnected", PLEmpty, p)), ll) -> 
	set_max_number_of_connections (int_of_string (parse_string p));
	parse_ocsigen ll
    | PLCons ((EPanytag ("timeout", PLEmpty, p)), ll) -> 
	set_connect_time_max (float_of_string (parse_string p));
	parse_ocsigen ll
    | PLCons ((EPanytag ("dynlink", PLEmpty,l)), ll) -> 
	(Cmo (parse_string l))::parse_ocsigen ll
    | PLCons ((EPanytag ("site", PLEmpty, l)), ll) -> 
	(parse_site l)@(parse_ocsigen ll)
    | PLCons ((EPcomment _), ll) -> parse_ocsigen ll
    | PLCons ((EPwhitespace _), ll) -> parse_ocsigen ll
    | PLCons ((EPanytag (tag, PLEmpty, l)), ll) -> 
	raise (Config_file_error ("tag "^tag^" unexpected inside <ocsigen>"))
    | _ ->
	raise (Config_file_error "Syntax error")
  in function 
      PLCons ((EPanytag ("ocsigen", PLEmpty, l)), ll) -> 
	verify_empty ll; 
	parse_ocsigen l
    | PLCons ((EPcomment _), ll) -> parser_config ll
    | PLCons ((EPwhitespace _), ll) -> parser_config ll
    | _ -> raise (Config_file_error "<ocsigen> tag expected")



let parse_config () = parser_config Ocsiconfig.config

(******************************************************************)

let _ = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

module Content = 
  struct
    type t = string
    let content_of_string c = c
    let string_of_content s = s
  end

module Http_frame = FHttp_frame (Content)

module Http_receiver = FHttp_receiver (Content)

(*
let _ = Unix.set_nonblock Unix.stdin
let _ = Unix.set_nonblock Unix.stdout
let _ = Unix.set_nonblock Unix.stderr
*)

let new_socket () = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
let local_addr num = Unix.ADDR_INET (Unix.inet_addr_any, num)

let ip_of_sockaddr = function
    Unix.ADDR_INET (ip,port) -> Unix.string_of_inet_addr ip
  | _ -> "127.0.0.1"

let error_page s =
          <<
          <html>
          <body>
          <h1> Error </h1>
          <p>$str:s$</p>
          </body>
          </html>
          >>


exception Ocsigen_Malformed_Url

(* *)
let get_frame_infos =
(*let full_action_param_prefix = action_prefix^action_param_prefix in
let action_param_prefix_end = String.length full_action_param_prefix - 1 in*)
  fun http_frame ->
  try 
    let url = Http_header.get_url http_frame.Http_frame.header in
    let url2 = 
      Neturl.parse_url 
	~base_syntax:(Hashtbl.find Neturl.common_url_syntax "http")
	url
    in
    let path = Neturl.string_of_url
	(Neturl.remove_from_url 
	   ~param:true
	   ~query:true 
	   ~fragment:true 
	   url2) in
    let params = Neturl.string_of_url
	(Neturl.remove_from_url
	   ~user:true
	   ~user_param:true
	   ~password:true
	   ~host:true
	   ~port:true
	   ~path:true
	   ~other:true
	   url2) in
    let params_string = try
      Neturl.url_query ~encoded:true url2
    with Not_found -> ""
    in
    let get_params = Netencoding.Url.dest_url_encoded_parameters params_string 
    in
    let post_params =
      match http_frame.Http_frame.content with
	  None -> []
	| Some s -> Netencoding.Url.dest_url_encoded_parameters s
    in
    let internal_state,post_params2 = 
      try (Some (int_of_string (List.assoc state_param_name post_params)),
	   List.remove_assoc state_param_name post_params)
      with Not_found -> (None, post_params)
    in
    let internal_state2,get_params2 = 
      try 
	match internal_state with
	    None ->
	      (Some (int_of_string (List.assoc state_param_name get_params)),
	       List.remove_assoc state_param_name get_params)
	  | _ -> (internal_state, get_params)
      with Not_found -> (internal_state, get_params)
    in
    let action_info, post_params3 =
      try
	let action_name, pp = 
	  ((List.assoc (action_prefix^action_name) post_params2),
	   (List.remove_assoc (action_prefix^action_name) post_params2)) in
	let reload,pp2 =
	  try
	    ignore (List.assoc (action_prefix^action_reload) pp);
	    (true, (List.remove_assoc (action_prefix^action_reload) pp))
	  with Not_found -> false, pp in
	let ap,pp3 = pp2,[]
(*	  List.partition 
	    (fun (a,b) -> 
	       ((String.sub a 0 action_param_prefix_end)= 
		   full_action_param_prefix)) pp2 *) in
	  (Some (action_name, reload, ap), pp3)
      with Not_found -> None, post_params2 in
    let useragent = try (Http_header.get_headers_value
			   http_frame.Http_frame.header "user-agent")
    with _ -> ""
    in
    (Ocsigen.remove_slash (Neturl.url_path url2), 
                              (* the url path (string list) *)
       path,
       params,
       internal_state2,
       get_params2,
       post_params3,
       useragent), action_info
  with _ -> raise Ocsigen_Malformed_Url


let rec getcookie s =
  let rec firstnonspace s i = 
    if s.[i] = ' ' then firstnonspace s (i+1) else i in
  let longueur = String.length s in
  let pointvirgule = try 
    String.index s ';'
  with Not_found -> String.length s in
  let egal = String.index s '=' in
  let first = firstnonspace s 0 in
  let nom = (String.sub s first (egal-first)) in
  if nom = cookiename 
  then String.sub s (egal+1) (pointvirgule-egal-1)
  else getcookie (String.sub s (pointvirgule+1) (longueur-pointvirgule-1))
(* On peut améliorer ça *)

let remove_cookie_str = "; expires=Wednesday, 09-Nov-99 23:12:40 GMT"

let find_static_page path =
  let rec aux dir (Ocsiconfig.Static_dir (dir_option, subdir_list)) = function
      [] -> (match dir_option with
	None -> dir
      | Some s -> s)
    | ""::l -> aux dir (Ocsiconfig.Static_dir (dir_option, subdir_list)) l
    | ".."::l -> aux dir (Ocsiconfig.Static_dir (dir_option, subdir_list)) l
	  (* For security reasons, .. is not allowed in paths *)
    | a::l -> try 
	let e = (List.assoc a subdir_list) in
	aux (dir^"/"^a) e l
    with Not_found -> 
      (match dir_option with
	None -> dir
      | Some s -> s)^"/"^(Ocsigen.reconstruct_url_path (a::l))
  in aux "/" (Ocsiconfig.get_static_tree ()) path

let service http_frame sockaddr 
    xhtml_sender file_sender empty_sender () =
  try 
    let cookie = 
      try 
	Some (getcookie (Http_header.get_headers_value 
			   http_frame.Http_frame.header "Cookie"))
      with _ -> None
    in
    let (path,stringpath,params,_,_,_,ua) as frame_info, action_info = 
      get_frame_infos http_frame in

      (* log *)
	let ip = ip_of_sockaddr sockaddr in
	accesslog ("connection from "^ip^" ("^ua^") : "^stringpath^params);
      (* end log *)

      match action_info with
	  None ->
	    (* Je préfère pour l'instant ne jamais faire de keep-alive pour
	       éviter d'avoir un nombre de threads qui croit sans arrêt *)
	    let keep_alive = false in
	    (try
	      let cookie2,page,path = get_page frame_info sockaddr cookie in
	      send_page ~keep_alive:keep_alive 
		?cookie:(if cookie2 <> cookie then 
		  (if cookie2 = None 
		  then Some remove_cookie_str
		  else cookie2) 
		else None)
		~path:path
		page xhtml_sender
	    with Ocsigen_404 ->
	      if params = "" then
		try
		  let filename = find_static_page path in
		  let dir = ((Unix.lstat filename).Unix.st_kind = Unix.S_DIR) in
		  let filename = 
		    if dir
		    then filename^"/index.html"
		    else filename
		  in
		  Messages.debug ("--- Is it a static file? ("^filename^")");
		  if ((Unix.lstat filename).Unix.st_kind = Unix.S_REG)
		  then begin
		    Unix.access filename [Unix.R_OK];
		    send_file 
		      ~keep_alive:keep_alive
		      ~last_modified:((Unix.stat filename).Unix.st_mtime)
		      ~code:200 filename file_sender
		  end
		  else 
		    send_error ~error_num:403 xhtml_sender (* Forbidden *)
		with 
		  Unix.Unix_error (Unix.EACCES,_,_) ->
		    send_error ~error_num:403 xhtml_sender (* Forbidden *)
		| _ -> raise Ocsigen_404
	      else raise Ocsigen_404
	    | Ocsigen.Ocsigen_Is_a_directory -> 
		send_empty
		  ~keep_alive:keep_alive
		  ~location:(stringpath^"/"^params)
                  ~code:301 (* Moved permanently *)
	          empty_sender		
	    )
	      >>= (fun _ -> return keep_alive)
	| Some (action_name, reload, action_params) ->
	    let cookie2,(),path = 
	      make_action 
		action_name action_params frame_info sockaddr cookie in
	    let keep_alive = false in
	      (if reload then
		 let cookie3,page,path = 
		   get_page frame_info sockaddr cookie2 in
		   (send_page ~keep_alive:keep_alive 
		      ?cookie:(if cookie3 <> cookie then 
				 (if cookie3 = None 
				  then Some remove_cookie_str
				  else cookie3) 
			       else None)
		      ~path:path
	              page xhtml_sender)
	       else
		 (send_empty ~keep_alive:keep_alive 
		    ?cookie:(if cookie2 <> cookie then 
			       (if cookie2 = None 
				then Some remove_cookie_str
				else cookie2) 
			     else None)
		    ~path:path
                    ~code:204
	            empty_sender)) >>=
		(fun _ -> return keep_alive)
  with Ocsigen_404 -> 
   (*really_write "404 Not Found" false in_ch "error 404 \n" 0 11 *)
   send_error ~error_num:404 xhtml_sender
   >>= (fun _ ->
     return true (* idem *))
    | Ocsigen_Malformed_Url ->
    (*really_write "404 Not Found ??" false in_ch "error ??? (Malformed URL) \n"
    * 0 11 *)
	send_error ~error_num:400 xhtml_sender
	>>= (fun _ ->
	       return true (* idem *))
    | e ->
	send_page ~keep_alive:false
	  (error_page ("Exception : "^(Printexc.to_string e)))
	  xhtml_sender
	>>= (fun _ ->
	       return true (* idem *))
                                              

let load_modules modules_list =
  let rec aux = function
      [] -> ()
    | (Cmo s)::l -> Dynlink.loadfile s; aux l
    | (Mod (path,cmo))::l -> 
	Ocsigen.load_ocsigen_module ~dir:path ~cmo:cmo; 
	aux l
  in
  Dynlink.init ();
  aux modules_list

(** Thread waiting for events on a the listening port *)
let listen modules_list =
  
  let listen_connexion receiver in_ch sockaddr 
      xhtml_sender file_sender empty_sender =
    
    let rec listen_connexion_aux () =
      let analyse_http () =
	choose
	  [Http_receiver.get_http_frame receiver ();
	   (Lwt_unix.sleep (get_connect_time_max ()) >>= 
	    (fun () -> fail Ocsigen_Timeout))] >>=
	(fun http_frame ->
          catch 
	    (service http_frame sockaddr 
	       xhtml_sender file_sender empty_sender)
	    fail
            >>= (fun keep_alive -> 
	      if keep_alive then
                listen_connexion_aux ()
                  (* Pour laisser la connexion ouverte, je relance *)
	      else begin 
		Unix.close in_ch; 
		return ()
	      end)
	) in
      catch 
	analyse_http
	(function
            Http_error.Http_exception (_,_) as http_ex ->
              (*let mes = Http_error.string_of_http_exception http_ex in
		 really_write "404 Plop" (* à revoir ! *) 
		 false in_ch mes 0 
		 (String.length mes);*)
	      Unix.close in_ch;
              send_error ~http_exception:http_ex xhtml_sender
          | exn -> fail exn)
	
    in listen_connexion_aux ()
      
  in 
  let wait_connexion socket =
    let handle_exn sockaddr in_ch exn = 
      let ip = ip_of_sockaddr sockaddr in
      Unix.close in_ch;
      match exn with
	Unix.Unix_error (e,func,param) ->
	  warning ("While talking to "^ip^": "^(Unix.error_message e)^
		  " in function "^func^" ("^param^") - (I continue)");
	  return ()
      | Com_buffer.End_of_file -> return ()
      | Ocsigen_HTTP_parsing_error (s1,s2) ->
	  errlog ("While talking to "^ip^": HTTP parsing error near ("^s1^") in:\n"^
		  (if (String.length s2)>2000 
		  then ((String.sub s2 0 1999)^"...<truncated>")
		  else s2)^"\n---");
	  return ()
      | Ocsigen_Timeout -> warning ("While talking to "^ip^": Timeout");
	  return ()
      | exn -> 
	  errlog ("While talking to "^ip^": Uncaught exception - "
		  ^(Printexc.to_string exn)^" - (I continue)");
	  return ()
    in
    let handle_connection (inputchan, sockaddr) =
      debug "\n__________________NEW CONNECTION__________________________";
      catch
	(fun () -> 
	  let server_name = ("Ocsigen server ("^Ocsiconfig.version_number^")") in
	  let xhtml_sender =
	    create_xhtml_sender ~server_name:server_name inputchan 
	  in
	  let file_sender =
	    create_file_sender ~server_name:server_name inputchan
	  in
	  let empty_sender =
	    create_empty_sender ~server_name:server_name inputchan
	  in
	  listen_connexion 
	    (Http_receiver.create inputchan) 
	    inputchan sockaddr xhtml_sender
	    file_sender empty_sender)
	(handle_exn sockaddr inputchan)
    in
    let rec wait_connexion_rec = 
      fun () ->
	Lwt_unix.accept socket >>= 
	(fun c ->
	  incr_connected ();
	  if (get_number_of_connected ()) <
	    (get_max_number_of_connections ()) then
	    ignore_result (wait_connexion_rec ())
	  else warning ("Max simultaneous connections ("^
			(string_of_int (get_max_number_of_connections ()))^
			")reached!!");
	  handle_connection c) >>= 
	(fun () -> 
	  decr_connected (); 
	  if (get_number_of_connected ()) = 
	    (get_max_number_of_connections ()) - 1
	  then begin
	    warning "Ok releasing one connection";
	    wait_connexion_rec ()
	  end
	  else return ())
    in wait_connexion_rec ()
  in
  ((* Initialize the listening address *)
     new_socket () >>= (fun listening_socket ->
       Unix.setsockopt listening_socket Unix.SO_REUSEADDR true;
       Unix.bind listening_socket (local_addr (Ocsiconfig.get_port ()));
       Unix.listen listening_socket 1;
       (* I change the user for the process *)
       (try
	 Unix.setgid (Unix.getgrnam (Ocsiconfig.get_group ())).Unix.gr_gid;
	 Unix.setuid (Unix.getpwnam (Ocsiconfig.get_user ())).Unix.pw_uid;
       with e -> errlog ("Error: Wrong user or group"); raise e);
       (* Now I can load the modules *)
       load_modules modules_list;
       Ocsigen.end_initialisation ();
       warning "Ocsigen has been launched (initialisations ok)";
       wait_connexion listening_socket >>=
       wait))

let _ = 
  try
    Lwt_unix.run (Unix.handle_unix_error listen (parse_config ()))
  with
    Ocsigen.Ocsigen_duplicate_registering s -> 
      errlog ("Fatal - Duplicate registering of url \""^s^"\". Please correct the module.")
  | Ocsigen.Ocsigen_there_are_unregistered_url s ->
      errlog ("Fatal - Some public url have not been registered. Please correct your modules. (ex: "^s^")")
  | Ocsigen.Ocsigen_page_erasing s ->
      errlog ("Fatal - Page or directory erased: "^s^". Please correct your modules.")
  | Ocsigen.Ocsigen_register_for_session_outside_session ->
      errlog ("Fatal - Register session during initialisation forbidden.")
  | Dynlink.Error e -> errlog ("Fatal - "^(Dynlink.error_message e))
  | exn -> errlog ("Fatal - Uncaught exception: "^(Printexc.to_string exn))
