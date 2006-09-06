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
open Ocsiconfig
open Parseconfig
open Error_pages

exception Ocsigen_Timeout
exception Ssl_Exception

(* Without the following line, it stops with "Broken Pipe" without raising
   an exception ... *)
let _ = Sys.set_signal Sys.sigpipe Sys.Signal_ignore
let ctx = Ssl.init ();
	  ref (Ssl.create_context Ssl.SSLv23 Ssl.Server_context)
	  
module Content = 
  struct
    type t = string
    let content_of_string c = Lwt.return c
    let stream_of_content s = 
    	let md5 = Digest.to_hex (Digest.string s) in
    	Lwt.return (String.length s, md5, Cont (s, (fun () -> Finished)))
  end

module Http_frame = FHttp_frame (Content)

module Http_receiver = FHttp_receiver (Content)


(* non blocking input and output (for use with lwt): *)
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


exception Ocsigen_Malformed_Url

let server_name = ("Ocsigen server ("^Ocsiconfig.version_number^")")


(* Ces deux trucs sont dans Neturl version 1.1.2 mais en attendant qu'ils
 soient dans debian, je les mets ici *)
let problem_re = Pcre.regexp "[ <>\"{}|\\\\^\\[\\]`]"

let fixup_url_string =
  Netstring_pcre.global_substitute
    problem_re
    (fun m s ->
       Printf.sprintf "%%%02x" (Char.code s.[Netstring_pcre.match_beginning m]))
;;

let get_boundary header =
	let cont_enc = Http_header.get_headers_value header "Content-Type" in
	let (_,res) = Netstring_pcre.search_forward
	   (Netstring_pcre.regexp "boundary=([^;]*);?") cont_enc 0 in
	  Netstring_pcre.matched_group res 1 cont_enc
let find_field field content_disp = 
	let (_,res) = Netstring_pcre.search_forward
	   (Netstring_pcre.regexp (field^"=.([^\"]*).;?")) content_disp 0 in
	  Netstring_pcre.matched_group res 1 content_disp

type to_write = No_File of string*Buffer.t | A_File of Lwt_unix.descr

(* *)
let get_frame_infos =
(*let full_action_param_prefix = action_prefix^action_param_prefix in
let action_param_prefix_end = String.length full_action_param_prefix - 1 in*)
  fun http_frame ->
  catch (fun () -> 
    let meth = Http_header.get_method http_frame.Http_frame.header in
    let url = Http_header.get_url http_frame.Http_frame.header in
    let url2 = 
      Neturl.parse_url 
	~base_syntax:(Hashtbl.find Neturl.common_url_syntax "http")
	(* ~accept_8bits:true *)
	(* Neturl.fixup_url_string url *)
	(fixup_url_string url)
    in
    let path = Neturl.string_of_url
	(Neturl.remove_from_url 
	   ~param:true
	   ~query:true 
	   ~fragment:true 
	   url2) in
    let host =
      if Ocsiconfig.get_virtual () then try
        let hostport = 
          Http_header.get_headers_value http_frame.Http_frame.header "Host" in
    	  String.sub hostport 0 (String.index hostport ':') 
        with Not_found -> ""
      else "" in
    Messages.debug ("host="^host);
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
    let find_post_params = 
    if meth = Some(Http_header.GET) || meth = Some(Http_header.HEAD) 
    then return [] else 
      match http_frame.Http_frame.content with
      Http_frame.Ready cnt -> begin match cnt with
	  None -> return []
	| Some s -> 
      	   Messages.debug ("content="^s);
	   (*
	   let ct = (Http_header.get_headers_value 
	   		http_frame.Http_frame.header "Content-Type") in
	   match (Netstring_pcre.string_match 
	   		(Netstring_pcre.regexp ".*multipart.*")) ct 0
	   with 
	    None -> *)return (Netencoding.Url.dest_url_encoded_parameters s) 
	   (*| _ -> 	(* File stockage *)*)
	   end
	|Http_frame.Streamed nstr -> begin
	     let bound = get_boundary http_frame.Http_frame.header in
             let param_names = ref [||] in
	     let create hs = 
	       let cd = List.assoc "content-disposition" hs in
	       let st = try Some (find_field "filename" cd) with _ -> None in
	       let p_name = find_field "name" cd in
	       match st with 
	       None -> No_File (p_name, Buffer.create 1024)
	       | Some store -> 
	           begin let now = Printf.sprintf "%sx-%f" store (Unix.gettimeofday ()) in
		   Messages.debug ("file="^now);
		   param_names := Array.append !param_names [|(p_name, now)|];
		   let fname = ((Ocsiconfig.get_uploaddir ())^"/"^now) in
		   let fd = Unix.openfile fname [Unix.O_CREAT;Unix.O_TRUNC;Unix.O_WRONLY;Unix.O_NONBLOCK] 0o666 in
		   A_File (Lwt_unix.Plain fd) end in
	       let add where from k n =  
		   let buf = Netbuffer.sub (from#window) k n in
		   match where with 
		   No_File (p_name, to_buf) -> Buffer.add_string to_buf buf;
		      				return ()
		   | A_File wh -> Messages.debug ("partsize="^string_of_int n);
		    		   Lwt_unix.write wh buf 0 n >>= 
				     (fun r -> Lwt_unix.yield ()) in
	     let stop  = function 
		  No_File (p_name, to_buf) -> 
		   param_names := Array.append !param_names 
		   			[|(p_name, Buffer.contents to_buf)|]
		| A_File wh -> match wh with 
			Lwt_unix.Plain fdscr-> Unix.close fdscr 
			| _ -> () in
	     Multipart.scan_multipart_body_from_netstream nstr bound create 
	     							add stop >>=
	     (fun () -> return (Array.to_list !param_names))
	     end
		(*	IN-MEMORY STOCKAGE *)
		(*	let bdlist = Mimestring.scan_multipart_body_and_decode s 0 
				(String.length s) bound in
			Messages.debug (string_of_int (List.length bdlist));
			let simplify (hs,b) = ((find_field "name" (List.assoc "content-disposition" hs)),b) in
			List.iter (fun (hs,b) -> List.iter (fun (h,v) -> Messages.debug (h^"=="^v)) hs) bdlist;
			List.map simplify bdlist *)
    in
    find_post_params >>= (fun post_params ->
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
    in return
    ((Ocsigen.remove_slash (host :: (Neturl.url_path url2)), 
                              (* the url path (string list) *)
       (host^path),
       params,
       internal_state2,
       get_params2,
       post_params3,
       useragent), action_info)))
  (function e -> Messages.debug (Printexc.to_string e); fail Ocsigen_Malformed_Url)


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
      | Some s -> s)^"/"^(Ocsigen.string_of_url_path (a::l))
  in 
  aux "/" (Ocsiconfig.get_static_tree ()) path

let service http_frame sockaddr 
    xhtml_sender file_sender empty_sender inputchan () =  			
    let head = ((Http_header.get_method http_frame.Http_frame.header) 
    		  = Some (Http_header.HEAD)) in
    let ka = try
      let kah =	String.lowercase 
	  (Http_header.get_headers_value
	     http_frame.Http_frame.header "Connection") 
      in 
      if kah = "close" then false else 
      (if kah = "keep-alive" then true else false (* should not happen *))
    with _ ->
      (* if prot.[(String.index prot '/')+3] = '1' *)
      if (Http_header.get_proto http_frame.Http_frame.header) = "HTTP/1.1"
      then true else false in
    Messages.debug ("Keep-Alive:"^(string_of_bool ka));
    Messages.debug("HEAD:"^(string_of_bool head));
   let serv =  
  catch (fun () ->
    let cookie = 
      try 
	Some (getcookie (Http_header.get_headers_value 
			   http_frame.Http_frame.header "Cookie"))
      with _ -> None
    in
    get_frame_infos http_frame >>=
    (fun ((path,stringpath,params,a,b,c,ua), action_info) -> 
      let frame_info = (path,stringpath,params,a,b,c,ua) in  
      (* log *)
	let ip = ip_of_sockaddr sockaddr in
	accesslog ("connection from "^ip^" ("^ua^") : "^stringpath^params);
      (* end log *)
      match action_info with
	  None ->
	    (* Je préfère pour l'instant ne jamais faire de keep-alive pour
	       éviter d'avoir un nombre de threads qui croit sans arrêt *)
	    let keep_alive = ka in
	    (catch
	       (fun () ->
		 get_page frame_info sockaddr cookie >>=
		 (fun cookie2,send_page,sender,path ->
		   send_page ~keep_alive:keep_alive 
		     ?cookie:(if cookie2 <> cookie then 
		       (if cookie2 = None 
		       then Some remove_cookie_str
		       else cookie2) 
		     else None)
		     ~path:path (* path pour le cookie *) ~head:head
		     (sender ~server_name:server_name inputchan)))
	       (function
		   Ocsigen_404 ->
		     if params = "" then
		       catch (fun () ->
			 let filename = find_static_page path in
			 Messages.debug ("--- Is it a static file? ("^filename^")");
			 ignore (Unix.lstat filename);
			 let dir = ((Unix.lstat filename).Unix.st_kind = Unix.S_DIR) in
			 let filename = 
			   if dir
			   then filename^"/index.html"
			   else filename
			 in
			 if ((Unix.lstat filename).Unix.st_kind = Unix.S_REG)
			 then begin
			   Unix.access filename [Unix.R_OK];
			   catch (fun () ->
			     send_file 
			       ~keep_alive:keep_alive
			       ~last_modified:((Unix.stat filename).Unix.st_mtime)
			       ~code:200 ~head:head filename file_sender)
			    (function _ -> Lwt.return ()) (* stops if error while sending *)
			 end
			 else 
			   send_error ~error_num:403 xhtml_sender (* Forbidden *))
		       (function
			   Unix.Unix_error (Unix.EACCES,_,_) ->
			     send_error ~error_num:403 xhtml_sender (* Forbidden *)
			 | _ -> fail Ocsigen_404 (*lstat errors, etc.*))
		     else fail Ocsigen_404
		 | Ocsigen.Ocsigen_Is_a_directory -> 
		     send_empty
		       ~keep_alive:keep_alive
		       ~location:(stringpath^"/"^params)
                       ~code:301 (* Moved permanently *)
	               ~head:head empty_sender		
		 | e -> fail e)
	      >>= (fun _ -> return keep_alive))
	| Some (action_name, reload, action_params) ->
	    make_action action_name action_params frame_info sockaddr cookie
	    >>= (fun (cookie2,path) ->
	      let keep_alive = ka in
	      (if reload then
		get_page frame_info sockaddr cookie2 >>=
		(fun cookie3,send_page,sender,path ->
		   (send_page ~keep_alive:keep_alive 
		      ?cookie:(if cookie3 <> cookie then 
				 (if cookie3 = None 
				  then Some remove_cookie_str
				  else cookie3) 
			       else None)
		      ~path:path ~head:head
	              (sender ~server_name:server_name inputchan)))
	       else
		 (send_empty ~keep_alive:keep_alive 
		    ?cookie:(if cookie2 <> cookie then 
			       (if cookie2 = None 
				then Some remove_cookie_str
				else cookie2) 
			     else None)
		    ~path:path
                    ~code:204 ~head:head
	            empty_sender)) >>=
		(fun _ -> return keep_alive))))
    (function
	Ocsigen_404 -> 
	  (*really_write "404 Not Found" false in_ch "error 404 \n" 0 11 *)
	  send_error ~error_num:404 xhtml_sender
	    >>= (fun _ ->
	      return ka (* keep_alive *))
      | Ocsigen_Malformed_Url ->
	  (*really_write "404 Not Found ??" false in_ch "error ??? (Malformed URL) \n"
	   * 0 11 *)
	  send_error ~error_num:400 xhtml_sender
	    >>= (fun _ -> return ka (* keep_alive *))
      | e ->
          send_error ~error_num:500 xhtml_sender
	    >>= (fun _ -> fail e))
       in 
       let meth = (Http_header.get_method http_frame.Http_frame.header) in
	if ((meth <> Some (Http_header.GET)) && 
	    (meth <> Some (Http_header.POST)) && 
	    (meth <> Some(Http_header.HEAD))) 
	then (send_error ~error_num:501 xhtml_sender>>=(fun _ -> return ka)) 
	else begin 
	  try
      	    if ((int_of_string 
		   (Http_header.get_headers_value http_frame.Http_frame.header 
		      "content-length")) > 0) &&
	      (meth = Some(Http_header.GET) || meth = Some(Http_header.HEAD))
	    then (send_error ~error_num:501 xhtml_sender >>= 
		  (fun _ -> return ka)) 
	    else serv  
          with _ -> 
	    if meth = Some(Http_header.POST)
	    then (send_error ~error_num:400 xhtml_sender
		    >>= (fun _ -> return ka )) else serv
	end 
       

let load_modules modules_list =
  let rec aux = function
      [] -> ()
    | (Cmo s)::l -> Dynlink.loadfile s; aux l
    | (Mod (path,cmo))::l -> 
	Ocsigen.load_ocsigen_module ~dir:path ~cmo:cmo; 
	aux l
  in
  Dynlink.init ();
  Dynlink.allow_unsafe_modules true;
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
	       xhtml_sender file_sender empty_sender in_ch)
	    fail
            >>= (fun keep_alive -> 
	      if keep_alive then
                listen_connexion_aux ()
                  (* Pour laisser la connexion ouverte, je relance *)
	      else begin 
		Lwt_unix.shutdown in_ch;
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
	      Lwt_unix.shutdown in_ch;
              send_error ~http_exception:http_ex xhtml_sender
          | exn -> fail exn)
	
    in listen_connexion_aux ()
      
  in 
  let wait_connexion socket =
    let handle_exn sockaddr in_ch exn = 
      let ip = ip_of_sockaddr sockaddr in
      (try
      Lwt_unix.shutdown in_ch;
      with e -> ());
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
	  return () (* should be a graceful close *)
      | Ssl.Write_error(Ssl.Error_ssl) -> errlog ("While talking to "^ip
                                       ^": Ssl broken pipe - (I continue)");
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
	  let xhtml_sender = 
	    Sender_helpers.create_xhtml_sender
	      ~server_name:server_name inputchan in
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
    let rec wait_connexion_rec = (fun () -> 
    	let rec do_accept () = Lwt_unix.accept (Lwt_unix.Plain(socket)) >>= 
    	(fun (s, sa) -> if Ocsiconfig.get_ssl () then begin
    	  let s_unix = match s with Lwt_unix.Plain fd -> fd 
    		         | _ -> raise Ssl_Exception (* impossible *) in
    	  catch 
    	     (fun () -> ((Lwt_unix.accept (Lwt_unix.Encrypted 
	                    (s_unix, Ssl.embed_socket s_unix !ctx))) >>=
			    (fun (ss, ssa) -> Lwt.return (ss, sa))))
    	  (function  Ssl.Accept_error e -> warning "Accept_error"; do_accept ()
    	     | e -> warning (Printexc.to_string e); do_accept ())
         end else Lwt.return (s, sa)) in
       (do_accept ()) >>= 
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
	  else return ()))
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
       if Ocsiconfig.get_ssl ()  then begin 
          if Ocsiconfig.get_passwd () <> "" then 
	    Ssl.set_password_callback !ctx (fun _ -> Ocsiconfig.get_passwd ());
	  Ssl.use_certificate !ctx (Ocsiconfig.get_certificate ()) (Ocsiconfig.get_key ());
	  print_string ("HTTPS server on port ");
	  print_endline (string_of_int (Ocsiconfig.get_port ()) ^" launched");
	  end;
       Ocsigen.end_initialisation ();
       warning "Ocsigen has been launched (initialisations ok)";
       wait_connexion listening_socket >>=
       Lwt.wait))

let _ = 
  parse_config ();
  (* let rec print_cfg n = Messages.debug (string_of_int n); if n < !Ocsiconfig.number_of_servers 
     then (Messages.debug ("port:" ^ (string_of_int (Ocsiconfig.cfgs.(n)).port )); print_cfg (n+1))
     else () in print_cfg 0; *)
  Messages.debug ("number_of_servers: "^ (string_of_int !Ocsiconfig.number_of_servers));
  let rec ask_for_passwds = function 
    [] -> ()
    | h :: t -> if Ocsiconfig.get_ssl_n h then begin
      if not (Ocsiconfig.get_port_n_modif h) then Ocsiconfig.set_port h 443; 
      print_string "Please enter the password for the HTTPS server listening on port ";
      print_int (Ocsiconfig.get_port_n h);
      print_string ": ";
      Ocsiconfig.set_passwd h (read_line ());
      print_newline ();
    end; ask_for_passwds t in
  let run s =
    Ocsiconfig.sconf := s;
    Ocsiconfig.cfgs := [];
    Gc.full_major ();
    if (get_maxthreads ())<(get_minthreads ())
    then 
      raise (Config_file_error "maxthreads should be greater than minthreads");
    Lwt_unix.run 
      (ignore (Preemptive.init 
		 (Ocsiconfig.get_minthreads ()) 
		 (Ocsiconfig.get_maxthreads ()));
       Unix.handle_unix_error listen (Ocsiconfig.get_modules ())) in
  let rec launch l = match l with [] -> () | (h :: t) -> begin 
    match Unix.fork () with
    | 0 -> begin try run h
    with
	Ocsigen.Ocsigen_duplicate_registering s -> 
	  errlog ("Fatal - Duplicate registering of url \""^s^"\". Please correct the module.")
      | Ocsigen.Ocsigen_there_are_unregistered_services s ->
	  errlog ("Fatal - Some public url have not been registered. Please correct your modules. (ex: "^s^")")
      | Ocsigen.Ocsigen_page_erasing s ->
	  errlog ("Fatal - You cannot create a page or directory here: "^s^". Please correct your modules.")
      | Ocsigen.Ocsigen_register_for_session_outside_session ->
	  errlog ("Fatal - Register session during initialisation forbidden.")
      | Dynlink.Error e -> errlog ("Fatal - "^(Dynlink.error_message e))
      | exn -> errlog ("Fatal - Uncaught exception: "^(Printexc.to_string exn))
    end
    | _ -> launch t
  end in
  let old_term= Unix.tcgetattr Unix.stdin in
  let old_echo = old_term.Unix.c_echo in
  old_term.Unix.c_echo <- false;
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH old_term;
  ask_for_passwds !Ocsiconfig.cfgs;
  old_term.Unix.c_echo <- old_echo;
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH old_term;
  if !Ocsiconfig.number_of_servers = 1 then run (List.hd !Ocsiconfig.cfgs) 
  else launch !Ocsiconfig.cfgs
