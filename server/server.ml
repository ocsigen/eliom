(* Ocsigen
 * http://www.ocsigen.org
 * Module server.ml
 * Copyright (C) 2005 Vincent Balat, Denis Berthod, Nataliya Guts
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

open Lwt
open Messages
open Ocsimisc
open Extensions
open Http_frame
open Http_com
open Predefined_senders
open Ocsiconfig
open Parseconfig
open Error_pages
open Lazy

exception Ocsigen_unsupported_media
exception Ssl_Exception
exception Ocsigen_upload_forbidden
exception Config_file_exn of exn

(* Without the following line, it stops with "Broken Pipe" without raising
   an exception ... *)
let _ = Sys.set_signal Sys.sigpipe Sys.Signal_ignore


(* non blocking input and output (for use with lwt): *)

(* let _ = Unix.set_nonblock Unix.stdin
let _ = Unix.set_nonblock Unix.stdout
let _ = Unix.set_nonblock Unix.stderr *)


let new_socket () = 
  Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 >>=
  (fun s -> Unix.set_close_on_exec s; 
    return s)
      
let local_addr num = Unix.ADDR_INET (Unix.inet_addr_any, num)
    
let _ = Ssl.init ()
let sslctx = ref (Ssl.create_context Ssl.SSLv23 Ssl.Server_context)


let ip_of_sockaddr = function
    Unix.ADDR_INET (ip,port) -> ip
  | _ -> raise (Ocsigen_Internal_Error "ip of unix socket")

let server_name = ("Ocsigen server ("^Ocsiconfig.version_number^")")


(* Closing socket cleanly 
   Must be called for each connection exactly once! 
   even if it has been closed by the client.
   Otherwise decr_connected won't decrease the number of connections.
 *)
let lingering_close ch =
  Messages.debug "** SHUTDOWN";
  (try Lwt_unix.shutdown ch Unix.SHUTDOWN_SEND 
  with e -> Messages.debug "** shutdown failed"; ());
  ignore (Lwt_unix.sleep 2.0 >>=
          (fun () -> 
            decr_connected ();
            Lwt.return
              (try
                (match ch with 
                  Lwt_unix.Plain fd -> Messages.debug "** CLOSE"; Unix.close fd
                | Lwt_unix.Encrypted (fd,sock) -> 
                    Messages.debug "** CLOSE (SSL)"; Unix.close fd)
              with e -> Messages.debug "** close failed"; ())))


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

let get_boundary cont_enc =
  let (_,res) = Netstring_pcre.search_forward
      (Netstring_pcre.regexp "boundary=([^;]*);?") cont_enc 0 in
  Netstring_pcre.matched_group res 1 cont_enc

let find_field field content_disp = 
  let (_,res) = Netstring_pcre.search_forward
      (Netstring_pcre.regexp (field^"=.([^\"]*).;?")) content_disp 0 in
  Netstring_pcre.matched_group res 1 content_disp

type to_write = 
    No_File of string * Buffer.t 
  | A_File of (string * string * string * Lwt_unix.descr)

let now = return ()

let counter = let c = ref (Random.int 1000000) in fun () -> c := !c + 1 ; !c


(* printing exceptions *)
let string_of_exn = function
  | Unix.Unix_error (ee,func,param) -> 
      (Unix.error_message ee)^" in function "^func^" ("^param^")"
  | e -> Printexc.to_string e


(* Errors during requests *)
let handle_light_request_errors 
    xhtml_sender sockaddr waiter exn = 
  (* EXCEPTIONS ABOUT THE REQUEST *)
  (* It can be an error during get_http_frame or during get_request_infos *)

  Messages.debug ("~~~~ Exception request: "^(Printexc.to_string exn));
  let ip = Unix.string_of_inet_addr (ip_of_sockaddr sockaddr) in
  waiter >>= (fun () ->
    match exn with
      
      (* First light errors: we answer, then we wait the following request *)
      (* For now: none *)
      (* If the request has not been fully read, it must have been consumed *)
      (* Find the value of keep-alive in the request *)
      
      (* Request errors: we answer, then we close *)
    | Http_error.Http_exception (_,_) ->
        send_error now ~cookies:[]
          ~keep_alive:false ~http_exception:exn xhtml_sender >>=
        (fun _ -> fail (Ocsigen_Request_interrupted exn))
    | Ocsigen_header_too_long ->
        Messages.debug "-> Sending 400";
        (* 414 URI too long. Actually, it is "header too long..." *)
        send_error now ~keep_alive:false ~error_num:400 xhtml_sender >>= 
        (fun _ -> fail (Ocsigen_Request_interrupted exn))
    | Ocsigen_Request_too_long ->
        Messages.debug "-> Sending 400";
        send_error now ~keep_alive:false ~error_num:400 xhtml_sender >>= 
        (fun _ -> fail (Ocsigen_Request_interrupted exn))
    | Ocsigen_Bad_Request ->
        Messages.debug "-> Sending 400";
        send_error now ~keep_alive:false ~error_num:400 xhtml_sender >>= 
        (fun _ -> fail (Ocsigen_Request_interrupted exn))
    | Ocsigen_upload_forbidden ->
        Messages.debug "-> Sending 403 Forbidden";
        send_error now ~keep_alive:false ~error_num:400 xhtml_sender >>= 
        (fun _ -> fail (Ocsigen_Request_interrupted exn))
    | Ocsigen_unsupported_media ->
        Messages.debug "-> Sending 415";
        send_error now ~keep_alive:false ~error_num:415 xhtml_sender >>= 
        (fun _ -> fail (Ocsigen_Request_interrupted exn))

    (* Now errors that close the socket: we raise the exception again: *)
    | Ocsigen_HTTP_parsing_error (s1,s2) as e ->
        warning ("While talking to "^ip^": HTTP parsing error near ("^s1^
                ") in:\n"^
                (if (String.length s2)>2000 
                then ((String.sub s2 0 1999)^"...<truncated>")
                else s2)^"\n---");
        fail (Ocsigen_Request_interrupted e)
    | Unix.Unix_error(Unix.ECONNRESET,_,_)
    | Ssl.Read_error Ssl.Error_zero_return
    | Ssl.Read_error Ssl.Error_syscall ->
        fail Connection_reset_by_peer
    | Ocsigen_Timeout 
    | Http_com.Ocsigen_KeepaliveTimeout
    | Connection_reset_by_peer
    | Ocsigen_Request_interrupted _ -> fail exn
    | _ -> fail (Ocsigen_Request_interrupted exn)
             )



let rec getcookies header =
  let rec aux s longueur =
    let rec firstnonspace s i = 
      if s.[i] = ' ' then firstnonspace s (i+1) else i 
    in
    try
      let pointvirgule = try 
        String.index s ';'
      with Not_found -> String.length s in
      let egal = String.index s '=' in
      let first = firstnonspace s 0 in
      let nom = (String.sub s first (egal-first)) in
      let value = String.sub s (egal+1) (pointvirgule-egal-1) in
      let long = (longueur-pointvirgule-1) in
      (nom, value)::
      (if long > 0
      then (aux (String.sub s (pointvirgule+1) long) long)
      else [])
    with _ -> []
  in 
  try 
    let s = Http_header.get_headers_value header "Cookie" in
    aux s (String.length s)
  with _ -> []
(* On peut améliorer ça *)


(* reading the request *)
let get_request_infos http_frame filenames sockaddr port =

  try
    
    let meth = Http_header.get_method http_frame.Stream_http_frame.header in

    let url = 
      fixup_url_string 
        (Http_header.get_url http_frame.Stream_http_frame.header) in

    let url2 = 
      (Neturl.parse_url 
         ~base_syntax:(Hashtbl.find Neturl.common_url_syntax "http")
         (* ~accept_8bits:true *)
         (* Neturl.fixup_url_string url *)
         url)
    in

(*    let path = 
      (Neturl.string_of_url
         (Neturl.remove_from_url 
            ~param:true
            ~query:true 
            ~fragment:true 
            url2)) in *)

    let host =
      try
        let hostport = 
          Http_header.get_headers_value
            http_frame.Stream_http_frame.header "Host" in
        try 
          Some (String.sub hostport 0 (String.index hostport ':'))
        with _ -> Some hostport
      with _ -> None
    in
    Messages.debug
      ("- host="^(match host with None -> "<none>" | Some h -> h));

    let useragent = 
      (try (Http_header.get_headers_value
              http_frame.Stream_http_frame.header "user-agent")
      with _ -> "")
    in
    
    let cookies = 
      lazy (getcookies http_frame.Stream_http_frame.header)
    in
    
    let ifmodifiedsince = 
      try 
        Some (Netdate.parse_epoch 
                (Http_header.get_headers_value
                   http_frame.Stream_http_frame.header "if-modified-since"))
      with _ -> None
    in
    
    let inet_addr = ip_of_sockaddr sockaddr in
    
    let params = 
      (Neturl.string_of_url
         (Neturl.remove_from_url
            ~user:true
            ~user_param:true
            ~password:true
            ~host:true
            ~port:true
            ~path:true
            ~other:true
            url2)) 
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

    let find_post_params = 
      lazy
        (if meth = Some(Http_header.GET) || meth = Some(Http_header.HEAD) 
        then return ([],[]) else 
          match http_frame.Stream_http_frame.content with
            None -> return ([],[])
          | Some body_gen ->
              try
                let body = body_gen () (* Here we get the stream.
                                          If it has already been taken,
                                          it raises an exception *)
                in
                let ct =
                  (Http_header.get_headers_value
                     http_frame.Stream_http_frame.header "Content-Type") in
                let ctlow = String.lowercase ct in
                if ctlow = "application/x-www-form-urlencoded"
                then 
                  catch
                    (fun () ->
                      Ocsistream.string_of_stream body >>=
                      (fun r -> return
                          ((Netencoding.Url.dest_url_encoded_parameters r),
                           [])))
                    (function
                        Ocsistream.String_too_large -> fail Input_is_too_large
                      | e -> fail e)
                else 
                  match
                    (Netstring_pcre.string_match 
                       (Netstring_pcre.regexp "multipart/form-data*")) ctlow 0
                  with 
                  | None -> fail Ocsigen_unsupported_media
                  | _ ->
                      let bound = get_boundary ct in
                      let params = ref [] in
                      let files = ref [] in
                      let create hs =
                        let cd = List.assoc "content-disposition" hs in
                        let st = try 
                          Some (find_field "filename" cd) 
                        with _ -> None in
                        let p_name = find_field "name" cd in
                        match st with 
                          None -> No_File (p_name, Buffer.create 1024)
                        | Some store -> 
                            let now = 
                              Printf.sprintf 
                                "%s-%f-%d" 
                                store (Unix.gettimeofday ()) (counter ())
                            in
                            match ((Ocsiconfig.get_uploaddir ())) with
                              Some dname ->
                                let fname = dname^"/"^now in
                                let fd = Unix.openfile fname 
                                    [Unix.O_CREAT;
                                     Unix.O_TRUNC;
                                     Unix.O_WRONLY;
                                     Unix.O_NONBLOCK] 0o666 in
                                (* Messages.debug "file opened"; *)
                                filenames := fname::!filenames;
                                A_File (p_name, fname, store, Lwt_unix.Plain fd)
                            | None -> raise Ocsigen_upload_forbidden
                      in
                      let add where s =
                        match where with 
                          No_File (p_name, to_buf) -> 
                            Buffer.add_string to_buf s;
                            return ()
                        | A_File (_,_,_,wh) -> 
                            Lwt_unix.write wh s 0 (String.length s) >>= 
                            (fun r -> Lwt_unix.yield ())
                      in
                      let stop size  = function 
                          No_File (p_name, to_buf) -> 
                            return 
                              (params := !params @
                                [(p_name, Buffer.contents to_buf)])
                              (* à la fin ? *)
                        | A_File (p_name,fname,oname,wh) -> 
                            (match wh with 
                              Lwt_unix.Plain fdscr -> 
                                (* Messages.debug "closing file"; *)
                                files := 
                                  !files@[(p_name, {tmp_filename=fname;
                                                    filesize=size;
                                                    original_filename=oname})];
                                Unix.close fdscr
                            | _ -> ());
                            return ()
                      in
                      Multipart.scan_multipart_body_from_stream 
                        body bound create add stop >>=
                      (fun () -> return (!params, !files))
              with e -> fail e)

(* AEFF *)              (*        IN-MEMORY STOCKAGE *)
              (* let bdlist = Mimestring.scan_multipart_body_and_decode s 0 
               * (String.length s) bound in
               * Messages.debug (string_of_int (List.length bdlist));
               * let simplify (hs,b) = 
               * ((find_field "name" 
               * (List.assoc "content-disposition" hs)),b) in
               * List.iter (fun (hs,b) -> 
               * List.iter (fun (h,v) -> Messages.debug (h^"=="^v)) hs) bdlist;
               * List.map simplify bdlist *)
    in
    let path =
      (Ocsimisc.remove_dotdot 
         (Ocsimisc.remove_slash_at_beginning (Neturl.url_path url2)))
        (* here we remove .. form paths, at it is dangerous.
           But in some very particular cases, we may want them?
           I prefer forbid that.
         *)
    in
    {ri_url = url;
     ri_path_string = (string_of_url_path path);
     ri_path = path;
     ri_params = params;
     ri_host = host;
     ri_get_params = get_params;
     ri_post_params = lazy ((force find_post_params) >>= 
                                (fun (a,b) -> return a));
     ri_files = lazy ((force find_post_params) >>= 
                      (fun (a,b) -> return b));
     ri_inet_addr = inet_addr;
     ri_ip = Unix.string_of_inet_addr inet_addr;
     ri_port = port;
     ri_user_agent = useragent;
     ri_cookies = cookies;
     ri_ifmodifiedsince = ifmodifiedsince;
     ri_http_frame = http_frame;
   }
      
  with e ->
    (Messages.debug ("~~~ Exn during get_request_infos : "^
                     (Printexc.to_string e));
     raise (Ocsigen_Request_interrupted e) (* ? *))
  

let find_keepalive http_header =
  try
    let kah = String.lowercase 
        (Http_header.get_headers_value http_header "Connection") 
    in
    if kah = "keep-alive" 
    then true 
    else false (* should be "close" *)
  with _ ->
    (* if prot.[(String.index prot '/')+3] = '1' *)
    if (Http_header.get_proto http_header) = "HTTP/1.1"
    then true
    else false







let service 
    wait_end_answer
    http_frame
    port
    sockaddr 
    xhtml_sender
    empty_sender
    inputchan
    () =
  (* wait_end_answer is here for pipelining: 
     we must wait before sending the page,
     because the previous one may not be sent *)
  let head = ((Http_header.get_method http_frame.Stream_http_frame.header) 
                    = Some (Http_header.HEAD)) in
  let ka = find_keepalive http_frame.Stream_http_frame.header in
  Messages.debug ("** Keep-Alive:"^(string_of_bool ka));
  Messages.debug("** HEAD:"^(string_of_bool head));

  let remove_files = 
    let rec aux = function
        (* We remove all the files created by the request 
           (files sent by the client) *)
        [] -> ()
      | a::l -> 
          (try Unix.unlink a 
          with e -> Messages.warning ("Error while removing file "^a^
                                      ": "^(Printexc.to_string e))); 
          aux l
    in function
        [] -> ()
      | l -> Messages.debug "** Removing files"; 
          aux l
  in


  let serv () =  

    let filenames = ref [] (* All the files sent by the request *) in

    catch (fun () ->
      
      (* *** First of all, we read all the request
         (that will possibly create files) *)
      let ri = get_request_infos http_frame filenames sockaddr port in
      
      (* *** Now we generate the page and send it *)
      catch
        (fun () ->
          
          (* log *)
          accesslog 
            ("connection"^
             (match ri.ri_host with 
               None -> ""
             | Some h -> (" for "^h))^
             " from "^ri.ri_ip^" ("^ri.ri_user_agent^") : "^
             ri.ri_path_string^ri.ri_params);
          (* end log *)
          
          
          (* Generation of pages is delegated to extensions: *)
          Extensions.do_for_host_matching 
	    ri.ri_host ri.ri_port (Extensions.get_virthosts ()) ri >>=
          
          (fun (res, cookieslist) ->
            
            match res.res_lastmodified, ri.ri_ifmodifiedsince with
              Some l, Some i when l<=i -> 
                Messages.debug "-> Sending 304 Not modified ";
                send_empty
                  ~content:()
                  ~cookies:(List.map change_cookie 
                              (res.res_cookies@cookieslist))
                  wait_end_answer
                  ~keep_alive:ka
                  ?last_modified:res.res_lastmodified
                  ?etag:res.res_etag
                  ~code:304 (* Not modified *)
                  ~head:head 
                  empty_sender
                  
            | _ ->
                res.res_send_page
                  ~cookies:(List.map change_cookie 
                              (res.res_cookies@cookieslist))
                  wait_end_answer
                  ~keep_alive:ka
                  ?last_modified:res.res_lastmodified
                  ?code:res.res_code
                  ?charset:res.res_charset
                  ~head:head
                  (res.res_create_sender ~server_name:server_name inputchan))
            
        )
        
        
        (fun e -> 

          (* Exceptions during page generation *)
          Messages.debug 
            ("~~~ Exception during generation/sending: "^
             (Printexc.to_string e));
          catch
            (fun () ->
              match e with
                (* EXCEPTIONS WHILE COMPUTING A PAGE *)
                Ocsigen_404 -> 
                  Messages.debug "-> Sending 404 Not Found";
                  send_error 
                    wait_end_answer ~keep_alive:ka ~error_num:404 xhtml_sender
              | Ocsigen_sending_error exn -> fail exn
              | Ocsigen_Is_a_directory -> 
                  Messages.debug "-> Sending 301 Moved permanently";
                  send_empty
                    ~content:()
                    ~cookies:[]
                    wait_end_answer
                    ~keep_alive:ka
                    ~location:("/"^ri.ri_path_string^"/"^ri.ri_params)
                    ~code:301 (* Moved permanently *)
                    ~head:head empty_sender
              | Extensions.Ocsigen_malformed_url
              | Neturl.Malformed_URL -> 
                  Messages.debug "-> Sending 400 (Malformed URL)";
                  send_error wait_end_answer ~keep_alive:ka
                    ~error_num:400 xhtml_sender (* Malformed URL *)
              | Unix.Unix_error (Unix.EACCES,_,_) ->
                  Messages.debug "-> Sending 303 Forbidden";
                  send_error wait_end_answer ~keep_alive:ka
                    ~error_num:403 xhtml_sender (* Forbidden *)
              | Stream_already_read ->
                  Messages.errlog "Cannot read the request twice. You probably have two incompatible extensions, or the order of the extensions in the config file is wrong.";
                  send_error wait_end_answer ~keep_alive:ka
                    ~error_num:500 xhtml_sender (* Internal error *)
              | e ->
                  Messages.warning
                    ("Exn during page generation: "^
                     (string_of_exn e)
                     ^" (sending 500)"); 
                  Messages.debug "-> Sending 500";
                  send_error
                    wait_end_answer ~keep_alive:ka ~error_num:500 xhtml_sender)
            (fun e -> fail (Ocsigen_sending_error e))
            (* All generation exceptions have been handled here *)
        ) >>=
      
      (fun () -> return (remove_files !filenames)))
      
      (fun e -> 
        remove_files !filenames;
        match e with
          Ocsigen_sending_error _ -> fail e
        | _ -> handle_light_request_errors
              xhtml_sender sockaddr wait_end_answer e)

  in 


  (* body of service *)
  let meth = (Http_header.get_method http_frame.Stream_http_frame.header) in
  if ((meth <> Some (Http_header.GET)) && 
      (meth <> Some (Http_header.POST)) && 
      (meth <> Some(Http_header.HEAD)))
  then send_error wait_end_answer ~keep_alive:ka ~error_num:501 xhtml_sender
  else 
    catch

      (* new version: in case of error, we close the request *)
      (fun () ->
        (try
          return 
            (Int64.of_string 
               (Http_header.get_headers_value 
                  http_frame.Stream_http_frame.header 
                  "content-length"))
        with
          Not_found -> return Int64.zero
        | _ -> fail (Ocsigen_Request_interrupted Ocsigen_Bad_Request))
        >>=
            (fun cl ->
              if (Int64.compare cl Int64.zero) > 0 &&
                (meth = Some Http_header.GET || meth = Some Http_header.HEAD)
              then fail (Ocsigen_Request_interrupted Ocsigen_Bad_Request)
              else serv ()))


          (* old version: in case of error, 
             we consume all the stream and wait another request
             fun () ->
             (try
             return 
             (Int64.of_string 
             (Http_header.get_headers_value 
             http_frame.Stream_http_frame.header 
             "content-length"))
             with
             Not_found -> return Int64.zero
             | _ -> (consume http_frame.Stream_http_frame.content >>=
             (fun () ->
             fail Ocsigen_Bad_Request)))
             >>=
             (fun cl ->
             if (Int64.compare cl Int64.zero) > 0 &&
             (meth = Some Http_header.GET || meth = Some Http_header.HEAD)
             then consume http_frame.Stream_http_frame.content >>=
             (fun () ->
             send_error wait_end_answer ~keep_alive:ka ~error_num:501 xhtml_sender)
             else serv ()) *)

      (function
        | Ocsigen_Request_interrupted _ as e -> fail e
        | Ocsigen_sending_error e ->
            Messages.debug ("~~~ Exn while sending: "^
                            (Printexc.to_string e)); 
            fail e
        | e -> Messages.debug ("~~~ Exn during service: "^
                               (Printexc.to_string e)); 
            fail e)




let handle_broken_pipe_exn sockaddr exn = 
  (* EXCEPTIONS WHILE REQUEST OR SENDING WHEN WE CANNOT ANSWER *)
  let ip = Unix.string_of_inet_addr (ip_of_sockaddr sockaddr) in
  (* We don't close here because it is already done *)
  match exn with
    Connection_reset_by_peer -> 
      Messages.debug "** Connection closed by client";
      return ()
  | Ssl.Write_error(Ssl.Error_ssl) -> 
      errlog ("While talking to "^ip^": Ssl broken pipe.");
      return ()
  | Ocsimisc.Ocsigen_Request_too_long ->
      warning ("Request from "^ip^" is too long for the server configuration.");
      return ()
  | exn -> 
      warning ("While talking to "^ip^": Uncaught exception - "
              ^(Printexc.to_string exn)^".");
      return ()




(** Thread waiting for events on a the listening port *)
let listen ssl port wait_end_init =
  
  let listen_connexion receiver in_ch sockaddr 
      xhtml_sender empty_sender =
    
    (* (With pipeline) *)

    let handle_severe_errors e =
        (* Serious error (we cannot answer to the request)
           Probably the pipe is broken.
           We awake all the waiting threads in cascade
           with an exception.
         *)
(*        Stop_sending -> 
          wakeup_exn wait_end_answer Stop_sending;
          return () *)
        (* Timeout errors: We close and do nothing *)
      lingering_close in_ch;
      match e with
      | Ocsigen_Timeout -> 
          let ip = Unix.string_of_inet_addr (ip_of_sockaddr sockaddr) in
          warning ("While talking to "^ip^": Timeout");
          return ()
      | Http_com.Ocsigen_KeepaliveTimeout -> 
          return ()
      | Ocsigen_Request_interrupted e -> 
          (* We decide to interrupt the request 
             (for ex if it is too long) *)
          handle_broken_pipe_exn sockaddr e
      | e ->
          handle_broken_pipe_exn sockaddr e
    in  

    let handle_request_errors wait_end_answer exn =
      catch
        (fun () ->
          handle_light_request_errors 
            xhtml_sender sockaddr wait_end_answer exn)
        (fun e -> handle_severe_errors exn)
    in

    let rec handle_request wait_end_answer http_frame =

      let keep_alive = find_keepalive http_frame.Stream_http_frame.header in

      if keep_alive 
      then begin
        Messages.debug "** KEEP ALIVE (pipelined)";
        let wait_end_answer2 = wait () in
        (* The following request must wait the end of this one
           before being answered *)
        (* wait_end_answer
           is awoken when the previous request has been answered *)
        ignore_result 
          (catch
             (fun () ->
               service 
                 wait_end_answer http_frame port sockaddr 
                 xhtml_sender empty_sender in_ch () >>=
               (fun () ->

                 (* If the request has not been fully read by the extensions,
                    we force it to be read here, to be able to take
                    another one on the same connexion: *)
                 (match http_frame.Stream_http_frame.content with
                   Some f -> 
                     (try
                       Ocsistream.consume (f ())
                     with _ -> return ())
                 | _ -> return ()) >>=

                 (fun () ->
                   wakeup wait_end_answer2 (); 
                   return ())))
             handle_severe_errors (* will close the connexion *) );
        
        catch
          (fun () ->
            (* The following request must wait the end of this one.
               (It may not be finished, 
               for example if we are downloading files) *)
            (* waiter_thread is automatically awoken 
               when the stream is terminated *)
            http_frame.Stream_http_frame.waiter_thread >>=
            (fun () ->
              Messages.debug "** Waiting for new request (pipeline)";
              Stream_receiver.get_http_frame wait_end_answer2
                receiver ~doing_keep_alive:true () >>=
              (handle_request wait_end_answer2)))
          (handle_request_errors wait_end_answer2)
      end

      else begin (* No keep-alive => no pipeline *)
        catch
          (fun () ->
            service wait_end_answer http_frame port sockaddr
              xhtml_sender empty_sender in_ch ())
          (fun e ->
            lingering_close in_ch; fail e) >>=
            (fun () ->
              (lingering_close in_ch;
               return ()))
      end

    in (* body of listen_connexion *)
    catch
      (fun () ->
        catch
          (fun () ->
            Stream_receiver.get_http_frame (return ())
              receiver ~doing_keep_alive:false () >>=
            handle_request (return ()))
          (handle_request_errors now))
      (handle_broken_pipe_exn sockaddr)

        (* Without pipeline:
*        Stream_receiver.get_http_frame receiver ~doing_keep_alive () >>=
*        (fun http_frame ->
*          (service http_frame sockaddr 
*             xhtml_sender empty_sender in_ch ())
*            >>= (fun keep_alive -> 
*              if keep_alive then begin
*                Messages.debug "** KEEP ALIVE";
*                listen_connexion_aux ~doing_keep_alive:true
*                  (* Pour laisser la connexion ouverte, je relance *)
*              end
*              else (lingering_close in_ch; 
*                    return ())))
        *)
        
  in 
  let wait_connexion port socket =
    let handle_connection (inputchan, sockaddr) =
      debug "\n__________________NEW CONNECTION__________________________";
      catch
        (fun () -> 
          let xhtml_sender = 
            Predefined_senders.create_xhtml_sender
              ~server_name:server_name inputchan in
          (* let file_sender =
            create_file_sender ~server_name:server_name inputchan
          in *)
          let empty_sender =
            create_empty_sender ~server_name:server_name inputchan
          in
          listen_connexion 
            (Stream_receiver.create inputchan)
            inputchan sockaddr xhtml_sender
            empty_sender)
        (handle_broken_pipe_exn sockaddr)
    in

    let rec wait_connexion_rec () =

      let rec do_accept () = 
        Lwt_unix.accept (Lwt_unix.Plain socket) >>= 
        (fun (s, sa) -> 
          if ssl
          then begin
                let s_unix = 
              match s with
                Lwt_unix.Plain fd -> fd 
                  | _ -> raise Ssl_Exception (* impossible *) 
            in
                catch 
                  (fun () -> 
                ((Lwt_unix.accept
                    (Lwt_unix.Encrypted 
                       (s_unix, 
                        Ssl.embed_socket s_unix !sslctx))) >>=
                 (fun (ss, ssa) -> Lwt.return (ss, sa))))
                  (function
                      Ssl.Accept_error e -> 
                        Messages.debug "~~~ Accept_error"; do_accept ()
                    | e -> warning ("Exn in do_accept : "^
                                    (Printexc.to_string e)); do_accept ())
          end 
          else Lwt.return (s, sa))
      in

      catch
        (fun () ->
          (do_accept ()) >>= 
          (fun c ->
            incr_connected ();


            if (get_number_of_connected ()) <
              (get_max_number_of_connections ()) then
              ignore_result (wait_connexion_rec ())
            else
              warning ("Max simultaneous connections ("^
                       (string_of_int (get_max_number_of_connections ()))^
                       ") reached.");

            handle_connection c
              
          ) >>= 
          
          (fun () -> 
            if (get_number_of_connected ()) = 
              (get_max_number_of_connections ()) - 1
            then begin
              warning "Ok releasing one connection";
              wait_connexion_rec ()
            end
            else return ()))
        (fun exn ->
          let t = Ocsiconfig.get_connect_time_max () in
          errlog ("Exception: "^(string_of_exn exn)^
                  ". Waiting "^
                  (string_of_float t)^
                  " seconds before accepting new connections.");
          Lwt_unix.sleep t >>=
          wait_connexion_rec)

    in wait_connexion_rec ()

  in (* body of listen *)
  (new_socket () >>= 
   (fun listening_socket ->
     catch

       (fun () ->
         Unix.setsockopt listening_socket Unix.SO_REUSEADDR true;
         Unix.bind listening_socket (local_addr port);
         Unix.listen listening_socket 1;
         
         wait_end_init >>=
         (fun () -> wait_connexion port listening_socket))

       (function
         | Unix.Unix_error (Unix.EACCES,"bind",s2) ->
             errlog ("Fatal - You are not allowed to use port "^
                     (string_of_int (port))^".");
             exit 7
         | Unix.Unix_error (Unix.EADDRINUSE,"bind",s2) ->
             errlog ("Fatal - The port "^
                     (string_of_int port)^
                     " is already in use.");
             exit 8
         | exn ->
             errlog ("Fatal - Uncaught exception: "^(Printexc.to_string exn));
             exit 100
       )
   ))

(* fatal errors messages *)
let errmsg = function
  | Dynlink.Error e -> 
      (("Fatal - Dynamic linking error: "^(Dynlink.error_message e)),
      6)
  | (Unix.Unix_error _) as e ->
      (("Fatal - "^(string_of_exn e)),
      9)
  | Ssl.Private_key_error ->
      (("Fatal - bad password"),
      10)
  | Stdpp.Exc_located (fl, exn) ->
      (("Fatal - Error in configuration file at position : "^
              (print_location fl)^". "^(Printexc.to_string exn)),
      51)
  | Config_file_exn exn ->
      (("Fatal - Error in configuration file: "^(Printexc.to_string exn)),
      50)
  | exn -> 
      try
        ((Extensions.get_init_exn_handler () exn),
        20)
      with
        exn ->
          (("Fatal - Uncaught exception: "^(Printexc.to_string exn)),
          100)
            
            
            

(* reloading the cmo *)
let reload _ =

  (* That function cannot be interrupted *)
  warning "Reloading config file";

  (try
    match parse_config () with
    | [] -> ()
    | s::_ ->
        begin
          Extensions.start_initialisation ();
          
          parse_server true s;
          
          Extensions.end_initialisation ();
        end
  with e -> errlog (fst (errmsg e)));
  
  warning "Config file reloaded"
    


let _ = try

  let config_servers = 

    parse_config ()

  in

  let number_of_servers = List.length config_servers in

  if number_of_servers > 1
  then Messages.warning "Multiple servers not supported anymore";

  let ask_for_passwd sslports _ =
    print_string "Please enter the password for the HTTPS server listening \
      on port(s) ";
      print_string
      (match sslports with
        [] -> assert false
      | a::l -> List.fold_left
            (fun deb i -> deb^", "^(string_of_int i)) (string_of_int a) l);
    print_string ": ";
    let old_term= Unix.tcgetattr Unix.stdin in
    let old_echo = old_term.Unix.c_echo in
    old_term.Unix.c_echo <- false;
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH old_term;
    try
      let r = read_line () in
      print_newline ();
      old_term.Unix.c_echo <- old_echo;
      Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH old_term;
      r
    with exn ->
      old_term.Unix.c_echo <- old_echo;
      Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH old_term;
      raise exn
  in

  let run (user,group) (_, ports, sslports) s =

    Messages.open_files ();

    Lwt_unix.run 
      (let wait_end_init = wait () in
      (* Listening on all ports: *)
      List.iter 
        (fun i -> 
          ignore (listen false i wait_end_init)) ports;
      List.iter 
        (fun i ->
          ignore (listen true i wait_end_init)) sslports;
      
      let gid = match group with
      | None -> Unix.getgid ()
      | Some group -> (try
          (Unix.getgrnam group).Unix.gr_gid
      with e -> errlog ("Error: Wrong group"); raise e)
      in
      let uid = match user with
      | None -> Unix.getuid ()
      | Some user -> (try
        (Unix.getpwnam user).Unix.pw_uid
      with e -> errlog ("Error: Wrong user"); raise e) 
      in

      (* A pipe to communicate with the server *)
      let commandpipe = get_command_pipe () in 
      (try
        ignore (Unix.stat commandpipe)
      with _ -> 
        (try
          let umask = Unix.umask 0 in
          Unix.mkfifo commandpipe 0o660;
          Unix.chown commandpipe uid gid;
          ignore (Unix.umask umask)
        with e -> 
          Messages.errlog 
            ("Cannot create the command pipe: "^(string_of_exn e))));

      (* I change the user for the process *)
      (try
        Unix.setgid gid;
        Unix.setuid uid;
      with e -> errlog ("Error: Wrong user or group"); raise e);

      set_user user;
      set_group group;
            
      (* Je suis fou :
         let rec f () = 
         (* print_string "-"; *)
         Lwt_unix.yield () >>= f
         in f(); *)

      (* Now I can load the modules *)
      Dynlink.init ();
      Dynlink.allow_unsafe_modules true;

      Extensions.start_initialisation ();

      parse_server false s;
      
      if (get_maxthreads ()) < (get_minthreads ())
      then 
        raise
          (Config_file_error "maxthreads should be greater than minthreads");

            (* Closing stderr, stdout stdin if silent *)
      if (Ocsiconfig.get_silent ())
      then begin
        (* redirect stdout and stderr to /dev/null *)
        let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
        Unix.dup2 devnull Unix.stdout;
        Unix.dup2 devnull Unix.stderr;
        Unix.close devnull;
        Unix.close Unix.stdin;
      end;
      
      (* detach from the terminal *)
      if (Ocsiconfig.get_daemon ())
      then ignore (Unix.setsid ());
          
      (* A thread that kills old connections every n seconds *)
      ignore (Http_com.Timeout.start_timeout_killer ());
      
      ignore (Preemptive.init 
                (Ocsiconfig.get_minthreads ()) 
                (Ocsiconfig.get_maxthreads ()));
      
      Extensions.end_initialisation ();

      (* Communication with the server through the pipe *)
      (try
        ignore (Unix.stat commandpipe)
      with _ -> 
          let umask = Unix.umask 0 in
          Unix.mkfifo commandpipe 0o660;
          ignore (Unix.umask umask);
          Messages.warning "Command pipe created");

      let pipe = Lwt_unix.in_channel_of_descr 
          (Lwt_unix.Plain 
             (Unix.openfile commandpipe 
                [Unix.O_RDWR;Unix.O_NONBLOCK;Unix.O_APPEND] 0o660)) in
      let rec f () = 
        Lwt_unix.input_line pipe >>=
        (fun _ -> reload (); f ())
      in ignore (f ());

      wakeup wait_end_init ();
      
      warning "Ocsigen has been launched (initialisations ok)";
      
      wait ()
      )
  in

  let set_passwd_if_needed (ssl,ports,sslports) =
    if sslports <> []
    then
      match ssl with
        None
      | Some (None, None) -> ()
      | Some (None, _) -> raise (Ocsiconfig.Config_file_error
                            "SSL certificate is missing")
      | Some (_, None) -> raise (Ocsiconfig.Config_file_error 
                            "SSL key is missing")
      | Some ((Some c), (Some k)) -> 
          Ssl.set_password_callback !sslctx (ask_for_passwd sslports);
          Ssl.use_certificate !sslctx c k
  in

  let write_pid pid =
    match Ocsiconfig.get_pidfile () with
      None -> ()
    | Some p ->
        let spid = (string_of_int pid)^"\n" in
        let len = String.length spid in
        let f =
          Unix.openfile
            p
            [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o640 in
        ignore (Unix.write f spid 0 len);
        Unix.close f
  in

  let rec launch = function
      [] -> () 
    | [h] -> 
        let user_info, sslinfo = extract_info h in
        set_passwd_if_needed sslinfo;
        let pid = Unix.fork () in
        if pid = 0
        then run user_info sslinfo h
        else begin
          Messages.console ("Process "^(string_of_int pid)^" detached");
          write_pid pid;
        end
    | _ -> () (* Multiple servers not supported any more *)

  in

  if (not (get_daemon ())) &&
    number_of_servers = 1 
  then
    let cf = List.hd config_servers in
    let (user_info,sslinfo) = extract_info cf in
    (set_passwd_if_needed sslinfo;
     write_pid (Unix.getpid ());
     run user_info sslinfo cf)
  else launch config_servers

with e ->
  let msg, errno = errmsg e in
  errlog msg;
  exit errno
