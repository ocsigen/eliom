(* Ocsigen
 * http://www.ocsigen.org
 * http_client.ml Copyright (C) 2005 Vincent Balat
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

(* 
   It is a beta version.

   TODO
   - get using pipeline
   - post
   - better heuristic for trusting server keepalive?
   I think it could be less strict, as we redo requests?
   It should probably be different for a proxy or a reverse proxy ...
   - keep the name of the server in pipeline table?
   - Avoid the keepalive table to become too big
   - Add a parameter to disable reuse of free connections?
   - Allow to set parameters in config file (probing_time, etc)
   - Find a way to pipeline POST requests? at least PUT?
   - Does it work well if the server is using HTTP/1.0? 
   (probably not because of chunks)

   Notes:
   - Pipeline: 
   If the server decided to close the pipeline when some request has already
   been sent, we redo the request. But ... it has never been tested!!!!!
   See exception Pipeline_failed.
   - Pipeline: we pipeline requests without body if possible, 
   and we resend on a new connection if the pipeline failed. 
   - We do not pipeline requests with non-empty body!!!!!!!!!!!!!!
   and requests using CONNECT method.
   CONNECT and POST because they are not idempotent (see RFC).
   Requests with body because we do not keep the content so we can not resend
   them ...

   - It is actually doing very few pipelining when used with
   Firefox or Konqueror ... The previous request is always received before
   sending the next one. Why?

   - Is it ok to reuse the same sonnection for several clients?
   May be restrict to several connections from the same client?
   What if the client is a proxy?
   - What to do with headers like user-agent, server, etc?
   For now we keep user-agent but send Ocsigen as server!
   - If I'm right, we are not supposed to change the User-Agent (end-to-end) 
   but is there a hop-to-hop equivalent? What about other headers?

   Note that for now, we pipeline only if incoming requests were pipelined.
   But we can reuse a free connection.

   If the server says Connection:close once, we do not trust it any more
   for an amount of time ...

*)

(* constants. Should be configurable *)
let max_free_open_connections = 2

exception Connection_refused
exception Pipeline_failed

let (>>=) = Lwt.(>>=)

let _ = Ssl.init ()
let sslcontext = ref (Ssl.create_context Ssl.SSLv23 Ssl.Both_context)

let request_sender =
  Http_com.create_sender ~proto:Http_frame.Http_header.HTTP11 ()

(*****************************************************************************)
module T = Hashtbl.Make(
  struct
    type t = int * (Unix.inet_addr * int * bool)
        (* IP, port, doing HEAD request *)
    let equal = (=)
    let hash = Hashtbl.hash
  end)

let connection_table = T.create 100
(* Only one for each client *)

module FT = struct
  module T =
    Hashtbl.Make(
      struct
        type t = Unix.inet_addr * int * bool (* IP, port, doing HEAD request *)
        let equal = (=)
        let hash = Hashtbl.hash
      end)

  let free_connection_table = T.create 100

  let add k v =
    let add_last v = 
      let rec aux v = function
        | [] -> [v], 2
        | a::l -> let l', size = aux v l in a::l', (size + 1)
      in
      function
        | [] -> v, [], 1
        | a::l -> let l', size = aux v l in a, l', size
    in
    try
      let l = T.find free_connection_table k in
      let first, new_l, size = add_last v l in
      let new_l =
        if size > max_free_open_connections then begin
          Ocsigen_messages.debug2 "--Http_client: Too much free connections. Removing the oldest one.";
          ignore
            (!(fst first) >>= fun conn ->
             Lwt_ssl.shutdown (Http_com.connection_fd conn) Unix.SHUTDOWN_ALL;
             Lwt.return ());
          new_l
        end
        else first::new_l
      in
      T.replace free_connection_table k new_l
    with Not_found -> 
      T.replace free_connection_table k [v]

  let find_remove k =
    match T.find free_connection_table k with
      | [] -> 
          T.remove free_connection_table k;
          raise Not_found
      | [a] -> 
          T.remove free_connection_table k;
          a
      | a::l -> 
          T.replace free_connection_table k l;
          a

  let remove k (conn, gf) =
    let rec aux = function
      | [] -> false, []
      | ((conn2, _) as a)::l -> 
          if conn2 == conn then
            true, l
          else
            let (b, ll) = aux l in
            b, a::ll
    in
    try
      match T.find free_connection_table k with
        | [] -> 
            T.remove free_connection_table k;
        | [(conn2, _)] -> 
            if conn == conn2 then T.remove free_connection_table k;
        | l ->
            let b, ll = aux l in
            if b then T.replace free_connection_table k ll;
    with Not_found ->
      ()
      | e -> 
          Ocsigen_messages.debug2 ("--Http_client: exception while removing from connection table: "^Ocsigen_lib.string_of_exn e)
          
end

let remove_on_error_from_free_conn key ((_, gf) as v) =
  ignore 
    (Lwt.catch
       (fun () -> gf >>= fun _ -> Lwt.return ())
       (fun _ -> 
         FT.remove key v;
         Lwt.return ()
       )
    )

(*****************************************************************************)
module KT = Hashtbl.Make(
  struct
    type t = Unix.inet_addr * int
    let equal = (=)
    let hash = Hashtbl.hash
  end)

type k = Probing of int | Yes | No of float (* last failure date *)

let pipelining_table = KT.create 100

let probing_time = 1000 (* number of requests for probing *)
let purgatory_time = 10000 (* number of requests for probing after purgatory *)
let purgatory_delay = 86400. (* 1 day *)

let appreciate_server_pipeline inet_addr port =
  let key = (inet_addr, port) in
  match
    try
      match KT.find pipelining_table key with
        | Yes -> None
        | No t when Unix.time () -. t < purgatory_delay -> None
        | No t -> 
            Ocsigen_messages.warning 
              ("--Http_client will give to server "^
                 (Unix.string_of_inet_addr inet_addr)^":"^(string_of_int port)
               ^" a new probing period for pipelining.");
            Some purgatory_time (* second chance *)
        | Probing n -> Some (n-1)
    with Not_found -> 
      Ocsigen_messages.warning 
        ("--Http_client will give to server "^
           (Unix.string_of_inet_addr inet_addr)^":"^(string_of_int port)
         ^" a first probing period for pipelining.");
      Some probing_time
  with
    | None -> ()
    | Some n ->
        if n < 0 then begin
          Ocsigen_messages.warning 
            ("--Http_client now trusts server "^
               (Unix.string_of_inet_addr inet_addr)^":"^(string_of_int port)
             ^" for pipelining. He passed the probing period.");
          KT.replace pipelining_table key Yes
        end
        else
          KT.replace pipelining_table key (Probing n)

let boycott_server_pipeline server_do_keepalive inet_addr port =
  if server_do_keepalive then
    Ocsigen_messages.warning 
      ("--Http_client does not trust server "^
         (Unix.string_of_inet_addr inet_addr)^":"^(string_of_int port)
       ^" any more for pipelining. He just closed the connection!");
  KT.replace pipelining_table (inet_addr, port) (No (Unix.time ()))

let keep_alive_server inet_addr port =
(* print_endline "désactivé !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"; true *)
  try 
    match KT.find pipelining_table (inet_addr, port) with
      | Yes -> true
      | No _ ->
          Ocsigen_messages.debug
            (fun () -> "--Http_client does not trust server "^
               (Unix.string_of_inet_addr inet_addr)^":"^(string_of_int port)
             ^" for pipelining.");
          false
      | Probing _ -> 
          Ocsigen_messages.debug
            (fun () -> "--Http_client is currently probing server "^
               (Unix.string_of_inet_addr inet_addr)^":"^(string_of_int port)
             ^" for pipelining. No pipeline for now.");
          false
  with Not_found -> false



(*****************************************************************************)

let raw_request 
    ?client ?(keep_alive = true) ?headers ?(https=false) ?port
    ~content ?content_length ~http_method ~host ~inet_addr ~uri () =

(* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv *)

  Ocsigen_messages.debug_noel2 "--Http_client: *************** new request to ";
  Ocsigen_messages.debug2 uri;
  
  let head = http_method = Http_frame.Http_header.HEAD in
  
  let port = match port with
  | None -> if https then 443 else 80
  | Some p -> p
  in
  
  let keep_alive_asked = keep_alive in
  let server_do_keepalive = keep_alive_server inet_addr port in
  let do_keep_alive = keep_alive_asked && server_do_keepalive in
  
  let client = 
    if not do_keep_alive then
      None
    else client
  in
  
(*  let do_pipeline = not client = None in *)
  
  if do_keep_alive then 
    Ocsigen_messages.debug2 "--Http_client: Doing keep_alive"
  else
    Ocsigen_messages.debug2 "--Http_client: NOT doing keep_alive";
  
  if client = None then 
    Ocsigen_messages.debug2 "--Http_client: NOT pipelining"
  else
    Ocsigen_messages.debug2 "--Http_client: will do pipelining if needed";
      
  let close_on_error thr_conn gf =
    (* No need for lingering close, if I am not wrong *)
    ignore
      (thr_conn >>= fun conn ->
        (Lwt.catch
           (fun () -> gf >>= fun _ -> Lwt.return ())
           (function
             | Http_com.Connection_closed -> 
                 Ocsigen_messages.debug2
                   "--Http_client: connection closed by server (closing)";
                 Lwt_ssl.close (Http_com.connection_fd conn);
                 Lwt.return ()
             | Http_com.Keepalive_timeout -> 
                 Ocsigen_messages.debug2
                   "--Http_client: connection closed by keepalive timeout";
                 Lwt_ssl.close (Http_com.connection_fd conn);
                 Lwt.return ()
             | e -> 
                 Ocsigen_messages.warning
                   ("--Http_client: exception caught while receiving frame: "^
                    Ocsigen_lib.string_of_exn e^
                    " - closing connection to the server.");
                 Lwt_ssl.close (Http_com.connection_fd conn);
                 Lwt.return ()
           )))
  in
  
  let new_conn () =
    let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.set_close_on_exec fd;
    
    let thr_conn =
      Lwt.catch
        (fun () ->
          Lwt_unix.connect
            fd (Unix.ADDR_INET (inet_addr, port)) >>= fun () ->
              
              (if https then
                Lwt_ssl.ssl_connect fd !sslcontext
              else
                Lwt.return (Lwt_ssl.plain fd))
            >>= fun socket -> 
                
            Lwt.return (Http_com.create_receiver 
                          (Ocsigen_config.get_server_timeout ())
                          Http_com.Answer socket))
        (function
          | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> 
              Lwt_unix.close fd; Lwt.fail Connection_refused
          | e -> Lwt_unix.close fd; Lwt.fail e)
    in
    let gf = thr_conn >>= fun conn -> Http_com.get_http_frame ~head conn in
    close_on_error thr_conn gf;
    (thr_conn, gf)
  in
  
  let find_conn () =
    (* If there is already a free connection for the same server, we reuse it *)
    try
      let c = FT.find_remove (inet_addr, port, head) in
      Ocsigen_messages.debug2
        "--Http_client: Free connection found";
      c
    with Not_found ->
      Ocsigen_messages.debug2
        "--Http_client: Free connection not found - creating new one";
      let (c, g) = new_conn () in
      (ref c, g)
  in
  
  let (key_new_waiter, ref_thr_conn, get_frame) =
    match client with
    | Some client when (content = None && 
                        (* Do not pipeline requests with content,
                           as we cannot resend them for now if the pipeline
                           failed.
                           Do not pipeline CONNECT and POST.
                         *)
                        http_method <> Http_frame.Http_header.CONNECT) 
      ->
        (* Trying to pipeline *)
        Ocsigen_messages.debug_noel2
          "--Http_client: Trying to find an opened connection for same client - connection number ";
        Ocsigen_messages.debug (fun () -> 
          string_of_int 
            (Ocsigen_extensions.client_id client));
        let new_waiter = Lwt.wait () in
        let key = (Ocsigen_extensions.client_id client, (inet_addr, port, head)) in
        (* Is there already a connection for the same client? *)
        let (ref_thr_conn, get_frame, nb_users) =
          try
            let r = T.find connection_table key in
            Ocsigen_messages.debug2 
              "--Http_client: Connection FOUND for this client! PIPELINING! <----------------";
            r
          with Not_found ->
            Ocsigen_messages.debug2
              "--Http_client: Connection not found for this client's connection";
            let (ref_thr_conn, gf) = find_conn () in
            (ref_thr_conn, gf, 0)
        in
        let new_get_frame = 
          new_waiter >>= fun () ->
            !ref_thr_conn >>= fun conn ->
            let gf = Http_com.get_http_frame ~head conn in
            close_on_error !ref_thr_conn gf;
            gf
        in
        Ocsigen_messages.debug2
          "--Http_client: Putting connection in connection_table";
        T.replace connection_table key 
          (ref_thr_conn, new_get_frame, nb_users + 1);
(*          remove_on_error key get_frame; *)
        (Some (key, new_waiter), ref_thr_conn, get_frame)
    | _ -> 
        (* No pipeline *)
        let (ref_thr_conn, gf) = find_conn () in
        (None, ref_thr_conn, gf)

  in

(* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ *)
(* Now the request is pipelined. It is safe to return the function    *)

  fun () ->

    let get_frame_ref = ref get_frame in
    let request_sent = Lwt.wait () in (* awoken when request sent *)

    let query = Http_frame.Http_header.Query (http_method, uri) in
    
    let headers = 
      Http_headers.replace 
        (Http_headers.name "host")
        (host^":"^string_of_int port)
        (match headers with
           | None -> Http_headers.empty
           | Some h -> h)
    in
    
    let f ?reopen slot =
      
      Ocsigen_messages.debug2 "--Http_client: Will send request when slot opened";
      Lwt.catch
        (fun () ->
           (match content with
              | None ->
                  let empty_result = Http_frame.empty_result () in
                  Http_com.send
                    ?reopen
                    slot
                    ~head:false (* We want to send the full request *)
                    ~mode:query
                    ~clientproto:Http_frame.Http_header.HTTP11
                    ~keep_alive:keep_alive_asked (* we request keep alive
                                                    even if we do not pipeline 
                                                    if we don't trust the server
                                                 *)
                    ~sender:request_sender
                    {empty_result with
                       Http_frame.res_headers = headers}
                    
              | Some stream -> 
                  Predefined_senders.Stream_content.result_of_content
                    stream >>= fun r ->
                    Http_com.send
                      ?reopen
                      slot
                      ~mode:query
                      ~head:false (* We want to send the full request *)
                      ~clientproto:Http_frame.Http_header.HTTP11
                      ~keep_alive:keep_alive_asked
                      ~sender:request_sender
                      {r with
                         Http_frame.res_content_length= content_length;
                         Http_frame.res_headers= headers;
                      }) >>= fun () ->

             Ocsigen_messages.debug2 "--Http_client: request sent";
             Lwt.wakeup request_sent ();
             Lwt.return ())
        (fun e -> Lwt.wakeup_exn request_sent e; Lwt.fail e)
        
    in
    
    
    let reopen () = 
      Ocsigen_messages.debug2
        "--Http_client: Server not responding. Trying to open a new connection";
      let (thr_conn, gf) = new_conn () in
      ref_thr_conn := thr_conn;
      get_frame_ref := gf;
      
      Ocsigen_messages.debug2 "--Http_client: Retrying to do the request";
      thr_conn >>= fun conn ->
      Http_com.start_processing conn (f ?reopen:None); (* starting the request *)
      Lwt.return ()
        
    in

    Ocsigen_messages.debug2 "--Http_client: Doing the request";
    let thr_conn = !ref_thr_conn in
    thr_conn >>= fun conn ->
    Http_com.start_processing conn (f ~reopen); (* starting the request *)


    let finalize do_keep_alive = 
      let put_in_free_conn ?gf () =
        Ocsigen_messages.debug2 "--Http_client: Putting in free connections";
        let gf = match gf with
          | None -> 
              let gf = 
                !ref_thr_conn >>= fun conn -> Http_com.get_http_frame ~head conn 
              in
              close_on_error !ref_thr_conn gf;
              gf
          | Some gf -> gf
        in
        try
          ignore (Lwt.poll gf);
          FT.add (inet_addr, port, head) (ref_thr_conn, gf);
          Ocsigen_messages.debug2 "--Http_client: Added in free connections";
          remove_on_error_from_free_conn 
            (inet_addr, port, head) (ref_thr_conn, gf) ;
          Lwt.return ()
        with e ->
          Ocsigen_messages.debug_noel2
            "--Http_client: exception while trying to keep free connection: ";
          Ocsigen_messages.debug2 (Ocsigen_lib.string_of_exn e);
          !ref_thr_conn >>= fun conn ->
          Lwt_ssl.close (Http_com.connection_fd conn);
          Lwt.return ()
      in
      if do_keep_alive then begin
        match key_new_waiter with
          | None -> (* no pipeline *) put_in_free_conn ()
          | Some (key, _) ->
              (try
                 let (ref_thr_conn, gf, nb_users) = T.find connection_table key in
                 if nb_users = 1 then begin
                   Ocsigen_messages.debug2
                     "--Http_client: The connection is not used any more by the client";
                   T.remove connection_table key;
                   put_in_free_conn ~gf ()
                 end
                 else begin
                   T.replace connection_table key (ref_thr_conn, gf, nb_users - 1);
                   Lwt.return ()
                 end
               with Not_found -> 
                 Ocsigen_messages.warning
                   "--Http_client - Strange: connection disappeared from \
                   connection_table";
                 Lwt.return ())
      end
      else begin
        !ref_thr_conn >>= fun conn ->
          Lwt_ssl.close (Http_com.connection_fd conn);
          Lwt.return ()
      end
    in
    
    
    Lwt.catch
      (fun () ->
         Lwt.catch
           (fun () ->
              (* We wait for the request to be sent, 
                 because get_frame_ref may change *)
              request_sent >>= fun () ->
                (* getting and sending back the result: *)
              !get_frame_ref)
           (function
              | Pipeline_failed -> 
                  (* Previous request closed the pipeline 
                     but the request has been sent. We redo it. *)
                  Ocsigen_messages.warning "Previous request closed the pipeline. Redoing the request on a new connection.";
                  reopen () >>= fun () ->
                  !get_frame_ref
              | e -> Lwt.fail e))
      
      (fun e ->       
         (* We advice subsequent get_frame that the pipeline failed: *)
         (match key_new_waiter with
            | None -> ()
            | Some (_, new_waiter) -> 
                Lwt.wakeup_exn new_waiter Pipeline_failed);
         
         finalize false >>= fun () ->
           Lwt.fail e)
      
    >>= fun http_frame ->
      
      
      let server_keepalive = 
        Ocsigen_headers.get_keepalive http_frame.Http_frame.header 
      in
      if keep_alive_asked && not server_keepalive then
        (* The server does not want to do keep-alive *)
        boycott_server_pipeline server_do_keepalive inet_addr port
      else if keep_alive_asked then
        appreciate_server_pipeline inet_addr port;
      
      let do_keep_alive = keep_alive_asked && server_keepalive in
      (* We keep alive even if we do not trust the server for pipelining *)
      
      (* It is now time for starting subsequent get_frame: *)
      (match key_new_waiter with
         | None -> ()
         | Some (_, new_waiter) -> 
             if server_keepalive then
               Lwt.wakeup new_waiter ()
             else
               Lwt.wakeup_exn new_waiter Pipeline_failed);
    
      Ocsigen_messages.debug2 "--Http_client: frame received";
      (match http_frame.Http_frame.content with
         | None   -> finalize do_keep_alive
         | Some c -> 
             Ocsistream.add_finalizer c (fun () -> finalize do_keep_alive);
             Lwt.return ()
      ) >>= fun () ->
        
        
      let headers =
        Http_headers.replace_opt 
          Http_headers.connection
          None
          http_frame.Http_frame.header.Http_frame.Http_header.headers
      in
      let headers =
        try
          let connection_value =
            Http_frame.Http_header.get_headers_value
              http_frame.Http_frame.header Http_headers.connection
          in
          Http_headers.replace_opt 
            (Http_headers.name connection_value)
            None
            headers
        with Not_found -> headers
      in
      Lwt.return 
        {Http_frame.header= 
            {Http_frame.Http_header.mode = 
                http_frame.Http_frame.header.Http_frame.Http_header.mode;
             Http_frame.Http_header.proto = 
                http_frame.Http_frame.header.Http_frame.Http_header.proto;
             Http_frame.Http_header.headers = headers};
         Http_frame.content = http_frame.Http_frame.content}


(*****************************************************************************)
let get ?https ?port ~host ~uri () =
  Ocsigen_lib.get_inet_addr host >>= fun inet_addr ->
    raw_request 
      ?https
      ?port
      ~http_method:Http_frame.Http_header.GET
      ~content:None
      ~host
      ~inet_addr
      ~uri
      ()
      ()


(*VVV missing: post *)



(*****************************************************************************)
let basic_raw_request 
    ?headers ?(https=false) ?port ~content ?content_length
    ~http_method ~host ~inet_addr ~uri () =

  let port = match port with
  | None -> if https then 443 else 80
  | Some p -> p
  in
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec fd;

  Lwt.catch
    (fun () ->
       Lwt_unix.connect fd (Unix.ADDR_INET (inet_addr, port)) >>= fun () ->
       (if https then
          Lwt_ssl.ssl_connect fd !sslcontext
        else
          Lwt.return (Lwt_ssl.plain fd)))
    (fun e -> Lwt_unix.close fd; Lwt.fail e)
  >>= fun socket ->
            
  let query = Http_frame.Http_header.Query (http_method, uri) in
  let conn = Http_com.create_receiver
      (Ocsigen_config.get_server_timeout ())
      Http_com.Answer socket in
  let headers = 
    Http_headers.replace 
      (Http_headers.name "host")
      (host^":"^string_of_int port)
      (match headers with
         | None -> Http_headers.empty
         | Some h -> h)
  in
  let f slot =
    
    match content with
    | None ->
        let empty_result = Http_frame.empty_result () in
        Http_com.send
          slot
          ~mode:query
          ~clientproto:Http_frame.Http_header.HTTP11
          ~head:false
          ~keep_alive:false
          ~sender:request_sender
          {empty_result with
           Http_frame.res_headers = headers}
    | Some stream -> 
        Predefined_senders.Stream_content.result_of_content stream >>= fun r ->
        Http_com.send
          slot
          ~mode:query
          ~clientproto:Http_frame.Http_header.HTTP11
          ~head:false
          ~keep_alive:false
          ~sender:request_sender
          {r with
           Http_frame.res_content_length= content_length;
           Http_frame.res_headers= headers;
          }
 
  in
  Http_com.start_processing conn f; (* starting the request *)
(*      Http_com.wait_all_senders conn >>= fun () -> (* not needed *) *)
  Lwt.catch 
    (fun () ->
       Http_com.get_http_frame
         ~head:(http_method = Http_frame.Http_header.HEAD)
         conn
       >>= fun http_frame ->
      (match http_frame.Http_frame.content with
      | None   -> Lwt_ssl.close socket
      | Some c -> 
          Ocsistream.add_finalizer c
            (fun () -> Lwt_ssl.close socket; Lwt.return ()));
        Lwt.return http_frame)
    (fun e -> Lwt_ssl.close socket; Lwt.fail e)
