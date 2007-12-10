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

(* TODO
   - The keepalive timeout should be different from timeout for clients
   (or no timeout at all?)
   - re-ouvrir la connexion quand l'écriture échoue
   - que se passe-t-il si le serveur dit keep-alive: close -> 
   ne pas faire de pipeline
*)

(* constants. Should be configurable *)
let max_free_open_connections = 10


let (>>=) = Lwt.(>>=)

let _ = Ssl.init ()
let sslcontext = ref (Ssl.create_context Ssl.SSLv23 Ssl.Both_context)


let request_sender =
  Http_com.create_sender ~proto:Http_frame.Http_header.HTTP11 ()

module T = Hashtbl.Make(
  struct
    type t = Extensions.client * (Unix.inet_addr * int)
    let equal = (=)
    let hash = Hashtbl.hash
  end)

let connection_table = T.create 100
(* Only one for each client *)

module FT = struct
  module T =
    Hashtbl.Make(
      struct
        type t = Unix.inet_addr * int
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
          Messages.debug2 "--Http_client: Too much free connections. Removing the oldest one.";
          Lwt_unix.shutdown (fst first) Unix.SHUTDOWN_ALL;
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

  let remove k (fd, (conn, gf)) =
    let rec aux = function
      | [] -> false, []
      | ((_, (conn2, _)) as a)::l -> 
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
        | [(_, (conn2, _))] -> 
            if conn == conn2 then begin
              T.remove free_connection_table k;
            end
        | l ->
            let b, ll = aux l in
            if b then begin
              T.replace free_connection_table k ll;
            end
    with Not_found ->
      ()
      | e -> 
          Messages.debug_noel2 ("--Http_client: exception while removing from connection table: "^Ocsimisc.string_of_exn e)
          
end






let raw_request 
    ?client ?(keep_alive = true) ?headers ?(https=false) ?port ?content
    ~http_method ~host ~inet_addr ~uri () =
(*VVV What do we do with User-agent: or other headers? *)

  Messages.debug_noel2 "--Http_client: *************** new request to ";
  Messages.debug2 uri;
  
  let port = match port with
  | None -> if https then 443 else 80
  | Some p -> p
  in

  let close_on_error fd gf =
    (* No need for lingering close, if I am not wrong *)
    ignore 
      (Lwt.catch
         (fun () -> gf >>= fun _ -> Lwt.return ())
         (function
            | Http_com.Connection_closed -> 
                Messages.debug_noel2 "--Http_client: connection closed by server (closing)";
                Lwt_unix.close fd;
                Lwt.return ()
            | Http_com.Keepalive_timeout -> 
                Messages.debug_noel2 "--Http_client: connection closed by keepalive timeout";
                Lwt_unix.close fd;
                Lwt.return ()
            | e -> 
                Messages.warning ("Http_client: exception caught while receiving frame: "^Ocsimisc.string_of_exn e^" - closing connection to the server.");
                Lwt_unix.close fd;
                Lwt.return ()
         ))
  in

  let new_conn () =
    (* If there is already a free connection for the same server, we reuse it *)
    try
      let c = FT.find_remove (inet_addr, port) in
      Messages.debug2
        "--Http_client: Free connection found";
      c
    with Not_found ->
      Messages.debug2
        "--Http_client: Free connection not found - creating new one";
      let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

      let conn =
        Lwt_unix.connect fd (Unix.ADDR_INET (inet_addr, port)) >>= fun () ->
          
        (if https then
           Lwt_ssl.ssl_connect fd !sslcontext
         else
           Lwt.return (Lwt_ssl.plain fd))
        >>= fun socket -> 

        Lwt.return (Http_com.create_receiver Http_com.Answer socket)
      in
      let gf = conn >>= fun conn -> Http_com.get_http_frame ~head:false conn in
      close_on_error fd gf;
      (fd, (conn, gf))
  in

  let remove_on_error key gf =
    ignore 
      (Lwt.catch
         (fun () -> gf >>= fun _ -> Lwt.return ())
         (fun e ->
            T.remove connection_table key;
            Lwt.return ()
         )
      )
  in

  let remove_on_error_from_free_conn key ((_, (_, gf)) as v) =
    ignore 
      (Lwt.catch
         (fun () -> gf >>= fun _ -> Lwt.return ())
         (fun _ -> 
            FT.remove key v;
            Lwt.return ()
         )
      )
  in

  let (key_new_waiter, fd, thr_conn, get_frame) =
    match client with
      | None -> 
          Messages.debug2 "--Http_client: No client specified";
          let (fd, (conn, gf)) = new_conn () in
          (None, fd, conn, gf)
      | Some client -> 
          Messages.debug2 ("--Http_client: Trying to find an opened connection for same client");
          let new_waiter = Lwt.wait () in
          let key = (client, (inet_addr, port)) in
          (* Is there already a connection for the same client? *)
          let (fd, conn, get_frame, nb_users) =
            try
              let r = T.find connection_table key in
              Messages.debug2 ("--Http_client: Connection FOUND for this client! PIPELINING!");
              r
            with Not_found ->
              Messages.debug2
                "--Http_client: Connection not found for this client";
              let (fd, (conn, gf)) = new_conn () in
              (fd, conn, gf, 0)
          in
          let new_get_frame = 
            new_waiter >>= fun () ->
            conn >>= fun conn ->
            let gf = Http_com.get_http_frame ~head:false conn in
            close_on_error fd gf;
            gf
          in
          Messages.debug2
            "--Http_client: Putting connection in connection_table";
          T.replace connection_table key (fd, conn, new_get_frame, nb_users + 1);
          remove_on_error key get_frame;
          (Some (key, new_waiter), fd, conn, get_frame)
  in


  let query = Http_frame.Http_header.Query (http_method, uri) in
  
  let headers = 
    Http_headers.replace 
      (Http_headers.name "host")
      (host^":"^string_of_int port)
      (match headers with
         | None -> Http_headers.empty
         | Some h -> h)
  in
    
  let f slot =

    Messages.debug2 "--Http_client: Will send request when slot opened";
    (match content with
    | None ->
        let empty_result = Http_frame.empty_result () in
        Http_com.send
          slot
          ~mode:query
          ~clientproto:Http_frame.Http_header.HTTP11
          ~head:false
          ~keep_alive
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
          ~keep_alive
          ~sender:request_sender
          {r with
           Http_frame.res_headers= headers;
          }) >>= fun () ->

    Messages.debug2 "--Http_client: request sent";
    Lwt.return ()

  in
  Messages.debug2 "--Http_client: Doing the request";
  thr_conn >>= fun conn ->
  Http_com.start_processing conn f; (* starting the request *)

(*      Http_com.wait_all_senders conn >>= fun () -> (* not needed *) *)


  (* getting and sending back the result: *)
  get_frame >>= fun http_frame ->

(*  if not (Ocsiheaders.get_keepalive http_frame.Http_frame.header) then
    ???
*)
  (match key_new_waiter with
    | None -> ()
    | Some (_, new_waiter) -> Lwt.wakeup new_waiter ());

  Messages.debug2 "--Http_client: frame received";
  let finalize () = 
    let put_in_free_conn ?gf () =
      Messages.debug2 "--Http_client: Putting in free connections";
      let gf = match gf with
        | None -> 
            let gf = Http_com.get_http_frame ~head:false conn in
            close_on_error fd gf;
            gf
        | Some gf -> gf
      in
      try
        ignore (Lwt.poll gf);
        FT.add (inet_addr, port) (fd, (thr_conn, gf));
        Messages.debug2 "--Http_client: Added in free connections";
        remove_on_error_from_free_conn (inet_addr, port) (fd, (thr_conn, gf)) ;
      with e ->
        Messages.debug_noel2 "--Http_client: exception while trying to keep free connection: ";
        Messages.debug2 (Ocsimisc.string_of_exn e);
        Lwt_unix.close fd
    in
    match key_new_waiter with
      | None -> (* no ~client supplied *) put_in_free_conn ()
      | Some (key, _) ->
          (try
             let (fd, conn, gf, nb_users) = T.find connection_table key in
             if nb_users = 1 then begin
               Messages.debug2 "--Http_client: The connection is not used any more by the client";
               T.remove connection_table key;
               put_in_free_conn ~gf ()
             end
             else
               T.replace connection_table key (fd, thr_conn, gf, nb_users - 1)
           with Not_found -> 
             Messages.warning
               "Http_client - Strange: connection disappeared from \
                   connection_table");
  in
  (match Lazy.force http_frame.Http_frame.content with
     | None   -> finalize ()
     | Some c -> 
         Ocsistream.add_finalizer c
           (fun () -> finalize (); Lwt.return ()));
  
  Lwt.return http_frame




let get ?https ?port ~host ~uri () =
  Lwt_lib.gethostbyname host >>= fun host_entry ->
  raw_request 
      ?https ?port ~http_method:Http_frame.Http_header.GET
      ~host ~inet_addr:host_entry.Unix.h_addr_list.(0) ~uri ()


(*VVV missing: post *)



let basic_raw_request 
    ?headers ?(https=false) ?port ?content
    ~http_method ~host ~inet_addr ~uri () =
(*VVV What do we do with Host: or Connection: etc.? *)
  let port = match port with
  | None -> if https then 443 else 80
  | Some p -> p
  in
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect fd (Unix.ADDR_INET (inet_addr, port)) >>= fun () ->
  (if https then
    Lwt_ssl.ssl_connect fd !sslcontext
  else
    Lwt.return (Lwt_ssl.plain fd)) >>= fun socket ->
            
  let query = Http_frame.Http_header.Query (http_method, uri) in
  let conn = Http_com.create_receiver Http_com.Query socket in
  let headers = 
    Http_headers.add 
      (Http_headers.name "host")
      host
      (Http_headers.add 
         (Http_headers.name "connection")
         "close"
         (match headers with
         | None -> Http_headers.empty
         | Some h -> h))
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
           Http_frame.res_headers= headers;
          }
 
  in
  Http_com.start_processing conn f; (* starting the request *)
(*      Http_com.wait_all_senders conn >>= fun () -> (* not needed *) *)
  Lwt.catch 
    (fun () ->
      Http_com.get_http_frame ~head:false conn >>= fun http_frame ->
      (match Lazy.force http_frame.Http_frame.content with
      | None   -> Lwt_unix.close fd
      | Some c -> 
          Ocsistream.add_finalizer c
            (fun () -> Lwt_unix.close fd; Lwt.return ()));
        Lwt.return http_frame)
    (fun e -> Lwt_unix.close fd; Lwt.fail e)
