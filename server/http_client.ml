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


let (>>=) = Lwt.(>>=)

let _ = Ssl.init ()
let sslcontext = ref (Ssl.create_context Ssl.SSLv23 Ssl.Both_context)


(*VVV TODO: add pipelining *)


let request_sender =
  Http_com.create_sender ~proto:Http_frame.Http_header.HTTP11 ()

let raw_request 
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
        Http_com.send
          slot
          ~mode:query
          ~clientproto:Http_frame.Http_header.HTTP11
          ~head:false
          ~keep_alive:false
          ~sender:request_sender
          {Http_frame.empty_result with
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
      (match http_frame.Http_frame.content with
      | None   -> Lwt_unix.close fd
      | Some c -> 
          Ocsistream.add_finalizer c
            (fun () -> Lwt_unix.close fd; Lwt.return ()));
        Lwt.return http_frame)
    (fun e -> Lwt_unix.close fd; Lwt.fail e)


let get ?https ?port ~host ~uri () =
  Preemptive.detach Unix.gethostbyname host >>= fun host_entry ->
  raw_request 
      ?https ?port ~http_method:Http_frame.Http_header.GET
      ~host ~inet_addr:host_entry.Unix.h_addr_list.(0) ~uri ()


(*VVV missing: post *)
