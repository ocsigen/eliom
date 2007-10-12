(* Ocsigen
 * http://www.ocsigen.org
 * http_com.ml Copyright (C) 2005 Denis Berthod
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
TODO
- timeouts
- check the code against the HTTP spec
- improved streams
- what if a stream is not consumed after a request has been handled
  (currently, we consume it again, which looks wrong)
- how are failures dealt with at a higher level (server.ml)?
- defunctorize ?
- when can we start processing the next request?
*)

(** this module provide a mecanisme to comunicate with some http frames *)
open Http_frame
open Ocsistream
open Ocsimisc
let (>>=) = Lwt.(>>=)

(* Internal exceptions *)
exception Buffer_full

(* Exported exceptions *)
exception Ocsigen_header_too_long
exception Ocsigen_HTTP_parsing_error of string * string
exception Ocsigen_Bad_chunked_data
exception Ocsigen_KeepaliveTimeout
exception Ocsigen_Timeout

exception MustClose
exception Connection_reset_by_peer
exception Ocsigen_sending_error of exn


type s_http_mode = Answer | Query | Nofirstline

(** buffer de comunication permettant la reception et la recupération
    des messages *)
type receiver =
  { r_fd : Lwt_ssl.socket;
    r_mode : s_http_mode;
    mutable buf : string;
    mutable read_pos : int;
    mutable write_pos : int }

let create_receiver ~mode fd =
  let buffer_size = Ocsiconfig.get_netbuffersize () in
  { r_fd = fd;
    r_mode = mode;
    buf=String.create buffer_size;
    read_pos = 0;
    write_pos = 0 }

module Com_buffer =
struct

  (** the number of byte in the buffer*)
  let buf_used buffer = buffer.write_pos - buffer.read_pos
  let buf_size buffer = String.length buffer.buf

  let buf_get_string buffer len =
    let pos = buffer.read_pos in
    assert (pos + len <= buffer.write_pos);
    buffer.read_pos <- buffer.read_pos + len;
    String.sub buffer.buf pos len

  (** Receive some more data. *)
(*XXXX Put back a timeout *)
  let receive receiver =
    let used = buf_used receiver in
    let free = buf_size receiver - used in
    if free = 0 then
      Lwt.fail Buffer_full
    else begin
      if receiver.read_pos > 0 then begin
        String.blit receiver.buf receiver.read_pos receiver.buf 0 used;
        receiver.write_pos <- used;
        receiver.read_pos <- 0
      end;
      Lwt_ssl.read receiver.r_fd receiver.buf receiver.write_pos free
        >>= fun len ->
      receiver.write_pos <- used + len;
      if len = 0 then
(*XXXX Make sure this is (always) caught *)
        Lwt.fail End_of_file
      else begin
        Lwt.return ()
      end
    end

  (** Receive data until at least [len] chars are available *)
  let rec fill receiver len =
    if buf_used receiver >= len then
      Lwt.return ()
    else begin
      receive receiver >>= fun () ->
      fill receiver len
    end

  type size = Exact of int64 | Bounded of int64 option

  let rec extract_aux finish receiver pos bound cont =
    let avail = buf_used receiver in
    if avail = 0 then
      Lwt.try_bind
        (fun () -> receive receiver)
        (fun () -> extract_aux finish receiver pos bound cont)
        (fun e ->
           Lwt.wakeup_exn finish e;
           match e, bound with
             End_of_file, Bounded _ ->
               Lwt.return (empty_stream None)
           | _ ->
               Lwt.fail Ocsistream.Interrupted_stream)
    else
      let pos' = Int64.add pos (Int64.of_int avail) in
      match bound with
        Exact l when pos' >= l ->
          let len = Int64.to_int (Int64.sub l pos) in
          let s = buf_get_string receiver len in
          Lwt.return (new_stream s cont)
      | Bounded (Some l) when pos' > l ->
          Lwt.wakeup_exn finish
            (Ocsimisc.Ocsigen_Request_interrupted Ocsigen_Request_too_long);
          Lwt.fail Ocsistream.Interrupted_stream
      | _ ->
          let s = buf_get_string receiver avail in
          Lwt.return
            (new_stream s (fun () ->
             extract_aux finish receiver pos' bound cont))

  (** Stream from the receiver channel. *)
  let extract finish receiver bound =
    extract_aux finish receiver 0L bound
      (fun () ->
         Lwt.wakeup finish ();
         Lwt.return (empty_stream None))

  type pat_res = Found of int | Retry of int

  (** Wait for a given pattern to be received *)
  let rec wait_pattern find_pattern receiver cur_pos =
    let read_pos = receiver.read_pos in
    let avail = receiver.write_pos - (cur_pos + read_pos) in
Format.eprintf "<%s>@." (String.sub receiver.buf cur_pos avail);
    match find_pattern receiver.buf (cur_pos + read_pos) avail with
      Found end_pos ->
        Lwt.return (end_pos - read_pos)
    | Retry retry_pos ->
        let pos = retry_pos - read_pos in
        receive receiver >>= fun () ->
        wait_pattern find_pattern receiver pos

  (** Find the first sequence crlfcrlf or lflf in the buffer *)
  let rec find_header buf pos rem =
    if rem < 2 then
      Retry pos
    else if buf.[pos + 1] <> '\n' then
      find_header buf (pos + 1) (rem - 1)
    else if buf.[pos] = '\n' then
      Found (pos + 2)
    else if
      rem >= 4 && buf.[pos] = '\r' &&
      buf.[pos + 2] = '\r' && buf.[pos + 3] = '\n'
    then
      Found (pos + 4)
    else
      find_header buf (pos + 1) (rem - 1)

  (** Wait until a full header is received.  Returns the length of
      the header *)
  let wait_http_header receiver =
    Lwt.catch
      (fun () -> wait_pattern find_header receiver 0)
      (fun e ->
         match e with
           Buffer_full -> Lwt.fail Ocsigen_header_too_long
         | _           -> Lwt.fail e)

  (** Find an end of line crlf or lf in the buffer *)
  let rec find_line buf pos rem =
    if rem < 1 then Retry pos else
    if buf.[pos] = '\n' then Found (pos + 1) else
    find_line buf (pos + 1) (rem - 1)

  (** Wait until a full line is received.
      Returns the length of the line *)
  let wait_line receiver = wait_pattern find_line receiver 0

  (** extract chunked data in destructive way from the buffer.
     The optional [?finish] parameter is an action that
     will be executed when the stream is finished.
    *)
  let extract_chunked finish receiver =
    let fail e =
      Lwt.wakeup_exn finish
        (match e with
           Buffer_full -> Ocsigen_Bad_chunked_data
         | _           -> e);
      Lwt.fail Ocsistream.Interrupted_stream
    in
    let extract_crlf receiver =
      Lwt.catch
        (fun () ->
           fill receiver 2 >>= fun () ->
           let pos = receiver.read_pos in
           if
             receiver.buf.[pos] = '\r' && receiver.buf.[pos + 1] = '\n'
           then begin
             receiver.read_pos <- pos + 2;
             Lwt.return ()
           end else
             Lwt.fail Ocsigen_Bad_chunked_data)
        fail
    in
    let rec aux () =
      Lwt.try_bind
        (fun () -> wait_line receiver)
        (fun len ->
           let chunksize = buf_get_string receiver len in
           (*XXX Should check that we really have chunked data *)
           let chunksize = Scanf.sscanf chunksize "%x" id in
           if chunksize = 0 then begin
             extract_crlf receiver >>= fun () ->
             Lwt.wakeup finish ();
             Lwt.return (empty_stream None)
           end else
             extract_aux finish receiver 0L (Exact (Int64.of_int chunksize))
               (fun () -> extract_crlf receiver >>= fun () -> aux ()))
        fail
    in
    aux ()

end

let code_with_empty_content code =
  (code > 99 && code < 200) ||
  (code = 204) ||
  (code = 205) ||
  (code = 304)
    (* Others??? *)

module type RECEIVER =
  sig
    type t
    val get_http_frame :
      unit Lwt.t ->
      receiver ->
      ?head:bool -> doing_keep_alive:bool -> unit ->
      t Http_frame.FHttp_frame.http_frame Lwt.t
  end

module FHttp_receiver =
  functor(C:Http_frame.HTTP_CONTENT) ->
    struct
      type t = C.t

      module Http = Http_frame.FHttp_frame

      let parse_http_header mode s =
        let lexbuf = Lexing.from_string s in
        try
          Lwt.return
            (if mode = Nofirstline then
               Http_parser.nofirstline Http_lexer.token lexbuf
             else
               Http_parser.header Http_lexer.token lexbuf)
        with
          Parsing.Parse_error ->
            Lwt.fail (Ocsigen_HTTP_parsing_error (Lexing.lexeme lexbuf, s))

      let get_maxsize = function
        | Nofirstline
        | Answer -> None (* Ocsiconfig.get_maxanswerbodysize () 
                       Do we need a limit? 
                       If yes, add an exception Ocsigen_Answer_too_long.
                       (like Ocsigen_Request_too_long)
                     *)
        | Query -> Ocsiconfig.get_maxrequestbodysize ()

      let no_wait = Lwt.return ()

      (** get an http frame *)
      let get_http_frame waiter receiver ?(head = false) ~doing_keep_alive () =
        (* waiter is here only for pipelining and timeout:
           we trigger the sleep only when the previous request has been
           answered (waiter awoken)
         *)
(*XXX Do we need a waiter here? *)
        waiter >>= fun () ->
(*XXX Keepalive timeout *)
        Com_buffer.wait_http_header receiver >>= fun len ->
        let string_header = Com_buffer.buf_get_string receiver len in
        parse_http_header receiver.r_mode string_header >>= fun header ->
(* RFC
   1.  Any response message  which "MUST  NOT" include  a message-body
   (such as the 1xx, 204, and 304 responses and any response to a HEAD
   request) is  always terminated  by the first  empty line  after the
   header fields,  regardless of  the entity-header fields  present in
   the message.
 *)
        begin match header.Http_header.mode with
          Http_header.Answer code when code_with_empty_content code ->
            Lwt.return (no_wait, None)
        | _ ->
            if head then Lwt.return (no_wait, None) else begin
(* RFC
   2. If  a Transfer-Encoding header field (section  14.41) is present
   and has  any value other than "identity",  then the transfer-length
   is defined  by use of the "chunked"  transfer-coding (section 3.6),
   unless the message is terminated by closing the connection.
*)
            let chunked =
              (*XXX Is that right?*)
              try
                Http_frame.Http_header.get_headers_value
                   header "Transfer-Encoding" <> "identity"
              with Not_found ->
                false
            in
            if chunked then
              let wait_end_of_stream = Lwt.wait () in
              Com_buffer.extract_chunked wait_end_of_stream receiver >>=
              C.content_of_stream >>= fun c ->
              Lwt.return (wait_end_of_stream, Some c)
            else begin
(* RFC
   3. If a Content-Length header field (section 14.13) is present, its
   decimal value  in OCTETs represents both the  entity-length and the
   transfer-length. The  Content-Length header field MUST  NOT be sent
   if these  two lengths are  different (i.e., if  a Transfer-Encoding
   header  field is present).  If a  message is  received with  both a
   Transfer-Encoding header  field and a  Content-Length header field,
   the latter MUST be ignored.
*)
              let content_length =
                try
                  (*XXX Check for overflow/malformed field... *)
                  Some
                    (Int64.of_string
                       (Http_frame.Http_header.get_headers_value
                          header "content-length"))
                with Not_found ->
                  None
              in
              match content_length with
                Some cl ->
                  if cl < 0L then
                    (*XXX Malformed field!!!*)
                    Lwt.fail Ocsimisc.Ocsigen_Bad_Request
                  else if cl = 0L then
                    Lwt.return ((Lwt.return ()), None)
                  else
                    let max = get_maxsize receiver.r_mode in
                    if
                      match max with
                        None   -> false
                      | Some m -> cl > m
                    then
                      (*XXX Why wrap the exception? *)
                      Lwt.fail (Ocsimisc.Ocsigen_Request_interrupted
                                   Ocsigen_Request_too_long)
                    else
                      let wait_end_of_stream = Lwt.wait () in
                      Com_buffer.extract wait_end_of_stream receiver
                        (Com_buffer.Exact cl) >>=
                      C.content_of_stream >>= fun c ->
                      Lwt.return (wait_end_of_stream, Some c)
              | None ->
(* RFC
   4. If  the message uses the media  type "multipart/byteranges", and
   the transfer-length is  not  otherwise specified,  then this  self-
   delimiting media type defines the transfer-length.  This media type
   MUST NOT be used unless the sender knows that the recipient can parse
   it; the presence in a request  of a Range header with multiple byte-
   range specifiers from a 1.1 client implies that the client can parse
   multipart/byteranges responses.

NOT IMPLEMENTED
   5. By  the server closing  the connection. (Closing  the connection
   cannot be  used to indicate the  end of a request  body, since that
   would leave no possibility for the server to send back a response.)

 *)
                  match header.Http_header.mode with
                    Http_header.Query _ ->
                      Lwt.return (no_wait, None)
                  | _ ->
                    let wait_end_of_stream = Lwt.wait () in
                    Com_buffer.extract wait_end_of_stream receiver
                      (Com_buffer.Bounded (get_maxsize receiver.r_mode))
                      >>= C.content_of_stream >>= fun c ->
                    Lwt.return (wait_end_of_stream, Some c)
            end
          end
        end >>= fun (wait_end_of_stream, b) ->
        Lwt.return {Http.header=header;
                    Http.content=b;
                    Http.waiter_thread=wait_end_of_stream}
    end

type sender_type = {
    (** the file descriptor*)
    s_fd: Lwt_ssl.socket;
    (** the mode of the sender Query or Answer *)
    mutable s_mode: s_http_mode; (* do we need that?? *)
    (** protocol to be used : HTTP/1.0 HTTP/1.1 *)
    mutable s_proto: Http_frame.Http_header.proto;
    (** the options to send with each frame, for exemple : server name , ... *)
    mutable s_headers: (string * string) list
  }


(** create a new sender *)
(* That function was in FHttp_sender but it does not depend on C
   It is easier to use everywhere if we put it here.
   -- Vincent 27/04/2007 *)
let create_sender
    ?server_name
    ~mode
    ?(headers=[])
    ?(proto=Http_frame.Http_header.HTTP11) fd = 
  let headers = ("Accept-Ranges","none")::headers in
  let headers =
    match server_name with
    | None -> headers
    | Some s -> ("Server", s)::headers
  in
  {s_fd =fd; 
   s_mode=mode;
   s_headers=headers;
   s_proto=proto}

module type SENDER =
    sig
      type t
      val really_write :
        ?chunked:bool ->
        Lwt_chan.out_channel ->
        (unit -> unit Lwt.t) -> Ocsistream.stream -> unit Lwt.t
      val send :
        ?filter:('a option ->
                 int64 option * Http_frame.etag * Ocsistream.stream *
                 (unit -> unit Lwt.t) ->
                 (int64 option * Http_frame.etag * Ocsistream.stream *
                  (unit -> unit Lwt.t))
                 Lwt.t) ->
        unit Lwt.t ->
        clientproto:Http_frame.Http_header.proto ->
        ?etag:Http_frame.etag ->
        mode:Http_frame.Http_header.http_mode ->
        ?proto:Http_frame.Http_header.proto ->
        ?headers:(string * string) list ->
        ?contenttype:'a ->
        ?content:t -> head:bool -> sender_type -> unit Lwt.t
    end

module FHttp_sender =
  functor(C:Http_frame.HTTP_CONTENT) ->
  struct
    type t = C.t

    module H = Http_frame.Http_header
    
    module Http = Http_frame.FHttp_frame

    module PP = Framepp.Fframepp(C)


(*    (* fonction de dump pour le débogage *)
    let dump str file =
      let out_chan = open_out file in
      output_string out_chan str;
      close_out out_chan 
*)
    (* fonction d'écriture sur le réseau *)
    let really_write ?(chunked=false) out_ch close_fun stream = 
      let cr = "\r\n" in
      let rec aux beg = function
          | Finished _ -> 
              (if chunked
              then 
                Lwt_chan.output_string out_ch 
                  (Printf.sprintf"%s0\r\n\r\n" beg)
              else Lwt.return ()) >>= fun () ->
                Messages.debug "write finished (closing stream)"; 
                (Lwt.catch
                   close_fun
                   (fun _ -> 
                     Messages.debug
                       "Error while closing stream (at end of stream)";
                     Lwt.return ()))
          | Cont (s, l, next) ->
              if l>0
              then
                Lwt_unix.yield () >>= fun () ->
                (if chunked
                then
                  Lwt_chan.output_string out_ch 
                    (Printf.sprintf"%s%x\r\n" beg l)
                else Lwt.return ()) >>= fun () ->
                Lwt_chan.output out_ch s 0 l >>= fun () ->
                next () >>= fun a ->
                aux cr a
              else next () >>= aux cr
      in Lwt.catch
        (fun () -> aux "" stream)
        (function
            Unix.Unix_error (Unix.EPIPE, _, _)
          | Unix.Unix_error (Unix.ECONNRESET, _, _) 
          | Ssl.Write_error _  ->
              Lwt.fail Connection_reset_by_peer
          | e -> Lwt.fail e)


(*    (** changes the protocol *)
    let change_protocol proto sender =
      sender.s_proto <- proto

    (** change the header list *)
    let change_headers headers sender =
      sender.s_headers <- headers

    (** change the mode *)
    let change_mode mode sender =
      sender.s_mode <- mode *)

    (* case non sensitive equality *)
    let non_case_equality s1 s2 =
      (String.lowercase s1) = (String.lowercase s2)

    (* case non sensitive comparison*)
    let non_case_compare s1 s2 =
      String.compare (String.lowercase s1) (String.lowercase s2)

    (* pair order*)
    let pair_order (s11,s12) (s21,s22) =
      if (non_case_compare s11 s21 <= 0)
      then true
      else (non_case_compare s12 s22 <= 0)

    (**adds a header option in the headers list or change the value if the name
    * already exists *)
    let add_header sender name value =
      let rec add_header_aux res =
        function
          |[] -> (name,value)::res
          |(n,v)::tl when (non_case_equality n name) -> (name,value)::(res @ tl)
          |(n,v)::tl -> add_header_aux ((n,v)::res) tl
      in
      sender.s_headers <- add_header_aux [] sender.s_headers

    (**removes a header option from the headers list if its exits*)
    let rem_header sender name =
      let rec rem_header_aux res =
        function
          |[] -> res
          |(n,_)::tl when (non_case_equality n name) -> res @ tl
          |(n,v)::tl -> rem_header_aux ((n,v)::res) tl
      in
      sender.s_headers <- rem_header_aux [] sender.s_headers

    (** gets the protocol*)
    let get_protocol sender =
      sender.s_proto

    (** gets the mode *)
    let get_mode sender =
      sender.s_mode

    (** gets the headers *)
    let get_headers sender =
      sender.s_headers

    (**gets the value of an header name, raise Not_found if it doesn't exists*)
    let get_header_value sender name =
      let rec get_aux = 
        function
          |[] -> raise Not_found
          |(n,v)::_ when (non_case_equality n name) -> v
          |_::tl -> get_aux tl
      in get_aux sender.s_headers
    
    (* fusion of the two headers list and add the content-length header *)
    let hds_fusion chunked content_length lst1 lst2 =
      let rec fusion_aux res =
        function
          |([],[]) -> res
          |((n,_)::tl,lst2) when (non_case_equality n "content-length") ->
              fusion_aux res (tl,lst2)
          |(lst1,(n,_)::tl) when (non_case_equality n "content-length") ->
              fusion_aux res (lst1,tl)
          |([],hd::tl) -> fusion_aux (hd::res) ([],tl)
          |(hd::tl,[])-> fusion_aux (hd::res) (tl,[])
          |((n1,_)::tl1,(n2,v2)::tl2) when (non_case_equality n1 n2) -> 
              fusion_aux ((n2,v2)::res) (tl1,tl2)
          |((n1,v1)::tl1,((n2,_)::_ as list2)) when
              (non_case_compare n1 n2 < 0) ->
              fusion_aux ((n1,v1)::res) (tl1,list2)
          |(lst1,hd2::tl2) -> fusion_aux (hd2::res) (lst1,tl2)
      in
      let h = 
        (fusion_aux [] 
           (Sort.list pair_order lst1, Sort.list pair_order lst2))
      in
      let h = 
        if chunked
        then ("Transfer-Encoding", "chunked")::h
        else h
      in
      match content_length with
      | None -> h
      | Some n -> ("Content-Length", Int64.to_string n)::h



    (** sends the http_frame mode and proto can be overright, the headers are
     * fusioned with those of the sender, the priority is given to the newly
     * defined header when there is a conflict
     * the content-length tag is automaticaly calculated *)
    let send 
        ?(filter = fun ct a -> Lwt.return a)
        waiter
        ~clientproto
        ?etag
        ~mode
        ?proto
        ?headers
        ?contenttype
        ?content
        ~head
        sender =

      (* waiter is here for pipelining: we must wait before sending the page,
         because the previous one may not be sent.
         If we don't want to wait, use waiter = return ()
       *)

      let out_ch = Lwt_ssl.out_channel_of_descr sender.s_fd in
      waiter >>=
      (fun () ->
        let prot = match proto with None -> sender.s_proto | Some p -> p in
        match content with
        | None -> Lwt.return ()
        | Some c -> 
            let empty_content =
              match mode with
              | H.Nofirstline -> false
              | H.Answer code -> code_with_empty_content code
              | H.Query _ -> false
            in
            (C.stream_of_content c >>=
             filter contenttype >>=
             (* Here the stream is opened *)
             (fun (lon, etag2, flux, close_fun) ->
               Lwt.catch
                 (fun () ->
                   let chunked = 
                     (lon = None && 
                      clientproto <> 
                      Http_frame.Http_header.HTTP10 &&
                      not empty_content
                     ) in
                   (* if HTTP/1.0 we do not use chunked encoding
                      even if the client tells that it supports it,
                      because it may be an HTTP/1.0 proxy that
                      transmits the header by mistake.
                      In that case, we close the connection after the
                      answer.
                    *)
                   Lwt.return (hds_fusion chunked lon
                                 (("ETag", ("\""^(match etag with 
                                 | None -> etag2
                                 | Some etag -> etag)^"\""))::sender.s_headers)
                                 (match headers with 
                                 | Some h -> h
                                 | None -> []) ) >>=
                   (fun hds -> 
                     let hd = {
                       H.mode = mode;
                       H.proto = prot;
                       H.headers = hds;
                     } in
                     Messages.debug "writing header";
                     really_write out_ch Lwt.return
                       (new_stream (Framepp.string_of_header hd)
                          (fun () -> 
                            Lwt.return (empty_stream None)))) >>=
                   (fun _ -> 
                     let close_fun2 () =
                       Lwt_chan.flush out_ch >>= fun () ->
                       close_fun ()
                     in
                     if empty_content || head
                     then close_fun2 ()
                     else begin
                       Messages.debug "writing body"; 
                       really_write
                         ~chunked:chunked 
                         out_ch 
                         close_fun2
                         flux >>= fun () ->
                       if (lon=None && 
                           clientproto = Http_frame.Http_header.HTTP10)
                       then Lwt.fail MustClose
                       else Lwt.return ()
                     end
                   )
                 )
                 (function
                   | MustClose -> Lwt.fail MustClose
                   | e -> 
                       (Lwt.catch
                          close_fun 
                          (fun e -> 
                            Messages.debug
                              ("Error while closing stream (error during stream) : "^
                               (Printexc.to_string e));
                            Lwt.return ())) >>= fun () ->
                       Lwt.fail (Ocsigen_sending_error e)))))
  end
