(* Ocsigen
 * http://www.ocsigen.org
 * http_com.ml Copyright (C) 2005 Denis Berthod
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

(** this module provide a mecanisme to comunicate with some htttp frames*)
open Http_frame
open Lwt
open Ocsistream
open Ocsimisc

exception Ocsigen_HTTP_parsing_error of string * string
exception Ocsigen_KeepaliveTimeout
exception Ocsigen_Timeout
exception MustClose
exception Ocsigen_buffer_is_full
exception Ocsigen_header_too_long
exception Ocsigen_Bad_chunked_data
exception Connection_reset_by_peer
exception Ocsigen_sending_error of exn

(** Implements a thread that will kill all connections that are too old. *)
module Timeout : sig

  type timeout
  val get_thread : timeout -> int Lwt.t
  val new_timeout : unit -> timeout
  val begin_timeout : timeout -> unit
  val remove_timeout : timeout -> unit
  val start_timeout_killer : unit -> unit Lwt.t

end = struct

  type timeout = 
      {timeout_thread: int Lwt.t option;
       mutable timeout_succ: timeout;
       mutable timeout_prev: timeout}

  let get_thread l = 
    match l.timeout_thread with
      None -> assert false (* Should not happen because last_timeout is
                              not visible outside the module *)
    | Some t -> t

  let new_timeout () =
    (* A new timeout, not started *)
    let rec t =
      {timeout_thread = Some (Lwt.wait ());
       timeout_succ = t;
       timeout_prev = t}
    in t

  let last_timeout () =
    (* A new timeout, not started *)
    let rec t =
      {timeout_thread = None;
       timeout_succ = t;
       timeout_prev = t}
    in t

  let waiters = ref (last_timeout ())
  let olds = ref (last_timeout ())

  let begin_timeout t =
    (* Starting a timeout = putting it in the list *)
    !waiters.timeout_succ.timeout_prev <- t;
    t.timeout_succ <- !waiters.timeout_succ;
    t.timeout_prev <- !waiters;
    !waiters.timeout_succ <- t

  let remove_timeout t =
    (* Stoping a timeout = removing it from the list *)
    t.timeout_prev.timeout_succ <- t.timeout_succ;
    t.timeout_succ.timeout_prev <- t.timeout_prev

  let start_timeout_killer () =
    let t = Ocsiconfig.get_connect_time_max () in
    let rec killall l =
      match l.timeout_thread with
        None -> ()
      | Some t -> (Lwt.wakeup_exn t Ocsigen_Timeout;
                   killall l.timeout_succ)
    in
    let rec aux () =
      Lwt_unix.sleep t >>=
      (fun () -> 
        Messages.debug "Killing timeouted connections";
        killall !olds.timeout_succ; 
        olds := !waiters; 
        waiters := last_timeout ();
        return ()) >>=
      aux
    in aux ()

end


(** buffer de comunication permettant la reception et la recupération des messages *)
module Com_buffer =
struct
  type t = { mutable buf :string; 
             mutable read_pos:int; 
             mutable write_pos:int; 
             mutable datasize:int;
             size:int }

  (** create a buffer *)
  let create size = { buf=String.create size;
                      read_pos=0;
                      write_pos=0; 
                      datasize=0; 
                      size=size }

  let sizedata a b m = 
    let diff = a - b in
    if diff > 0
    then diff
    else m + diff

  (** the number of byte in the buffer*)
  let content_length buffer = buffer.datasize

  (** the number of free byte in the buffer *)
  let nb_free buffer = buffer.size - (content_length buffer)
                         
  (** wait until the buffer has free slot and 
     return the number of free slots till the end of the buffer *)
  let rec wait_can_write buffer =
    let free = nb_free buffer in
      if free = 0 
      then fail Ocsigen_buffer_is_full
      else Lwt.return (min free (buffer.size - buffer.write_pos))
                
  let now = return ()

  (** returns a thread that returns when data were received *)
  let receive waiter file_descr buffer =
    (* waiter will be awoken when we should begin counting the timeout *)
    catch
      (fun () ->
        wait_can_write buffer >>= 
        (fun free ->
          let wait_timeout = Timeout.new_timeout () in
          choose
            [Lwt_unix.yield () >>= 
             (fun () ->
               Lwt_unix.read file_descr buffer.buf buffer.write_pos free) >>=
             (fun l -> 
               Timeout.remove_timeout wait_timeout; 
               return l);
             waiter >>= 
             (fun () -> Timeout.begin_timeout wait_timeout; 
               Timeout.get_thread wait_timeout)
             (* old solution: too much sleeps. fun () ->
               (Lwt_unix.sleep (Ocsiconfig.get_connect_time_max ()) >>= 
                (fun () -> fail Ocsigen_Timeout)) *)] >>=
          (fun len ->
            if len = 0 
            then fail Connection_reset_by_peer
            else begin 
              buffer.write_pos <- (buffer.write_pos + len) mod buffer.size;
              buffer.datasize <- buffer.datasize + len;
              return ()
            end
          ))
      )
      (function
	 | Unix.Unix_error (Unix.EBADF,"read",_) -> fail Connection_reset_by_peer
	 | e -> fail e)

  let min64 a b = if (Int64.compare a b) < 0 then a else b
  let min3 int1 int2 int3 = min (min int1 int2) int3
  let min3' (int1 : int64) int2 int3 = 
    Int64.to_int (min64 (min64 int1 (Int64.of_int int2)) (Int64.of_int int3))
            
  (** read a given number of bytes from the buffer without destroying the buffer content *)
(* j'enlève ; ça n'a pas l'air utilisé... à remettre ???
   ça doit être tout faux
 let read fd buffer off len () =
    let rec read_aux result read_p rem_len =
      let co = (Int64.compare rem_len 0) in
      if co < 0 
      then assert false 
      else if co = 0 
      then Lwt.return result
      else (
        let available = buffer.write_pos - read_p in
        (* convert the positions into buffer indices *)
        let r_buffer_pos = read_p mod buffer.size in
        let co = (Int64.compare available 0) in
        if co < 0 
        then assert false 
        else if co = 0 
        then (* wait until can read more bytes*)
          receive fd buffer >>=
          (fun () -> read_aux result read_p rem_len)
        else let nb_read = 
          min3 rem_len available (buffer.size - r_buffer_pos) in
        let string_read = String.sub buffer.buf read_p nb_read in
        read_aux 
          (result^string_read) 
          (Int64.add read_p nb_read)
          (Int64.sub rem_len nb_read)
       )
    in
      if (buffer.read_pos + off> buffer.write_pos) 
      then Lwt.fail (Invalid_argument ("offset out of bound"))
      else read_aux "" (off + buffer.read_pos) len *)

  type size = Size of int64 | Max of int64 option

  (** extract a given number bytes in destructive way from the buffer.
     The optional [?finish] parameter is an action that
     will be executed when the stream is finished.
    *)
  let extract ?(finish = id) fd buffer (len : size) =
    (* (Max None) means up to exception raised *)
    let rec extract_aux rem_len =

      let extract_one available rem_len = 
        let nb_extract = 
          match rem_len with
          | Size rl ->
              min3' rl available (buffer.size - buffer.read_pos)
          | _ -> min available (buffer.size - buffer.read_pos)
        in

        let string_extract = 
          String.sub buffer.buf buffer.read_pos nb_extract in
        buffer.read_pos <- (buffer.read_pos + nb_extract) mod buffer.size;
        buffer.datasize <- buffer.datasize - nb_extract;
        if buffer.datasize = 0
        then (buffer.read_pos <- 0; buffer.write_pos <- 0);
        (* je recale le buffer pour avoir des blocs de buffersize exactement *)
        (match rem_len with
        | Size rl -> return (Size (Int64.sub rl (Int64.of_int nb_extract)))
        | Max (Some rl) -> 
            let v = Int64.sub rl (Int64.of_int nb_extract) in
            if Int64.compare v Int64.zero >= 0
            then return (Max (Some v))
            else begin
	      finish (); 
	      fail (Ocsimisc.Ocsigen_Request_interrupted
                      Ocsigen_Request_too_long)
	    end
        | a -> return a) >>= fun v ->
        return (string_extract, v)

      in try
        if rem_len = Size Int64.zero 
        then (finish (); return (empty_stream None))
        else 
          let available = content_length buffer in
          match available with
          | x when x < 0 -> assert false
          | 0 ->
              (* wait more bytes to read *)
              catch
                (fun () -> 
                  receive now fd buffer >>= fun () ->
                  extract_aux rem_len
                )
                (function
                  | Connection_reset_by_peer as e ->
                      (match rem_len with
                      | Max _ -> finish (); return (empty_stream None)
                      | _ -> finish (); fail e)
                  | e -> finish (); fail e)
          | _ -> 
              extract_one available rem_len >>= 
              (fun (s, rem_len) ->
                Lwt.return (new_stream s (fun () -> extract_aux rem_len)))
      with e -> finish (); fail e
    in extract_aux len

(** find the sequence crlfcrlf or lflf in the buffer *)
  let rec find_header buffer ind nb_read rem_len =
    (if rem_len < nb_read 
    then begin
      if (rem_len = 2) 
          && buffer.buf.[ind mod buffer.size] = '\n'
          && buffer.buf.[(ind+1) mod buffer.size] = '\n'
      then ind +1
      else
      if (rem_len = 3) 
      then 
        match (buffer.buf.[ind mod buffer.size],
               buffer.buf.[(ind+1) mod buffer.size],
               buffer.buf.[(ind +2) mod buffer.size]) with
        | ('\n','\n',_) -> ind + 1
        | (_,'\n','\n') -> ind + 2
        | _ -> raise Not_found
      else raise Not_found
    end
    else
      match nb_read with
      | 4 ->
          (match (buffer.buf.[ind mod buffer.size ],
                  buffer.buf.[(ind+1) mod buffer.size ],
                  buffer.buf.[(ind+2) mod buffer.size],
                  buffer.buf.[(ind+3) mod buffer.size]) with
          | ('\r','\n','\r','\n') -> ind + 3
          | ('\n','\n',_,_) -> ind + 1
          | (_,'\r','\n','\r') -> 
              find_header buffer (ind + 4) 1 (rem_len -4)
          | (_,'\n','\n',_) -> ind + 2
          | (_,_,'\r','\n') -> 
              find_header buffer (ind +4) 2 (rem_len -4)
          | (_,_,'\n','\n') -> ind + 3
          | (_,_,_,'\n') -> find_header buffer (ind + 4) 1 (rem_len -4)
          | (_,_,_,'\r') -> find_header buffer (ind+4) 3 (rem_len -4)
          | (_,_,_,_) -> find_header buffer (ind+4) 4 (rem_len -4)
          )
      | 3 ->
          (
           match (buffer.buf.[ind mod buffer.size],
                  buffer.buf.[(ind+1) mod buffer.size],
                  buffer.buf.[(ind +2) mod buffer.size]) with
           | ('\n','\r','\n') -> ind + 2
           | (_,'\r','\n') -> 
               find_header buffer (ind + 3) 2 (rem_len -3)
           | (_,_,'\r') -> find_header buffer (ind +3) 3 (rem_len -3)
           | (_,_,_) -> find_header buffer (ind+3) 4 (rem_len -3)
          )
      | 2 -> 
          (
           match (buffer.buf.[ind mod buffer.size],
                  buffer.buf.[(ind+1) mod buffer.size]) with
           | ('\r','\n') -> ind + 1
           | (_,'\r') -> find_header buffer (ind+2) 3 (rem_len -2)
           | (_,_) -> find_header buffer (ind+2) 4 (rem_len -2)
          )
      | 1 -> 
          (
           match buffer.buf.[ind mod buffer.size] with
           | '\n' -> ind
           | '\r' -> find_header buffer (ind+1) 3 (rem_len -1)
           | _ -> find_header buffer (ind+1) 4 (rem_len -1)
          )
      | _ -> assert false
      ) mod buffer.size


  (** returns when the seqence \r\n\r\n is found in the buffer
     the result is number of char to read *)
  let wait_http_header waiter fd buffer ~doing_keep_alive =
    let rec wait_http_header_aux cur_ind =
      (* here the buffer is not empty *)
      match sizedata buffer.write_pos cur_ind buffer.size with
      | x when x <= 0 -> assert false
      | available ->
          try
            let end_ind = find_header buffer cur_ind 4 available in
            Lwt.return (sizedata (end_ind+1) buffer.read_pos buffer.size)
         with 
            Not_found ->
              receive now fd buffer >>= (fun () ->
		wait_http_header_aux 
                  ((cur_ind + available - 
                      (min available 3)) mod buffer.size))
    in 
    catch 
      (fun () ->
        (if doing_keep_alive && (buffer.datasize = 0)
        then
          Lwt.choose
            [receive waiter fd buffer;
             waiter >>= 
             (fun () -> 
               (Lwt_unix.sleep (Ocsiconfig.get_keepalive_timeout ()) >>= 
                (fun () -> fail Ocsigen_KeepaliveTimeout)))]
        else return ()) >>= 
        (fun () -> 
          (if buffer.datasize = 0
          then receive waiter fd buffer
          else return ()) >>= 
          (fun () -> wait_http_header_aux buffer.read_pos))
      )
      (function
        | Ocsigen_buffer_is_full -> fail Ocsigen_header_too_long
        | e -> fail e)


(** find the sequence crlf or lf in the buffer *)
  let find_line buffer ind av =
    let rec aux ind nb_read rem_len =
      (if rem_len = 1
          && buffer.buf.[ind mod buffer.size] = '\n'
      then ind
      else
        if rem_len < nb_read
        then raise Not_found
        else
          match nb_read with
          | 2 -> 
              (
               match (buffer.buf.[ind mod buffer.size],
                      buffer.buf.[(ind+1) mod buffer.size]) with
               | ('\r','\n') -> ind + 1
               | ('\n',_) -> ind
               | (_,'\r') -> aux (ind+2) 1 (rem_len -2)
               | (_,'\n') -> ind + 1
               | (_,_) -> aux (ind+2) 2 (rem_len -2)
              )
          | 1 -> 
              (
               match buffer.buf.[ind mod buffer.size] with
               | '\n' -> ind
               | _ -> aux (ind+1) 2 (rem_len -1)
              )
          | _ -> assert false
      ) mod buffer.size
    in aux ind 2 av

  (** returns when the sequence \r\n or \n is found in the buffer
     the result is number of char to read *)
  let wait_line fd buffer =
    let rec aux cur_ind =
      (* here the buffer is not empty *)
      match sizedata buffer.write_pos cur_ind buffer.size with
      | x when x <= 0 -> assert false
      | available ->
          try
            let end_ind = find_line buffer cur_ind available in
            Lwt.return (sizedata (end_ind+1) buffer.read_pos buffer.size)
          with 
            Not_found ->
              receive now fd buffer >>= (fun () ->
                aux ((cur_ind + available - 
                        (min available 1)) mod buffer.size))
    in 
    try
      (if buffer.datasize = 0
      then receive now fd buffer
      else return ()) >>= fun () -> 
      aux buffer.read_pos
    with e -> fail e

  (** extract chunked data in destructive way from the buffer.
     The optional [?finish] parameter is an action that
     will be executed when the stream is finished.
    *)
  let extract_chunked ?(finish = id) fd buffer =

    let extract_crlf fd buffer =
      extract fd buffer (Size (Int64.of_int 2)) >>= 
      string_of_stream >>= fun s ->
      if s = "\r\n"
      then return ()
      else fail Ocsigen_Bad_chunked_data
    in

    let rec aux () =
      wait_line fd buffer >>= fun len ->
        catch
          (fun () -> extract fd buffer (Size (Int64.of_int len)))
          (function
            | Ocsigen_buffer_is_full -> fail Ocsigen_Bad_chunked_data
            | e -> fail e) >>=
        string_of_stream >>= fun chunksize ->
        let chunksize = Scanf.sscanf chunksize "%x" id in
        if chunksize = 0
        then begin
          extract_crlf fd buffer >>= fun () ->
          finish ();
          return (empty_stream None)
        end
        else 
          (extract fd buffer (Size (Int64.of_int chunksize)) >>=
           transform_stream)

    and transform_stream = function
      | Finished _ -> extract_crlf fd buffer >>= aux
      | Cont (s, l, f) ->
          return (new_stream ~len:l s (fun () -> f () >>= transform_stream))
    in

    aux ()

end


let code_with_empty_content code =
  (code > 99 && code < 200) ||
  (code = 204) ||
  (code = 205) ||
  (code = 304)
    (* Others??? *)


type s_http_mode = Answer | Query | Nofirstline
    
type receiver_type = {
    r_buffer: Com_buffer.t; 
    r_fd: Lwt_unix.descr;
    r_mode: s_http_mode;
  }

(** create a new receiver *)
(* That function was in FHttp_receiver but it does not depend on C
   It is easier to use everywhere if we put it here.
   -- Vincent 18/06/2007 *)
let create_receiver ~mode fd =
  let buffer_size = Ocsiconfig.get_netbuffersize () in
  let buffer = Com_buffer.create buffer_size in
  {r_buffer=buffer; r_fd=fd; r_mode=mode}

module FHttp_receiver =
  functor(C:Http_frame.HTTP_CONTENT) ->
    struct
      
      module Http = Http_frame.FHttp_frame (C)
          
          (** convert a stream into an header *)
      let http_header_of_stream ?(withoutfirstline=false) s =
          catch
            (fun () -> 
              (string_of_stream s) >>=
              (fun s ->
                let lexbuf = Lexing.from_string s in
                catch
                  (fun () ->
                    Lwt.return (
                    if withoutfirstline
                    then
                      Http_parser.nofirstline Http_lexer.token lexbuf
                    else
                      Http_parser.header Http_lexer.token lexbuf))
                  (function
                      Parsing.Parse_error -> 
                        fail (Ocsigen_HTTP_parsing_error
                                ((Lexing.lexeme lexbuf),s))
                    | e -> fail e)))
          (function
            | String_too_large -> fail Ocsigen_header_too_long
            | e -> fail e)

      let get_maxsize = function
        | Nofirstline
        | Answer -> None (* Ocsiconfig.get_maxanswerbodysize () 
                       Do we need a limit? 
                       If yes, add an exception Ocsigen_Answer_too_long.
                       (like Ocsigen_Request_too_long)
                     *)
        | Query -> Ocsiconfig.get_maxrequestbodysize ()

      (** get an http frame *)
      let get_http_frame waiter receiver ~doing_keep_alive () =
        (* waiter is here only for pipelining and timeout:
           we trigger the sleep only when the previous request has been
           answered (waiter awoken)
         *)
        Com_buffer.wait_http_header waiter
          ~doing_keep_alive receiver.r_fd receiver.r_buffer >>=
        (fun len -> 
          Com_buffer.extract 
            receiver.r_fd receiver.r_buffer 
            (Com_buffer.Size (Int64.of_int len)) >>=
          (fun string_header ->
            try
              (http_header_of_stream
                 ~withoutfirstline:(receiver.r_mode = Nofirstline)
                 string_header) >>=
              (fun header ->

(* RFC 

   1.  Any response message  which "MUST  NOT" include  a message-body
   (such as the 1xx, 204, and 304 responses and any response to a HEAD
   request) is  always terminated  by the first  empty line  after the
   header fields,  regardless of  the entity-header fields  present in
   the message.

 *)
                
                (match header.Http_header.mode with
                | Http_header.Answer code when code_with_empty_content code ->
                    return ((return ()), None)
                | _ ->

(*  RFC  

   2. If  a Transfer-Encoding header field (section  14.41) is present
   and has  any value other than "identity",  then the transfer-length
   is defined  by use of the "chunked"  transfer-coding (section 3.6),
   unless the message is terminated by closing the connection.

*)

                    let chunked =
                      try
                        (Http_frame.Http_header.get_headers_value 
                           header "Transfer-Encoding") <> "identity"
                      with _ -> false
                    in

                    if chunked
                    then 
                      let waiter_end_stream = wait () in
                      Com_buffer.extract_chunked
                        ~finish:(Lwt.wakeup waiter_end_stream)
                        receiver.r_fd receiver.r_buffer >>=
                      C.content_of_stream >>= fun c ->
                      return (waiter_end_stream, Some c)
                    else


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
                          let bl = 
                            Int64.of_string
                              (Http_frame.Http_header.get_headers_value 
                                 header "content-length")
                          in Some bl
                        with _ -> None
                      in
      
                      match content_length with
                      | Some cl ->
                          let comp = Int64.compare cl Int64.zero in
                          (if comp < 0 
                          then fail Ocsimisc.Ocsigen_Bad_Request
                          else if comp = 0 
                          then return ((return ()), None)
                          else 
                            let max = get_maxsize receiver.r_mode in
                            if 
                              (match max with
                                None -> false
                              | Some m -> (Int64.compare cl m) > 0)
                            then
                              fail (Ocsimisc.Ocsigen_Request_interrupted
                                      Ocsigen_Request_too_long)
                            else
                              let waiter_end_stream = wait () in
                              Com_buffer.extract
                                ~finish:(Lwt.wakeup waiter_end_stream)
                                receiver.r_fd receiver.r_buffer
                                (Com_buffer.Size cl) >>=
                              C.content_of_stream >>= fun c -> 
                              return (waiter_end_stream, Some c))
                      | None ->
(* RFC

   4. If  the message uses the media  type "multipart/byteranges", and
   the  ransfer-length is  not  otherwise specified,  then this  self-
   elimiting media  type defines the transfer-length.  This media type
   UST NOT be used unless the sender knows that the recipient can arse
   it; the presence in a request  of a Range header with ultiple byte-
   range specifiers from a 1.1 client implies that the lient can parse
   multipart/byteranges responses.

NOT IMPLEMENTED

   5. By  the server closing  the connection. (Closing  the connection
   cannot be  used to indicate the  end of a request  body, since that
   would leave no possibility for the server to send back a response.)

 *)

                          match header.Http_header.mode with
                          | Http_header.Query (meth, _) -> 
                              return ((return ()), None)
                          | _ ->
                            let waiter_end_stream = wait () in
                            Com_buffer.extract
                              ~finish:(Lwt.wakeup waiter_end_stream)
                              receiver.r_fd 
			      receiver.r_buffer 
                              (Com_buffer.Max (get_maxsize receiver.r_mode))
                              >>= C.content_of_stream >>= 
                            (fun c -> return (waiter_end_stream, Some c))


                ) >>=
                (fun (waiter_end_stream, b) -> 
                  Lwt.return {Http.header=header; 
                              Http.content=b;
                              Http.waiter_thread=waiter_end_stream})

              ) 
            with e -> fail e
          )
        )
 
    end


type sender_type = { 
    (** the file descriptor*)
    s_fd: Lwt_unix.descr;
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


module FHttp_sender =
  functor(C:Http_frame.HTTP_CONTENT) ->
  struct

    module H = Http_frame.Http_header
    
    module Http = Http_frame.FHttp_frame (C)

    module PP = Framepp.Fframepp(C)


(*    (*fonction de dump pour le debuggage*)
    let dump str file =
      let out_chan = open_out file in
      output_string out_chan str;
      close_out out_chan 
*)
    (*fonction d'écriture sur le réseau*)
    let really_write ?(chunked=false) out_descr close_fun stream = 
      let cr = "\r\n" in
      let out_ch = Lwt_unix.out_channel_of_descr out_descr in
      let rec aux beg beginning_of_chunk = function
          | Finished _ -> 
              (if chunked
              then 
                Lwt_unix.output_string out_ch 
                  (Printf.sprintf"%s0\r\n\r\n" beg) >>= fun () ->
                Lwt_unix.flush out_ch
              else return ()) >>= fun () ->
                Messages.debug "write finished (closing stream)"; 
                (try 
                  close_fun () 
                with _ -> 
                  Messages.debug
                    "Error while closing stream (at end of stream)");
                Lwt.return ()
          | Cont (s, l, next) ->
              if l>0
              then
                Lwt_unix.yield () >>= fun () ->
                  (if chunked && beginning_of_chunk
                  then
                    Lwt_unix.output_string out_ch 
                      (Printf.sprintf"%s%x\r\n" beg l) >>= fun () -> 
                    Lwt_unix.flush out_ch
                  else return ()) >>= fun () ->
                    Lwt_unix.write out_descr s 0 l >>= fun len' ->
                      if l = len'
                      then next () >>= aux cr true
                      else
                        return 
                          (new_stream (String.sub s len' (l-len')) next) >>=
                        aux cr false
              else next () >>= aux cr true
      in catch
        (fun () -> aux "" true stream)
        (function
            Unix.Unix_error (Unix.EPIPE, _, _)
          | Unix.Unix_error (Unix.ECONNRESET, _, _) 
          | Ssl.Write_error _  ->
              fail Connection_reset_by_peer
          | e -> fail e)


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
        waiter
        ~clientproto
        ?etag
        ~mode
        ?proto
        ?headers
        ?content
        ?head
        sender =
      (* creation d'une http_frame *)
      (* creation du header *)
      (* waiter is here for pipelining: we must wait before sending the page,
         because the previous one may not be sent.
         If we don't want to wait, use waiter = return ()
       *)

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
             (* Here the stream is opened *)
             (fun (lon, etag2, flux, close_fun) ->
               catch
                 (fun () ->
                   let chunked = 
                     (lon=None && 
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
                                   None -> etag2
                                 | Some etag -> etag)^"\""))::sender.s_headers)
                                 (match headers with 
                                   Some h -> h
                                 | None -> []) ) >>=
                   (fun hds -> 
                     let hd = {
                       H.mode = mode;
                       H.proto = prot;
                       H.headers = hds;
                     } in
                     Messages.debug "writing header";
                     really_write sender.s_fd (fun () -> ())
                       (new_stream (Framepp.string_of_header hd)
                          (fun () -> 
                            Lwt.return (empty_stream None)))) >>=
                   (fun _ -> 
                     if empty_content || (head = Some true)
                     then Lwt.return (close_fun ())
                     else begin
                       Messages.debug "writing body"; 
                       really_write
                         ~chunked:chunked 
                         sender.s_fd close_fun flux >>= fun r ->
                       if (lon=None && 
                           clientproto = Http_frame.Http_header.HTTP10)
                       then fail MustClose
                       else return r
                     end
                   )
                 )
                 (function
                   | MustClose -> fail MustClose
                   | e -> 
                       (try 
                         close_fun () 
                       with e -> 
                         Messages.debug
                           ("Error while closing stream (error during stream) : "^
                            (Printexc.to_string e)));
                       fail (Ocsigen_sending_error e)))))
  end
