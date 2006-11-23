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
exception Ocsigen_buffer_is_full
exception Ocsigen_header_too_long
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
            then Lwt.fail Connection_reset_by_peer
            else begin
              buffer.write_pos <- (buffer.write_pos + len) mod buffer.size;
              buffer.datasize <- buffer.datasize + len;
              return ()
            end
          ))
      )
      fail

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

  (** extract a given number bytes in destructive way from the buffer *)
  let extract fd buffer (len : int64) =
    let rec extract_aux rem_len =
      let extract_one available rem_len = 
        let nb_extract = 
          min3' rem_len available (buffer.size - buffer.read_pos) in
        let string_extract = 
          String.sub buffer.buf buffer.read_pos nb_extract in
        buffer.read_pos <- (buffer.read_pos + nb_extract) mod buffer.size;
        buffer.datasize <- buffer.datasize - nb_extract;
        if buffer.datasize = 0
        then (buffer.read_pos <- 0; buffer.write_pos <- 0);
        (* je recale le buffer pour avoir des blocs de buffersize exactement *)
        Lwt.return 
          (string_extract,
           (Int64.sub rem_len (Int64.of_int nb_extract)))
      in try
        if rem_len = Int64.zero 
        then Lwt.return (empty_stream None)
        else 
          let available = content_length buffer in
          match available with
          | x when x < 0 -> assert false
          | 0 ->
              (* wait more bytes to read *)
              receive now fd buffer >>= (fun () -> extract_aux rem_len)
          | _ -> 
              extract_one available rem_len >>= 
              (fun (s,rem_len) -> 
                Lwt.return (new_stream s (fun () -> extract_aux rem_len)))
      with e -> fail e
    in extract_aux len

(** find the sequence crlfcrlf in the buffer *)
  let rec find buffer ind nb_read =
    function
      | rem_len when rem_len < nb_read -> raise Not_found
      | rem_len ->
          (
          match nb_read with
            | 4 ->
                (match (buffer.buf.[ind mod buffer.size ],
                        buffer.buf.[(ind+1) mod buffer.size ],
                        buffer.buf.[(ind+2) mod buffer.size],
                        buffer.buf.[(ind+3) mod buffer.size]) with
                  |('\r','\n','\r','\n') -> ind + 3
                  |(_,'\r','\n','\r') -> find buffer (ind + 4) 1 (rem_len -4)
                  |(_,_,'\r','\n') -> find buffer (ind +4) 2 (rem_len -4)
                  |(_,_,_,'\r') -> find buffer (ind+4) 3 (rem_len -4)
                  |(_,_,_,_) -> find buffer (ind+4) 4 (rem_len -4)
                )
            | 3 ->
                (
                  match (buffer.buf.[ind mod buffer.size],
                         buffer.buf.[(ind+1) mod buffer.size],
                         buffer.buf.[(ind +2) mod buffer.size]) with
                    |('\n','\r','\n') -> ind + 2
                    |(_,'\r','\n') -> find buffer (ind + 3) 2 (rem_len -3)
                    |(_,_,'\r') -> find buffer (ind +3) 3 (rem_len -3)
                    |(_,_,_) -> find buffer (ind+3) 4 (rem_len -3)
                )
            | 2 -> 
                (
                  match (buffer.buf.[ind mod buffer.size],
                         buffer.buf.[(ind+1) mod buffer.size]) with
                    |('\r','\n') -> ind + 1
                    |(_,'\r') -> find buffer (ind+2) 3 (rem_len -2)
                    |(_,_) -> find buffer (ind+2) 4 (rem_len -2)
                )
            |1 -> 
                (
                  match buffer.buf.[ind mod buffer.size] with
                    |'\n' -> ind
                    |_ -> find buffer (ind+1) 4 (rem_len -1)
                )
            |_ -> assert false
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
            let end_ind = find buffer cur_ind 4 available in
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
          Ocsigen_buffer_is_full -> fail Ocsigen_header_too_long
        | e -> fail e)

end
    

module FHttp_receiver =
  functor(C:Http_frame.HTTP_CONTENT) ->
    struct
      
      module Http = Http_frame.FHttp_frame (C)
          
      type t = {buffer:Com_buffer.t; fd:Lwt_unix.descr}
            
            (** create a new receiver *)
      let create fd =
        let buffer_size = Ocsiconfig.get_netbuffersize () in
        let buffer = Com_buffer.create buffer_size in
        {buffer=buffer;fd=fd}
          
          (** convert a stream into an header *)
      let http_header_of_stream s =
          catch
            (fun () -> 
              (string_of_stream s) >>=
              (fun s ->
                let lexbuf = Lexing.from_string s in
                catch
                  (fun () ->
                    Lwt.return (Http_parser.header Http_lexer.token lexbuf))
                  (function
                      Parsing.Parse_error -> 
                        fail (Ocsigen_HTTP_parsing_error
                                ((Lexing.lexeme lexbuf),s))
                    | e -> fail e)))
          (function
            | String_too_large -> fail Ocsigen_header_too_long
            | e -> fail e)
         
      (** get an http frame *)
      let get_http_frame waiter receiver ~doing_keep_alive () =
        (* waiter is here only for pipelining and timeout:
           we trigger the sleep only when the previous request has been
           answered (waiter awoken)
         *)
        Com_buffer.wait_http_header waiter
          ~doing_keep_alive receiver.fd receiver.buffer >>=
        (fun len ->
          Com_buffer.extract receiver.fd receiver.buffer (Int64.of_int len) >>=
          (fun string_header ->
            try
              (http_header_of_stream string_header) >>=
              (fun header ->
                let body_length=
                  try
                    Int64.of_string
                      (Http_frame.Http_header.get_headers_value 
                         header "content-length")
                  with
                    _ -> Int64.zero
                in 
                let comp = Int64.compare body_length Int64.zero in
                (if comp < 0 
                then fail Ocsimisc.Ocsigen_Bad_Request
                else if comp = 0 
                then Lwt.return None
                else 
                  let max = Ocsiconfig.get_maxrequestbodysize () in
                  if 
                    (match max with
                      None -> false
                    | Some m ->
                        (Int64.compare body_length m) > 0)
                  then
                    fail (Ocsimisc.Ocsigen_Request_interrupted
                            Ocsigen_Request_too_long)
                  else
                    Com_buffer.extract 
                      receiver.fd receiver.buffer body_length
                      >>= C.content_of_stream >>= 
                    (fun c -> Lwt.return (Some c))) >>=
                (fun b -> Lwt.return {Http.header=header; Http.content=b}))
            with e -> fail e
          )
        )
 
    end


type sender_type = { 
    (** the file descriptor*)
    fd : Lwt_unix.descr;
    (**the mode of the sender Query or Answer*)
    mutable mode : Http_frame.Http_header.http_mode;
    (**protocole to be used : HTTP/1.0 HTTP/1.1*)
    mutable proto : string;
    (**the options to send with each frame, for exemple : server name , ...*)
    mutable headers : (string*string) list
  }

module FHttp_sender =
  functor(C:Http_frame.HTTP_CONTENT) ->
  struct

    module H = Http_frame.Http_header
    
    module Http = Http_frame.FHttp_frame (C)

    module PP = Framepp.Fframepp(C)

    type t = sender_type

(*    (*fonction de dump pour le debuggage*)
    let dump str file =
      let out_chan = open_out file in
      output_string out_chan str;
      close_out out_chan 
*)
    (*fonction d'écriture sur le réseau*)
    let really_write out_ch close_fun stream = 
      let rec aux = function
          | Finished _ -> 
              Messages.debug "write finished (closing stream)"; 
              (try 
                close_fun () 
              with _ -> 
                Messages.debug
                  "Error while closing stream (at end of stream)");
              Lwt.return ()
          | Cont (s, l, next) ->
              Lwt_unix.yield () >>= 
              (fun () ->         
                (Lwt_unix.write out_ch s 0 l >>=
                 (fun len' ->
                   if l = len'
                   then next () 
                   else return 
                       (new_stream (String.sub s len' (l-len')) next)))) >>=
              aux
      in catch
        (fun () -> aux stream)
        (function
            Unix.Unix_error (Unix.EPIPE, _, _)
          | Unix.Unix_error (Unix.ECONNRESET, _, _) 
          | Ssl.Write_error _  ->
              fail Connection_reset_by_peer
          | e -> fail e)


    (** create a new sender *)
    let create ?(mode=Http_frame.Http_header.Answer) ?(headers=[])
    ?(proto="HTTP/1.1") fd = 
      {fd =fd;mode=mode;headers=headers;proto=proto}

    (** changes the protocol *)
    let change_protocol proto sender =
      sender.proto <- proto

    (** change the header list *)
    let change_headers headers sender =
      sender.headers <- headers

    (** change the mode *)
    let change_mode  mode sender =
      sender.mode <- mode

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
      sender.headers <- add_header_aux [] sender.headers

    (**removes a header option from the headers list if its exits*)
    let rem_header sender name =
      let rec rem_header_aux res =
        function
          |[] -> res
          |(n,_)::tl when (non_case_equality n name) -> res @ tl
          |(n,v)::tl -> rem_header_aux ((n,v)::res) tl
      in
      sender.headers <- rem_header_aux [] sender.headers

    (** gets the protocol*)
    let get_protocol sender =
      sender.proto

    (**gets the mode*)
    let get_mode sender =
      sender.mode

    (**gets the headers*)
    let get_headers sender =
      sender.headers

    (**gets the value of an header name, raise Not_found if it doesn't exists*)
    let get_header_value sender name =
      let rec get_aux = 
        function
          |[] -> raise Not_found
          |(n,v)::_ when (non_case_equality n name) -> v
          |_::tl -> get_aux tl
      in get_aux sender.headers
    
    (* fusion of the two headers list and add the content-length header*)
    let hds_fusion content_length lst1 lst2 =
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
          |((n1,v1)::tl1,((n2,_)::_ as list2)) when (non_case_compare n1 n2 < 0) ->
              fusion_aux ((n1,v1)::res) (tl1,list2)
          |(lst1,hd2::tl2) -> fusion_aux (hd2::res) (lst1,tl2)
      in
      let length_header =
        match content_length with
        |None -> []
        |Some n -> [("Content-Length", Int64.to_string n)]
      in
      length_header@
        (fusion_aux [] 
        (Sort.list pair_order lst1,Sort.list pair_order lst2)) 


    (** sends the http_frame mode et proto can be overright, the headers are
    * fusioned with those of the sender, the priority is given to the newly
    * defined header when there is a conflict
    * the content-length tag is automaticaly calculated*)
    let send waiter ?etag
        ?mode ?proto ?headers ?meth ?url ?code ?content ?head sender =
      (*creation d'une http_frame*)
      (*creation du header*)
      (* waiter is here for pipelining: we must wait before sending the page,
         because the previous one may not be sent.
         If we don't want to wait, use waiter = return ()
       *)

      waiter >>=
      (fun () ->
        let md = match mode with None -> sender.mode | Some m -> m in
        let prot = match proto with None -> sender.proto | Some p -> p in
        match content with
        | None -> Lwt.return ()
        | Some c -> 
            (C.stream_of_content c >>=
             (* Here the stream is opened *)
             (fun (lon, etag2, flux, close_fun) ->
               catch
                 (fun () ->
                   Lwt.return (hds_fusion (Some lon)
                                 (("ETag", ("\""^(match etag with 
                                   None -> etag2
                                 | Some etag -> etag)^"\""))::sender.headers)
                                 (match headers with 
                                   Some h ->h
                                 | None -> []) ) >>=
                   (fun hds -> 
                     let hd = {
                       H.mode = md;
                       H.meth=meth;
                       H.url=url;
                       H.code=code;
                       H.proto = prot;
                       H.headers = hds;
                     } in
                     Messages.debug "writing header";
                     really_write sender.fd (fun () -> ())
                       (new_stream (Framepp.string_of_header hd)
                          (fun () -> 
                            Lwt.return (empty_stream None)))) >>=
                   (fun _ -> match head with 
                   | Some true -> Lwt.return ()
                   | _ -> Messages.debug "writing body"; 
                       really_write sender.fd close_fun flux)
                 )
                 (fun e -> 
                   (try 
                     close_fun () 
                   with e -> 
                     Messages.debug
                       ("Error while closing stream (error during stream) : "^
                        (Printexc.to_string e)));
                   fail (Ocsigen_sending_error e)))))
  end
