(* Ocsigen
 * ocsigen_stream.ml Copyright (C) 2005 Vincent Balat
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

let (>>=) = Lwt.bind

exception Interrupted of exn
exception Cancelled
exception Already_read
exception Finalized

type 'a stream = 'a step Lwt.t Lazy.t

and 'a step =
  | Finished of 'a stream option (* If there is another stream following
                                    (usefull for substreams) *)
  | Cont of 'a * 'a stream       (* Current buffer, what follows *)

type outcome = [`Success | `Failure]

type 'a t =
  { mutable stream : 'a stream;
    mutable in_use : bool;
    mutable finalizer : outcome -> unit Lwt.t }

let empty follow =
  match follow with
    None    -> Lwt.return (Finished None)
  | Some st -> Lwt.return (Finished (Some (Lazy.lazy_from_fun st)))

let cont stri f =
  Lwt.return (Cont (stri, Lazy.lazy_from_fun f))

let make ?finalize:(g = fun _ -> Lwt.return ()) f =
  { stream = Lazy.lazy_from_fun f; in_use = false; finalizer = g }

let next = Lazy.force

let rec get_aux st =
  lazy
    (Lwt.try_bind
       (fun () -> Lazy.force st.stream)
       (fun e ->
          Lwt.return
            (match e with
               Cont (s, rem) -> st.stream <- rem; Cont (s, get_aux st)
             | _             -> e))
       (fun e ->
          st.stream <- lazy (Lwt.fail e);
          Lwt.fail (Interrupted e)))

let get st =
  if st.in_use then raise Already_read;
  st.in_use <- true;
  get_aux st

(** read the stream until the end, without decoding *)
let rec consume_aux st =
  next st >>= fun e ->
  match e with
  | Cont (_, f)        -> consume_aux f
  | Finished (Some ss) -> consume_aux ss
  | Finished None      -> Lwt.return ()

let cancel st =
  let st' = st.stream in
  st.stream <- lazy (Lwt.fail Cancelled);
  consume_aux st'

let consume st =
  consume_aux st.stream

let finalize st status =
  let f = st.finalizer in
  st.finalizer <- (fun _ -> Lwt.return ());
  f status >>= fun () ->
  st.stream <- lazy (Lwt.fail Finalized);
  Lwt.return ()

let add_finalizer st g =
  let f = st.finalizer in
  st.finalizer <- fun status -> f status >>= fun () -> g status

(****)

(** String streams *)

open Lwt

exception Stream_too_small
exception Stream_error of string
exception String_too_large

let string_of_stream m s =
  let buff = Buffer.create (m/4) in
  let rec aux i s =
    next s >>= function
    | Finished _ -> Lwt.return buff
    | Cont (s, f) ->
        let i = i + String.length s in
        if i > m
        then Lwt.fail String_too_large
        else (Buffer.add_string buff s; aux i f)
  in
  aux 0 s >|= Buffer.contents

(*
(*XXX Quadratic!!! *)
let string_of_streams =
  let rec aux l = function
    | Finished None -> return ""
    | Finished (Some s) -> next s >>= fun r -> aux l r
    | Cont (s, f) ->
        let l2 = l+String.length s in
        if l2 > Ocsigen_config.get_netbuffersize ()
        then fail String_too_large
        else
          next f >>= fun r ->
          aux l2 r >>= fun r ->
          return (s^r)
  in aux 0
*)

let enlarge_stream = function
  | Finished a -> fail Stream_too_small
  | Cont (s, f) ->
      let long = String.length s in
      let max = Ocsigen_config.get_netbuffersize () in
      if long >= max
      then fail Ocsigen_lib.Input_is_too_large
      else
        next f >>= fun e ->
        match e with
        | Finished _ -> fail Stream_too_small
        | Cont (r, ff) ->
            let long2 = String.length r in
            let long3=long+long2 in
            let new_s = s^r in
            if long3 <= max
            then return (Cont (new_s, ff))
            else let long4 = long3 - max in
            cont (String.sub new_s 0 max)
                 (fun () ->
                    Lwt.return (Cont (String.sub new_s max long4, ff)))

let rec stream_want s len =
 (* returns a stream with at least len bytes read if possible *)
  match s with
  | Finished _ -> return s
  | Cont (stri, f)  ->
      if String.length stri >= len then
        return s
      else
        catch
          (fun () -> enlarge_stream s >>= fun r -> return (`OK r))
          (function
             | Stream_too_small -> return `Too_small
             | e -> fail e)
        >>= function
          | `OK r -> stream_want r len
          | `Too_small -> return s


let current_buffer = function
  | Finished _  -> raise Stream_too_small
  | Cont (s, _) -> s

let rec skip s k = match s with
    | Finished _ -> raise Stream_too_small
    | Cont (s, f) ->
        let len = String.length s in
        let len64 = Int64.of_int len in
        if Int64.compare k len64 <= 0
        then 
          let k = Int64.to_int k in
          Lwt.return (Cont (String.sub s k (len - k), f))
        else (enlarge_stream (Cont ("", f)) >>=
                (fun s -> skip s (Int64.sub k len64)))

let substream delim s =
  let ldelim = String.length delim in
  if ldelim = 0 then fail (Stream_error "Empty delimiter")
  else
    let rdelim = Netstring_pcre.regexp_string delim in
    let rec aux =
      function
        | Finished _ -> fail Stream_too_small
        | Cont (s, f) as stre ->
            let len = String.length s in
            if len < ldelim
            then enlarge_stream stre >>= aux
            else try
              let p,_ = Netstring_pcre.search_forward rdelim s 0 in
              cont (String.sub s 0 p)
                   (fun () ->
                      empty
                           (Some (fun () -> Lwt.return (Cont (String.sub s p (len - p),
                                               f)))))
            with Not_found ->
              let pos = (len + 1 - ldelim) in
              cont (String.sub s 0 pos)
                       (fun () -> 
                          next f >>= function
                          | Finished _ -> fail Stream_too_small
                          | Cont (s', f') ->
                              aux
                                (Cont (String.sub s pos (len - pos) ^ s',
                                       f'))
                       )
    in aux s

(*****************************************************************************)

(*VVV Is it the good place for this? *)

let of_file filename =
  let fd = Lwt_unix.of_unix_file_descr
    (Unix.openfile filename [Unix.O_RDONLY;Unix.O_NONBLOCK] 0o666)
  in
  let ch = Lwt_chan.in_channel_of_descr fd in
  let buf = String.create 1024 in
  let rec aux () =
    Lwt_chan.input ch buf 0 1024 >>= fun n ->
    if n = 0 then empty None else
      (* Streams should be immutable, thus we always make a copy
         of the buffer *)
      cont (String.sub buf 0 n) aux
  in make ~finalize:(fun _ -> Lwt_unix.close fd) aux

let of_string s =
  make (fun () -> cont s (fun () -> empty None))
