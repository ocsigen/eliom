(* Ocsigen
 * ocsistream.ml Copyright (C) 2005 Vincent Balat
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

open Lwt

exception Stream_too_small
exception Stream_error of string
exception String_too_large

(* The type must be private! *)
type stream = 
  | Finished of stream option (* If there is another stream following
                                 (usefull for substreams) *)
  | Cont of string * int * (unit -> stream Lwt.t)
        (* current buffer, size, follow *)

let empty_stream follow = Finished follow

let new_stream ?len stri f = 
  let l =
    match len with
    | None -> String.length stri
    | Some l -> l
  in
  Cont (stri, l, f)

let rec is_finished = function
  | Finished None -> true
  | Finished (Some s) -> is_finished s
  | _ -> false

let string_of_stream = 
  let rec aux l = function
    | Finished _ -> return ""
    | Cont (s, long, f) -> 
        let l2 = l+long in
        if l2 > Ocsiconfig.get_netbuffersize ()
        then fail String_too_large
        else 
          (f () >>= 
           (fun r -> aux l2 r >>=
             (fun r -> return (s^r))))
  in aux 0

let string_of_streams = 
  let rec aux l = function
    | Finished None -> return ""
    | Finished (Some s) -> aux l s
    | Cont (s, long, f) -> 
        let l2 = l+long in
        if l2 > Ocsiconfig.get_netbuffersize ()
        then fail String_too_large
        else 
          (f () >>= 
           (fun r -> aux l2 r >>=
             (fun r -> return (s^r))))
  in aux 0

let enlarge_stream = function 
  | Finished a -> fail Stream_too_small
  | Cont (s, long, f) ->
      let max = Ocsiconfig.get_netbuffersize () in
      if long >= max
      then fail Ocsimisc.Input_is_too_large
      else
        f () >>= 
        (function
          | Finished _ -> fail Stream_too_small
          | Cont (r, long2, ff) -> 
              let long3=long+long2 in
              let new_s = s^r in
              if long3 <= max
              then return (Cont (new_s, long+long2, ff))
              else let long4 = long3 - max in
              return
                (Cont ((String.sub new_s 0 max), max,
                       (fun () -> return 
                           (Cont ((String.sub new_s max long4), long4, ff))))))

let rec stream_want s len =
 (* returns a stream with at most len bytes read if possible *)
  match s with
  | Finished _ -> return s
  | Cont (stri, long, f)  -> if long >= len
  then return s
  else catch
        (fun () -> enlarge_stream s >>= (fun r -> stream_want s len))
        (function
            Stream_too_small -> return s
          | e -> fail e)

let current_buffer = function
  | Finished _ -> raise Stream_too_small
  | Cont (s, l, _) -> s
        
let rec skip s k = match s with
| Finished _ -> raise Stream_too_small
| Cont (s, len, f) -> 
    if k <= len
    then return (Cont ((String.sub s k (len - k)), (len - k), f))
    else (enlarge_stream (Cont ("", 0, f)) >>= 
          (fun s -> skip s (k - len)))

let substream delim s = 
  let ldelim = String.length delim in
  if ldelim = 0 then fail (Stream_error "Empty delimiter")
  else 
    let rdelim = Netstring_pcre.regexp_string delim in
    let rec aux =
      function
        | Finished _ -> fail Stream_too_small
        | Cont (s, len, f) as stre -> 
            if len < ldelim
            then enlarge_stream stre >>= aux
            else try 
              let p,_ = Netstring_pcre.search_forward rdelim s 0 in
              return 
                (Cont ((String.sub s 0 p), 
                       p,
                       (fun () -> return 
                           (Finished
                              (Some (Cont ((String.sub s p (len - p)), 
                                           (len -p), 
                                           f)))))))
            with Not_found ->
              let pos = (len + 1 - ldelim) in
              return
                (Cont ((String.sub s 0 pos),
                       pos,
                       (fun () -> f () >>=
                         (function
                             Finished _ -> fail Stream_too_small
                           | Cont (s', long, f') -> 
                               aux 
                                 (Cont ((String.sub s pos (len - pos))^s', 
                                        (long+len-pos),
                                        f'))
                         ))))
    in aux s

      
(** read the stream until the end, without decoding *)
let rec consume = function
  | Cont (_, _, f) -> Lwt_unix.yield () >>= (fun () -> f () >>= consume)
  | Finished (Some ss) -> consume ss
  | _ -> return ()

