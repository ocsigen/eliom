(* Ocsigen
 * http://www.ocsigen.org
 * Module server.ml
 * Copyright (C) 2010
 * Raphaël Proust
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

(* The Comet server extension only provides untyped channels (channels that
 * transport string content).
 * The first abstraction layer we add here is typped channels. The whole
 * marshalling/unmarshalling process is taken care of automatically. The client
 * dual of this file is eliom_client_comet.ml, located in ./client/, the two
 * modules work together and uses identical marshalling/unmarshalling
 * convention. Don't forget to adapt the dual file when necessary. *)

module Ecc = Eliom_common_comet
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)




(* A module that provides primitive for server-side channel handling. The only
 * needed operations are : creating, writing, getting id. This just wraps
 * functions from the Comet module. *)
module Channels :
sig

  (* Type of typed channels *)
  type 'a chan = Comet.Channels.chan

  val create : unit -> 'a chan

  val write  : 'a chan -> 'a -> unit

  val get_id : 'a chan -> 'a Ecc.chan_id

end = struct

  type 'a chan = Comet.Channels.chan
  let create () = Comet.Channels.create ()
  let encode_data r = Ocsigen_lib.encode ~plus:false (Marshal.to_string r [])
  let write c x =
    Comet.Channels.write c
      (Ocsigen_lib.encode ~plus:false (Marshal.to_string x []))
  let get_id c = Comet.Channels.get_id c

end


(* Here is a wrap for channels. This can be used to transmit it to a client. *)
let wrap ~sp (c : 'a Channels.chan)
      : 'a Ecc.chan_id Eliom_client_types.data_key =
  Eliom_client.wrap ~sp (Channels.get_id c)



module Buffers :
sig

  exception Value_larger_than_buffer_max_size

  type 'a t

  val create :
    max_size:int -> sizer:('a -> int) -> timer:('a -> float option) -> 'a t

  val set_max_size : int -> 'a t -> unit

  val push : 'a -> 'a t -> unit
  val pop : 'a t -> 'a option
  val peek : 'a t -> 'a option

end = struct

  exception Value_larger_than_buffer_max_size

  type 'a disposable_value =
      {
        mutable dv_val   : 'a option ;
        (*   *) dv_size  : int ;
      }

  type 'a t =
      {
        (*   *) b_queue    : 'a disposable_value Queue.t ;
        mutable b_max_size : int ;
        mutable b_size     : int ;
        (*   *) b_sizer    : 'a -> int ;
        (*   *) b_timer    : 'a -> float option ;
      }

  let set_max_size s t =  t.b_max_size <- s

  let new_disposable_value size timer x =
    let v =
      {
        dv_val  = Some x ;
        dv_size = size ;
      }
    in
      match timer x with
        | None -> v
        | Some t ->
            let _ = Lwt_unix.sleep t >|= fun () -> v.dv_val <- None in
            v

  let create ~max_size ~sizer ~timer =
    {
      b_queue    = Queue.create () ;
      b_max_size = max_size ;
      b_size     = 0 ;
      b_sizer    = sizer ;
      b_timer    = timer ;
    }

  let peek t =
    let rec aux () =
      try
        let p = Queue.peek t.b_queue in
        match p.dv_val with
          | Some _ as x -> x
          | None ->
              t.b_size <- t.b_size - p.dv_size ;
              ignore (Queue.pop t.b_queue) ;
              aux ()
      with
        | Queue.Empty -> None
    in aux ()

  let pop t =
    let rec aux () =
      try
        let p = Queue.pop t.b_queue in
        t.b_size <- t.b_size - p.dv_size ;
        match p.dv_val with
          | None -> aux () (* Value already lost *)
          | Some p -> Some p
      with
        | Queue.Empty -> None
    in aux ()

  let push x t =
    (* computing the value size *)
    let size = t.b_sizer x in
    (* checking for fitability *)
    if size > t.b_max_size
    then raise Value_larger_than_buffer_max_size
    else
      (* making room for value *)
      let rec aux () =
        if t.b_size + size > t.b_max_size
        then (ignore (pop t) ; aux ())
        else ()
      in
        aux () ;
        t.b_size <- t.b_size + size ;
        Queue.push (new_disposable_value size t.b_timer x) t.b_queue

end
(*
module Buffered_channels :
sig

  type 'a chan

  val create :
       max_size:int -> ?sizer:('a -> int) -> ?timer:('a -> float option) -> unit
    -> 'a chan

  val write : 'a chan -> 'a -> unit

  val get_id : 'a chan -> Eliom_common_comet.chan_id

end = struct

  type 'a chan = (int * 'a) Buffers.t * (int * 'a) Channels.chan

  let create ~max_size ?(sizer = fun _ -> 1) ?(timer = fun _ -> None) () =
    let c = Channels.create () in
    let b = Buffers.create ~max_size ~sizer ~timer in
      (b, c)

  (* beware : while an "acker" is attached, the channel won't get GCed *)
  let rec attach_acker (buff, chan) =
    Comet.Channels.notification chan >|=
    Messages.decode_ack              >|=
    throw_until buff                 >|= fun () ->
    Buffers.is_empty buff            >|= function
      | true -> Lwt.return ()
      | false ->
          begin
            Channels.write () ;
            attach_acker (buff, chan)
          end



  let write (b, c) x = Buffers.push x b ; Channels.write c [x]

  let get_id (_, c) = Channels.get_id c


end
 *)
