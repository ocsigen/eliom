(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010-2011
 * Raphaël Proust
 * Pierre Chambart
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

open Eliom_pervasives

type 'a t =
    {
      channel : 'a Eliom_comet_base.chan_id;
      chan_service : Eliom_comet_base.comet_service;
      queue : 'a Queue.t;
      mutable max_size : int;
      write : 'a list -> unit Lwt.t;
      mutable waiter : unit -> unit Lwt.t;
      mutable last_wait : unit Lwt.t;
    }

let create service chan_service channel waiter =
  let write x =
    lwt _ = Eliom_client.call_service ~service () x in
    Lwt.return ()
  in
  {
    channel;
    chan_service = chan_service;
    queue = Queue.create ();
    max_size = 20;
    write;
    waiter;
    last_wait = Lwt.return ();
  }

let internal_unwrap (((chan_id:'a Eliom_comet_base.chan_id),
		      (chan_service:Eliom_comet_base.comet_service),
		      service),unwrapper) =
  let waiter () = Lwt_js.sleep 0.05 in
  create service chan_service chan_id waiter

let () = Eliom_unwrap.register_unwrapper Eliom_common.bus_unwrap_id internal_unwrap

let stream {channel;chan_service} =
  Eliom_comet.register chan_service channel

let flush t =
  let l = List.rev (Queue.fold (fun l v -> v::l) [] t.queue) in
  Queue.clear t.queue;
  t.write l

let try_flush t =
  Lwt.cancel t.last_wait;
  if Queue.length t.queue >= t.max_size
  then flush t
  else
    let th = Lwt.protected (t.waiter ()) in
    t.last_wait <- th;
    let _ = th >>= (fun () -> flush t) in
    Lwt.return ()

let write t v =
  Queue.add v t.queue;
  try_flush t

let close {channel;chan_service} = Eliom_comet.close chan_service channel

let set_queue_size b s =
  b.max_size <- s

let set_time_before_flush b t =
  b.waiter <-
    if t <= 0.
    then Lwt.pause
    else (fun () -> Lwt_js.sleep t)

