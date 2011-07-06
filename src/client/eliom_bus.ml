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

module Ecb = Eliom_comet_base

type 'a t =
    {
      channel : 'a Ecb.wrapped_channel;
      queue : 'a Queue.t;
      mutable max_size : int;
      write : 'a list -> unit Lwt.t;
      mutable waiter : unit -> unit Lwt.t;
      mutable last_wait : unit Lwt.t;
    }


type 'a callable_bus_service =
    (unit, 'a list, Eliom_services.service_kind,
     [ `WithoutSuffix ], unit,
     [ `One of 'a list Eliom_parameters.caml ]
       Eliom_parameters.param_name, [ `Registrable ],
     Eliom_output.Action.return)
      Eliom_services.service

let create service channel waiter =
  let write x =
    lwt _ = Eliom_client.call_service
      ~service:(service:>'a callable_bus_service) () x in
    Lwt.return ()
  in
  {
    channel;
    queue = Queue.create ();
    max_size = 20;
    write;
    waiter;
    last_wait = Lwt.return ();
  }

let internal_unwrap ((wrapped_bus:'a Ecb.wrapped_bus),unwrapper) =
  let waiter () = Lwt_js.sleep 0.05 in
  let (channel,service) = wrapped_bus in
  create service channel waiter

let () = Eliom_unwrap.register_unwrapper Eliom_common.bus_unwrap_id internal_unwrap

let stream {channel} =
  Eliom_comet.register channel

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

let close {channel} = Eliom_comet.close channel

let set_queue_size b s =
  b.max_size <- s

let set_time_before_flush b t =
  b.waiter <-
    if t <= 0.
    then Lwt.pause
    else (fun () -> Lwt_js.sleep t)

