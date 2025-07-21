open Eio.Std

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

open Eliom_lib

let section = Logs.Src.create "eliom:bus"

module Ecb = Eliom_comet_base

type 'a consumers = {mutable consumers : ('a option -> unit) list}

type ('a, 'b) t =
  { channel : 'b Ecb.wrapped_channel
  ; consumers : 'b consumers Lazy.t
  ; queue : 'a Queue.t
  ; mutable max_size : int
  ; write : 'a list -> unit
  ; mutable waiter : unit -> unit
  ; mutable last_wait : unit Promise.t }

type ('a, 'att, 'co, 'ext, 'reg) callable_bus_service =
  ( unit
    , 'a list
    , Eliom_service.post
    , 'att
    , 'co
    , 'ext
    , 'reg
    , [`WithoutSuffix]
    , unit
    , [`One of 'a list Eliom_parameter.ocaml] Eliom_parameter.param_name
    , Eliom_registration.Action.return )
    Eliom_service.t

(** Register a callback in the underlying comet. *)
let comet_register chan =
  let t = {consumers = []} in
  let notify data = List.iter (fun callback -> callback data) t.consumers in
  let teardown () =
    let
        (* Notify that the channel reached its end. Clear the [consumers] list to
       avoid memory leaks. *)
          ()
      =
      notify None
    in
    t.consumers <- []
  in
  let _chan =
    Eliom_comet.register_wrapped chan (function
      | Some data -> notify (Some data)
      | None -> teardown ())
  in
  t

let create service channel waiter =
  let write x =
    try
      let _ =
        Eliom_client.call_service
          ~service:(service :> ('a, _, _, _, _) callable_bus_service)
          () x
      in
      ()
    with Eliom_request.Failed_request 204 -> ()
  in
  let consumers = lazy (comet_register channel) in
  { channel
  ; consumers
  ; queue = Queue.create ()
  ; max_size = 20
  ; write
  ; waiter
  ; last_wait = () }

let internal_unwrap ((wrapped_bus : ('a, 'b) Ecb.wrapped_bus), _unwrapper) =
  let waiter () = Js_of_ocaml_lwt.Lwt_js.sleep 0.05 in
  let channel, Eliom_comet_base.Bus_send_service service = wrapped_bus in
  create service channel waiter

let () =
  Eliom_unwrap.register_unwrapper Eliom_common.bus_unwrap_id internal_unwrap

let register t callback =
  let (lazy c) = t.consumers in
  c.consumers <- callback :: c.consumers

let stream t =
  let stream, push = Lwt_stream.create () in
  register t (fun data -> push data);
  stream

let original_stream = stream

let flush t =
  let l = List.rev (Queue.fold (fun l v -> v :: l) [] t.queue) in
  Queue.clear t.queue; t.write l

let try_flush t =
  Lwt.cancel
    (* TODO: lwt-to-direct-style: Use [Switch] or [Cancel] for defining a cancellable context. *)
    (* TODO: lwt-to-direct-style: Use [Switch] or [Cancel] for defining a cancellable context. *)
    t.last_wait;
  if Queue.length t.queue >= t.max_size
  then flush t
  else
    let th =
      Lwt.protected
        (* TODO: lwt-to-direct-style: Use [Switch] or [Cancel] for defining a cancellable context. *)
        (* TODO: lwt-to-direct-style: Use [Switch] or [Cancel] for defining a cancellable context. *)
        (t.waiter ())
    in
    t.last_wait <- th;
    let _ = th; flush t in
    ()

let write t v = Queue.add v t.queue; try_flush t
let close {channel; _} = Eliom_comet.close channel
let set_queue_size b s = b.max_size <- s

let set_time_before_flush b t =
  b.waiter <-
    (if t <= 0.
     then fun x1 -> Fiber.yield x1
     else fun () -> Js_of_ocaml_lwt.Lwt_js.sleep t)

let force_link = ()
