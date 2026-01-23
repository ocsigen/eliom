open Eio.Std

let () = print_endline "[DEBUG ELIOM] eliom_bus.client: module start"

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

type ('a, 'b) t =
  { channel : 'b Ecb.wrapped_channel
  ; stream : 'b Eliom_stream.t Lazy.t
  ; queue : 'a Queue.t
  ; mutable max_size : int
  ; write : 'a list -> unit
  ; mutable waiter : unit -> unit
  ; mutable last_wait : Switch.t option
  ; mutable original_stream_available : bool
  ; error_h : ('b option Promise.or_exn * (exn, exn) result Promise.u) Lazy.t }

(* clone streams such that each clone of the original stream raise the same exceptions *)
let consume (t, u) s =
  let p, w = Promise.create () in
  Eliom_lib.fork (fun () ->
    try
      let v = Eliom_stream.iter (fun _ -> ()) s in
      ignore (Eio.Promise.try_resolve w (Ok v))
    with e ->
      (match Promise.peek t with None -> ignore (Eio.Promise.try_resolve u (Error e)) | _ -> ());
      ignore (Eio.Promise.try_resolve w (Error e)));
  Eliom_lib.fork (fun () ->
    try
      ignore (Promise.await_exn t);
      ignore (Eio.Promise.try_resolve w (Ok ()))
    with e -> ignore (Eio.Promise.try_resolve w (Error e)));
  Promise.await_exn p

let clone_exn (t, u) s =
  let s' = Eliom_stream.clone s in
  Eliom_stream.from (fun () ->
    try
      let p, w = Promise.create () in
      Eliom_lib.fork (fun () ->
        try ignore (Eio.Promise.try_resolve w (Ok (Eliom_stream.get s')))
        with e -> ignore (Eio.Promise.try_resolve w (Error e)));
      Eliom_lib.fork (fun () ->
        try ignore (Eio.Promise.try_resolve w (Ok (Promise.await_exn t)))
        with e -> ignore (Eio.Promise.try_resolve w (Error e)));
      Promise.await_exn p
    with e ->
      (match Promise.peek t with None -> ignore (Eio.Promise.try_resolve u (Error e)) | _ -> ());
      raise e)

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

let create_error_h () =
  let t, (u : (exn, exn) result Promise.u) =
    Promise.create
      ()
  in
  let tt, uu = Promise.create () in
  ( (try
       ignore (Promise.await t);
       assert false
     with e -> ignore (Eio.Promise.try_resolve uu (Error e)); tt)
  , u )

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
  (* error_h is lazy to avoid Promise.create/await outside Eio context *)
  let error_h = lazy (create_error_h ()) in
  let stream =
    lazy
      (let stream = Eliom_comet.register channel in
       (* iterate on the stream to consume messages: avoid memory leak *)
       let _ = consume (Lazy.force error_h) stream in
       stream)
  in
  let t =
    { channel
    ; stream
    ; queue = Queue.create ()
    ; max_size = 20
    ; write
    ; waiter
    ; last_wait = None
    ; original_stream_available = true
    ; error_h }
  in
  (* the comet channel start receiving after the load phase, so the
     original channel (i.e. without message lost) is only available in
     the first loading phase.
     Note: we use fork to avoid calling wait_load_end during unmarshalling,
     which happens outside an Eio context. *)
  let _ =
    Eliom_lib.fork (fun () ->
      Eliom_client.wait_load_end ();
      t.original_stream_available <- false)
  in
  t

let internal_unwrap ((wrapped_bus : ('a, 'b) Ecb.wrapped_bus), _unwrapper) =
  let waiter () = Js_of_ocaml_eio.Eio_js.sleep 0.05 in
  let channel, Eliom_comet_base.Bus_send_service service = wrapped_bus in
  create service channel waiter

let () =
  Eliom_unwrap.register_unwrapper Eliom_common.bus_unwrap_id internal_unwrap

let stream t = clone_exn (Lazy.force t.error_h) (Lazy.force t.stream)

let original_stream t =
  if Eliom_client_core.in_onload () && t.original_stream_available
  then stream t
  else
    raise_error ~section
      "original_stream: the original stream is not available anymore"

let flush t =
  let l = List.rev (Queue.fold (fun l v -> v :: l) [] t.queue) in
  Queue.clear t.queue; t.write l

exception Cancelled

let try_flush t =
  Option.iter (fun o -> Switch.fail o Cancelled) t.last_wait;
  if Queue.length t.queue >= t.max_size
  then flush t
  else
    Eliom_lib.fork (fun () ->
      Switch.run_protected (fun sw ->
        (*VVV ???*)
        t.last_wait <- Some sw;
        t.waiter ());
      flush t)

let write t v = Queue.add v t.queue; try_flush t
let close {channel; _} = Eliom_comet.close channel
let set_queue_size b s = b.max_size <- s

let set_time_before_flush b t =
  b.waiter <-
    (if t <= 0. then Fiber.yield else fun () -> Js_of_ocaml_eio.Eio_js.sleep t)

let force_link = ()
