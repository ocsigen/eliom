open Lwt.Syntax

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

let section = Lwt_log.Section.make "eliom:bus"

module Ecb = Eliom_comet_base

type ('a, 'b) t = {
  channel : 'b Ecb.wrapped_channel;
  stream : 'b Lwt_stream.t Lazy.t;
  queue : 'a Queue.t;
  mutable max_size : int;
  write : 'a list -> unit Lwt.t;
  mutable waiter : unit -> unit Lwt.t;
  mutable last_wait : unit Lwt.t;
  mutable original_stream_available : bool;
  error_h : 'b option Lwt.t * exn Lwt.u;
}

(* clone streams such that each clone of the original stream raise the same exceptions *)
let consume (t, u) s =
  let t' =
    Lwt.catch
      (fun () -> Lwt_stream.iter (fun _ -> ()) s)
      (fun e ->
        (match Lwt.state t with Lwt.Sleep -> Lwt.wakeup_exn u e | _ -> ());
        Lwt.fail e)
  in
  Lwt.choose [ Lwt.bind t (fun _ -> Lwt.return_unit); t' ]

let clone_exn (t, u) s =
  let s' = Lwt_stream.clone s in
  Lwt_stream.from (fun () ->
      Lwt.catch
        (fun () -> Lwt.choose [ Lwt_stream.get s'; t ])
        (fun e ->
          (match Lwt.state t with Lwt.Sleep -> Lwt.wakeup_exn u e | _ -> ());
          Lwt.fail e))

type ('a, 'att, 'co, 'ext, 'reg) callable_bus_service =
  ( unit,
    'a list,
    Eliom_service.post,
    'att,
    'co,
    'ext,
    'reg,
    [ `WithoutSuffix ],
    unit,
    [ `One of 'a list Eliom_parameter.ocaml ] Eliom_parameter.param_name,
    Eliom_registration.Action.return )
  Eliom_service.t

let create service channel waiter =
  let write x =
    Lwt.catch
      (fun () ->
        let* _ =
          Eliom_client.call_service
            ~service:(service :> ('a, _, _, _, _) callable_bus_service)
            () x
        in
        Lwt.return_unit)
      (function
        | Eliom_request.Failed_request 204 -> Lwt.return_unit
        | exc -> Lwt.reraise exc)
  in
  let error_h =
    let t, u = Lwt.wait () in
    ( Lwt.catch
        (fun () ->
          let* _ = t in
          assert false)
        (fun e -> Lwt.fail e),
      u )
  in
  let stream =
    lazy
      (let stream = Eliom_comet.register channel in
       (* iterate on the stream to consume messages: avoid memory leak *)
       let _ = consume error_h stream in
       stream)
  in
  let t =
    {
      channel;
      stream;
      queue = Queue.create ();
      max_size = 20;
      write;
      waiter;
      last_wait = Lwt.return_unit;
      original_stream_available = true;
      error_h;
    }
  in
  (* the comet channel start receiving after the load phase, so the
     original channel (i.e. without message lost) is only available in
     the first loading phase. *)
  let _ =
    let* () = Eliom_client.wait_load_end () in
    t.original_stream_available <- false;
    Lwt.return_unit
  in
  t

let internal_unwrap ((wrapped_bus : ('a, 'b) Ecb.wrapped_bus), _unwrapper) =
  let waiter () = Js_of_ocaml_lwt.Lwt_js.sleep 0.05 in
  let channel, Eliom_comet_base.Bus_send_service service = wrapped_bus in
  create service channel waiter

let () =
  Eliom_unwrap.register_unwrapper Eliom_common.bus_unwrap_id internal_unwrap

let stream t = clone_exn t.error_h (Lazy.force t.stream)

let original_stream t =
  if Eliom_client_core.in_onload () && t.original_stream_available then stream t
  else
    raise_error ~section
      "original_stream: the original stream is not available anymore"

let flush t =
  let l = List.rev (Queue.fold (fun l v -> v :: l) [] t.queue) in
  Queue.clear t.queue;
  t.write l

let try_flush t =
  Lwt.cancel t.last_wait;
  if Queue.length t.queue >= t.max_size then flush t
  else
    let th = Lwt.protected (t.waiter ()) in
    t.last_wait <- th;
    let _ = th >>= fun () -> flush t in
    Lwt.return_unit

let write t v =
  Queue.add v t.queue;
  try_flush t

let close { channel; _ } = Eliom_comet.close channel
let set_queue_size b s = b.max_size <- s

let set_time_before_flush b t =
  b.waiter <-
    (if t <= 0. then Lwt.pause else fun () -> Js_of_ocaml_lwt.Lwt_js.sleep t)

let force_link = ()
