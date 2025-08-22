(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Lwt.Infix

type 'a event = 'a React.event
type 'a signal = 'a React.signal

module E = struct
  include React.E

  (* +---------------------------------------------------------------+
     | Lwt-specific utilities                                        |
     +---------------------------------------------------------------+ *)

  let finalise f _ = f ()

  let with_finaliser f event =
    let r = ref () in
    Gc.finalise (finalise f) r;
    map
      (fun x ->
         ignore (Sys.opaque_identity r);
         x)
      event

  let limit f e =
    (* Thread which prevents [e] from occurring while it is sleeping *)
    let limiter = ref Lwt.return_unit in
    (* The occurrence that is delayed until the limiter returns. *)
    let delayed = ref None in
    (* The resulting event. *)
    let event, push = create () in
    let iter =
      fmap
        (fun x ->
           if Lwt.is_sleeping !limiter
           then (
             (* The limiter is sleeping, we queue the event for later
                delivering. *)
             match !delayed with
             | Some cell ->
                 (* An occurrence is already queued, replace it. *)
                 cell := x;
                 None
             | None ->
                 let cell = ref x in
                 delayed := Some cell;
                 Lwt.on_success !limiter (fun () ->
                   if Lwt.is_sleeping !limiter
                   then delayed := None
                   else
                     let x = !cell in
                     delayed := None;
                     limiter := f ();
                     push x);
                 None)
           else (
             (* Set the limiter for future events. *)
             limiter := f ();
             (* Send the occurrence now. *)
             push x;
             None))
        e
    in
    select [iter; event]

  let cancel_thread t () = Lwt.cancel t

  let to_stream event =
    let stream, push, set_ref = Lwt_stream.create_with_reference () in
    set_ref (map (fun x -> push (Some x)) event);
    stream

  let of_stream stream =
    let event, push = create () in
    let t =
      Lwt.pause () >>= fun () ->
      Lwt_stream.iter
        (fun v ->
           try push v
           with exn when Lwt.Exception_filter.run exn ->
             !Lwt.async_exception_hook exn)
        stream
    in
    with_finaliser (cancel_thread t) event
end
