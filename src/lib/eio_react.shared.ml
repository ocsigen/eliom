(* This file is released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

type 'a event = 'a React.event
type 'a signal = 'a React.signal

module E = struct
  include React.E

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
    let is_sleeping = ref false in
    (* The occurrence that is delayed until the limiter returns. *)
    let delayed = ref None in
    (* The resulting event. *)
    let event, push = create () in
    let rec start_sleeping () =
      f ();
      match !delayed with
      | None -> is_sleeping := false
      | Some v ->
          Eliom_lib.fork start_sleeping;
          delayed := None;
          push v
    in
    let iter =
      fmap
        (fun x ->
           if !is_sleeping
           then delayed := Some x
           else (
             is_sleeping := true;
             Eliom_lib.fork start_sleeping;
             (* Send the occurrence now. *)
             push x);
           None)
        e
    in
    (* iter never happens, but we put it in the select to keep it alive
    (effectful event) *)
    select [iter; event]

  let cancel_switch sw () =
    Eio.Switch.fail sw (Failure "Eio_react.cancel_switch")

  let to_stream event =
    let stream, push, set_ref = Eliom_stream.create_with_reference () in
    set_ref (map (fun x -> push (Some x)) event);
    stream

  let of_stream stream =
    let event, push = create () in
    Eio.Switch.run (fun sw ->
      Eio.Fiber.fork ~sw (fun () -> Eliom_stream.iter push stream);
      with_finaliser (cancel_switch sw) event)
end

module S = struct
  include React.S
end