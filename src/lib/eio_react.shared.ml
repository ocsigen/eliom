open Eio.Std

(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
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

  let next ev =
    let waiter, wakener =
      Promise.create
        (* TODO: ciao-lwt: Use [Switch] or [Cancel] for defining a cancellable context. *)
        (* TODO: ciao-lwt: Translation is incomplete, [Promise.await] must be called on the promise when it's part of control-flow. *)
        ()
    in
    let _ev = map (fun x -> Promise.resolve wakener x) (once ev) in
    (* XXX restore this if promise can be cancelled
      Lwt.on_cancel
      (* TODO: ciao-lwt: Use [Switch] or [Cancel] for defining a cancellable context. *)
      (* TODO: ciao-lwt: Use [Switch] or [Cancel] for defining a cancellable context. *)
      waiter (fun () -> stop ev);
    *)
    waiter

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
             delayed := None;
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

  let from f =
    let event, push = create () in
    let rec loop () =
      let x = f () in
      push x; loop ()
    in
    Switch.run (fun sw ->
      Eio.Fiber.fork ~sw (fun () -> Fiber.yield (); loop ());
      with_finaliser (cancel_switch sw) event)

  let to_stream event =
    let stream, push, set_ref = Eliom_stream.create_with_reference () in
    set_ref (map (fun x -> push (Some x)) event);
    stream

  let of_stream stream =
    let event, push = create () in
    Switch.run (fun sw ->
      Eio.Fiber.fork ~sw (fun () ->
        Fiber.yield ();
        (*XXX Check what happens if an exception is raised *)
        Eliom_stream.iter push stream);
      with_finaliser (cancel_switch sw) event)

  let delay promise =
    match Eio.Promise.peek promise with
    | Some e -> e
    | None ->
        let event, send = create () in
        Eliom_lib.fork (fun () ->
          let e = Eio.Promise.await promise in
          send e; stop event);
        switch never event

  let keeped = ref []
  let keep e = keeped := map ignore e :: !keeped

  (* +---------------------------------------------------------------+
     | Event transformations                                         |
     +---------------------------------------------------------------+ *)

  let run_p e =
    let event, push = create () in
    let iter =
      fmap
        (fun p ->
           Eliom_lib.fork (fun () -> push (Eio.Promise.await p));
           None)
        e
    in
    select [iter; event]

  let run_s e =
    let event, push = create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      fmap
        (fun p ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () ->
               push (Eio.Promise.await p)));
           None)
        e
    in
    select [iter; event]

  let map_p f e =
    let event, push = create () in
    let iter =
      fmap
        (fun x ->
           Eliom_lib.fork (fun () -> push (f x));
           None)
        e
    in
    select [iter; event]

  let map_s f e =
    let event, push = create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      fmap
        (fun x ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () -> push (f x)));
           None)
        e
    in
    select [iter; event]

  let app_p ef e =
    let event, push = create () in
    let iter =
      fmap
        (fun (f, x) ->
           Eliom_lib.fork (fun () -> push (f x));
           None)
        (app (map (fun f x -> f, x) ef) e)
    in
    select [iter; event]

  let app_s ef e =
    let event, push = create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      fmap
        (fun (f, x) ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () -> push (f x)));
           None)
        (app (map (fun f x -> f, x) ef) e)
    in
    select [iter; event]

  let filter_p f e =
    let event, push = create () in
    let iter =
      fmap
        (fun x ->
           Eliom_lib.fork (fun () -> if f x then push x);
           None)
        e
    in
    select [iter; event]

  let filter_s f e =
    let event, push = create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      fmap
        (fun x ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () ->
               if f x then push x));
           None)
        e
    in
    select [iter; event]

  let fmap_p f e =
    let event, push = create () in
    let iter =
      fmap
        (fun x ->
           Eliom_lib.fork (fun () ->
             match f x with Some x -> push x | None -> ());
           None)
        e
    in
    select [iter; event]

  let fmap_s f e =
    let event, push = create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      fmap
        (fun x ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () ->
               match f x with Some x -> push x | None -> ()));
           None)
        e
    in
    select [iter; event]

  let diff_s f e =
    let previous = ref None in
    let event, push = create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      fmap
        (fun x ->
           match !previous with
           | None ->
               previous := Some x;
               None
           | Some y ->
               previous := Some x;
               Eliom_lib.fork (fun () ->
                 Eio.Mutex.use_rw ~protect:false mutex (fun () -> push (f x y)));
               None)
        e
    in
    select [iter; event]

  let accum_s ef acc =
    let acc = ref acc in
    let event, push = create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      fmap
        (fun f ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () ->
               let x = f !acc in
               acc := x;
               push x));
           None)
        ef
    in
    select [iter; event]

  let fold_s f acc e =
    let acc = ref acc in
    let event, push = create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      fmap
        (fun x ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () ->
               let x = f !acc x in
               acc := x;
               push x));
           None)
        e
    in
    select [iter; event]

  let rec rev_fold f acc = function
    | [] -> acc
    | x :: l ->
        let acc = rev_fold f acc l in
        f acc x

  let merge_s f acc el =
    let event, push = create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      fmap
        (fun l ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () ->
               push (rev_fold f acc l)));
           None)
        (merge (fun acc x -> x :: acc) [] el)
    in
    select [iter; event]
end

module S = struct
  include React.S

  (* +---------------------------------------------------------------+
     | Lwt-specific utilities                                        |
     +---------------------------------------------------------------+ *)

  let finalise f _ = f ()

  let with_finaliser f signal =
    let r = ref () in
    Gc.finalise (finalise f) r;
    map
      (fun x ->
         ignore (Sys.opaque_identity r);
         x)
      signal

  let limit ?eq f s =
    let is_sleeping = ref true in
    (* The occurrence that is delayed until the limiter returns. *)
    let delayed = ref None in
    (* The resulting event. *)
    let event, push = E.create () in
    let rec start_sleeping () =
      f ();
      match !delayed with
      | None -> is_sleeping := false
      | Some v ->
          Eliom_lib.fork start_sleeping;
          delayed := None;
          push v
    in
    Eliom_lib.fork start_sleeping;
    let iter =
      E.fmap
        (fun x ->
           if !is_sleeping
           then delayed := Some x
           else (
             is_sleeping := true;
             delayed := None;
             Eliom_lib.fork start_sleeping;
             (* Send the occurrence now. *)
             push x);
           None)
        (changes s)
    in
    (* iter never happens, but we put it in the select to keep it alive
    (effectful event) *)
    hold ?eq (value s) (E.select [iter; event])

  let keeped = ref []
  let keep s = keeped := map ignore s :: !keeped

  (* +---------------------------------------------------------------+
     | Signal transformations                                        |
     +---------------------------------------------------------------+ *)

  let run_s ?eq s =
    let event, push = E.create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      E.fmap
        (fun p ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () ->
               push (Eio.Promise.await p)));
           None)
        (changes s)
    in
    Eliom_lib.fork_promise (fun () ->
      let x =
        Eio.Mutex.use_rw ~protect:false mutex (fun () ->
          Eio.Promise.await (value s))
      in
      hold ?eq x (E.select [iter; event]))

  let map_s ?eq f s =
    let event, push = E.create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      E.fmap
        (fun x ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () -> push (f x)));
           None)
        (changes s)
    in
    Eliom_lib.fork_promise (fun () ->
      let x = Eio.Mutex.use_rw ~protect:false mutex (fun () -> f (value s)) in
      hold ?eq x (E.select [iter; event]))

  let app_s ?eq sf s =
    let event, push = E.create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      E.fmap
        (fun (f, x) ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () -> push (f x)));
           None)
        (E.app (E.map (fun f x -> f, x) (changes sf)) (changes s))
    in
    Eliom_lib.fork_promise (fun () ->
      let x =
        Eio.Mutex.use_rw ~protect:false mutex (fun () -> (value sf) (value s))
      in
      hold ?eq x (E.select [iter; event]))

  let filter_s ?eq f i s =
    let event, push = E.create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      E.fmap
        (fun x ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () ->
               if f x then push x));
           None)
        (changes s)
    in
    let x = value s in
    Eliom_lib.fork_promise (fun () ->
      match Eio.Mutex.use_rw ~protect:false mutex (fun () -> f x) with
      | true -> hold ?eq x (E.select [iter; event])
      | false -> hold ?eq i (E.select [iter; event]))

  let fmap_s ?eq f i s =
    let event, push = E.create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      E.fmap
        (fun x ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () ->
               match f x with Some x -> push x | None -> ()));
           None)
        (changes s)
    in
    Eliom_lib.fork_promise (fun () ->
      match Eio.Mutex.use_rw ~protect:false mutex (fun () -> f (value s)) with
      | Some x -> hold ?eq x (E.select [iter; event])
      | None -> hold ?eq i (E.select [iter; event]))

  let diff_s f s =
    let previous = ref (value s) in
    let event, push = E.create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      E.fmap
        (fun x ->
           let y = !previous in
           previous := x;
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () -> push (f x y)));
           None)
        (changes s)
    in
    E.select [iter; event]

  let sample_s f e s = E.map_s (fun x -> f x (value s)) e
  let accum_s ?eq ef i = hold ?eq i (E.accum_s ef i)
  let fold_s ?eq f i e = hold ?eq i (E.fold_s f i e)

  let rec rev_fold f acc = function
    | [] -> acc
    | x :: l ->
        let acc = rev_fold f acc l in
        f acc x

  let merge_s ?eq f acc sl =
    let s = merge (fun acc x -> x :: acc) [] sl in
    let event, push = E.create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      E.fmap
        (fun l ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () ->
               push (rev_fold f acc l)));
           None)
        (changes s)
    in
    Eliom_lib.fork_promise (fun () ->
      let x =
        Eio.Mutex.use_rw ~protect:false mutex (fun () ->
          rev_fold f acc (value s))
      in
      hold ?eq x (E.select [iter; event]))

  let l1_s ?eq f s1 = map_s ?eq f s1

  let l2_s ?eq f s1 s2 =
    (* Some details about the use of [fun _ _ -> false] on
       https://github.com/ocsigen/lwt/pull/893#pullrequestreview-783083496 *)
    map_s ?eq
      (fun (x1, x2) -> f x1 x2)
      (l2 ~eq:(fun _ _ -> false) (fun x1 x2 -> x1, x2) s1 s2)

  let l3_s ?eq f s1 s2 s3 =
    map_s ?eq
      (fun (x1, x2, x3) -> f x1 x2 x3)
      (l3 ~eq:(fun _ _ -> false) (fun x1 x2 x3 -> x1, x2, x3) s1 s2 s3)

  let l4_s ?eq f s1 s2 s3 s4 =
    map_s ?eq
      (fun (x1, x2, x3, x4) -> f x1 x2 x3 x4)
      (l4
         ~eq:(fun _ _ -> false)
         (fun x1 x2 x3 x4 -> x1, x2, x3, x4)
         s1 s2 s3 s4)

  let l5_s ?eq f s1 s2 s3 s4 s5 =
    map_s ?eq
      (fun (x1, x2, x3, x4, x5) -> f x1 x2 x3 x4 x5)
      (l5
         ~eq:(fun _ _ -> false)
         (fun x1 x2 x3 x4 x5 -> x1, x2, x3, x4, x5)
         s1 s2 s3 s4 s5)

  let l6_s ?eq f s1 s2 s3 s4 s5 s6 =
    map_s ?eq
      (fun (x1, x2, x3, x4, x5, x6) -> f x1 x2 x3 x4 x5 x6)
      (l6
         ~eq:(fun _ _ -> false)
         (fun x1 x2 x3 x4 x5 x6 -> x1, x2, x3, x4, x5, x6)
         s1 s2 s3 s4 s5 s6)

  (* +---------------------------------------------------------------+
     | Monadic interface                                             |
     +---------------------------------------------------------------+ *)

  let return = const

  let bind_s ?eq s f =
    let event, push = E.create () in
    let mutex = Eio.Mutex.create () in
    let iter =
      E.fmap
        (fun x ->
           Eliom_lib.fork (fun () ->
             Eio.Mutex.use_rw ~protect:false mutex (fun () -> push (f x)));
           None)
        (changes s)
    in
    Eliom_lib.fork_promise (fun () ->
      let x = Eio.Mutex.use_rw ~protect:false mutex (fun () -> f (value s)) in
      switch ?eq (hold ~eq:( == ) x (E.select [iter; event])))
end
