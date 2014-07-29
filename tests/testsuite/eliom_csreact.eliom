(* Copyright Vincent Balat *)
{shared{
open Eliom_lib
module type RE = sig
  module S : sig
    type 'a t
    val create : 'a -> 'a t * ('a -> unit)
    val value : 'a t -> 'a
  end
end

module type SH = sig
  val local : 'a shared_value -> 'a
  val client : 'a shared_value -> 'a client_value
end
}}
{server{
module Shared : SH = struct
  let local x = fst (Eliom_lib.shared_value_server_repr x)
  let client x = snd (Eliom_lib.shared_value_server_repr x)
end
}}
{client{
module Shared : SH = struct
  let local x = x
  let client x = x
end
module Eliom_lib = struct
  include Eliom_lib
  let create_shared_value a b = b
end
}}

{shared{
module ReactiveData = struct
  module RList = struct
    include ReactiveData.RList

    let from_signal s =
      let l, handle = ReactiveData.RList.make (React.S.value s) in
(*VVV effectful_signal could be garbage collected. FIX!
  Keeping the warning for now. *)
      let effectful_signal = React.S.map (ReactiveData.RList.set handle) s in
      l

    module Lwt = struct

      let map_data_p_lwt = Lwt_list.map_p

      let map_patch_p_lwt f v =
        let open ReactiveData.RList in
            match v with
              | I (i, x) -> lwt p = f x in Lwt.return (I (i, p))
              | R i -> Lwt.return (R i)
              | X (i, j) -> Lwt.return (X (i, j))
              | U (i, x) -> lwt p = f x in Lwt.return (U (i, p))
      let map_patch_p_lwt f = Lwt_list.map_p (map_patch_p_lwt f)

      let map_msg_p_lwt f v =
        let open ReactiveData.RList in
            match v with
              | Set l -> lwt p = map_data_p_lwt f l in Lwt.return (Set p)
              | Patch p -> lwt p = map_patch_p_lwt f p in Lwt.return (Patch p)

      let map_p_aux r_th f l =
      (* We react to all events occurring on initial list *)
        let event = ReactiveData.RList.event l in
      (* the waiter is used a a lock to keep the order of events *)
        let waiter = ref (Lwt.wait ()) in
        Lwt.wakeup (snd !waiter) ();
        React.E.map
          (fun msg ->
            Lwt.async (fun () ->
              let waiter1 = !waiter in
              let new_waiter = Lwt.wait () in
              waiter := new_waiter;
              lwt new_msg = map_msg_p_lwt f msg in
              lwt rr, rhandle = r_th in
              lwt () = fst waiter1 in
              (match new_msg with
                | ReactiveData.RList.Set s -> ReactiveData.RList.set rhandle s
                | ReactiveData.RList.Patch p ->
                  ReactiveData.RList.patch rhandle p);
              Lwt.wakeup (snd new_waiter) ();
              Lwt.return ())
          )
          event

    (** Same as map_p but we do not compute the initial list.
        Instead, we give the initial list as parameter.
        To be used when the initial list has been computed on client side.
    *)
      let map_p_init ~init (f : 'a -> 'b Lwt.t) (l : 'a t) : 'b t =
        let (rr, _) as r = ReactiveData.RList.make init in
        let effectul_event = map_p_aux (Lwt.return r) f l in
      (* We keep a reference to the effectul_event in the resulting
         reactive list in order that the effectul_event is garbage collected
         only if the resulting list is garbage collected. *)
        ignore (React.E.retain (ReactiveData.RList.event rr)
                  (fun () -> ignore effectul_event));
        rr

    (** [map_p f l] is the equivalent of [ReactiveData.Rlist.map]
        but with a function that may yield.
        If a patch arrives
        when the previous one has not finished to be computed,
        we launch the computation of [f] in parallel,
        but we wait for the previous one to be applied
        before applying it.
    *)
      let map_p (f : 'a -> 'b Lwt.t) (l : 'a t) : 'b t Lwt.t =
      (* First we build the initial value of the result list *)
        let r_th =
          lwt r = Lwt_list.map_p f (ReactiveData.RList.value l) in
          Lwt.return (ReactiveData.RList.make r)
        in
        let effectul_event = map_p_aux r_th f l in
        lwt rr, rhandle = r_th in
      (* We keep a reference to the effectul_event in the resulting
         reactive list in order that the effectul_event is garbage collected
         only if the resulting list is garbage collected. *)
        ignore (React.E.retain (ReactiveData.RList.event rr)
                  (fun () -> ignore effectul_event));
        Lwt.return rr

    end
  end
end
}}

{client{
module React = struct
  module S = struct
    include React.S
    let create x =
      let s,u = create x in
      s,(fun x -> u x)
    module Lwt = struct
      let map_s = Lwt_react.S.map_s
      let map_s_init ~init ?eq f s =
        let event, push = React.E.create () in
        let mutex = Lwt_mutex.create () in
        let iter = React.E.fmap
          (fun x -> Lwt.on_success
            (Lwt_mutex.with_lock mutex (fun () -> f x)) push; None)
          (changes s)
        in
        hold ?eq init (React.E.select [iter; event])
    end
  end
end

module FakeReact = React
module FakeReactiveData = ReactiveData
module SharedReact = React
module SharedReactiveData = ReactiveData
}}
{server{
module FakeReact = struct
  module S = struct
    type 'a t = 'a
    let create x =
      (x, fun _ ->
          failwith "Fact react values cannot be changed on server side")
    let value x = x
    let map ?eq (f : 'a -> 'b) (s : 'a t) : 'b t = f s
  end
end

module FakeReactiveData = struct
  module RList : sig
    type 'a t
    type 'a handle
    val make : 'a list -> 'a t * 'a handle
    val from_signal : 'a list FakeReact.S.t -> 'a t
    val value : 'a t -> 'a list
    val singleton_s : 'a FakeReact.S.t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    module Lwt : sig
      val map_p : ('a -> 'b Lwt.t) -> 'a t -> 'b t Lwt.t
    end
  end = struct
    type 'a t = 'a list
    type 'a handle = unit
    let make l = l, ()
    let from_signal s = FakeReact.S.value s
    let singleton_s s = [s]
    let value l = l
    let map f s = List.map f s
    module Lwt = struct
      let map_p = Lwt_list.map_p
    end
  end
end
}}
{server{
module SharedReact = struct
  module S = struct
    type 'a t = 'a FakeReact.S.t shared_value
    let value (x : 'a t) : 'a shared_value =
      Eliom_lib.create_shared_value
        (FakeReact.S.value (Shared.local x))
        {'a{ FakeReact.S.value (Shared.local %x) }}

    let create x =
      let sv = FakeReact.S.create x in
      let cv = {'a FakeReact.S.t * ('a -> unit){ FakeReact.S.create %x }} in
      let si =
        Eliom_lib.create_shared_value (fst sv) {'a FakeReact.S.t{ fst %cv }} in
      let up =
        Eliom_lib.create_shared_value (snd sv) {'a -> unit{ snd %cv }} in
      (si, up)

    let map ?eq (f : ('a -> 'b) shared_value) (s : 'a t) : 'b t =
      Eliom_lib.create_shared_value
        (FakeReact.S.map (Shared.local f) (Shared.local s))
        {'b FakeReact.S.t{ React.S.map ?eq:%eq %f %s }}

    module Lwt = struct
      let map_s ?eq (f : ('a -> 'b Lwt.t) shared_value) (s : 'a t) : 'b t Lwt.t
          =
        lwt server_result =
          (Shared.local f) (FakeReact.S.value (Shared.local s))
        in
        Lwt.return
          (Eliom_lib.create_shared_value
             (fst (FakeReact.S.create server_result))
             {'b FakeReact.S.t{ SharedReact.S.Lwt.map_s_init
                ~init:%server_result
                ?eq:%eq
                (Shared.local %f) (Shared.local %s) }})

    end
  end
end
module SharedReactiveData = struct
  module RList = struct
    type 'a t = 'a FakeReactiveData.RList.t shared_value
    type 'a handle = 'a FakeReactiveData.RList.handle shared_value

    let from_signal (x : 'a list SharedReact.S.t) =
      let sv = FakeReactiveData.RList.from_signal (Shared.local x) in
      let cv = {'a FakeReactiveData.RList.t{
        FakeReactiveData.RList.from_signal (Shared.local %x) }} in
      (Eliom_lib.create_shared_value sv {{ %cv }})

    let make x =
      let sv = FakeReactiveData.RList.make x in
      let cv = {'a FakeReactiveData.RList.t * 'a FakeReactiveData.RList.handle{
        FakeReactiveData.RList.make %x }} in
      (Eliom_lib.create_shared_value (fst sv)
         {'a FakeReactiveData.RList.t{ fst %cv }},
       Eliom_lib.create_shared_value (snd sv)
         {'b FakeReactiveData.RList.handle{ snd %cv }})

    let singleton_s s =
      let sv = FakeReactiveData.RList.singleton_s (Shared.local s) in
      let cv = {'a FakeReactiveData.RList.t{
        FakeReactiveData.RList.singleton_s %s }} in
      (Eliom_lib.create_shared_value sv {'a FakeReactiveData.RList.t{ %cv }})

    let map f s =
      let sv = FakeReactiveData.RList.map (Shared.local f) (Shared.local s) in
      let cv = {'a FakeReactiveData.RList.t{
        FakeReactiveData.RList.map %f %s }} in
      (Eliom_lib.create_shared_value sv {'a FakeReactiveData.RList.t{ %cv }})

    module Lwt = struct
      let map_p (f : ('a -> 'b Lwt.t) shared_value) (l : 'a t) : 'b t Lwt.t =
        lwt server_result =
          Lwt_list.map_p
            (Shared.local f)
            (FakeReactiveData.RList.value (Shared.local l))
        in
        Lwt.return
          (Eliom_lib.create_shared_value
             (fst (FakeReactiveData.RList.make server_result))
             {{ SharedReactiveData.RList.Lwt.map_p_init
                ~init:%server_result
                (Shared.local %f) (Shared.local %l) }})

    end

  end
end
}}


{server{
      module React = SharedReact
      (* module ReactiveData = SharedReactiveData *)


      module R = struct
        let node (signal : 'a Eliom_content.Html5.elt SharedReact.S.t) =
          Eliom_content.Html5.C.node
(*VVV
 * This will blink at startup! FIX!
 * How to avoid the span? (implement D.pcdata ...)
*)
          ~init:(FakeReact.S.value (Shared.local signal))
          {{ Eliom_content.Html5.R.node %signal }}



        let pcdata (s : string React.S.t) = Eliom_content.Html5.C.node
(*VVV
 * This will blink at startup! FIX!
 * How to avoid the span? (implement D.pcdata ...)
*)
          ~init:(Eliom_content.Html5.D.(span [pcdata (Shared.local s)]))
          {{ Eliom_content.Html5.R.pcdata %s }}

        let div l = Eliom_content.Html5.C.node
(*VVV
 * This will blink at startup! FIX!
*)
          ~init:(Eliom_content.Html5.D.div
                   (FakeReactiveData.RList.value (Shared.local l)))
          {{ Eliom_content.Html5.R.div (Shared.local %l) }}

        let p l = Eliom_content.Html5.C.node
(*VVV
 * This will blink at startup! FIX!
*)
          ~init:(Eliom_content.Html5.D.p
                   (FakeReactiveData.RList.value (Shared.local l)))
          {{ Eliom_content.Html5.R.p (Shared.local %l) }}

end

}}
