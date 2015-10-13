(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2014
 * Vincent Balat
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

{shared{

   (* put this in Lwt_react? Find a better name? *)

   let to_signal ~init (th : 'a React.S.t Lwt.t) : 'a React.S.t =
     let s, set = React.S.create init in
     Lwt.async (fun () ->
       lwt ss = th in
       let effectful_signal = React.S.map (fun v -> set v) ss in
       ignore (React.S.retain s (fun () -> ignore effectful_signal));
       Lwt.return ());
     s

}}

{client{
module Value = struct
  type 'a t = 'a Eliom_lib.shared_value
  let client x = x
  let local x = x
end
}}

{server{
module Value = struct
  type 'a t = 'a Eliom_lib.shared_value
  let client x = snd (Eliom_lib.shared_value_server_repr x)
  let local x = fst (Eliom_lib.shared_value_server_repr x)
end
}}

{client{
module React = struct

  type 'a event = 'a React.event
  type 'a signal = 'a React.signal
  type step = React.step

  module S = struct

    include React.S

    let create ?eq ?default ?(reset_default = false) v =
      match default with
      | Some (Some ((_, set) as s)) ->
        if reset_default then set ?step:None v; s
      | _ ->
        create ?eq v

    module Infix = struct
      let (>|=) a f = map f a
      let (=|<) f a = map f a
    end

    module Lwt = struct
      let map_s = Lwt_react.S.map_s
      let map_s_init ~init ?eq f s =
        let th = map_s ?eq f s in
        to_signal ~init th
      let l2_s = Lwt_react.S.l2_s
      let l2_s_init ~init ?eq f s1 s2 =
        let th = l2_s ?eq f s1 s2 in
        to_signal ~init th
      let l3_s = Lwt_react.S.l3_s
      let l3_s_init ~init ?eq f s1 s2 s3 =
        let th = l3_s ?eq f s1 s2 s3 in
        to_signal ~init th
      let l4_s = Lwt_react.S.l4_s
      let l4_s_init ~init ?eq f s1 s2 s3 s4 =
        let th = l4_s ?eq f s1 s2 s3 s4 in
        to_signal ~init th
      let l5_s = Lwt_react.S.l5_s
      let l5_s_init ~init ?eq f s1 s2 s3 s4 s5 =
        let th = l5_s ?eq f s1 s2 s3 s4 s5 in
        to_signal ~init th
      let l6_s = Lwt_react.S.l6_s
      let l6_s_init ~init ?eq f s1 s2 s3 s4 s5 s6 =
        let th = l6_s ?eq f s1 s2 s3 s4 s5 s6 in
        to_signal ~init th
      let merge_s = Lwt_react.S.merge_s
      let merge_s_init ~init ?eq f a l =
        let th = merge_s ?eq f a l in
        to_signal ~init th
    end

  end

  module E = React.E

end

module ReactiveData = struct

  module RList = struct

    include ReactiveData.RList

    module Lwt = struct

      let map_data_p_lwt = Lwt_list.map_p

      let map_patch_p_lwt f = function
        | I (i, x) -> lwt p = f x in Lwt.return (I (i, p))
        | R i -> Lwt.return (R i)
        | X (i, j) -> Lwt.return (X (i, j))
        | U (i, x) -> lwt p = f x in Lwt.return (U (i, p))
      let map_patch_p_lwt f = Lwt_list.map_p (map_patch_p_lwt f)

      let map_msg_p_lwt f = function
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
                | ReactiveData.RList.Set s ->
                  ReactiveData.RList.set rhandle s
                | ReactiveData.RList.Patch p ->
                  ReactiveData.RList.patch rhandle p);
               Lwt.wakeup (snd new_waiter) ();
               Lwt.return ()))
          event

      (** Same as map_p but we do not compute the initial list.
          Instead, we give the initial list as parameter.  To be used
          when the initial list has been computed on server side.  *)
      let map_p_init ~init (f : 'a -> 'b Lwt.t) (l : 'a t) : 'b t =
        let (rr, _) as r = ReactiveData.RList.make init in
        let effectul_event = map_p_aux (Lwt.return r) f l in
        (* We keep a reference to the effectul_event in the resulting
           reactive list in order that the effectul_event is garbage
           collected only if the resulting list is garbage
           collected. *)
        ignore (React.E.retain (ReactiveData.RList.event rr)
                  (fun () -> ignore effectul_event));
        rr

      (** [map_p f l] is the equivalent of [ReactiveData.Rlist.map]
          but with a function that may yield.  If a patch arrives when
          the previous one has not finished to be computed, we launch
          the computation of [f] in parallel, but we wait for the
          previous one to be applied before applying it.  *)
      let map_p (f : 'a -> 'b Lwt.t) (l : 'a t) : 'b t Lwt.t =
        (* First we build the initial value of the result list *)
        let r_th =
          lwt r = Lwt_list.map_p f (ReactiveData.RList.value l) in
          Lwt.return (ReactiveData.RList.make r)
        in
        let effectul_event = map_p_aux r_th f l in
        lwt rr, rhandle = r_th in
        (* We keep a reference to the effectul_event in the resulting
           reactive list in order that the effectul_event is garbage
           collected only if the resulting list is garbage
           collected. *)
        ignore (React.E.retain (ReactiveData.RList.event rr)
                  (fun () -> ignore effectul_event));
        Lwt.return rr

    end

    let make ?default ?(reset_default = false) v =
      match default with
      | Some (Some ((_, handle) as s)) ->
        if reset_default then ReactiveData.RList.set handle v; s
      | _ ->
        ReactiveData.RList.make v

    let acc_e ?init e =
      let l, h =
        match init with
        | Some p ->
          p
        | None ->
          make []
      in
      let _ =
        let f x = ReactiveData.RList.cons x h in
        React.E.map f e
      in
      l

  end

end

module FakeReact = React

module FakeReactiveData = ReactiveData
}}

{server{
module FakeReact = struct
  module S : sig
    type 'a t
    val create :
      ?synced:bool -> 'a -> 'a t * (?step:React.step -> 'a -> unit)
    val value : 'a t -> 'a
    val const : ?synced:bool -> 'a -> 'a t
    val synced : 'a t -> bool
    val map : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a t -> 'b t
    val merge : ?eq:('a -> 'a -> bool) ->
      ('a -> 'b -> 'a) -> 'a -> 'b t list -> 'a t
    val l2 : ?eq:('d -> 'd -> bool) ->
      ('a -> 'b -> 'c) ->
      'a t -> 'b t -> 'c t
    val l3 : ?eq:('d -> 'd -> bool) ->
      ('a -> 'b -> 'c -> 'd) ->
      'a t -> 'b t -> 'c t -> 'd t
    val l4 : ?eq:('e -> 'e -> bool) ->
      ('a -> 'b -> 'c -> 'd -> 'e) ->
      'a t -> 'b t -> 'c t -> 'd t -> 'e t
    val l5 : ?eq:('f -> 'f -> bool) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
      'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
    val l6 : ?eq:('g -> 'g -> bool) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t
    val switch : ?eq:('a -> 'a -> bool) -> 'a t t -> 'a t
  end = struct
    type 'a t = 'a * bool
    let create ?synced:(synced = false) x =
      ((x, synced),
       fun ?step _ ->
         failwith "Fact react values cannot be changed on server side")
    let value (x, _) = x
    let const ?synced:(synced = false) x = (x, synced)
    let synced (_, b) = b
    let map ?eq (f : 'a -> 'b) ((x, b) : 'a t) : 'b t = f x, b
    let merge ?eq f acc l =
      let f (acc, acc_b) (x, b) = f acc x, acc_b && b in
      List.fold_left f (acc, true) l
    let l2 ?eq f (x1, b1) (x2, b2) =
      f x1 x2, b1 && b2
    let l3 ?eq f (x1, b1) (x2, b2) (x3, b3) =
      f x1 x2 x3, b1 && b2 && b3
    let l4 ?eq f (x1, b1) (x2, b2) (x3, b3) (x4, b4) =
      f x1 x2 x3 x4, b1 && b2 && b3 && b4
    let l5 ?eq f (x1, b1) (x2, b2) (x3, b3) (x4, b4) (x5, b5) =
      f x1 x2 x3 x4 x5, b1 && b2 && b3 && b4 && b5
    let l6 ?eq f (x1, b1) (x2, b2) (x3, b3) (x4, b4) (x5, b5) (x6, b6) =
      f x1 x2 x3 x4 x5 x6, b1 && b2 && b3 && b4 && b5 && b6
    let switch ?eq (((x, b1), b2): 'a t t) : 'a t = x, b1 && b2
  end
end

module FakeReactiveData = struct
  module RList : sig
    type 'a t
    type 'a handle
    val make : ?synced:bool -> 'a list -> 'a t * 'a handle
    val concat : 'a t -> 'a t -> 'a t
    val value : 'a t -> 'a list
    val synced : 'a t -> bool
    val value_s : 'a t -> 'a list FakeReact.S.t
    val singleton_s : 'a FakeReact.S.t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val make_from_s : 'a list FakeReact.S.t -> 'a t
    module Lwt : sig
      val map_p : ('a -> 'b Lwt.t) -> 'a t -> 'b t Lwt.t
    end
  end = struct
    type 'a t = 'a list * bool
    type 'a handle = unit
    let make ?synced:(synced = false) l = (l, synced), ()
    let concat (l1, b1) (l2, b2) = List.append l1 l2, b1 && b2
    let singleton_s s = [FakeReact.S.value s], FakeReact.S.synced s
    let value (l, _) = l
    let synced (_, b) = b
    let value_s (l, synced) = fst (FakeReact.S.create ~synced l)
    let map f (l, b) = List.map f l, b
    let make_from_s s = FakeReact.S.(value s, synced s)
    module Lwt = struct
      let map_p f (l, b) =
        lwt l = Lwt_list.map_p f l in
        Lwt.return (l, b)
    end
  end
end
}}
{server{
module React = struct
  type 'a event = 'a React.event
  type 'a signal = 'a React.signal
  type step = React.step
  module S = struct
    type 'a t = 'a FakeReact.S.t Value.t
    let value (x : 'a t) = {shared# 'a {
      FakeReact.S.value (Value.local %x) }}

    (*VVV What is the good default value for reset_default?  Setting
      default to true may be difficult to understand.  I prefer
      false.  *)
    let create ?default ?(reset_default = false) x =
      let cv, synced = match default with
        | None ->
          {{ FakeReact.S.create %x }}, true
        | Some v ->
          {'a FakeReact.S.t * (?step:React.step -> 'a -> unit){
             match %v with
             | Some ((_, set) as s) ->
               (* The reactive data is already on client side.  But
                  the value sent by server is more recent.  I update
                  the signal. *)
               if %reset_default then set ?step:None %x; s
             | None ->
               FakeReact.S.create %x
               (*VVV Make possible to disable this?  Warning: removing
                 this or changing the default will break some existing
                 code relying on this!  Do not change the default
                 without wide announcement. *)
           }}, reset_default
      in
      let v, f = FakeReact.S.create ~synced x in
      let si =
        Eliom_lib.create_shared_value v {'a FakeReact.S.t{ fst %cv }}
      and up =
        Eliom_lib.create_shared_value f
          {?step:React.step -> 'a -> unit{ snd %cv }}
      in
      (si, up)

    let map ?eq (f : ('a -> 'b) Value.t) (s : 'a t) : 'b t =
      Eliom_lib.create_shared_value
        (FakeReact.S.map (Value.local f) (Value.local s))
        {'b FakeReact.S.t{ FakeReact.S.map ?eq:%eq %f %s }}

    let merge ?eq (f : ('a -> 'b -> 'a) Value.t)
        (acc : 'a) (l : 'b t list) : 'a t =
      (* we do not use {shared# ... { ... }} to avoid client-side
         List.map Value.local *)
      Eliom_lib.create_shared_value
        (FakeReact.S.merge (Value.local f) acc (List.map Value.local l))
        {'a FakeReact.S.t{ FakeReact.S.merge ?eq:%eq %f %acc %l }}

    let const (v : 'a) : 'a t =
      Eliom_lib.create_shared_value
        (FakeReact.S.const ~synced:true v)
        {'a FakeReact.S.t{ React.S.const %v }}

    let l2 ?eq (f : ('a -> 'b -> 'c) Value.t)
        (s1 : 'a t) (s2 : 'b t) : 'c t =
      Eliom_lib.create_shared_value
        (FakeReact.S.l2 (Value.local f)
           (Value.local s1) (Value.local s2))
        {'d FakeReact.S.t{ React.S.l2 ?eq:%eq %f %s1 %s2 }}

    let l3 ?eq (f : ('a -> 'b -> 'c -> 'd) Value.t)
        (s1 : 'a t) (s2 : 'b t) (s3 : 'c t) : 'd t =
      Eliom_lib.create_shared_value
        (FakeReact.S.l3 (Value.local f)
           (Value.local s1) (Value.local s2) (Value.local s3))
        {'d FakeReact.S.t{ React.S.l3 ?eq:%eq %f %s1 %s2 %s3 }}

    let l4 ?eq (f : ('a -> 'b -> 'c -> 'd -> 'e) Value.t)
        (s1 : 'a t) (s2 : 'b t) (s3 : 'c t) (s4 : 'd t) : 'e t =
      Eliom_lib.create_shared_value
        (FakeReact.S.l4 (Value.local f)
           (Value.local s1) (Value.local s2) (Value.local s3)
           (Value.local s4))
        {'e FakeReact.S.t{ React.S.l4 ?eq:%eq %f %s1 %s2 %s3 %s4 }}

    let l5 ?eq (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) Value.t)
        (s1 : 'a t) (s2 : 'b t) (s3 : 'c t) (s4 : 'd t) (s5 : 'e t)
      : 'f t =
      Eliom_lib.create_shared_value
        (FakeReact.S.l5 (Value.local f)
           (Value.local s1) (Value.local s2) (Value.local s3)
           (Value.local s4) (Value.local s5))
        {'f FakeReact.S.t{
           React.S.l5 ?eq:%eq %f %s1 %s2 %s3 %s4 %s5 }}

    let l6 ?eq
        (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) Value.t)
        (s1 : 'a t) (s2 : 'b t) (s3 : 'c t)
        (s4 : 'd t) (s5 : 'e t) (s6 : 'f t)
      : 'g t =
      Eliom_lib.create_shared_value
        (FakeReact.S.l6 (Value.local f)
           (Value.local s1) (Value.local s2) (Value.local s3)
           (Value.local s4) (Value.local s5) (Value.local s6))
        {'g FakeReact.S.t{
           React.S.l6 ?eq:%eq %f %s1 %s2 %s3 %s4 %s5 %s6 }}

    let switch ?eq (s : 'a t t) : 'a t =
      (* TODO : setting synced to false is safe, but can we do
         better? *)
      Eliom_lib.create_shared_value
        (Value.local s |>
         FakeReact.S.value |>
         Value.local |>
         FakeReact.S.value |>
         FakeReact.S.create ~synced:false |>
         fst)
        {'a FakeReact.S.t{ React.S.switch ?eq:%eq %s }}

    let synced s = Value.local s |> FakeReact.S.synced

    module Infix = struct
      let (>|=) a f = map f a
      let (=|<) f a = map f a
    end

    module Lwt = struct
      let map_s ?eq (f : ('a -> 'b Lwt.t) Value.t) (s : 'a t)
        : 'b t Lwt.t =
        let s = Value.local s in
        lwt server_result = (Value.local f) (FakeReact.S.value s) in
        let synced = FakeReact.S.synced s in
        Lwt.return
          (Eliom_lib.create_shared_value
             (fst (FakeReact.S.create ~synced server_result))
             {'b FakeReact.S.t{
                React.S.Lwt.map_s_init
                  ~init:%server_result ?eq:%eq %f %s }})

      let l2_s ?eq (f : ('a -> 'b -> 'c Lwt.t) Value.t)
          (s1 : 'a t) (s2 : 'b t) : 'c t Lwt.t =
        let s1 = Value.local s1 and s2 = Value.local s2 in
        lwt server_result =
          (Value.local f) (FakeReact.S.value s1) (FakeReact.S.value s2)
        in
        let synced = FakeReact.S.(synced s1 && synced s2) in
        Lwt.return
          (Eliom_lib.create_shared_value
             (fst (FakeReact.S.create ~synced server_result))
             {'c FakeReact.S.t{
                React.S.Lwt.l2_s_init
                  ~init:%server_result ?eq:%eq %f %s1 %s2 }})

      let l3_s ?eq
          (f : ('a -> 'b -> 'c -> 'd Lwt.t) Value.t)
          (s1 : 'a t) (s2 : 'b t) (s3 : 'c t) : 'd t Lwt.t =
        let s1 = Value.local s1
        and s2 = Value.local s2
        and s3 = Value.local s3 in
        lwt server_result =
          (Value.local f)
            (FakeReact.S.value s1)
            (FakeReact.S.value s2)
            (FakeReact.S.value s3)
        in
        let synced = FakeReact.S.(synced s1 && synced s2 && synced s3) in
        Lwt.return
          (Eliom_lib.create_shared_value
             (fst (FakeReact.S.create ~synced server_result))
             {'d FakeReact.S.t{
                React.S.Lwt.l3_s_init ?eq:%eq
                  ~init:%server_result
                  %f %s1 %s2 %s3 }})

      let l4_s ?eq
          (f : ('a -> 'b -> 'c -> 'd -> 'e Lwt.t) Value.t)
          (s1 : 'a t) (s2 : 'b t) (s3 : 'c t) (s4 : 'd t) : 'e t Lwt.t =
        let s1 = Value.local s1 and s2 = Value.local s2
        and s3 = Value.local s3 and s4 = Value.local s4 in
        lwt server_result =
          (Value.local f)
            (FakeReact.S.value s1) (FakeReact.S.value s2)
            (FakeReact.S.value s3) (FakeReact.S.value s4)
        in
        let synced =
          FakeReact.S.(synced s1 && synced s2 && synced s3 && synced s4)
        in
        Lwt.return
          (Eliom_lib.create_shared_value
             (fst (FakeReact.S.create ~synced server_result))
             {'e FakeReact.S.t{
                React.S.Lwt.l4_s_init ?eq:%eq ~init:%server_result
                  %f %s1 %s2 %s3 %s4 }})

      let l5_s ?eq
          (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f Lwt.t) Value.t)
          (s1 : 'a t) (s2 : 'b t) (s3 : 'c t) (s4 : 'd t) (s5 : 'e t)
        : 'f t Lwt.t =
        let s1 = Value.local s1 and s2 = Value.local s2
        and s3 = Value.local s3 and s4 = Value.local s4
        and s5 = Value.local s5 in
        lwt server_result =
          (Value.local f)
            (FakeReact.S.value s1) (FakeReact.S.value s2)
            (FakeReact.S.value s3) (FakeReact.S.value s4)
            (FakeReact.S.value s5)
        in
        let synced = FakeReact.S.(
          synced s1 && synced s2 && synced s3 &&
          synced s4 && synced s5)
        in
        Lwt.return
          (Eliom_lib.create_shared_value
             (fst (FakeReact.S.create ~synced server_result))
             {'f FakeReact.S.t{
                React.S.Lwt.l5_s_init ?eq:%eq ~init:%server_result
                  %f %s1 %s2 %s3 %s4 %s5 }})

      let l6_s ?eq
          (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g Lwt.t)
               Value.t)
          (s1 : 'a t) (s2 : 'b t) (s3 : 'c t)
          (s4 : 'd t) (s5 : 'e t) (s6 : 'f t) : 'g t Lwt.t =
        let s1 = Value.local s1 and s2 = Value.local s2
        and s3 = Value.local s3 and s4 = Value.local s4
        and s5 = Value.local s5 and s6 = Value.local s6 in
        lwt server_result =
          (Value.local f)
            (FakeReact.S.value s1) (FakeReact.S.value s2)
            (FakeReact.S.value s3) (FakeReact.S.value s4)
            (FakeReact.S.value s5) (FakeReact.S.value s6)
        in
        let synced = FakeReact.S.(
          synced s1 && synced s2 && synced s3 &&
          synced s4 && synced s5 && synced s6)
        in
        Lwt.return
          (Eliom_lib.create_shared_value
             (fst (FakeReact.S.create ~synced server_result))
             {'g FakeReact.S.t{
                React.S.Lwt.l6_s_init ?eq:%eq ~init:%server_result
                  %f %s1 %s2 %s3 %s4 %s5 %s6 }})

      let merge_s ?eq (f : ('a -> 'b -> 'a Lwt.t) Value.t)
          (acc : 'a) (l : 'b t list) : 'a t Lwt.t =
        lwt server_result, synced =
          let f (acc, acc_b) v =
            let v = Value.local v and f = Value.local f in
            lwt acc = f acc (FakeReact.S.value v) in
            let acc_b = FakeReact.S.synced v in
            Lwt.return (acc, acc_b)
          in
          Lwt_list.fold_left_s f (acc, true) l
        in
        Lwt.return
          (Eliom_lib.create_shared_value
             (fst (FakeReact.S.create ~synced server_result))
             {'a FakeReact.S.t{
                React.S.Lwt.merge_s_init
                  ~init:%server_result ?eq:%eq %f %acc %l }})

    end
  end
end
module ReactiveData = struct
  module RList = struct
    type 'a t = 'a FakeReactiveData.RList.t Value.t
    type 'a handle = 'a FakeReactiveData.RList.handle Value.t

    let make ?default ?(reset_default = false) x =
      let cv, synced = match default with
        | None ->
          {{ FakeReactiveData.RList.make %x }}, true
        | Some v ->
          {'a FakeReactiveData.RList.t *
           'a FakeReactiveData.RList.handle{
             match %v with
             | Some ((_, handle) as s) ->
               if %reset_default then ReactiveData.RList.set handle %x;
               s
             | None ->
               FakeReactiveData.RList.make %x
           }}, reset_default
      in
      let sv = FakeReactiveData.RList.make ~synced x in
      Eliom_lib.create_shared_value (fst sv)
        {'a FakeReactiveData.RList.t{ fst %cv }},
      Eliom_lib.create_shared_value (snd sv)
        {'b FakeReactiveData.RList.handle{ snd %cv }}

    let concat a b =
      let sv =
        FakeReactiveData.RList.concat
          (Value.local a)
          (Value.local b)
      and cv = {'a FakeReactiveData.RList.t{
        FakeReactiveData.RList.concat %a %b
      }} in
      Eliom_lib.create_shared_value sv cv

    let singleton_s s = {shared# 'a FakeReactiveData.RList.t {
      FakeReactiveData.RList.singleton_s (Value.local %s) }}

    let value (s : 'a t) = {shared# 'a list {
      FakeReactiveData.RList.value (Value.local %s) }}

    let value_s (s : 'a t) = {shared# 'a list FakeReact.S.t {
      FakeReactiveData.RList.value_s (Value.local %s) }}

    let map f s = {shared# 'a FakeReactiveData.RList.t {
      FakeReactiveData.RList.map (Value.local %f) (Value.local %s) }}

    let make_from_s (s : 'a list React.S.t) : 'a t =
      let sv = FakeReactiveData.RList.make_from_s (Value.local s)
      and cv = {{ ReactiveData.RList.make_from_s (Value.local %s) }} in
      Eliom_lib.create_shared_value sv cv

    let acc_e ?init e =
      let l, h =
        match init with
        | Some p ->
          p
        | None ->
          make []
      in
      let _ = {unit{
        let f x = ReactiveData.RList.cons x (Value.local %h) in
        ignore (React.E.map f %e)
      }} in
      l

    let synced s = Value.local s |> FakeReactiveData.RList.synced

    module Lwt = struct
      let map_p (f : ('a -> 'b Lwt.t) Value.t) (l : 'a t) : 'b t Lwt.t =
        let l = Value.local l in
        lwt server_result =
          Lwt_list.map_p (Value.local f) (FakeReactiveData.RList.value l)
        in
        let synced = FakeReactiveData.RList.synced l in
        Lwt.return
          (Eliom_lib.create_shared_value
             (fst (FakeReactiveData.RList.make ~synced server_result))
             {{ ReactiveData.RList.Lwt.map_p_init
                  ~init:%server_result %f %l }})

    end

  end
end
}}
