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

[%%shared
open Lwt.Syntax

(* put this in Eio_react? Find a better name? *)
let to_signal ~init ?eq (th : 'a React.S.t Lwt.t) : 'a React.S.t =
  let s, set = React.S.create ?eq init in
  Lwt.async (fun () ->
    let* ss = th in
    let effectful_signal = React.S.map (fun v -> set v) ss in
    ignore (React.S.retain s (fun () -> ignore effectful_signal));
    Lwt.return_unit);
  s]

[%%client
module Value = struct
  type 'a t = 'a

  let create _ x = x
  let client x = x
  let local x = x
end]

[%%server
module Value = struct
  type +'a t =
    { sh_server : 'a
    ; sh_client : 'a Eliom_client_value.t
    ; sh_mark : 'a t Eliom_wrap.wrapper }
  [@@warning "-69"]

  let internal_wrap {sh_client; _} = sh_client

  let shared_value_mark () : 'a t Eliom_wrap.wrapper =
    Eliom_wrap.create_wrapper internal_wrap

  let create sh_server sh_client =
    {sh_server; sh_client; sh_mark = shared_value_mark ()}

  let client {sh_client; _} = sh_client
  let local {sh_server; _} = sh_server
end]

[%%client
module React = struct
  type step = React.step

  module S = struct
    include React.S

    let create ?eq ?default ?(reset_default = false) v =
      match default with
      | Some (Some ((_, set) as s)) ->
          if reset_default then set ?step:None v;
          s
      | _ -> create ?eq v

    module Infix = struct
      let ( >|= ) a f = map f a
      let ( =|< ) f a = map f a
    end

    module Lwt = struct
      let map_s = Eio_react.S.map_s

      let map_s_init ~init ?eq f s =
        let th = map_s ?eq f s in
        to_signal ~init ?eq th

      let l2_s = Eio_react.S.l2_s

      let l2_s_init ~init ?eq f s1 s2 =
        let th = l2_s ?eq f s1 s2 in
        to_signal ~init ?eq th

      let l3_s = Eio_react.S.l3_s

      let l3_s_init ~init ?eq f s1 s2 s3 =
        let th = l3_s ?eq f s1 s2 s3 in
        to_signal ~init ?eq th

      let l4_s = Eio_react.S.l4_s

      let l4_s_init ~init ?eq f s1 s2 s3 s4 =
        let th = l4_s ?eq f s1 s2 s3 s4 in
        to_signal ~init ?eq th

      let l5_s = Eio_react.S.l5_s

      let l5_s_init ~init ?eq f s1 s2 s3 s4 s5 =
        let th = l5_s ?eq f s1 s2 s3 s4 s5 in
        to_signal ~init ?eq th

      let l6_s = Eio_react.S.l6_s

      let l6_s_init ~init ?eq f s1 s2 s3 s4 s5 s6 =
        let th = l6_s ?eq f s1 s2 s3 s4 s5 s6 in
        to_signal ~init ?eq th

      let merge_s = Eio_react.S.merge_s

      let merge_s_init ~init ?eq f a l =
        let th = merge_s ?eq f a l in
        to_signal ~init ?eq th
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
        | I (i, x) ->
            let* p = f x in
            Lwt.return (I (i, p))
        | R i -> Lwt.return (R i)
        | X (i, j) -> Lwt.return (X (i, j))
        | U (i, x) ->
            let* p = f x in
            Lwt.return (U (i, p))

      let map_patch_p_lwt f = Lwt_list.map_p (map_patch_p_lwt f)

      let map_msg_p_lwt f = function
        | Set l ->
            let* p = map_data_p_lwt f l in
            Lwt.return (Set p)
        | Patch p ->
            let* p = map_patch_p_lwt f p in
            Lwt.return (Patch p)

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
               let* new_msg = map_msg_p_lwt f msg in
               let* _, rhandle = r_th in
               let* () = fst waiter1 in
               (match new_msg with
               | ReactiveData.RList.Set s -> ReactiveData.RList.set rhandle s
               | ReactiveData.RList.Patch p ->
                   ReactiveData.RList.patch rhandle p);
               Lwt.wakeup (snd new_waiter) ();
               Lwt.return_unit))
          event

      (** Same as map_p but we do not compute the initial list.
          Instead, we give the initial list as parameter.  To be used
          when the initial list has been computed on server side.  *)
      let map_p_init ~init (f : 'a -> 'b Lwt.t) (l : 'a t) : 'b t =
        let ((rr, _) as r) = ReactiveData.RList.create init in
        let effectul_event = map_p_aux (Lwt.return r) f l in
        (* We keep a reference to the effectul_event in the resulting
           reactive list in order that the effectul_event is garbage
           collected only if the resulting list is garbage
           collected. *)
        ignore
          (React.E.retain (ReactiveData.RList.event rr) (fun () ->
             ignore effectul_event));
        rr

      (** [map_p f l] is the equivalent of [ReactiveData.Rlist.map]
          but with a function that may yield.  If a patch arrives when
          the previous one has not finished to be computed, we launch
          the computation of [f] in parallel, but we wait for the
          previous one to be applied before applying it.  *)
      let map_p (f : 'a -> 'b Lwt.t) (l : 'a t) : 'b t Lwt.t =
        (* First we build the initial value of the result list *)
        let r_th =
          let* r = Lwt_list.map_p f (ReactiveData.RList.value l) in
          Lwt.return (ReactiveData.RList.create r)
        in
        let effectul_event = map_p_aux r_th f l in
        let* rr, _ = r_th in
        (* We keep a reference to the effectul_event in the resulting
           reactive list in order that the effectul_event is garbage
           collected only if the resulting list is garbage
           collected. *)
        ignore
          (React.E.retain (ReactiveData.RList.event rr) (fun () ->
             ignore effectul_event));
        Lwt.return rr
    end

    let create ?default ?(reset_default = false) v =
      match default with
      | Some (Some ((_, handle) as s)) ->
          if reset_default then ReactiveData.RList.set handle v;
          s
      | _ -> ReactiveData.RList.create v

    let acc_e ?init e =
      let l, h = match init with Some p -> p | None -> create [] in
      let _ =
        let f x = ReactiveData.RList.cons x h in
        React.E.map f e
      in
      l
  end
end

module FakeReact = React
module FakeReactiveData = ReactiveData]

[%%server
module FakeReact = struct
  module S : sig
    type 'a t

    val create : ?synced:bool -> 'a -> 'a t * (?step:React.step -> 'a -> unit)
    val value : 'a t -> 'a
    val const : ?synced:bool -> 'a -> 'a t
    val synced : 'a t -> bool
    val map : ('a -> 'b) -> 'a t -> 'b t
    val fmap : ('a -> 'b option) -> 'b -> 'a t -> 'b t
    val merge : ('a -> 'b -> 'a) -> 'a -> 'b t list -> 'a t
    val l2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val l3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

    val l4 :
       ('a -> 'b -> 'c -> 'd -> 'e)
      -> 'a t
      -> 'b t
      -> 'c t
      -> 'd t
      -> 'e t

    val l5 :
       ('a -> 'b -> 'c -> 'd -> 'e -> 'f)
      -> 'a t
      -> 'b t
      -> 'c t
      -> 'd t
      -> 'e t
      -> 'f t

    val l6 :
       ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
      -> 'a t
      -> 'b t
      -> 'c t
      -> 'd t
      -> 'e t
      -> 'f t
      -> 'g t
  end = struct
    type 'a t = 'a * bool

    let create ?(synced = false) x =
      ( (x, synced)
      , fun ?step:_ _ ->
          failwith "Fact react values cannot be changed on server side" )

    let value (x, _) = x
    let const ?(synced = false) x = x, synced
    let synced (_, b) = b
    let map (f : 'a -> 'b) ((x, b) : 'a t) : 'b t = f x, b
    let fmap f i (s, b) = (match f s with Some v -> v | None -> i), b

    let merge f acc l =
      let f (acc, acc_b) (x, b) = f acc x, acc_b && b in
      List.fold_left f (acc, true) l

    let l2 f (x1, b1) (x2, b2) = f x1 x2, b1 && b2
    let l3 f (x1, b1) (x2, b2) (x3, b3) = f x1 x2 x3, b1 && b2 && b3

    let l4 f (x1, b1) (x2, b2) (x3, b3) (x4, b4) =
      f x1 x2 x3 x4, b1 && b2 && b3 && b4

    let l5 f (x1, b1) (x2, b2) (x3, b3) (x4, b4) (x5, b5) =
      f x1 x2 x3 x4 x5, b1 && b2 && b3 && b4 && b5

    let l6 f (x1, b1) (x2, b2) (x3, b3) (x4, b4) (x5, b5) (x6, b6) =
      f x1 x2 x3 x4 x5 x6, b1 && b2 && b3 && b4 && b5 && b6
  end
end

module FakeReactiveData = struct
  module RList : sig
    type 'a t
    type 'a handle

    val create : ?synced:bool -> 'a list -> 'a t * 'a handle
    val concat : 'a t -> 'a t -> 'a t
    val value : 'a t -> 'a list
    val synced : 'a t -> bool
    val signal : ?eq:('a -> 'a -> bool) -> 'a t -> 'a list FakeReact.S.t
    val singleton_s : 'a FakeReact.S.t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val from_signal : ?eq:('a -> 'a -> bool) -> 'a list FakeReact.S.t -> 'a t
  end = struct
    type 'a t = 'a list * bool
    type 'a handle = unit

    let create ?(synced = false) l = (l, synced), ()
    let concat (l1, b1) (l2, b2) = List.append l1 l2, b1 && b2
    let singleton_s s = [FakeReact.S.value s], FakeReact.S.synced s
    let value (l, _) = l
    let synced (_, b) = b
    let signal ?eq:_ (l, synced) = fst (FakeReact.S.create ~synced l)
    let map f (l, b) = List.map f l, b
    let from_signal ?eq:_ s = FakeReact.S.(value s, synced s)
  end
end]

[%%server
module React = struct
  module S = struct
    type 'a t = 'a FakeReact.S.t Value.t

    let value (x : 'a FakeReact.S.t Value.t) =
      Value.create
        (FakeReact.S.value (Value.local x))
        [%client.unsafe (FakeReact.S.value (Value.local ~%x) : 'a)]

    (*VVV What is the good default value for reset_default?  Setting
      default to true may be difficult to understand.  I prefer
      false.  *)
    let create ?default ?(reset_default = false) ?(eq : _ Value.t option) x =
      let cv, synced =
        match default with
        | None -> [%client.unsafe FakeReact.S.create ?eq:~%eq ~%x], true
        | Some v ->
            ( [%client.unsafe
                ((match (~%v : (_ * (?step:_ -> _ -> _)) option) with
                 | Some ((_, set) as s) ->
                     (* The reactive data is already on client side.  But
                  the value sent by server is more recent.  I update
                  the signal. *)
                     if ~%reset_default then set ?step:None ~%x;
                     s
                 | None -> FakeReact.S.create ?eq:~%eq ~%x)
                 (*VVV Make possible to disable this?  Warning: removing
                 this or changing the default will break some existing
                 code relying on this!  Do not change the default
                 without wide announcement. *)
                 : 'a FakeReact.S.t * (?step:React.step -> 'a -> unit))]
            , reset_default )
      in
      let v, f = FakeReact.S.create ~synced x in
      let si = Value.create v [%client.unsafe (fst ~%cv : 'a FakeReact.S.t)]
      and up =
        Value.create f
          [%client.unsafe (snd ~%cv : ?step:React.step -> 'a -> unit)]
      in
      si, up

    let map ?eq (f : ('a -> 'b) Value.t) (s : 'a FakeReact.S.t Value.t) : 'b t =
      Value.create
        (FakeReact.S.map (Value.local f) (Value.local s))
        [%client.unsafe (FakeReact.S.map ?eq:~%eq ~%f ~%s : 'b FakeReact.S.t)]

    let fmap
          ?(eq : ('b -> 'b -> bool) Value.t option)
          (f : ('a -> 'b option) Value.t)
          (i : 'b Value.t)
          (s : 'a FakeReact.S.t Value.t) : 'b t
      =
      Value.create
        (FakeReact.S.fmap (Value.local f) (Value.local i) (Value.local s))
        [%client.unsafe
          (FakeReact.S.fmap ?eq:~%eq ~%f ~%i ~%s : 'b FakeReact.S.t)]

    let merge
          ?eq
          (f : ('a -> 'b -> 'a) Value.t)
          (acc : 'a)
          (l : 'b FakeReact.S.t Value.t list) : 'a t
      =
      Value.create
        (FakeReact.S.merge (Value.local f) acc (List.map Value.local l))
        [%client.unsafe
          (FakeReact.S.merge ?eq:~%eq ~%f ~%acc ~%l : 'a FakeReact.S.t)]

    let const (v : 'a) : 'a t =
      Value.create
        (FakeReact.S.const ~synced:true v)
        [%client.unsafe (React.S.const ~%v : 'a FakeReact.S.t)]

    let l2
          ?eq
          (f : ('a -> 'b -> 'c) Value.t)
          (s1 : 'a FakeReact.S.t Value.t)
          (s2 : 'b FakeReact.S.t Value.t) : 'c t
      =
      Value.create
        (FakeReact.S.l2 (Value.local f) (Value.local s1) (Value.local s2))
        [%client.unsafe (React.S.l2 ?eq:~%eq ~%f ~%s1 ~%s2 : 'd FakeReact.S.t)]

    let l3
          ?eq
          (f : ('a -> 'b -> 'c -> 'd) Value.t)
          (s1 : 'a FakeReact.S.t Value.t)
          (s2 : 'b FakeReact.S.t Value.t)
          (s3 : 'c FakeReact.S.t Value.t) : 'd t
      =
      Value.create
        (FakeReact.S.l3 (Value.local f) (Value.local s1) (Value.local s2)
           (Value.local s3))
        [%client.unsafe
          (React.S.l3 ?eq:~%eq ~%f ~%s1 ~%s2 ~%s3 : 'd FakeReact.S.t)]

    let l4
          ?eq
          (f : ('a -> 'b -> 'c -> 'd -> 'e) Value.t)
          (s1 : 'a FakeReact.S.t Value.t)
          (s2 : 'b FakeReact.S.t Value.t)
          (s3 : 'c FakeReact.S.t Value.t)
          (s4 : 'd FakeReact.S.t Value.t) : 'e t
      =
      Value.create
        (FakeReact.S.l4 (Value.local f) (Value.local s1) (Value.local s2)
           (Value.local s3) (Value.local s4))
        [%client.unsafe
          (React.S.l4 ?eq:~%eq ~%f ~%s1 ~%s2 ~%s3 ~%s4 : 'e FakeReact.S.t)]

    let l5
          ?eq
          (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) Value.t)
          (s1 : 'a FakeReact.S.t Value.t)
          (s2 : 'b FakeReact.S.t Value.t)
          (s3 : 'c FakeReact.S.t Value.t)
          (s4 : 'd FakeReact.S.t Value.t)
          (s5 : 'e FakeReact.S.t Value.t) : 'f t
      =
      Value.create
        (FakeReact.S.l5 (Value.local f) (Value.local s1) (Value.local s2)
           (Value.local s3) (Value.local s4) (Value.local s5))
        [%client.unsafe
          (React.S.l5 ?eq:~%eq ~%f ~%s1 ~%s2 ~%s3 ~%s4 ~%s5 : 'f FakeReact.S.t)]

    let l6
          ?eq
          (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) Value.t)
          (s1 : 'a FakeReact.S.t Value.t)
          (s2 : 'b FakeReact.S.t Value.t)
          (s3 : 'c FakeReact.S.t Value.t)
          (s4 : 'd FakeReact.S.t Value.t)
          (s5 : 'e FakeReact.S.t Value.t)
          (s6 : 'f FakeReact.S.t Value.t) : 'g t
      =
      Value.create
        (FakeReact.S.l6 (Value.local f) (Value.local s1) (Value.local s2)
           (Value.local s3) (Value.local s4) (Value.local s5) (Value.local s6))
        [%client.unsafe
          (React.S.l6 ?eq:~%eq ~%f ~%s1 ~%s2 ~%s3 ~%s4 ~%s5 ~%s6
           : 'g FakeReact.S.t)]

    let switch ?eq (s : 'a FakeReact.S.t Value.t FakeReact.S.t Value.t) : 'a t =
      (* TODO : setting synced to false is safe, but can we do
         better? *)
      Value.create
        (Value.local s |> FakeReact.S.value |> Value.local |> FakeReact.S.value
        |> FakeReact.S.create ~synced:false
        |> fst)
        [%client.unsafe (React.S.switch ?eq:~%eq ~%s : 'a FakeReact.S.t)]

    let synced s = Value.local s |> FakeReact.S.synced

    module Infix = struct
      let ( >|= ) a f = map f a
      let ( =|< ) f a = map f a
    end

    module Lwt = struct
      let map_s
            ?eq
            (f : ('a -> 'b Lwt.t) Value.t)
            (s : 'a FakeReact.S.t Value.t) : 'b t Lwt.t
        =
        let s' = Value.local s in
        let* server_result = (Value.local f) (FakeReact.S.value s') in
        let synced = FakeReact.S.synced s' in
        Lwt.return
          (Value.create
             (fst (FakeReact.S.create ~synced server_result))
             [%client.unsafe
               (React.S.Lwt.map_s_init ~init:~%server_result ?eq:~%eq ~%f ~%s
                : 'b FakeReact.S.t)])

      let l2_s
            ?eq
            (f : ('a -> 'b -> 'c Lwt.t) Value.t)
            (s1 : 'a FakeReact.S.t Value.t)
            (s2 : 'b FakeReact.S.t Value.t) : 'c t Lwt.t
        =
        let s1' = Value.local s1 and s2' = Value.local s2 in
        let* server_result =
          (Value.local f) (FakeReact.S.value s1') (FakeReact.S.value s2')
        in
        let synced = FakeReact.S.(synced s1' && synced s2') in
        Lwt.return
          (Value.create
             (fst (FakeReact.S.create ~synced server_result))
             [%client.unsafe
               (React.S.Lwt.l2_s_init ~init:~%server_result ?eq:~%eq ~%f ~%s1
                  ~%s2
                : 'c FakeReact.S.t)])

      let l3_s
            ?eq
            (f : ('a -> 'b -> 'c -> 'd Lwt.t) Value.t)
            (s1 : 'a FakeReact.S.t Value.t)
            (s2 : 'b FakeReact.S.t Value.t)
            (s3 : 'c FakeReact.S.t Value.t) : 'd t Lwt.t
        =
        let s1' = Value.local s1
        and s2' = Value.local s2
        and s3' = Value.local s3 in
        let* server_result =
          (Value.local f) (FakeReact.S.value s1') (FakeReact.S.value s2')
            (FakeReact.S.value s3')
        in
        let synced = FakeReact.S.(synced s1' && synced s2' && synced s3') in
        Lwt.return
          (Value.create
             (fst (FakeReact.S.create ~synced server_result))
             [%client.unsafe
               (React.S.Lwt.l3_s_init ?eq:~%eq ~init:~%server_result ~%f ~%s1
                  ~%s2 ~%s3
                : 'd FakeReact.S.t)])

      let l4_s
            ?eq
            (f : ('a -> 'b -> 'c -> 'd -> 'e Lwt.t) Value.t)
            (s1 : 'a FakeReact.S.t Value.t)
            (s2 : 'b FakeReact.S.t Value.t)
            (s3 : 'c FakeReact.S.t Value.t)
            (s4 : 'd FakeReact.S.t Value.t) : 'e t Lwt.t
        =
        let s1' = Value.local s1
        and s2' = Value.local s2
        and s3' = Value.local s3
        and s4' = Value.local s4 in
        let* server_result =
          (Value.local f) (FakeReact.S.value s1') (FakeReact.S.value s2')
            (FakeReact.S.value s3') (FakeReact.S.value s4')
        in
        let synced =
          FakeReact.S.(synced s1' && synced s2' && synced s3' && synced s4')
        in
        Lwt.return
          (Value.create
             (fst (FakeReact.S.create ~synced server_result))
             [%client.unsafe
               (React.S.Lwt.l4_s_init ?eq:~%eq ~init:~%server_result ~%f ~%s1
                  ~%s2 ~%s3 ~%s4
                : 'e FakeReact.S.t)])

      let l5_s
            ?eq
            (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f Lwt.t) Value.t)
            (s1 : 'a FakeReact.S.t Value.t)
            (s2 : 'b FakeReact.S.t Value.t)
            (s3 : 'c FakeReact.S.t Value.t)
            (s4 : 'd FakeReact.S.t Value.t)
            (s5 : 'e FakeReact.S.t Value.t) : 'f t Lwt.t
        =
        let s1' = Value.local s1
        and s2' = Value.local s2
        and s3' = Value.local s3
        and s4' = Value.local s4
        and s5' = Value.local s5 in
        let* server_result =
          (Value.local f) (FakeReact.S.value s1') (FakeReact.S.value s2')
            (FakeReact.S.value s3') (FakeReact.S.value s4')
            (FakeReact.S.value s5')
        in
        let synced =
          FakeReact.S.(
            synced s1' && synced s2' && synced s3' && synced s4' && synced s5')
        in
        Lwt.return
          (Value.create
             (fst (FakeReact.S.create ~synced server_result))
             [%client.unsafe
               (React.S.Lwt.l5_s_init ?eq:~%eq ~init:~%server_result ~%f ~%s1
                  ~%s2 ~%s3 ~%s4 ~%s5
                : 'f FakeReact.S.t)])

      let l6_s
            ?eq
            (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g Lwt.t) Value.t)
            (s1 : 'a FakeReact.S.t Value.t)
            (s2 : 'b FakeReact.S.t Value.t)
            (s3 : 'c FakeReact.S.t Value.t)
            (s4 : 'd FakeReact.S.t Value.t)
            (s5 : 'e FakeReact.S.t Value.t)
            (s6 : 'f FakeReact.S.t Value.t) : 'g t Lwt.t
        =
        let s1' = Value.local s1
        and s2' = Value.local s2
        and s3' = Value.local s3
        and s4' = Value.local s4
        and s5' = Value.local s5
        and s6' = Value.local s6 in
        let* server_result =
          (Value.local f) (FakeReact.S.value s1') (FakeReact.S.value s2')
            (FakeReact.S.value s3') (FakeReact.S.value s4')
            (FakeReact.S.value s5') (FakeReact.S.value s6')
        in
        let synced =
          FakeReact.S.(
            synced s1' && synced s2' && synced s3' && synced s4' && synced s5'
            && synced s6')
        in
        Lwt.return
          (Value.create
             (fst (FakeReact.S.create ~synced server_result))
             [%client.unsafe
               (React.S.Lwt.l6_s_init ?eq:~%eq ~init:~%server_result ~%f ~%s1
                  ~%s2 ~%s3 ~%s4 ~%s5 ~%s6
                : 'g FakeReact.S.t)])

      let merge_s
            ?eq
            (f : ('a -> 'b -> 'a Lwt.t) Value.t)
            (acc : 'a)
            (l : 'b FakeReact.S.t Value.t list) : 'a t Lwt.t
        =
        let* server_result, synced =
          let f (acc, _acc_b) v =
            let v = Value.local v and f = Value.local f in
            let* acc = f acc (FakeReact.S.value v) in
            let acc_b = FakeReact.S.synced v in
            Lwt.return (acc, acc_b)
          in
          Lwt_list.fold_left_s f (acc, true) l
        in
        Lwt.return
          (Value.create
             (fst (FakeReact.S.create ~synced server_result))
             [%client.unsafe
               (React.S.Lwt.merge_s_init ~init:~%server_result ?eq:~%eq ~%f
                  ~%acc ~%l
                : 'a FakeReact.S.t)])
    end
  end
end

module ReactiveData = struct
  module RList = struct
    type 'a t = 'a FakeReactiveData.RList.t Value.t
    type 'a handle = 'a FakeReactiveData.RList.handle Value.t

    let create ?default ?(reset_default = false) x =
      let cv, synced =
        match default with
        | None -> [%client.unsafe FakeReactiveData.RList.create ~%x], true
        | Some v ->
            ( [%client.unsafe
                ((match ~%v with
                 | Some ((_, handle) as s) ->
                     if ~%reset_default then ReactiveData.RList.set handle ~%x;
                     s
                 | None -> FakeReactiveData.RList.create ~%x)
                 : 'a FakeReactiveData.RList.t
                   * 'a FakeReactiveData.RList.handle)]
            , reset_default )
      in
      let sv = FakeReactiveData.RList.create ~synced x in
      ( Value.create (fst sv)
          [%client.unsafe (fst ~%cv : 'a FakeReactiveData.RList.t)]
      , Value.create (snd sv)
          [%client.unsafe (snd ~%cv : 'b FakeReactiveData.RList.handle)] )

    let concat a b =
      let sv = FakeReactiveData.RList.concat (Value.local a) (Value.local b)
      and cv =
        [%client.unsafe
          (FakeReactiveData.RList.concat ~%a ~%b : 'a FakeReactiveData.RList.t)]
      in
      Value.create sv cv

    let singleton_s s =
      Value.create
        (FakeReactiveData.RList.singleton_s (Value.local s))
        [%client.unsafe
          (FakeReactiveData.RList.singleton_s (Value.local ~%s)
           : 'a FakeReactiveData.RList.t)]

    let value (s : 'a FakeReactiveData.RList.t Value.t) =
      Value.create
        (FakeReactiveData.RList.value (Value.local s))
        [%client.unsafe
          (FakeReactiveData.RList.value (Value.local ~%s) : 'a list)]

    let signal ?eq (s : 'a FakeReactiveData.RList.t Value.t) =
      let sv =
        let eq =
          match eq with Some eq -> Some (Value.local eq) | None -> None
        in
        FakeReactiveData.RList.signal ?eq (Value.local s)
      and cv =
        [%client.unsafe
          (FakeReactiveData.RList.signal ?eq:~%eq (Value.local ~%s)
           : 'a list FakeReact.S.t)]
      in
      Value.create sv cv

    let map f s =
      Value.create
        (FakeReactiveData.RList.map (Value.local f) (Value.local s))
        [%client.unsafe
          (FakeReactiveData.RList.map (Value.local ~%f) (Value.local ~%s)
           : 'a FakeReactiveData.RList.t)]

    let from_signal ?eq (s : 'a list React.S.t) : 'a t =
      let sv =
        let eq =
          match eq with Some eq -> Some (Value.local eq) | None -> None
        in
        FakeReactiveData.RList.from_signal ?eq (Value.local s)
      and cv =
        [%client.unsafe
          ReactiveData.RList.from_signal ?eq:~%eq (Value.local ~%s)]
      in
      Value.create sv cv

    let acc_e ?init e =
      let l, h = match init with Some p -> p | None -> create [] in
      let _ =
        [%client.unsafe
          (let f x = ReactiveData.RList.cons x (Value.local ~%h) in
           ignore (React.E.map f ~%e)
           : unit)]
      in
      l

    let synced s = Value.local s |> FakeReactiveData.RList.synced

    module Lwt = struct
      let map_p
            (f : ('a -> 'b Lwt.t) Value.t)
            (l : 'a FakeReactiveData.RList.t Value.t) : 'b t Lwt.t
        =
        let l' = Value.local l in
        let* server_result =
          Lwt_list.map_p (Value.local f) (FakeReactiveData.RList.value l')
        in
        let synced = FakeReactiveData.RList.synced l' in
        Lwt.return
          (Value.create
             (fst (FakeReactiveData.RList.create ~synced server_result))
             [%client.unsafe
               ReactiveData.RList.Lwt.map_p_init ~init:~%server_result ~%f ~%l])
    end
  end
end]
