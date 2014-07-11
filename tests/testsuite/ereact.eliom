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
}}


{client{
module React = struct
  module S = struct
    include React.S
    let create x =
      let s,u = create x in
      s,(fun x -> u x)
  end
end

module FakeReact = React
module FakeReactiveData = ReactiveData
}}
{server{
module FakeReact = struct
  module S = struct
    type 'a t = 'a
    let create x =
      (x, fun _ ->
          failwith "Fact react values cannot be changed on server side")
    let value x = x
  end
end
module FakeReactiveData = struct
  module RList : sig
    type 'a t = 'a list
    type 'a handle
    val make : 'a list -> 'a t * 'a handle
  end = struct
    type 'a t = 'a list
    type 'a handle = unit
    let make l = l, ()
  end
end
}}
{server{
module SharedReact = struct
  module S = struct
    type 'a t = 'a FakeReact.S.t shared_value
    let value (x : 'a t) = x

    let create : 'a . 'a -> ('a FakeReact.S.t shared_value) * (('a -> unit) shared_value) = fun x ->
      let x' : unit = Obj.magic x in
      let cv = {unit FakeReact.S.t * (unit -> unit){ let s,set = FakeReact.S.create %x' in (s,(fun t -> set t)) }} in
      let csig : unit FakeReact.S.t Eliom_lib.client_value = {{ fst %cv }} in
      let cup = {{ snd %cv }} in
      let sv = FakeReact.S.create x' in
      let si = Eliom_lib.create_shared_value (fst sv) csig  in
      let up = Eliom_lib.create_shared_value (snd sv) cup in
      (Obj.magic si, Obj.magic up)

  end
end
(* module SharedReactiveData = struct *)
(*         module RList = struct *)
(*         let make l = *)
(*           let l' : unit list = Obj.magic l in *)
(*           let cv : (unit ReactiveData.RList.t * unit ReactiveData.RList.handle) client_value = *)
(*             {unit ReactiveData.RList.t * unit ReactiveData.RList.handle{ ReactiveData.RList.make %l' }} in *)
(*             let sv = FakeReactiveData.RList.make l in *)
(*             (Eliom_lib.create_shared_value (fst sv) (Obj.magic {unit ReactiveData.RList.t{ fst %cv }})), *)
(*             (Eliom_lib.create_shared_value (snd sv) (Obj.magic {unit ReactiveData.RList.handle{ snd %cv }})) *)
(*         end *)
(*       end *)
}}


{server{
      module React = SharedReact
      (* module ReactiveData = SharedReactiveData *)

      module R = struct
        let pcdata (s : string React.S.t) = Eliom_content.Html5.C.node
          ~init:(Eliom_content.Html5.D.(span [pcdata (Shared.local s)]))
          {{ Eliom_content.Html5.R.pcdata %s }}
      end
}}
