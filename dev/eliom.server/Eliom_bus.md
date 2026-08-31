# Module `Eliom_bus`

Broadcasting facilities between clients and server

**Please read the [chapter on communication](./../clientserver-communication.md) of Eliom's manual before this page to learn how client and server parts communicate.**

```ocaml
type ('a, 'b) t
```
The type of bus's carrying values of type `'a`. Bus's are values that can be easily shared among clients. Each of these clients along with the server can send a value on the bus. Values can be received by each of the participants as a stream. Note that no effort is put to order message receptions on the different participants.

```ocaml
val create : 
  ?scope:[< Eliom_comet.Channel.comet_scope ] ->
  ?name:string ->
  ?size:int ->
  'a Deriving_Json.t ->
  ('a, 'a) t
```
`create ?scope ?name ?size ?filter t` creates a fresh bus. The `scope` parameter is used to chose the kind of channel on which the bus rely (See `Eliom_comet.create` for more information). The `?name` argument allow one to make bus's persistent over server restart. The `size` argument behaves like the one on [`Eliom_comet.Channel.create`](./Eliom_comet-Channel.md#val-create). If `?filter` argument is present, data is filtered through this function before entering the bus. Use this for example if you want to add some information, like IP address, or user id.

```ocaml
val create_filtered : 
  ?scope:[< Eliom_comet.Channel.comet_scope ] ->
  ?name:string ->
  ?size:int ->
  filter:('a -> 'b Lwt.t) ->
  'a Deriving_Json.t ->
  ('a, 'b) t
```
Same as `create`, but data is filtered through `filter` function before entering the bus. Use this for example if you want to add some information, like IP address, or user id.

```ocaml
val stream : ('a, 'b) t -> 'b Lwt_stream.t
```
`stream b` returns the stream of data sent to bus `b`. Notice you should not use that function multiple times on the same bus, it will return the same stream. If you want to receive multiple times the same data, you should copy the stream with `Lwt_stream.clone`

```ocaml
val write : ('a, 'b) t -> 'a -> unit Lwt.t
```
`write b x` sends the value `x` on the bus `b`. Every participant, including the server, will receive `x`.
