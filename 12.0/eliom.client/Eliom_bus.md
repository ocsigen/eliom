
# Module `Eliom_bus`

Broadcasting facilities between clients and server.

See the Eliom manual for a detailed introduction to the concept of [client server communication](./../clientserver-communication.md).

```ocaml
type ('a, 'b) t
```
```ocaml
val stream : ('a, 'b) t -> 'b Lwt_stream.t
```
`stream b` returns the stream of data sent to bus `b`. A new stream is created each time this function is called. Some messages from the bus can be lost if they were sent before the call to `stream`. If you need to receive every message, use original stream instead.

```ocaml
val original_stream : ('a, 'b) t -> 'b Lwt_stream.t
```
`stream b` returns the stream of data sent to bus `b`. A new stream is created each time this function is called. Every messages sent to the bus after the generation of the page are received. This function can be called only in the onload event handler, if called outside, it will raise a Failure.

```ocaml
val write : ('a, 'b) t -> 'a -> unit Lwt.t
```
`write b v` send `v` to the bus `b`. Every participant of the bus will receive `v`, including the sender.

```ocaml
val close : ('a, 'b) t -> unit
```
after `close b`, `stream b` stops receiving new messages from the bus, but it is still possible to write to the bus. It is also possible to close the bus by canceling a thread reading on the stream.

```ocaml
val set_queue_size : ('a, 'b) t -> int -> unit
```
To reduce traffic from the client busses try to send messages by group. `set_queue_size bus size` set the maximum number of messages that are accumulated before forcing a flush. default is 20

```ocaml
val set_time_before_flush : ('a, 'b) t -> float -> unit
```
`set_time_before_flush bus time` set the maximum time to wait for a new message to enqueue before flushing. Set this to a small value to make your app more responsive, but remember that it will consume more bandwidth. default is 0\.05 second.
