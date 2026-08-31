# Module `Eliom_comet`

Handle unsolicited server to client communications.

See the Eliom manual for a detailed introduction to the concept of [client server communication](./../clientserver-communication.md).

When the page is not active the client stops making comet requests to the server, implying that the client can't be notified by the server anymore. The activity status is changed when the page is focused or unfocused.

To stop receiving inputs from a channel, use Lwt.cancel on a thread waiting for data. For instance, if you iterate with ` let t = Lwt_stream.iter f %channel ` calling `Lwt.cancel t` will close the channel.

```ocaml
exception Channel_full
```
`Channel_full` is raised when trying to read on a channel marked full by the server. It is not possible to read anything else from a full channel.

```ocaml
exception Channel_closed
```
`Channel_closed` is raised when reading on a channel and the server side of the application closed channel ( the server was restarted, a session was closed, or a stateless channel was garbage collected).

```ocaml
val is_active : unit -> [ `Active | `Idle | `Inactive ]
```
`is_active ()` returns the current activity state

```ocaml
val activate : unit -> unit
```
if the client is inactive `activate ()` launch a new xhr connection to start receiving server messages

```ocaml
val set_handle_exn_function : (?exn:exn -> unit -> unit Lwt.t) -> unit
```
Makes possible to customize the function called when comet fails for unknown reason. The usual practice is to warn the user and ask to reload the page. This function is not called when a channel is full or closed. It is called only once, for the first exception.

```ocaml
module Configuration : sig ... end
```
Change the reactivity of channels. Multiples configurations ( of type `t` ) can be created. The resulting behaviour is the minimal ( in the meaning of maximal reactivity ) between all configurations

```ocaml
module Channel : sig ... end
```
