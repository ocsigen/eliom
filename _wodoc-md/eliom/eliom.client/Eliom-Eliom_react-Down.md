
# Module `Eliom_react.Down`

```ocaml
type 'a t = 'a React.E.t
```
```ocaml
val set_handle_react_exn_function : (?exn:exn -> unit -> unit Lwt.t) -> unit
```
Makes possible to customize the function called when comet fails in Eliom\_react, for example because a channel is full or closed. It is called for each exception.
