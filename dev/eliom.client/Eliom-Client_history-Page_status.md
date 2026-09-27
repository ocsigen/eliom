# Module `Client_history.Page_status`

See [`Client.Page_status`](./Eliom-Client-Page_status.md).

```ocaml
type t = 
  | Generating
  | Active
  | Cached
  | Dead
```
```ocaml
val signal : unit -> t React.S.t
```
```ocaml
module Events : sig ... end
```
```ocaml
val onactive : 
  ?now:bool ->
  ?once:bool ->
  ?stop:unit React.E.t ->
  (unit -> unit) ->
  unit
```
```ocaml
val oncached : ?once:bool -> ?stop:unit React.E.t -> (unit -> unit) -> unit
```
```ocaml
val ondead : ?stop:unit React.E.t -> (unit -> unit) -> unit
```
```ocaml
val oninactive : ?once:bool -> ?stop:unit React.E.t -> (unit -> unit) -> unit
```
```ocaml
val while_active : 
  ?now:bool ->
  ?stop:unit React.E.t ->
  (unit -> unit Lwt.t) ->
  unit
```
