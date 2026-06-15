
# Module `Page_status.Events`

convenience functions for retrieving a react event for the current page that is triggered whenever it reaches the respective status

```ocaml
val active : unit -> unit React.E.t
```
```ocaml
val cached : unit -> unit React.E.t
```
```ocaml
val dead : unit -> unit React.E.t
```
```ocaml
val inactive : unit -> unit React.E.t
```
`inactive` occurs when the `Active` state is left (`Cached` or `Dead`)
