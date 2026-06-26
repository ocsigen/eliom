
# Module `Eliom_react.Down`

Event from server to client.

A "Down event" (AKA down-going event) is an event which occurrences are transmitted asynchronously to the client. Even if they are named "events", it might be better to consider them as asynchronous server-to-client edges in the react events dependency graph.

To use this, call function `of_react` on server side, and just use the returned value as a react event on client side. Example: `let e = of_react ... in ... {{ ... React.E.map f %e; ... }}`

```ocaml
type 'a t
```
The abstract type of down events.

```ocaml
val of_react : 
  ?scope:[< Comet.Channel.comet_scope ] ->
  ?throttling:float ->
  ?name:string ->
  ?size:int ->
  'a React.E.t ->
  'a t
```
`of_react ?scope ?throttling ?name e` create an asynchronous edge originating from `e`. The parameters are:

- `throttling` for the limit to event propagation rate (minimum time, in second, between two consecutive events \- other events are lost),
- `name` for named edges,
- `size` for the size of the server side buffer.
- `scope` tell which kind of channel this rely on (See `Comet.create`).