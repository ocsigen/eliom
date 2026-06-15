
# Module `Eliom_react.Up`

Event from client to server.

Up events are quite different from Down events. Because of the asymmetrical nature of web programming and because of the reactive model used, an Up event must be created on the server and wrapped into a callback (or something the client can build a callback with).

Example of use: `let e_up = Eliom_react.Up.create (Eliom_parameter.ocaml "a" [%json: string]) in ... {{ ignore ( %e_up "A") }} ... `

```ocaml
type 'a t
```
The type of events that, while being "on the server", are triggered by clients. On the server such an event is /primitive/ (hence the `create` function) whereas it is /effect-full/ on the client.

```ocaml
val to_react : 'a t -> 'a React.E.t
```
`to_react e` injects the up events `e` into react events so that it can be manipulated as a standard event.

```ocaml
val create : 
  ?scope:Eliom_common.scope ->
  ?name:string ->
  ('a,
    [ `WithoutSuffix ],
    [ `One of 'a Eliom_parameter.ocaml ] Eliom_parameter.param_name)
    Eliom_parameter.params_type ->
  'a t
```
`create param` creates an Up event. If `~name` is present, the coservice used to transmit the event will always have the same name, even if the server is restarted. `~scope` describes the visibility of the event. By default, it is ``Site` if it is called during initialisation, ``Client_process` otherwise.
