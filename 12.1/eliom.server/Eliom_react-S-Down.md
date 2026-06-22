
# Module `S.Down`

```ocaml
type 'a t
```
The abstract type of down signals.

```ocaml
val of_react : 
  ?scope:[< Eliom_comet.Channel.comet_scope ] ->
  ?throttling:float ->
  ?name:string ->
  'a React.S.t ->
  'a t
```