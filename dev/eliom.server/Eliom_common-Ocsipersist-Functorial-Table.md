# Module `Functorial.Table`

## Parameters

```ocaml
module _ : sig ... end
```
```ocaml
module Key : COLUMN
```
```ocaml
module Value : COLUMN
```

## Signature

```ocaml
type key = Key.t
```
```ocaml
type value = Value.t
```
```ocaml
val name : string
```
```ocaml
val find : key -> value Lwt.t
```
```ocaml
val add : key -> value -> unit Lwt.t
```
```ocaml
val replace_if_exists : key -> value -> unit Lwt.t
```
```ocaml
val remove : key -> unit Lwt.t
```
```ocaml
val modify_opt : key -> (value option -> value option) -> unit Lwt.t
```
```ocaml
val length : unit -> int Lwt.t
```
```ocaml
val iter : 
  ?count:int64 ->
  ?gt:key ->
  ?geq:key ->
  ?lt:key ->
  ?leq:key ->
  (key -> value -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val fold : 
  ?count:int64 ->
  ?gt:key ->
  ?geq:key ->
  ?lt:key ->
  ?leq:key ->
  (key -> value -> 'a -> 'a Lwt.t) ->
  'a ->
  'a Lwt.t
```
```ocaml
val iter_block : 
  ?count:int64 ->
  ?gt:key ->
  ?geq:key ->
  ?lt:key ->
  ?leq:key ->
  (key -> value -> unit) ->
  unit Lwt.t
```
```ocaml
val iter_batch : 
  ?count:int64 ->
  ?gt:key ->
  ?geq:key ->
  ?lt:key ->
  ?leq:key ->
  ((key * value) list -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
module Variable : sig ... end
```
