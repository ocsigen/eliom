
# Module `Ocsipersist.Store`

The variable store allows for the persistent storage of individual variables. Relies on [`Stdlib.Marshal`](./../../ocaml-compiler/stdlib/Stdlib-Marshal.md) for (de)serialisation, which entails the same limitations as for the [`Polymorphic`](./Eliom-Common-Ocsipersist-Polymorphic.md) frontend. If this is an issue you can rely on [`Functorial`](./Eliom-Common-Ocsipersist-Functorial.md) frontend instead (see [`TABLE.Variable`](./../../ocsipersist-lib/ocsipersist-lib/Ocsipersist_lib-Sigs-module-type-TABLE-Variable.md)).

```ocaml
type 'a t
```
Type of persistent data

```ocaml
type store
```
Data are divided into stores. Create one store for your project, where you will save all your data.

```ocaml
val open_store : string -> store Lwt.t
```
Open a store (and create it if it does not exist)

```ocaml
val make_persistent : store:store -> name:string -> default:'a -> 'a t Lwt.t
```
`make_persistent store name default` find a persistent value named `name` in store `store` from database, or create it with the default value `default` if it does not exist.

```ocaml
val make_persistent_lazy : 
  store:store ->
  name:string ->
  default:(unit -> 'a) ->
  'a t Lwt.t
```
Same as make\_persistent but the default value is evaluated only if needed

```ocaml
val make_persistent_lazy_lwt : 
  store:store ->
  name:string ->
  default:(unit -> 'a Lwt.t) ->
  'a t Lwt.t
```
Lwt version of make\_persistent\_lazy.

```ocaml
val get : 'a t -> 'a Lwt.t
```
`get pv` gives the value of `pv`

```ocaml
val set : 'a t -> 'a -> unit Lwt.t
```
`set pv value` sets a persistent value `pv` to `value`
