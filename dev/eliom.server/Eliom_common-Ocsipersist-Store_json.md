# Module `Ocsipersist.Store_json`

Type-safe variable store using [`Deriving_Json`](./../../js_of_ocaml/js_of_ocaml.deriving/Deriving_Json.md) for serialisation. Unlike [`Store`](./Eliom_common-Ocsipersist-Store.md), this does not rely on [`Stdlib.Marshal`](./../../ocaml-compiler/stdlib/Stdlib-Marshal.md) and is safe across OCaml versions. Requires types annotated with `[@@deriving json]` (from `js_of_ocaml-ppx_deriving_json`).

Note: The dependency on `js_of_ocaml` is only for the [`Deriving_Json`](./../../js_of_ocaml/js_of_ocaml.deriving/Deriving_Json.md) runtime library, which provides type-safe JSON serialisation. This is the same serialisation mechanism used by Eliom for client-server communication. No JavaScript compilation is involved.

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
val make_persistent : 
  store:store ->
  name:string ->
  json:'a Deriving_Json.t ->
  default:'a ->
  'a t Lwt.t
```
`make_persistent ~store ~name ~json ~default` find a persistent value named `name` in store `store` from database, or create it with the default value `default` if it does not exist. Uses [`Deriving_Json`](./../../js_of_ocaml/js_of_ocaml.deriving/Deriving_Json.md) for type-safe serialisation.

```ocaml
val make_persistent_lazy : 
  store:store ->
  name:string ->
  json:'a Deriving_Json.t ->
  default:(unit -> 'a) ->
  'a t Lwt.t
```
Same as make\_persistent but the default value is evaluated only if needed

```ocaml
val make_persistent_lazy_lwt : 
  store:store ->
  name:string ->
  json:'a Deriving_Json.t ->
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
