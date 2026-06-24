
# Module `Ocsipersist.Ref_json`

Type-safe persistent references using [`Deriving_Json`](./../../js_of_ocaml/js_of_ocaml.deriving/Deriving_Json.md) for serialisation. Unlike [`Ref`](./Eliom_common-Ocsipersist-Ref.md), this does not rely on [`Stdlib.Marshal`](./../../ocaml-compiler/stdlib/Stdlib-Marshal.md) and is safe across OCaml versions. Requires types annotated with `[@@deriving json]` (from `js_of_ocaml-ppx_deriving_json`).

Type-safe persistent references using [`Deriving_Json`](./../../js_of_ocaml/js_of_ocaml.deriving/Deriving_Json.md) for serialisation.

```ocaml
type 'a t
```
The type of (persistent or not) references

```ocaml
val ref : ?persistent:string -> 'a Deriving_Json.t -> 'a -> 'a t
```
`ref ?persistent json default` creates a reference. If optional parameter `?persistent` is absent, the reference will not be persistent (implemented using OCaml references). Otherwise, the value of `persistent` will be used as key for the value in the persistent reference table. If the reference already exists, the current value is kept.

```ocaml
val get : 'a t -> 'a Lwt.t
```
Get the value of a reference

```ocaml
val set : 'a t -> 'a -> unit Lwt.t
```
Set the value of a reference
