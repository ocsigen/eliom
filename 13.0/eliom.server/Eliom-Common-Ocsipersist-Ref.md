# Module `Ocsipersist.Ref`

Simple interface for persistent references. Relies on [`Stdlib.Marshal`](https://ocaml.org/p/ocaml-compiler/5.4.0/doc/stdlib/Stdlib/Marshal/index.html) for (de)serialisation, which entails the same limitations as for the [`Polymorphic`](./Eliom-Common-Ocsipersist-Polymorphic.md) frontend. If this is an issue you can rely on [`Functorial`](./Eliom-Common-Ocsipersist-Functorial.md) frontend instead (see [`TABLE.Variable`](https://ocaml.org/p/ocsipersist-lib/3.1.2/doc/ocsipersist-lib/Ocsipersist_lib/Sigs/module-type-TABLE/Variable/index.html)).

Persistent references for OCaml

```ocaml
type 'a t
```
The type of (persistent or not) references

```ocaml
val ref : ?persistent:string -> 'a -> 'a t
```
`ref ?persistent default` creates a reference. If optional parameter `?persistent` is absent, the reference will not be persistent (implemented using OCaml references). Otherwise, the value of `persistent` will be used as key for the value in the persistent reference table. If the reference already exists, the current value is kept. Be careful to change this name every time you change the type of the value.

```ocaml
val get : 'a t -> 'a Lwt.t
```
Get the value of a reference

```ocaml
val set : 'a t -> 'a -> unit Lwt.t
```
Set the value of a reference
