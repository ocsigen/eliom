# Module `Eliom_lib.Let_syntax`

This module provides support for [ppx\_let](https://github.com/janestreet/ppx_let).

since 4\.2.0
```ocaml
val return : 'a -> 'a Lwt.t
```
See [`Lwt.return`](./../../lwt/lwt/Lwt.md#val-return).

```ocaml
val map : 'a Lwt.t -> f:('a -> 'b) -> 'b Lwt.t
```
See [`Lwt.map`](./../../lwt/lwt/Lwt.md#val-map).

```ocaml
val bind : 'a Lwt.t -> f:('a -> 'b Lwt.t) -> 'b Lwt.t
```
See [`Lwt.bind`](./../../lwt/lwt/Lwt.md#val-bind).

```ocaml
val both : 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t
```
See [`Lwt.both`](./../../lwt/lwt/Lwt.md#val-both).

```ocaml
module Open_on_rhs : sig ... end
```
