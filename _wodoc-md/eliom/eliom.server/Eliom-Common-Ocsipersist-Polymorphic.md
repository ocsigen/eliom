
# Module `Ocsipersist.Polymorphic`

Polymorphic frontent. Relies on [`Marshal`](./../../ocaml-compiler/stdlib/Stdlib-Marshal.md) for (de)serialisation, which means that data will be stored in the backend in a fashion that is not necessarily easily readable by non-OCaml-based life forms. If this is an issue for you, you can rely on the functorial frontend instead.

```ocaml
type 'value table
```
Type of persistent table

```ocaml
val table_name : 'value table -> string Lwt.t
```
returns the name of the table

```ocaml
val open_table : string -> 'value table Lwt.t
```
Open a table (and create it if it does not exist)

```ocaml
val find : 'value table -> string -> 'value Lwt.t
```
`find table key` gives the value associated to `key`. Fails with `Not_found` if not found.

```ocaml
val add : 'value table -> string -> 'value -> unit Lwt.t
```
`add table key value` associates `value` to `key`. If the database already contains data associated with `key`, that data is discarded and silently replaced by the new data.

```ocaml
val replace_if_exists : 'value table -> string -> 'value -> unit Lwt.t
```
`replace_if_exists table key value` associates `value` to `key` only if `key` is already bound. If the database does not contain any data associated with `key`, fails with `Not_found`.

```ocaml
val remove : 'value table -> string -> unit Lwt.t
```
`remove table key` removes the entry in the table if it exists

```ocaml
val length : 'value table -> int Lwt.t
```
Size of a table.

```ocaml
val iter : (string -> 'a -> unit Lwt.t) -> 'a table -> unit Lwt.t
```
Important warning: this iterator may not iter on all data of the table if another thread is modifying it in the same time. Nonetheless, it should not miss more than a very few data from time to time, except if the table is very old (at least 9 223 372 036 854 775 807 insertions).

```ocaml
val fold : (string -> 'a -> 'b -> 'b Lwt.t) -> 'a table -> 'b -> 'b Lwt.t
```
Important warning: this iterator may not iter on all data of the table if another thread is modifying it in the same time. Nonetheless, it should not miss more than a very few data from time to time, except if the table is very old (at least 9 223 372 036 854 775 807 insertions).

```ocaml
val iter_step : (string -> 'a -> unit Lwt.t) -> 'a table -> unit Lwt.t
```
deprecated Use iter instead.
```ocaml
val fold_step : (string -> 'a -> 'b -> 'b Lwt.t) -> 'a table -> 'b -> 'b Lwt.t
```
deprecated Use fold instead.
```ocaml
val iter_block : (string -> 'a -> unit) -> 'a table -> unit Lwt.t
```
MAJOR WARNING: Unlike iter\_step, this iterator won't miss any entry and will run in one shot. It is therefore more efficient, BUT: it will lock the WHOLE database during its execution, thus preventing ANYBODY from accessing it (including the function f which is iterated). As a consequence: you MUST NOT use any function from ocsipersist in f, otherwise you would lock yourself and everybody else\! Be VERY cautious.
