
# Module `Eliom.Cscache`

```ocaml
type ('a, 'b) t
```
The type of association tables (implemented using Hashtbl)

```ocaml
val find : ('a, 'b) t -> ('a -> 'b Lwt.t) -> 'a -> 'b Lwt.t
```
`find cache get_from_db key` returns the value associated to `key` in cache. If not present, it calls `get_from_db` to fetch the data from the database. `get_from_db` must be implemented both server and client sides. Several simultaneous call to `find` will result in a single call to `get_from_db`. Exceptions are not cached.

```ocaml
val do_cache : ('a, 'b) t -> 'a -> 'b -> unit
```
`do_cache cache key value` adds the association from `key` to `value` in `cache`, or replaces it if not already present. Called from client side, it affects only client side cache. Called from server side, it will have effect both on the server cache (scope: request) and the client side cache.

```ocaml
val local_find : ('a, 'b) t -> 'a -> 'b Lwt.t
```
Find a piece of data in local cache, without trying to fetch it from server. Raises `Not_found` instead. If the value is currently being retrieved, it waits for it to be ready before returning.

```ocaml
exception Not_ready
```
```ocaml
val find_if_ready : ('a, 'b) t -> 'a -> 'b
```
Find a piece of data in local cache, without trying to fetch it from server. Raises `Not_found` instead. If the value is currently being retrieved, it raises `Not_ready`.

```ocaml
val load : ('a, 'b) t -> ('a -> 'b Lwt.t) -> 'a -> 'b Lwt.t
```
Load (or reload) in cache the piece of data from server
