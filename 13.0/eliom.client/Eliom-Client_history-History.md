# Module `Client_history.History`

```ocaml
val find_by_state_index : int -> page option
```
```ocaml
val replace : page -> unit
```
Replaces the page with the same state index.

```ocaml
val past : unit -> string list
```
```ocaml
val future : unit -> string list
```
```ocaml
val max_num_doms : int option ref
```
The maximum distance from the active page of the pages whose DOM is kept in the cache.

```ocaml
val garbage_collect_doms : unit -> unit
```
Removes the DOMs too far from the active page from the cache.
