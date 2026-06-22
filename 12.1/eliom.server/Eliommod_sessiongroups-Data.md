
# Module `Eliommod_sessiongroups.Data`

```ocaml
type group_of_group_data =
  [ `Session ] Eliom_common.sessgrp Ocsigen_cache.Dlist.node
```
```ocaml
val add : 
  ?set_max:int ->
  Eliom_common.sitedata ->
  string ->
  [< Eliom_common.cookie_level ] Eliom_common.sessgrp ->
  string Ocsigen_cache.Dlist.node
```
```ocaml
val remove : 'a Ocsigen_cache.Dlist.node -> unit
```
```ocaml
val remove_group : [< Eliom_common.cookie_level ] Eliom_common.sessgrp -> unit
```
```ocaml
val find : 
  [< Eliom_common.cookie_level ] Eliom_common.sessgrp ->
  string Ocsigen_cache.Dlist.t
```
returns the dlist containing all session group elements

```ocaml
val find_node_in_group_of_groups : 
  [< `Session ] Eliom_common.sessgrp ->
  group_of_group_data option
```
Groups of browser sessions belong to a group of groups. As these groups are not associated to a cookie, we put this information here.

```ocaml
val move : 
  ?set_max:int ->
  Eliom_common.sitedata ->
  string Ocsigen_cache.Dlist.node ->
  [< Eliom_common.cookie_level ] Eliom_common.sessgrp ->
  string Ocsigen_cache.Dlist.node
```
```ocaml
val up : string Ocsigen_cache.Dlist.node -> unit
```
```ocaml
val nb_of_groups : unit -> int
```
```ocaml
val group_size : [< Eliom_common.cookie_level ] Eliom_common.sessgrp -> int
```
```ocaml
val set_max : 'a Ocsigen_cache.Dlist.node -> int -> unit
```