
# Module type `Mod_sessiongroups.MEMTAB`

```ocaml
type group_of_group_data
```
```ocaml
val add : 
  ?set_max:int ->
  Common.sitedata ->
  string ->
  [< Common.cookie_level ] Common.sessgrp ->
  string Ocsigen_base.Cache.Dlist.node
```
```ocaml
val remove : 'a Ocsigen_base.Cache.Dlist.node -> unit
```
```ocaml
val remove_group : [< Common.cookie_level ] Common.sessgrp -> unit
```
```ocaml
val find : 
  [< Common.cookie_level ] Common.sessgrp ->
  string Ocsigen_base.Cache.Dlist.t
```
returns the dlist containing all session group elements

```ocaml
val find_node_in_group_of_groups : 
  [< `Session ] Common.sessgrp ->
  group_of_group_data option
```
Groups of browser sessions belong to a group of groups. As these groups are not associated to a cookie, we put this information here.

```ocaml
val move : 
  ?set_max:int ->
  Common.sitedata ->
  string Ocsigen_base.Cache.Dlist.node ->
  [< Common.cookie_level ] Common.sessgrp ->
  string Ocsigen_base.Cache.Dlist.node
```
```ocaml
val up : string Ocsigen_base.Cache.Dlist.node -> unit
```
```ocaml
val nb_of_groups : unit -> int
```
```ocaml
val group_size : [< Common.cookie_level ] Common.sessgrp -> int
```
```ocaml
val set_max : 'a Ocsigen_base.Cache.Dlist.node -> int -> unit
```