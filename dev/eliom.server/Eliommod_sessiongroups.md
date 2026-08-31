# Module `Eliommod_sessiongroups`

```ocaml
val make_full_named_group_name_ : 
  cookie_level:Eliom_common.cookie_level ->
  Eliom_common.sitedata ->
  string ->
  Eliom_common.scope Eliom_common.sessgrp
```
```ocaml
val make_full_group_name : 
  cookie_level:Eliom_common.cookie_level ->
  Ocsigen_request.t ->
  string ->
  int ->
  int ->
  string option ->
  Eliom_common.scope Eliom_common.sessgrp
```
```ocaml
val make_persistent_full_group_name : 
  cookie_level:Eliom_common.cookie_level ->
  string ->
  string option ->
  Eliom_common.perssessgrp option
```
```ocaml
val getsessgrp : 
  Eliom_common.scope Eliom_common.sessgrp ->
  string * Eliom_common.cookie_level * (string, Ipaddr.t) Eliom_lib.leftright
```
```ocaml
val getperssessgrp : 
  Eliom_common.perssessgrp ->
  string * Eliom_common.cookie_level * (string, Ipaddr.t) Eliom_lib.leftright
```
```ocaml
module type MEMTAB = sig ... end
```
```ocaml
module Serv : 
  MEMTAB
    with type group_of_group_data =
           Eliom_common.tables ref
           * [ `Session ] Eliom_common.sessgrp Ocsigen_cache.Dlist.node
```
```ocaml
module Data : 
  MEMTAB
    with type group_of_group_data =
           [ `Session ] Eliom_common.sessgrp Ocsigen_cache.Dlist.node
```
```ocaml
module Pers : sig ... end
```
