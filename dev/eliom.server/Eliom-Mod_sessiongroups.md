# Module `Eliom.Mod_sessiongroups`

```ocaml
val make_full_named_group_name_ : 
  cookie_level:Common.cookie_level ->
  Common.sitedata ->
  string ->
  Common.scope Common.sessgrp
```
```ocaml
val make_full_group_name : 
  cookie_level:Common.cookie_level ->
  sitedata:Common.sitedata ->
  Ocsigen.Request.t ->
  string option ->
  Common.scope Common.sessgrp
```
```ocaml
val make_persistent_full_group_name : 
  cookie_level:Common.cookie_level ->
  string ->
  string option ->
  Common.perssessgrp option
```
```ocaml
val getperssessgrp : Common.perssessgrp -> Common.full_session_group
```
```ocaml
module type MEMTAB = sig ... end
```
```ocaml
module Serv : 
  MEMTAB
    with type group_of_group_data =
           Common.tables ref
           * [ `Session ] Common.sessgrp Ocsigen_base.Cache.Dlist.node
```
```ocaml
module Data : 
  MEMTAB
    with type group_of_group_data =
           [ `Session ] Common.sessgrp Ocsigen_base.Cache.Dlist.node
```
```ocaml
module Pers : sig ... end
```
