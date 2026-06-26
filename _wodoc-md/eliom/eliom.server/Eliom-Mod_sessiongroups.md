
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
  Ocsigen.Request.t ->
  string ->
  int ->
  int ->
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
val getsessgrp : 
  Common.scope Common.sessgrp ->
  string * Common.cookie_level * (string, Ipaddr.t) Lib.leftright
```
```ocaml
val getperssessgrp : 
  Common.perssessgrp ->
  string * Common.cookie_level * (string, Ipaddr.t) Lib.leftright
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