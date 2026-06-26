
# Module `Mod_sessiongroups.Pers`

```ocaml
val find : Common.perssessgrp option -> string list Lwt.t
```
```ocaml
val add : 
  ?set_max:int option ->
  int option ->
  string ->
  Common.perssessgrp option ->
  string list Lwt.t
```
```ocaml
val remove : 
  Common.sitedata ->
  string ->
  Common.perssessgrp option ->
  unit Lwt.t
```
```ocaml
val remove_group : 
  cookie_level:[ `Session | `Client_process of Common.perssessgrp option ] ->
  Common.sitedata ->
  Common.perssessgrp option ->
  unit Lwt.t
```
```ocaml
val move : 
  Common.sitedata ->
  ?set_max:int option ->
  int option ->
  string ->
  Common.perssessgrp option ->
  Common.perssessgrp option ->
  string list Lwt.t
```
```ocaml
val up : string -> Common.perssessgrp option -> unit Lwt.t
```
```ocaml
val nb_of_groups : unit -> int Lwt.t
```
```ocaml
val close_persistent_session2 : 
  cookie_level:Common.cookie_level ->
  Common.sitedata ->
  Common.perssessgrp option ->
  string ->
  unit Lwt.t
```