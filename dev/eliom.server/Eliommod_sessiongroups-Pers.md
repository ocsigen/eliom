# Module `Eliommod_sessiongroups.Pers`

```ocaml
val find : Eliom_common.perssessgrp option -> string list Lwt.t
```
```ocaml
val add : 
  ?set_max:int option ->
  int option ->
  string ->
  Eliom_common.perssessgrp option ->
  string list Lwt.t
```
```ocaml
val remove : 
  Eliom_common.sitedata ->
  string ->
  Eliom_common.perssessgrp option ->
  unit Lwt.t
```
```ocaml
val remove_group : 
  cookie_level:
    [ `Session | `Client_process of Eliom_common.perssessgrp option ] ->
  Eliom_common.sitedata ->
  Eliom_common.perssessgrp option ->
  unit Lwt.t
```
```ocaml
val move : 
  Eliom_common.sitedata ->
  ?set_max:int option ->
  int option ->
  string ->
  Eliom_common.perssessgrp option ->
  Eliom_common.perssessgrp option ->
  string list Lwt.t
```
```ocaml
val up : string -> Eliom_common.perssessgrp option -> unit Lwt.t
```
```ocaml
val nb_of_groups : unit -> int Lwt.t
```
```ocaml
val close_persistent_session2 : 
  cookie_level:Eliom_common.cookie_level ->
  Eliom_common.sitedata ->
  Eliom_common.perssessgrp option ->
  string ->
  unit Lwt.t
```
