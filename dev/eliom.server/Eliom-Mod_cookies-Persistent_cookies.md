# Module `Mod_cookies.Persistent_cookies`

```ocaml
module Cookies : 
  Ocsipersist.TABLE with type key = string and type value = cookie
```
```ocaml
module Expiry_dates : sig ... end
```
```ocaml
val add : string -> cookie -> unit Lwt.t
```
```ocaml
val update : 
  string ->
  expiry:date option ->
  timeout:Common.timeout ->
  ?session_group:Common.perssessgrp option ->
  unit ->
  unit Lwt.t
```
`update cookie ~expiry ~timeout ?session_group ()` changes the expiration date and the timeout of `cookie`, if it still exists, and its session group if `?session_group` is given: the group may have been changed meanwhile by another request of the session.

```ocaml
val garbage_collect : 
  section:Logs.src ->
  (Cookies.key -> unit Lwt.t) ->
  unit Lwt.t
```
