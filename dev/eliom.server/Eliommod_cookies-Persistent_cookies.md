
# Module `Eliommod_cookies.Persistent_cookies`

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
val replace_if_exists : string -> cookie -> unit Lwt.t
```
```ocaml
val garbage_collect : 
  section:Logs.src ->
  (Cookies.key -> unit Lwt.t) ->
  unit Lwt.t
```