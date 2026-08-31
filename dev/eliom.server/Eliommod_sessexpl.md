# Module `Eliommod_sessexpl`

```ocaml
val iter_service_cookies : 
  ((string * Eliom_common.tables Eliom_common.Service_cookie.t) -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val iter_data_cookies : 
  ((string * Eliom_common.Data_cookie.t) -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val iter_persistent_cookies : 
  ((string * Eliommod_cookies.cookie) -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val fold_service_cookies : 
  ((string * Eliom_common.tables Eliom_common.Service_cookie.t) ->
    'a ->
    'a Lwt.t) ->
  'a ->
  'a Lwt.t
```
```ocaml
val fold_data_cookies : 
  ((string * Eliom_common.Data_cookie.t) -> 'a -> 'a Lwt.t) ->
  'a ->
  'a Lwt.t
```
```ocaml
val fold_persistent_cookies : 
  ((string * Eliommod_cookies.cookie) -> 'a -> 'a Lwt.t) ->
  'a ->
  'a Lwt.t
```
```ocaml
val number_of_service_cookies : unit -> int
```
```ocaml
val number_of_data_cookies : unit -> int
```
```ocaml
val number_of_tables : unit -> int
```
```ocaml
val number_of_table_elements : unit -> int list
```
```ocaml
val number_of_persistent_cookies : unit -> int Lwt.t
```
