val perstables : string list ref
val persistent_cookies_table :
  (string * float option * Eliom_common.timeout *
   Eliommod_sessiongroups.perssessgrp option)
  Ocsipersist.table
val number_of_persistent_tables : unit -> int
val number_of_persistent_table_elements : unit -> (string * int) list Lwt.t
val close_persistent_session2 :
  Eliommod_sessiongroups.perssessgrp option -> string -> unit Lwt.t
val close_persistent_group :
  Eliommod_sessiongroups.perssessgrp option -> unit Lwt.t
val close_persistent_session :
  ?close_group:bool ->
  ?session_name:string -> 
  secure:bool option ->
  sp:Eliom_common.server_params -> unit -> unit Lwt.t
val new_persistent_cookie :
  Eliom_common.sitedata ->
  Eliommod_sessiongroups.perssessgrp option ->
  string -> Eliom_common.one_persistent_cookie_info Lwt.t
val find_or_create_persistent_cookie :
  ?session_group:string ->
  ?session_name:string ->
  secure:bool option ->
  sp:Eliom_common.server_params ->
  unit -> Eliom_common.one_persistent_cookie_info Lwt.t
val find_persistent_cookie_only :
  ?session_name:string ->
  secure:bool option ->
  sp:Eliom_common.server_params ->
  unit -> Eliom_common.one_persistent_cookie_info Lwt.t
