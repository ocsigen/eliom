val perstables : string list ref
val persistent_cookies_table :
  (Eliom_common.fullsessionname * float option * Eliom_common.timeout *
   Eliom_common.perssessgrp option)
  Ocsipersist.table Lazy.t
val number_of_persistent_tables : unit -> int
val number_of_persistent_table_elements : unit -> (string * int) list Lwt.t
val close_persistent_session2 :
  Eliom_common.perssessgrp option -> string -> unit Lwt.t
val close_persistent_group :
  Eliom_common.perssessgrp option -> unit Lwt.t
val close_persistent_session :
  ?close_group:bool ->
  ?session_name:string -> 
  ?cookie_type:Eliom_common.cookie_type ->
  secure:bool option ->
  sp:Eliom_common.server_params -> unit -> unit Lwt.t
val new_persistent_cookie :
  Eliom_common.sitedata ->
  Eliom_common.perssessgrp option ->
  Eliom_common.fullsessionname -> Eliom_common.one_persistent_cookie_info Lwt.t
val find_or_create_persistent_cookie :
  ?session_group:string ->
  ?session_name:string ->
  ?cookie_type:Eliom_common.cookie_type ->
  secure:bool option ->
  sp:Eliom_common.server_params ->
  unit -> Eliom_common.one_persistent_cookie_info Lwt.t
val find_persistent_cookie_only :
  ?session_name:string ->
  ?cookie_type:Eliom_common.cookie_type ->
  secure:bool option ->
  sp:Eliom_common.server_params ->
  unit -> Eliom_common.one_persistent_cookie_info Lwt.t
