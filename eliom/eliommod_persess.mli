val perstables : string list ref
val persistent_cookies_table :
  (Eliom_common.fullsessionname * float option * Eliom_common.timeout *
   Eliom_common.perssessgrp option)
  Ocsipersist.table Lazy.t
val number_of_persistent_tables : unit -> int
val number_of_persistent_table_elements : unit -> (string * int) list Lwt.t
val close_persistent_session2 :
  cookie_scope:Eliom_common.cookie_scope ->
  Eliom_common.sitedata ->
  Eliom_common.perssessgrp option -> string -> unit Lwt.t
val close_persistent_session :
  ?state_name:string -> 
  ?scope:Eliom_common.user_scope ->
  secure:bool option ->
  sp:Eliom_request_info.server_params -> unit -> unit Lwt.t
val find_or_create_persistent_cookie :
  ?set_session_group:string ->
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  secure:bool option ->
  sp:Eliom_request_info.server_params ->
  unit -> Eliom_common.one_persistent_cookie_info Lwt.t
val find_persistent_cookie_only :
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  secure:bool option ->
  sp:Eliom_request_info.server_params ->
  unit -> Eliom_common.one_persistent_cookie_info Lwt.t
