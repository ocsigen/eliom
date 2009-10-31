val close_service_group :
  Eliom_common.sitedata -> Eliommod_sessiongroups.sessgrp option -> unit
val close_service_session :
  ?close_group:bool ->
  ?session_name:string -> 
  secure: bool option ->
  sp:Eliom_common.server_params -> unit -> unit
val new_service_cookie :
  Eliom_common.sitedata ->
  Eliommod_sessiongroups.sessgrp option ->
  'a ->
  ('a * Eliom_common.tables *
   float option ref * Eliom_common.timeout ref *
   Eliommod_sessiongroups.sessgrp option ref)
  Eliom_common.SessionCookies.t ->
  Eliom_common.tables
  Eliom_common.one_service_cookie_info
val find_or_create_service_cookie :
  ?session_group:string ->
  ?session_name:string ->
  secure: bool option ->
  sp:Eliom_common.server_params ->
  unit -> Eliom_common.tables Eliom_common.one_service_cookie_info
val find_service_cookie_only :
  ?session_name:string ->
  secure: bool option ->
  sp:Eliom_common.server_params ->
  unit -> Eliom_common.tables Eliom_common.one_service_cookie_info
