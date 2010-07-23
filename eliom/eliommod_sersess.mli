val close_service_group :
  Eliom_common.level Eliom_common.sessgrp -> unit
val close_service_session :
  ?close_group:bool ->
  ?session_name:string -> 
  ?cookie_level:Eliom_common.cookie_level ->
  secure: bool option ->
  sp:Eliom_common.server_params -> unit -> unit
val find_or_create_service_cookie :
  ?set_session_group:string ->
  ?session_name:string ->
  ?cookie_level:Eliom_common.cookie_level ->
  secure: bool option ->
  sp:Eliom_common.server_params ->
  unit -> Eliom_common.tables Eliom_common.one_service_cookie_info
val find_service_cookie_only :
  ?session_name:string ->
  ?cookie_level:Eliom_common.cookie_level ->
  secure: bool option ->
  sp:Eliom_common.server_params ->
  unit -> Eliom_common.tables Eliom_common.one_service_cookie_info
