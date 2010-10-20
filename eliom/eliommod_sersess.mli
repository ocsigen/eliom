val close_service_session :
  ?state_name:string -> 
  ?scope:Eliom_common.user_scope ->
  secure: bool option ->
  sp:Eliom_common.server_params -> unit -> unit
val find_or_create_service_cookie :
  ?set_session_group:string ->
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  secure: bool option ->
  sp:Eliom_common.server_params ->
  unit -> Eliom_common.tables Eliom_common.one_service_cookie_info
val find_service_cookie_only :
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  secure: bool option ->
  sp:Eliom_common.server_params ->
  unit -> Eliom_common.tables Eliom_common.one_service_cookie_info
