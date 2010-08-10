val close_all_service_sessions :
  ?session_name:string -> 
  ?cookie_level:Eliom_common.cookie_level ->
  Eliom_common.sitedata -> unit Lwt.t
val close_all_data_sessions :
  ?session_name:string -> 
  ?cookie_level:Eliom_common.cookie_level ->
  Eliom_common.sitedata -> unit Lwt.t
val close_all_persistent_sessions :
  ?session_name:string -> 
  ?cookie_level:Eliom_common.cookie_level ->
  Eliom_common.sitedata -> unit Lwt.t
val update_serv_exp :
  Eliom_common.fullsessionname ->
  Eliom_common.sitedata -> float option -> float option -> unit Lwt.t
val update_data_exp :
  Eliom_common.fullsessionname ->
  Eliom_common.sitedata -> float option -> float option -> unit Lwt.t
val update_pers_exp : 
  Eliom_common.fullsessionname -> 
  Eliom_common.sitedata -> float option -> float option -> unit Lwt.t
