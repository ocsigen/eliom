val close_all_service_sessions2 :
  ?close_group:bool -> 
  Eliom_common.fullsessionname -> Eliom_common.sitedata -> unit Lwt.t
val close_all_service_sessions :
  ?close_group:bool ->
  ?session_name:string -> 
  ?cookie_type:Eliom_common.cookie_type ->
  Eliom_common.sitedata -> unit Lwt.t
val close_all_data_sessions2 :
  ?close_group:bool ->
  Eliom_common.fullsessionname -> Eliom_common.sitedata -> unit Lwt.t
val close_all_data_sessions :
  ?close_group:bool ->
  ?session_name:string -> 
  ?cookie_type:Eliom_common.cookie_type ->
  Eliom_common.sitedata -> unit Lwt.t
val close_all_persistent_sessions2 :
  ?close_group:bool -> Eliom_common.fullsessionname -> unit Lwt.t
val close_all_persistent_sessions :
  ?close_group:bool ->
  ?session_name:string -> 
  ?cookie_type:Eliom_common.cookie_type ->
  Eliom_common.sitedata -> unit Lwt.t
val update_serv_exp :
  Eliom_common.fullsessionname ->
  Eliom_common.sitedata -> float option -> float option -> unit Lwt.t
val update_data_exp :
  Eliom_common.fullsessionname ->
  Eliom_common.sitedata -> float option -> float option -> unit Lwt.t
val update_pers_exp : 
  Eliom_common.fullsessionname -> 
  'a -> float option -> float option -> unit Lwt.t
