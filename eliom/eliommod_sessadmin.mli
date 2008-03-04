val close_all_service_sessions2 :
  ?close_group:bool -> string -> Eliom_common.sitedata -> unit Lwt.t
val close_all_service_sessions :
  ?close_group:bool ->
  ?session_name:string -> Eliom_common.sitedata -> unit Lwt.t
val close_all_data_sessions2 :
  ?close_group:bool -> string -> Eliom_common.sitedata -> unit Lwt.t
val close_all_data_sessions :
  ?close_group:bool ->
  ?session_name:string -> Eliom_common.sitedata -> unit Lwt.t
val close_all_persistent_sessions2 :
  ?close_group:bool -> string -> unit Lwt.t
val close_all_persistent_sessions :
  ?close_group:bool ->
  ?session_name:string -> Eliom_common.sitedata -> unit Lwt.t
val update_serv_exp :
  string ->
  Eliom_common.sitedata -> float option -> float option -> unit Lwt.t
val update_data_exp :
  string ->
  Eliom_common.sitedata -> float option -> float option -> unit Lwt.t
val update_pers_exp : string -> float option -> float option -> unit Lwt.t
