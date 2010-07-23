val set_default_service_timeout : [< Eliom_common.cookie_level ] -> float option -> unit
val set_default_data_timeout : [< Eliom_common.cookie_level ] -> float option -> unit
val set_default_persistent_timeout : [< Eliom_common.cookie_level ] -> float option -> unit
val get_default_service_timeout : [< Eliom_common.cookie_level ] -> float option
val get_default_data_timeout : [< Eliom_common.cookie_level ] -> float option
val get_default_persistent_timeout : [< Eliom_common.cookie_level ] -> float option
val set_default_volatile_timeout :  [< Eliom_common.cookie_level ] -> float option -> unit
val add : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

val find_global_service_timeout :
  Eliom_common.fullsessionname -> Eliom_common.sitedata -> float option
val find_global_data_timeout :
  Eliom_common.fullsessionname -> Eliom_common.sitedata -> float option
val find_global_persistent_timeout :
  Eliom_common.fullsessionname -> Eliom_common.sitedata -> float option

val get_global_service_timeout :
  session_name:string option ->
  cookie_level:[< Eliom_common.cookie_level ] ->
  Eliom_common.sitedata -> float option
val get_global_data_timeout :
  session_name:string option ->
  cookie_level:[< Eliom_common.cookie_level ] ->
  Eliom_common.sitedata -> float option
val get_global_persistent_timeout :
  session_name:string option ->
  cookie_level:[< Eliom_common.cookie_level ] ->
  Eliom_common.sitedata -> float option

val set_global_service_timeout :
  session_name:string option ->
  cookie_level:[< Eliom_common.cookie_level ] ->
  recompute_expdates:bool -> 
  bool -> Eliom_common.sitedata -> float option -> unit
val set_global_data_timeout :
  session_name:string option ->
  cookie_level:[< Eliom_common.cookie_level ] ->
  recompute_expdates:bool -> 
  bool -> Eliom_common.sitedata -> float option -> unit
val set_global_persistent_timeout :
  session_name:string option ->
  cookie_level:[< Eliom_common.cookie_level ] ->
  recompute_expdates:bool -> 
  bool -> Eliom_common.sitedata -> float option -> unit

val set_global_service_timeout_ :
  ?fullsessname:Eliom_common.fullsessionname ->
  ?cookie_level:[< Eliom_common.cookie_level ] ->
  recompute_expdates:bool ->
  bool -> 
  bool -> Eliom_common.sitedata -> float option -> unit
val set_global_data_timeout_ :
  ?fullsessname:Eliom_common.fullsessionname ->
  ?cookie_level:[< Eliom_common.cookie_level ] ->
  recompute_expdates:bool ->
  bool -> 
  bool -> Eliom_common.sitedata -> float option -> unit
val set_global_persistent_timeout_ :
  ?fullsessname:Eliom_common.fullsessionname ->
  ?cookie_level:[< Eliom_common.cookie_level ] ->
  recompute_expdates:bool ->
  bool -> 
  bool -> Eliom_common.sitedata -> float option -> unit


val set_default_global_service_timeout :
  [< Eliom_common.cookie_level ] ->
  bool -> bool -> Eliom_common.sitedata -> float option -> unit
val set_default_global_data_timeout :
  [< Eliom_common.cookie_level ] ->
  bool -> bool -> Eliom_common.sitedata -> float option -> unit
val set_default_global_persistent_timeout :
  [< Eliom_common.cookie_level ] ->
  bool -> bool -> Eliom_common.sitedata -> float option -> unit
