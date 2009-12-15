val set_default_service_timeout : float option -> unit
val set_default_data_timeout : float option -> unit
val set_default_persistent_timeout : float option -> unit
val get_default_service_timeout : unit -> float option
val get_default_data_timeout : unit -> float option
val get_default_persistent_timeout : unit -> float option
val set_default_volatile_timeout : float option -> unit
val add : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

val find_global_service_timeout :
  string -> Eliom_common.sitedata -> float option
val find_global_data_timeout :
  string -> Eliom_common.sitedata -> float option
val find_global_persistent_timeout :
  string -> Eliom_common.sitedata -> float option

val get_global_service_timeout :
  session_name:string option -> Eliom_common.sitedata -> float option
val get_global_data_timeout :
  session_name:string option -> Eliom_common.sitedata -> float option
val get_global_persistent_timeout :
  session_name:string option -> Eliom_common.sitedata -> float option

val set_global_service_timeout :
  session_name:string option ->
  recompute_expdates:bool -> 
  bool -> Eliom_common.sitedata -> float option -> unit
val set_global_data_timeout :
  session_name:string option ->
  recompute_expdates:bool -> 
  bool -> Eliom_common.sitedata -> float option -> unit
val set_global_persistent_timeout :
  session_name:string option ->
  recompute_expdates:bool -> 
  bool -> Eliom_common.sitedata -> float option -> unit

val set_global_service_timeout2 :
  ?fullsessname:string ->
  recompute_expdates:bool ->
  bool -> 
  bool -> Eliom_common.sitedata -> float option -> unit
val set_global_data_timeout2 :
  ?fullsessname:string ->
  recompute_expdates:bool ->
  bool -> 
  bool -> Eliom_common.sitedata -> float option -> unit
val set_global_persistent_timeout2 :
  ?fullsessname:string ->
  recompute_expdates:bool ->
  bool -> 
  bool -> Eliom_common.sitedata -> float option -> unit

val set_default_global_service_timeout :
  bool -> bool -> Eliom_common.sitedata -> float option -> unit
val set_default_global_data_timeout :
  bool -> bool -> Eliom_common.sitedata -> float option -> unit
val set_default_global_persistent_timeout :
  bool -> bool -> Eliom_common.sitedata -> float option -> unit
