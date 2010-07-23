val close_data_session :
  ?session_name:string -> 
  ?level:Eliom_common.level ->
  secure:bool option ->
  sp:Eliom_common.server_params -> unit -> unit
val find_or_create_data_cookie :
  ?set_session_group:string ->
  ?session_name:string ->
  ?cookie_level:Eliom_common.cookie_level ->
  secure:bool option ->
  sp:Eliom_common.server_params -> unit -> Eliom_common.one_data_cookie_info
val find_data_cookie_only :
  ?session_name:string ->
  ?cookie_level:Eliom_common.cookie_level ->
  secure:bool option ->
  sp:Eliom_common.server_params -> unit -> Eliom_common.one_data_cookie_info
val counttableelements : (unit -> int) list ref
val create_volatile_table : 
  level:Eliom_common.level ->
  session_name:string option ->
  secure:bool ->
  (Eliom_common.level * string option * bool * 'a Eliom_common.SessionCookies.t)
val create_volatile_table_during_session :
  level:Eliom_common.level ->
  session_name:string option ->
  secure:bool ->
  Eliom_common.server_params -> 
  (Eliom_common.level * string option * bool *'a Eliom_common.SessionCookies.t)
