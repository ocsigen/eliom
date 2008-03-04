val close_data_session2 :
  Eliom_common.sitedata ->
  Eliommod_sessiongroups.sessgrp option ->
  Eliom_common.SessionCookies.key -> unit
val close_data_group :
  Eliom_common.sitedata -> Eliommod_sessiongroups.sessgrp option -> unit
val close_data_session :
  ?close_group:bool ->
  ?session_name:string -> sp:Eliom_common.server_params -> unit -> unit
val new_data_cookie :
  Eliom_common.sitedata ->
  Eliommod_sessiongroups.sessgrp option ->
  'a ->
  ('a * float option ref * Eliom_common.timeout ref *
   Eliommod_sessiongroups.sessgrp option ref)
  Eliom_common.SessionCookies.t -> Eliom_common.one_data_cookie_info
val find_or_create_data_cookie :
  ?session_group:string ->
  ?session_name:string ->
  sp:Eliom_common.server_params -> unit -> Eliom_common.one_data_cookie_info
val find_data_cookie_only :
  ?session_name:string ->
  sp:Eliom_common.server_params -> unit -> Eliom_common.one_data_cookie_info
val counttableelements : (unit -> int) list ref
val create_volatile_table : unit -> 'a Eliom_common.SessionCookies.t
val create_volatile_table_during_session :
  Eliom_common.server_params -> 'a Eliom_common.SessionCookies.t
