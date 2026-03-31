val close_data_state :
   scope:[< Common.user_scope]
  -> secure_o:bool option
  -> ?sp:Common.server_params
  -> unit
  -> unit

val find_or_create_data_cookie :
   ?set_session_group:string
  -> cookie_scope:[< Common.cookie_scope]
  -> secure_o:bool option
  -> ?sp:Common.server_params
  -> unit
  -> Common.one_data_cookie_info

val find_data_cookie_only :
   cookie_scope:[< Common.cookie_scope]
  -> secure_o:bool option
  -> ?sp:Common.server_params
  -> unit
  -> Common.one_data_cookie_info

val counttableelements : (unit -> int) list ref

val create_volatile_table :
   scope:([< Common.user_scope] as 'b)
  -> secure:bool
  -> 'b * bool * 'a Common.SessionCookies.t

val create_volatile_table_during_session :
   scope:([< Common.user_scope] as 'b)
  -> secure:bool
  -> Common.sitedata
  -> 'b * bool * 'a Common.SessionCookies.t
