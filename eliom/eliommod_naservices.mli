val add_naservice_table :
  Eliom_common.naservice_table ->
  Eliom_common.NAserv_Table.key *
  (int * int ref option * (float * float ref) option *
   (Eliom_common.server_params -> Eliom_common.result_to_send Lwt.t)) ->
  Eliom_common.naservice_table
val find_naservice_table :
  Eliom_common.naservice_table ->
  Eliom_common.NAserv_Table.key ->
  int * int ref option * (float * float ref) option *
  (Eliom_common.server_params -> Eliom_common.result_to_send Lwt.t)
val remove_naservice_table :
  Eliom_common.naservice_table ->
  Eliom_common.NAserv_Table.key -> Eliom_common.naservice_table
val add_naservice :
  'a * Eliom_common.naservice_table ref * 'b * bool ref ->
  bool ->
  Eliom_common.NAserv_Table.key ->
  int ref option * (float * float ref) option *
  (Eliom_common.server_params -> Eliom_common.result_to_send Lwt.t) -> 
  unit
val remove_naservice :
  'a * Eliom_common.naservice_table ref * 'b * 'c ->
  Eliom_common.NAserv_Table.key -> unit
val find_naservice :
  float ->
  'a * Eliom_common.naservice_table ref * 'b * 'c ->
  Eliom_common.NAserv_Table.key ->
  int * int ref option * (float * float ref) option *
  (Eliom_common.server_params -> Eliom_common.result_to_send Lwt.t)
val make_naservice :
  float ->
  Extensions.request_info * Eliom_common.sess_info * Http_frame.cookieset *
  ((string option *
    Eliom_common.tables Eliom_common.one_service_cookie_info
    Eliom_common.session_cookie ref)
   Http_frame.Cookievalues.t ref *
   (string option *
    Eliom_common.one_data_cookie_info Eliom_common.session_cookie ref)
   Lazy.t Http_frame.Cookievalues.t ref *
   ((string * Eliom_common.timeout * float option *
     Eliommod_sessiongroups.perssessgrp option)
    option *
    Eliom_common.one_persistent_cookie_info Eliom_common.session_cookie ref)
   Lwt.t Lazy.t Http_frame.Cookievalues.t ref) ->
  Eliom_common.sitedata -> Eliom_common.result_to_send Lwt.t
