val add_naservice_table :
  Eliom_common.naservice_table ->
  Eliom_common.NAserv_Table.key *
  (int * int ref option * (float * float ref) option *
   (Eliom_common.server_params -> Ocsigen_http_frame.result Lwt.t)) ->
  Eliom_common.naservice_table
val find_naservice_table :
  Eliom_common.naservice_table ->
  Eliom_common.NAserv_Table.key ->
  int * int ref option * (float * float ref) option *
  (Eliom_common.server_params -> Ocsigen_http_frame.result Lwt.t)
val remove_naservice_table :
  Eliom_common.naservice_table ->
  Eliom_common.NAserv_Table.key -> Eliom_common.naservice_table
val add_naservice :
  Eliom_common.tables ->
  bool ->
  Eliom_common.NAserv_Table.key ->
  int ref option * (float * float ref) option *
  (Eliom_common.server_params -> Ocsigen_http_frame.result Lwt.t) ->
  unit
val remove_naservice :
  Eliom_common.tables ->
  Eliom_common.NAserv_Table.key -> unit
val find_naservice :
  float ->
  Eliom_common.tables ->
  Eliom_common.NAserv_Table.key ->
  int * int ref option * (float * float ref) option *
  (Eliom_common.server_params -> Ocsigen_http_frame.result Lwt.t)
val make_naservice :
  float ->
  Ocsigen_extensions.request * Eliom_common.sess_info * 
    Eliom_common.tables Eliom_common.cookie_info ->
  Eliom_common.sitedata -> Ocsigen_http_frame.result Lwt.t
