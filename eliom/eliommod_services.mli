val add_service :
  Eliom_common.tables ->
  ?sp:Eliom_common.server_params ->
  Ocsigen_lib.String_Table.key list ->
  Eliom_common.Serv_Table.key ->
  (Eliom_common.anon_params_type * Eliom_common.anon_params_type) *
    (int ref option * (float * float ref) option *
       (bool ->
        Eliom_common.server_params -> Ocsigen_http_frame.result Lwt.t)) ->
  unit
val remove_service :
  Eliom_common.tables ->
  Ocsigen_lib.String_Table.key list ->
  Eliom_common.Serv_Table.key ->
  Eliom_common.anon_params_type * Eliom_common.anon_params_type ->
  unit
val get_page :
  float ->
  Eliom_common.info ->
  Eliom_common.sitedata -> 
  Ocsigen_http_frame.result Lwt.t
