val find_page_table :
  bool ->
  float ->
  Eliom_common.page_table ref ->
  Eliom_common.fullsessionname option ->
  Eliom_common.sitedata ->
  Eliom_common.tables Eliom_common.cookie_info ->
  Ocsigen_extensions.request ->
  Ocsigen_lib.url_path option ->
  Eliom_common.page_table_key ->
  Eliom_common.sess_info -> Ocsigen_http_frame.result Lwt.t
val insert_as_last_of_generation :
  'a -> 'b * ('a * 'c) -> ('b * ('a * 'c)) list -> ('b * ('a * 'c)) list
val add_service :
  Eliom_common.tables ->
  ?sp:Eliom_common.server_params ->
  Ocsigen_lib.String_Table.key list ->
  Eliom_common.page_table_key ->
  ((Eliom_common.anon_params_type * Eliom_common.anon_params_type) *
     (int ref option * (float * float ref) option *
        (bool -> 
           Eliom_common.server_params -> 
             Ocsigen_http_frame.result Lwt.t))) ->
  unit
val remove_service :
  Eliom_common.tables ->
  Ocsigen_lib.String_Table.key list ->
  Eliom_common.page_table_key ->
  (Eliom_common.anon_params_type * Eliom_common.anon_params_type) ->
  unit
exception Exn1
val find_service :
  float ->
  Eliom_common.tables ->
  Eliom_common.fullsessionname option ->
  Eliom_common.sitedata * Eliom_common.tables Eliom_common.cookie_info *
  Ocsigen_extensions.request * Eliom_common.sess_info ->
  Ocsigen_http_frame.result Lwt.t
val get_page :
  float ->
  Ocsigen_extensions.request * Eliom_common.sess_info * 
    Eliom_common.tables Eliom_common.cookie_info ->
  Eliom_common.sitedata -> 
  Ocsigen_http_frame.result Lwt.t
