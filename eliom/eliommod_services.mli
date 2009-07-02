val find_page_table :
  float ->
  Eliom_common.page_table ref ->
  string option ->
  Eliom_common.sitedata ->
  Eliom_common.tables Eliom_common.cookie_info ->
  Ocsigen_extensions.request ->
  Ocsigen_extensions.url_path ->
  Eliom_common.page_table_key ->
  Eliom_common.sess_info -> Ocsigen_http_frame.result Lwt.t
val insert_as_last_of_generation :
  'a -> 'b * ('a * 'c) -> ('b * ('a * 'c)) list -> ('b * ('a * 'c)) list
val add_page_table :
  bool ->
  string list ->
  (Eliom_common.page_table_key * ('a * (int * 'b)) list) list ->
  Eliom_common.page_table_key * ('a * 'b) ->
  (Eliom_common.page_table_key * ('a * (int * 'b)) list) list
val add_dircontent :
  Eliom_common.dircontent ->
  Ocsigen_lib.String_Table.key * Eliom_common.direlt ref ->
  Eliom_common.dircontent
val find_dircontent :
  Eliom_common.dircontent ->
  Ocsigen_lib.String_Table.key -> Eliom_common.direlt ref
val add_service :
  Eliom_common.dircontent ref * 'a * bool ref * 'b ->
  bool ->
  Ocsigen_lib.String_Table.key list ->
  Eliom_common.page_table_key *
  ((Eliom_common.anon_params_type * Eliom_common.anon_params_type) *
   int ref option * (float * float ref) option *
   (Eliom_common.server_params -> Ocsigen_http_frame.result Lwt.t)) ->
  unit
exception Exn1
val find_service :
  float ->
  Eliom_common.dircontent ref * 'a * 'b * 'c ->
  string option ->
  Eliom_common.sitedata * Eliom_common.tables Eliom_common.cookie_info *
  Ocsigen_extensions.request * Eliom_common.sess_info ->
  Ocsigen_http_frame.result Lwt.t
val get_page :
  float ->
  Ocsigen_extensions.request * Eliom_common.sess_info * 
    Eliom_common.tables Eliom_common.cookie_info ->
  Eliom_common.sitedata -> 
  Ocsigen_http_frame.result Lwt.t
