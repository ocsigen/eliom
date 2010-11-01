val def_handler : exn -> 'b Lwt.t
val handle_site_exn :
  exn ->
  Eliom_common.info ->
  Eliom_common.sitedata -> Ocsigen_http_frame.result Lwt.t
val execute :
  float ->
  (float ->
  Eliom_common.info ->
   Eliom_common.sitedata -> Ocsigen_http_frame.result Lwt.t) ->
  Eliom_common.info ->
  Eliom_common.sitedata -> Ocsigen_http_frame.result Lwt.t
val gen :
  Eliom_extensions.eliom_extension_sig option ->
  Eliom_common.sitedata ->
  Ocsigen_extensions.request_state -> Ocsigen_extensions.answer Lwt.t
val update_cookie_table :
  ?now:float ->
  Eliom_common.sitedata ->
  Eliom_common.tables Eliom_common.cookie_info ->
  unit Lwt.t
