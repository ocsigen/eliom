val def_handler : 'a -> exn -> 'b Lwt.t
val handle_site_exn :
  exn ->
  Ocsigen_extensions.request * Eliom_common.sess_info *
  Eliom_common.tables Eliom_common.cookie_info *
  Eliom_common.tables Eliom_common.cookie_info ->
  Eliom_common.sitedata -> Ocsigen_http_frame.result Lwt.t
val execute :
  float ->
  (float ->
   Ocsigen_extensions.request * Eliom_common.sess_info *
     Eliom_common.tables Eliom_common.cookie_info *
     Eliom_common.tables Eliom_common.cookie_info ->
   Eliom_common.sitedata -> Ocsigen_http_frame.result Lwt.t) ->
  Ocsigen_extensions.request * Eliom_common.sess_info *
    Eliom_common.tables Eliom_common.cookie_info *
    Eliom_common.tables Eliom_common.cookie_info ->
  Eliom_common.sitedata -> Ocsigen_http_frame.result Lwt.t
val gen :
  Eliommod_extensions.eliom_extension_sig option ->
  Eliom_common.sitedata ->
  Ocsigen_extensions.request_state -> Ocsigen_extensions.answer Lwt.t
