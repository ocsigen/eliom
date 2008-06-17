val def_handler : 'a -> exn -> 'b Lwt.t
val handle_site_exn :
  exn ->
  Ocsigen_extensions.request_info * Eliom_common.sess_info * 'a *
  Eliom_common.tables Eliom_common.cookie_info ->
  Eliom_common.sitedata -> Eliom_common.result_to_send Lwt.t
val execute :
  float ->
  (float ->
   Ocsigen_extensions.request_info * Eliom_common.sess_info * 'a *
     Eliom_common.tables Eliom_common.cookie_info ->
   Eliom_common.sitedata -> Eliom_common.result_to_send Lwt.t) ->
  Ocsigen_extensions.request_info * Eliom_common.sess_info * 'a *
     Eliom_common.tables Eliom_common.cookie_info ->
  Eliom_common.sitedata -> Eliom_common.result_to_send Lwt.t
val compute_exn : string list -> exn list
val gen :
  Eliom_common.sitedata ->
  string * string * int * int -> 
  Ocsigen_extensions.request_state -> Ocsigen_extensions.answer Lwt.t
