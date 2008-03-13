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
   ((string option *
     Eliom_common.tables Eliom_common.one_service_cookie_info
     Eliom_common.session_cookie ref)
    Ocsigen_http_frame.Cookievalues.t ref *
    (string option *
     Eliom_common.one_data_cookie_info Eliom_common.session_cookie ref)
    Lazy.t Ocsigen_http_frame.Cookievalues.t ref *
    ((string * Eliom_common.timeout * float option *
      Eliommod_sessiongroups.perssessgrp option)
     option *
     Eliom_common.one_persistent_cookie_info Eliom_common.session_cookie ref)
    Lwt.t Lazy.t Ocsigen_http_frame.Cookievalues.t ref) ->
   Eliom_common.sitedata -> Eliom_common.result_to_send Lwt.t) ->
  Ocsigen_extensions.request_info * Eliom_common.sess_info * 'a *
  ((string option *
    Eliom_common.tables Eliom_common.one_service_cookie_info
    Eliom_common.session_cookie ref)
   Ocsigen_http_frame.Cookievalues.t ref *
   (string option *
    Eliom_common.one_data_cookie_info Eliom_common.session_cookie ref)
   Lazy.t Ocsigen_http_frame.Cookievalues.t ref *
   ((string * Eliom_common.timeout * float option *
     Eliommod_sessiongroups.perssessgrp option)
    option *
    Eliom_common.one_persistent_cookie_info Eliom_common.session_cookie ref)
   Lwt.t Lazy.t Ocsigen_http_frame.Cookievalues.t ref) ->
  Eliom_common.sitedata -> Eliom_common.result_to_send Lwt.t
val compute_exn : string list -> exn list
val gen :
  Eliom_common.sitedata ->
  string -> Ocsigen_extensions.request_state -> Ocsigen_extensions.answer Lwt.t
