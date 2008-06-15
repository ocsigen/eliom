val make_new_cookie_value : unit -> string

val get_cookie_info :
  float ->
  Eliom_common.sitedata ->
  Eliom_common.SessionCookies.key Ocsigen_http_frame.Cookievalues.t ->
  Eliom_common.SessionCookies.key Ocsigen_http_frame.Cookievalues.t ->
  string Ocsigen_http_frame.Cookievalues.t ->
  (Eliom_common.SessionCookies.key Ocsigen_http_frame.Cookievalues.t *
     Eliom_common.SessionCookies.key Ocsigen_http_frame.Cookievalues.t *
     string Ocsigen_http_frame.Cookievalues.t) option ->
  Eliom_common.tables Eliom_common.cookie_info *
  Ocsigen_http_frame.Cookievalues.key list

val new_service_cookie_table :
  unit -> Eliom_common.tables Eliom_common.servicecookiestable
val new_data_cookie_table : unit -> Eliom_common.datacookiestable
val compute_session_cookies_to_send :
  Eliom_common.sitedata ->
  Eliom_common.tables Eliom_common.cookie_info ->
  Ocsigen_http_frame.cookieset -> Ocsigen_http_frame.cookieset Lwt.t
val compute_cookies_to_send :
  Eliom_common.sitedata ->
  Eliom_common.tables Eliom_common.cookie_info ->
  Ocsigen_http_frame.cookieset -> Ocsigen_http_frame.cookieset Lwt.t
val add_cookie_list_to_send :
  Eliom_common.sitedata ->
  Eliom_common.cookie list -> Ocsigen_http_frame.cookieset -> Ocsigen_http_frame.cookieset
val compute_new_ri_cookies' :
  float ->
  string list ->
  string Ocsigen_http_frame.Cookievalues.t ->
  Eliom_common.cookie list -> string Ocsigen_http_frame.Cookievalues.t
val compute_new_ri_cookies :
  float ->
  string list ->
  string Ocsigen_http_frame.Cookievalues.t ->
  Eliom_common.tables Eliom_common.cookie_info ->
  Eliom_common.cookie list -> string Ocsigen_http_frame.Cookievalues.t Lwt.t
