val iter_service_sessions :
  Eliom_common.sitedata ->
  (Eliom_common.SessionCookies.key *
   Eliom_common.tables Eliom_common.servicecookiestablecontent *
   Eliom_common.sitedata -> unit Lwt.t) ->
  unit Lwt.t
val iter_data_sessions :
  Eliom_common.sitedata ->
  (Eliom_common.SessionCookies.key * Eliom_common.datacookiestablecontent *
   Eliom_common.sitedata -> unit Lwt.t) ->
  unit Lwt.t
val iter_persistent_sessions :
  (string *
   (string * float option * Eliom_common.timeout *
    Eliommod_sessiongroups.perssessgrp option) ->
   unit Lwt.t) ->
  unit Lwt.t
val fold_service_sessions :
  Eliom_common.sitedata ->
  (Eliom_common.SessionCookies.key *
   Eliom_common.tables Eliom_common.servicecookiestablecontent *
   Eliom_common.sitedata -> 'a -> 'a Lwt.t) ->
  'a -> 'a Lwt.t
val fold_data_sessions :
  Eliom_common.sitedata ->
  (Eliom_common.SessionCookies.key * Eliom_common.datacookiestablecontent *
   Eliom_common.sitedata -> 'a -> 'a Lwt.t) ->
  'a -> 'a Lwt.t
val fold_persistent_sessions :
  (string *
   (string * float option * Eliom_common.timeout *
    Eliommod_sessiongroups.perssessgrp option) ->
   'a -> 'a Lwt.t) ->
  'a -> 'a Lwt.t
val number_of_service_sessions : sp:Eliom_common.server_params -> int
val number_of_data_sessions : sp:Eliom_common.server_params -> int
val number_of_tables : unit -> int
val number_of_table_elements : unit -> int list
val number_of_persistent_sessions : unit -> int Lwt.t
