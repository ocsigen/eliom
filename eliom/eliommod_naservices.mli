val add_naservice :
  Eliom_common.tables ->
  ?sp:Eliom_common.server_params ->
  Eliom_common.NAserv_Table.key ->
  int ref option * (float * float ref) option *
    (Eliom_common.server_params -> Ocsigen_http_frame.result Lwt.t) ->
  unit
val remove_naservice :
  Eliom_common.tables -> Eliom_common.NAserv_Table.key -> unit
val make_naservice :
  float ->
  Eliom_common.info ->
  Eliom_common.sitedata -> Ocsigen_http_frame.result Lwt.t
