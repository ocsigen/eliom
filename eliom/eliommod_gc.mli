val servicesessiongcfrequency : float option ref
val datasessiongcfrequency : float option ref
val persistentsessiongcfrequency : float option ref
val set_servicesessiongcfrequency : float option -> unit
val set_datasessiongcfrequency : float option -> unit
val get_servicesessiongcfrequency : unit -> float option
val get_datasessiongcfrequency : unit -> float option
val set_persistentsessiongcfrequency : float option -> unit
val get_persistentsessiongcfrequency : unit -> float option
val gc_timeouted_services :
  float -> Eliom_common.dircontent ref -> unit Lwt.t
val gc_timeouted_naservices :
  float -> Eliom_common.naservice_table ref -> unit Lwt.t
val service_session_gc : Eliom_common.sitedata -> unit
val data_session_gc : Eliom_common.sitedata -> unit
val persistent_session_gc : Eliom_common.sitedata -> unit
