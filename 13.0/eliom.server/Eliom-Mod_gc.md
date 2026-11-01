# Module `Eliom.Mod_gc`

```ocaml
val servicesessiongcfrequency : float option ref
```
```ocaml
val datasessiongcfrequency : float option ref
```
```ocaml
val persistentsessiongcfrequency : float option ref
```
```ocaml
val set_servicesessiongcfrequency : float option -> unit
```
```ocaml
val set_datasessiongcfrequency : float option -> unit
```
```ocaml
val get_servicesessiongcfrequency : unit -> float option
```
```ocaml
val get_datasessiongcfrequency : unit -> float option
```
```ocaml
val set_persistentsessiongcfrequency : float option -> unit
```
```ocaml
val get_persistentsessiongcfrequency : unit -> float option
```
```ocaml
val service_session_gc : Common.sitedata -> unit
```
```ocaml
val data_session_gc : Common.sitedata -> unit
```
```ocaml
val persistent_session_gc : Common.sitedata -> unit
```
```ocaml
val collect_service_sessions : Common.sitedata -> unit Lwt.t
```
`collect_service_sessions sitedata` removes the expired service sessions and coservices of `sitedata`, and the service sessions left unused: one run of the collector started by [`service_session_gc`](./#val-service_session_gc).

```ocaml
val collect_data_sessions : Common.sitedata -> unit Lwt.t
```
`collect_data_sessions sitedata` removes the expired volatile data sessions of `sitedata`, and the sessions left unused: one run of the collector started by [`data_session_gc`](./#val-data_session_gc).

```ocaml
val collect_persistent_sessions : Common.sitedata -> unit Lwt.t
```
`collect_persistent_sessions sitedata` removes the expired persistent sessions of `sitedata`: one run of the collector started by [`persistent_session_gc`](./#val-persistent_session_gc).

```ocaml
val section : Logs.src
```
