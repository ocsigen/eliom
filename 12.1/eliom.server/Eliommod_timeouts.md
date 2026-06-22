
# Module `Eliommod_timeouts`

```ocaml
type kind = [ 
  | `Service
  | `Data
  | `Persistent
 ]
```
```ocaml
val set_default : 
  ?scope_hierarchy:Eliom_common.scope_hierarchy ->
  [< kind ] ->
  [< Eliom_common.cookie_level ] ->
  float option ->
  unit
```
```ocaml
val find_global : 
  [< kind ] ->
  Eliom_common.full_state_name ->
  Eliom_common.sitedata ->
  float option
```
```ocaml
val get_global : 
  kind:[< kind ] ->
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  secure:bool ->
  Eliom_common.sitedata ->
  float option
```
```ocaml
val set_global : 
  kind:[< kind ] ->
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  secure:bool ->
  recompute_expdates:bool ->
  bool ->
  Eliom_common.sitedata ->
  float option ->
  unit
```
```ocaml
val set_global_ : 
  ?full_st_name:Eliom_common.full_state_name ->
  ?cookie_level:[< Eliom_common.cookie_level ] ->
  kind:[< kind ] ->
  recompute_expdates:bool ->
  bool ->
  bool ->
  Eliom_common.sitedata ->
  float option ->
  unit
```
```ocaml
val set_default_global : 
  [< kind ] ->
  [< Eliom_common.cookie_level ] ->
  bool ->
  bool ->
  Eliom_common.sitedata ->
  float option ->
  unit
```