
# Module `Eliom.Mod_timeouts`

```ocaml
type kind = [ 
  | `Service
  | `Data
  | `Persistent
 ]
```
```ocaml
val set_default : 
  ?scope_hierarchy:Common.scope_hierarchy ->
  [< kind ] ->
  [< Common.cookie_level ] ->
  float option ->
  unit
```
```ocaml
val find_global : 
  [< kind ] ->
  Common.full_state_name ->
  Common.sitedata ->
  float option
```
```ocaml
val get_global : 
  kind:[< kind ] ->
  cookie_scope:[< Common.cookie_scope ] ->
  secure:bool ->
  Common.sitedata ->
  float option
```
```ocaml
val set_global : 
  kind:[< kind ] ->
  cookie_scope:[< Common.cookie_scope ] ->
  secure:bool ->
  recompute_expdates:bool ->
  bool ->
  Common.sitedata ->
  float option ->
  unit
```
```ocaml
val set_global_ : 
  ?full_st_name:Common.full_state_name ->
  ?cookie_level:[< Common.cookie_level ] ->
  kind:[< kind ] ->
  recompute_expdates:bool ->
  bool ->
  bool ->
  Common.sitedata ->
  float option ->
  unit
```
```ocaml
val set_default_global : 
  [< kind ] ->
  [< Common.cookie_level ] ->
  bool ->
  bool ->
  Common.sitedata ->
  float option ->
  unit
```