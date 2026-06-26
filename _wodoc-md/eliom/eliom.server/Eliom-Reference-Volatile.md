
# Module `Reference.Volatile`

Same functions as in `Reference` but a non-Lwt interface for non-persistent Eliom references.

```ocaml
type 'a eref = ('a, [ `Volatile ]) eref'
```
The type of volatile Eliom references. Note that `('a Reference.Volatile.eref :> 'a Reference.eref)`, i.e. wherever you can use an `'a Reference.eref` you can also use an `'a Reference.Volatile.eref :> 'a Reference.eref`.

```ocaml
val eref : scope:[< Common.all_scope ] -> ?secure:bool -> 'a -> 'a eref
```
```ocaml
val eref_from_fun : 
  scope:[< Common.all_scope ] ->
  ?secure:bool ->
  (unit -> 'a) ->
  'a eref
```
```ocaml
val get : 'a eref -> 'a
```
```ocaml
val set : 'a eref -> 'a -> unit
```
```ocaml
val modify : 'a eref -> ('a -> 'a) -> unit
```
```ocaml
val unset : 'a eref -> unit
```
```ocaml
module Ext : sig ... end
```
This module allows access to volatile references for other groups, sessions, or client processes. Use it in conjunction with functions like [`State.Ext.iter_volatile_sub_states`](./Eliom-State-Ext.md#val-iter_volatile_sub_states) to get the sessions from a group (or the processes from a session).
