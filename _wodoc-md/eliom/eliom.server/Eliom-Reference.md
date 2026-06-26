
# Module `Eliom.Reference`


### Server side state data, a.k.a Eliom references

**Please read the [Eliom manual](./../server-state.md) before this page to learn how server side state works.**

```ocaml
type ('a, +'storage) eref'
```
Eliom references come in two flavors: they may be stored persistently or the may be volatile. The module `Volatile` allows creation of references which can be, get, set, modify, and unset volatile references through *non-Lwt* functions.

```ocaml
type 'a eref = ('a, [ `Volatile | `Persistent ]) eref'
```
The type of Eliom references whose content is of type `'a`.

```ocaml
exception Eref_not_initialized
```
Exception raised when trying to access an eref that has not been initaliazed, when we don't want to initialize it.

```ocaml
val eref : 
  scope:[< Common.all_scope ] ->
  ?secure:bool ->
  ?persistent:(string * 'a Deriving_Json.t) ->
  'a ->
  'a eref
```
The function `eref ~scope value` creates an Eliom reference for the given `scope` and initialize it with `value`. See the Eliom manual for more information about [scopes](./../server-state.md).

Use the optional parameter `?persistent` if you want the data to survive after relaunching the server. The argument is a pair `(name, json)` where `name` is a unique table name used by Ocsipersist on disk, and `json` is a [`Deriving_Json.t`](./../../js_of_ocaml/js_of_ocaml.deriving/Deriving_Json.md#type-t) codec (typically `[%json: t]` when `t` has `[@@deriving json]`). Be very careful to use unique names, and to change the name if you change the type of the data. This parameter has no effect for scope [`Common.request_scope`](./Eliom-Common.md#type-request_scope).

Use the optional parameter `~secure:true` if you want the data to be available only using HTTPS. This parameter has no effect for scopes [`Common.global_scope`](./Eliom-Common.md#type-global_scope), [`Common.site_scope`](./Eliom-Common.md#type-site_scope), and [`Common.request_scope`](./Eliom-Common.md#type-request_scope). The default is `false`, but this default can be changed in configuration file (`<securecookies value="true"/>`). This option can be placed as global Eliom option (inside the `<extension>` tag which is loading Eliom), or in the local configuration of one site (inside the `<eliom>` tag). But in the latter case, it will only have effect on the actions performed after in the same `<site>` (and NOT on the top-level instructions of the modules loaded before).

*Warning: Eliom references of scope [`Common.global_scope`](./Eliom-Common.md#type-global_scope), [`Common.site_scope`](./Eliom-Common.md#type-site_scope) or [`Common.request_scope`](./Eliom-Common.md#type-request_scope) may be created at any time ; but for other scopes, they must be created when the site information is available to Eliom, that is, either during the initialization phase of the server (while reading the configuration file) or during a request. Otherwise, it will raise the exception [`Common.Site_information_not_available`](./Eliom-Common.md#exception-Site_information_not_available). If you are using static linking, you must delay the call to this function until the configuration file is read, using [`Service.register_eliom_module`](./Eliom-Service.md#val-register_eliom_module). Otherwise you will also get this exception.*

```ocaml
val eref_from_fun : 
  scope:[< Common.all_scope ] ->
  ?secure:bool ->
  ?persistent:(string * 'a Deriving_Json.t) ->
  (unit -> 'a) ->
  'a eref
```
The function `eref_from_fun` works like the above [`Reference.eref`](./#type-eref), but instead of providing a value for the initial content, a function `f` for *creating the initial content* is provided (cf. also [`Lazy.from_fun`](./../../ocaml-compiler/stdlib/Stdlib-Lazy.md#val-from_fun)).

In each scope, the function `f` is called for creating the value of the reference the first time the reference is read (by [`Reference.get`](./#val-get)), if the value has not been set explicitly before (by [`Reference.set`](./#val-set)).

```ocaml
val get : 'a eref -> 'a Lwt.t
```
The function `get eref` returns the current value of the Eliom reference `eref`.

*Warning: this function cannot be used outside of a service handler when `eref` has been created with a scope different of [`Common.global_scope`](./Eliom-Common.md#type-global_scope); it can neither be used outside of an Eliom module when `eref` has been created with scope [`Common.site_scope`](./Eliom-Common.md#type-site_scope)*

```ocaml
val set : 'a eref -> 'a -> unit Lwt.t
```
The function `set eref v` set `v` as current value of the Eliom reference `eref`.

*Warning: this function could not be used outside af a service handler when `eref` has been created with a scope different of [`Common.global_scope`](./Eliom-Common.md#type-global_scope); it can neither be used outside of an Eliom module when `eref` has been created with scope [`Common.site_scope`](./Eliom-Common.md#type-site_scope)*

```ocaml
val modify : 'a eref -> ('a -> 'a) -> unit Lwt.t
```
The function `modify eref f` modifies the content of the Eliom reference `eref` by applying the function `f` on it.

*Warning: this function could not be used outside af a service handler when `eref` has been created with a scope different of [`Common.global_scope`](./Eliom-Common.md#type-global_scope); it can neither be used outside of an Eliom module when `eref` has been created with scope [`Common.site_scope`](./Eliom-Common.md#type-site_scope)*

```ocaml
val unset : 'a eref -> unit Lwt.t
```
The function `unset eref` reset the content of the Eliom reference `eref` to its initial value.

*Warning: this function could not be used outside af a service handler when `eref` has been created with a scope different of [`Common.global_scope`](./Eliom-Common.md#type-global_scope); it can neither be used outside of an Eliom module when `eref` has been created with scope [`Common.site_scope`](./Eliom-Common.md#type-site_scope)*

```ocaml
module Volatile : sig ... end
```
Same functions as in `Reference` but a non-Lwt interface for non-persistent Eliom references.

```ocaml
module Ext : sig ... end
```
This module allows access to references for other groups, sessions, or client processes. Use it in conjunction with functions like [`State.Ext.iter_sub_states`](./Eliom-State-Ext.md#val-iter_sub_states) to get the sessions from a group (or the processes from a session).
