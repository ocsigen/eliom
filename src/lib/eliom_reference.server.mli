(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** {2 Server side state data, a.k.a Eliom references} *)

(** {b Please read the
    {% <<a_manual chapter="server-state" | Eliom manual >>%}
    before this page to learn how server side state works. }

*)

(** Eliom references come in two flavors: they may be stored persistently or
    the may be volatile.  The module [Volatile] allows creation of
    references which can be, get, set, modify, and unset volatile references
    through {e non-Lwt} functions. *)
type ('a, +'storage) eref'

(** The type of Eliom references whose content is of type ['a].  *)
type 'a eref = ('a, [ `Volatile | `Persistent ]) eref'

(** Exception raised when trying to access an eref
    that has not been initaliazed, when we don't want to initialize it. *)
exception Eref_not_initialized

(** The function [eref ~scope value] creates an Eliom reference for
    the given [scope] and initialize it with [value]. See the Eliom
    manual for more information about {% <<a_manual
    chapter="server-state"|scopes>>%}.

    Use the optional parameter [?persistent] if you want the data to
    survive after relaunching the server. You must give an unique name
    to the table in which it will be stored on the hard disk (using
    Ocsipersist).  Be very careful to use unique names, and to change
    the name if you change the type of the data, otherwise the server
    may crash (unsafe unmarshaling). This parameter has no effect for
    scope {!Eliom_common.request_scope}.

    Use the optional parameter [~secure:true] if you want the data to
    be available only using HTTPS. This parameter has no effect for
    scopes {!Eliom_common.global_scope}, {!Eliom_common.site_scope}, and
    {!Eliom_common.request_scope}. The default is [false], but this default
    can be changed in configuration file ([<securecookies value="true"/>]).
    This option can be placed as global Eliom option (inside the [<extension>]
    tag which is loading Eliom), or in the local configuration of one site
    (inside the [<eliom>] tag). But in the latter case,
    it will only have effect on the actions performed after in the same [<site>]
    (and NOT on the top-level instructions of the modules loaded before).

    {e Warning: Eliom references of scope {!Eliom_common.global_scope},
    {!Eliom_common.site_scope} or
    {!Eliom_common.request_scope} may be created at any time ; but for other
    scopes, they must be created when the site information is
    available to Eliom, that is, either during the initialization
    phase of the server (while reading the configuration file) or
    during a request. Otherwise, it will raise the exception
    {!Eliom_common.Eliom_site_information_not_available}. If you are
    using static linking, you must delay the call to this function
    until the configuration file is read, using
    {!Eliom_service.register_eliom_module}. Otherwise you will also
    get this exception.}
*)
val eref :
  scope:[< Eliom_common.all_scope ] ->
  ?secure:bool ->
  ?persistent:string ->
  'a ->
  'a eref

(** The function [eref_from_fun] works like the above {!Eliom_reference.eref},
    but instead of providing a value for the initial content, a function [f] for
    {e creating the initial content} is provided (cf. also {!Lazy.from_fun}).

    In each scope, the function [f] is called for creating the value of the
    reference the first time the reference is read (by {!Eliom_reference.get}),
    if the value has not been set explicitly before (by {!Eliom_reference.set}).
  *)
val eref_from_fun :
  scope:[< Eliom_common.all_scope ] ->
  ?secure:bool ->
  ?persistent:string ->
  (unit -> 'a) ->
  'a eref

(** The function [get eref] returns the current value of the Eliom
    reference [eref].

    {e Warning: this function cannot be used outside of a service
    handler when [eref] has been created with a scope different of
    {!Eliom_common.global_scope}; it can neither be used outside of an
    Eliom module when [eref] has been created with scope
    {!Eliom_common.site_scope}}
  *)
val get : 'a eref -> 'a Lwt.t
(* That function introduces a Lwt cooperation point only for persistent
   references. *)

(** The function [set eref v] set [v] as current value of the Eliom
    reference [eref].

    {e Warning: this function could not be used outside af a service
    handler when [eref] has been created with a scope different of
    {!Eliom_common.global_scope}; it can neither be used outside of an
    Eliom module when [eref] has been created with scope
    {!Eliom_common.site_scope}}
  *)
val set : 'a eref -> 'a -> unit Lwt.t
(* That function introduces a Lwt cooperation point only for persistent
   references. *)

(** The function [modify eref f] modifies the content of the Eliom
    reference [eref] by applying the function [f] on it.

    {e Warning: this function could not be used outside af a service
    handler when [eref] has been created with a scope different of
    {!Eliom_common.global_scope}; it can neither be used outside of an
    Eliom module when [eref] has been created with scope
    {!Eliom_common.site_scope}}
  *)
val modify : 'a eref -> ('a -> 'a) -> unit Lwt.t
(* That function introduces a Lwt cooperation point only for persistent
   references. *)

(** The function [unset eref] reset the content of the Eliom reference
    [eref] to its initial value.

    {e Warning: this function could not be used outside af a service
    handler when [eref] has been created with a scope different of
    {!Eliom_common.global_scope}; it can neither be used outside of an
    Eliom module when [eref] has been created with scope
    {!Eliom_common.site_scope}}
  *)
val unset : 'a eref -> unit Lwt.t
(* That function introduces a Lwt cooperation point only for persistent
   references. *)

(** Same functions as in [Eliom_reference] but a non-Lwt interface
    for non-persistent Eliom references. *)
module Volatile : sig
  (** The type of volatile Eliom references.
      Note that [('a Eliom_reference.Volatile.eref :> 'a Eliom_reference.eref)], i.e. wherever you can use an ['a
      Eliom_reference.eref] you can also use an ['a Eliom_reference.Volatile.eref :> 'a Eliom_reference.eref].  *)
  type 'a eref = ('a, [`Volatile]) eref'
  val eref : scope:[< Eliom_common.all_scope] -> ?secure:bool -> 'a -> 'a eref
  val eref_from_fun : scope:[< Eliom_common.all_scope] -> ?secure:bool -> (unit -> 'a) -> 'a eref
  val get : 'a eref -> 'a
  val set : 'a eref -> 'a -> unit
  val modify : 'a eref -> ('a -> 'a) -> unit
  val unset : 'a eref -> unit

  module Ext : sig
    (** This module allows access to volatile references for other groups,
        sessions, or client processes.
        Use it in conjunction with functions like
        {!Eliom_state.Ext.iter_volatile_sub_states}
        to get the sessions from a group (or the processes from a session).
    *)

    (** get the value of a reference from outside the state.
        If the value has not been set yet for this state,
        it will raise exception [Eref_not_initialized].
    *)
    val get : ([< `Session_group | `Session | `Client_process ],
               [< `Data ]) Eliom_state.Ext.state ->
      'a eref -> 'a
    val set : ([< `Session_group | `Session | `Client_process ],
               [< `Data ]) Eliom_state.Ext.state ->
      'a eref -> 'a -> unit

    (** Warning: the function will be executed with the current context *)
    val modify :
      ([< `Session_group | `Session | `Client_process ],
       [< `Data ]) Eliom_state.Ext.state ->
      'a eref -> ('a -> 'a) -> unit

    val unset :
      ([< `Session_group | `Session | `Client_process ],
       [< `Data ]) Eliom_state.Ext.state ->
      'a eref -> unit

  end

end


(** This module allows access to references for other groups,
    sessions, or client processes.
    Use it in conjunction with functions like
    {!Eliom_state.Ext.iter_sub_states}
    to get the sessions from a group (or the processes from a session).
*)
module Ext : sig

  (** get the value of a reference from outside the state.
      If the value has not been set yet for this state,
      it will raise exception [Eref_not_initialized].
  *)
  val get : ([< `Session_group | `Session | `Client_process ],
             [< `Data | `Pers ]) Eliom_state.Ext.state ->
    'a eref -> 'a Lwt.t

  val set :
    ([< `Session_group | `Session | `Client_process ],
     [< `Data | `Pers ]) Eliom_state.Ext.state ->
    'a eref -> 'a -> unit Lwt.t

  (** Warning: the function will be executed with the current context *)
  val modify :
    ([< `Session_group | `Session | `Client_process ],
     [< `Data | `Pers ]) Eliom_state.Ext.state ->
    'a eref -> ('a -> 'a) -> unit Lwt.t

  val unset :
    ([< `Session_group | `Session | `Client_process ],
     [< `Data | `Pers ]) Eliom_state.Ext.state ->
    'a eref -> unit Lwt.t
end
