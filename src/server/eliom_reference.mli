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

(** Eliom references come in two flavors: they may be stored persistently or
    the may be volatile.  The module [Volatile] allows one to creation of
    references which can be, get, set, modify, and unset volatile references
    through {e non-Lwt} functions. *)
type ('a, +'storage) eref'

(** The type of Eliom references whose content is of type ['a].  *)
type 'a eref = ('a, [ `Volatile | `Persistent ]) eref'

(** The function [eref ~scope value] creates an Eliom reference for
    the given [scope] and initialize it with [value]. See the Eliom
    manual for more information about {% <<a_manual
    chapter="state"|scopes>>%}.

    Use the optional parameter [?persistent] if you want the data to
    survive after relaunching the server. You must give an unique name
    to the table in which it will be stored on the hard disk (using
    Ocsipersist).  Be very careful to use unique names, and to change
    the name if you change the type of the data, otherwise the server
    may crash (unsafe unmarshaling). This parameter has no effect for
    scope {!Eliom_common.request}.

    Use the optional parameter [~secure:true] if you want the data to
    be available only using HTTPS. This parameter has no effect for
    scopes {!Eliom_common.global}, {!Eliom_common.site}, and
    {!Eliom_common.request}.

    {e Warning: Eliom references of scope {!Eliom_common.global}, {!Eliom_common.site} or
    {!Eliom_common.request} may be created at any time ; but for other
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
    {e creating the initial content} is provided (cf. also {!Lazy.lazy_from_fun}).

    In each scope, the function [f] is called for creating the value of the
    reference the first time the reference is read (by {!Eliom_reference.get}),
    if the value has not been set explicitly before (by {!Eliom_reference.set});
    or if the reference was reset (by {!Eliom_reference.reset}) before.
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
    {!Eliom_common.global}; it can neither be used outside of an
    Eliom module when [eref] has been created with scope
    {!Eliom_common.site}}
  *)
val get : 'a eref -> 'a Lwt.t
(* That function introduces a Lwt cooperation point only for persistent
   references. *)

(** The function [set eref v] set [v] as current value of the Eliom
    reference [eref].

    {e Warning: this function could not be used outside af a service
    handler when [eref] has been created with a scope different of
    {!Eliom_common.global}; it can neither be used outside of an
    Eliom module when [eref] has been created with scope
    {!Eliom_common.site}}
  *)
val set : 'a eref -> 'a -> unit Lwt.t
(* That function introduces a Lwt cooperation point only for persistent
   references. *)

(** The function [modify eref f] modifies the content of the Eliom
    reference [eref] by applying the function [f] on it.

    {e Warning: this function could not be used outside af a service
    handler when [eref] has been created with a scope different of
    {!Eliom_common.global}; it can neither be used outside of an
    Eliom module when [eref] has been created with scope
    [!Eliom_common.site]}
  *)
val modify : 'a eref -> ('a -> 'a) -> unit Lwt.t
(* That function introduces a Lwt cooperation point only for persistent
   references. *)

(** The function [unset eref] reset the content of the Eliom reference
    [eref] to its initial value.

    {e Warning: this function could not be used outside af a service
    handler when [eref] has been created with a scope different of
    {!Eliom_common.global}; it can neither be used outside of an
    Eliom module when [eref] has been created with scope
    [!Eliom_common.site]}
  *)
val unset : 'a eref -> unit Lwt.t
(* That function introduces a Lwt cooperation point only for persistent
   references. *)

(** Same functions as in [Eliom_reference] but a non-Lwt interface
    for non-persistent Eliom references. *)
module Volatile : sig
  (** The type of volatile Eliom references.
      Note that [('a Eliom_reference.Volatile.eref :> 'a Eliom_reference.eref)], i.e. whereever you can use an ['a
      Eliom_reference.eref] you can also use an ['a Eliom_reference.Volatile.eref :> 'a Eliom_reference.eref].  *)
  type 'a eref = ('a, [`Volatile]) eref'
  val eref : scope:[< Eliom_common.all_scope] -> ?secure:bool -> 'a -> 'a eref
  val eref_from_fun : scope:[< Eliom_common.all_scope] -> ?secure:bool -> (unit -> 'a) -> 'a eref
  val get : 'a eref -> 'a
  val set : 'a eref -> 'a -> unit
  val modify : 'a eref -> ('a -> 'a) -> unit
  val unset : 'a eref -> unit
end
