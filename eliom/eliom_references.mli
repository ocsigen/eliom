(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(*****************************************************************************)
(** {2 Server side state data: Eliom references} *)

(** Eliom references are some kind of references with limited scope.
    You define the reference with an initial value and a scope
    (group of sessions, session, client process, or current request).
    When you change the value, it actually changes only for the scope
    you specified.

    Eliom references are used for example to store session data,
    or server side data for a client process, or to keep some information
    about the current request.
*)

(** The type of Eliom references. *)
type 'a eref

(** Create an Eliom reference for the given scope (default: [`Session]).

    Use the optional parameter [?persistent] if you want the data to survive
    after relaunching the server. You must give an unique name to the
    table in which it will be stored on the hard disk (using Ocsipersist).
    Be very careful to use unique names, and to change the name if
    you change the type of the data.
    This parameter has no effect for scope [`Request].

    Use the optional parameter [?secure] if you want the data to be available
    only using HTTPS (default: false).

    Use the optional parameter [?state_name] if you want to distinguish
    between several server side states for the same scope.

    If you create the eref during a request, do not forget to give
    to [~sp] parameter.
*)
val eref :
  ?state_name:string ->
  ?scope:[ `Request | Eliom_common.user_scope ] ->
  ?secure:bool ->
  ?persistent:string ->
  'a -> 'a eref

(** Get the value of an Eliom reference. *)
val get : 'a eref -> 'a Lwt.t

(** Change the value of an Eliom reference. *)
val set : 'a eref -> 'a -> unit Lwt.t

(** Turn back to the default value 
    (by removing the entry in the server side table) *)
val unset : 'a eref -> unit Lwt.t
