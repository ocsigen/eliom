(* Ocsigen
 * Copyright (C) 2011 Pierre Chambart
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

(** This module provides tables to manipulates Obj.t values. It
    handles automaticaly the rebuilding of tables on gc
    compactions. *)

val sons : Obj.t -> ( int * Obj.t ) list
(** [sons v] are the caml values descendents of v and their position *)

type table

val make : Obj.t -> table
(** [make v] make a table containing [v] and all its descendents. The
    table is initialised with no value marked as [no_copy] and with
    [v] as root *)

val add : table -> Obj.t -> unit
(** [add t v] add the value [v] and all its descendents to the table
    [t]. It is necessary to add a value before using it to replace
    another value. *)

val copy : table -> table
(** [copy t f] copies all the values of t such that were not set to be
    no copiable. else, the value is referenced in the new table but not
    copied. *)

val root : table -> Obj.t
(** returns the root of the table *)

val find_ancessors : table -> Obj.t -> ( int * Obj.t ) list
(** [find_ancessors t v] list all values with reference to [v] in the
    table [t]. It is presented as a list of couples [(field,value)] *)

val mem : table -> Obj.t -> bool
(** Tells wether a value is present in the table. *)

val replace : table -> Obj.t -> Obj.t -> unit
(** replace a value in the table. You should copy the table before
    replacing some values in it. After replacing values, some old
    values can become unreachable from the root in the table. They are
    not cleaned. To do this, you should build a clean table from the
    root. *)

val no_copy : table -> Obj.t -> unit
(** mark a value as not copiable in the table. *)
