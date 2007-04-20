(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsipersist.mli
 * Copyright (C) 2007 Vincent Balat
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


(** Module Ocsipersist: persistent data in DBM database *)


(*****************************************************************************)
(** {2 Persistent references} *)
(** When launching the program, if the value exists on hard disk,
    it is loaded, otherwise it is initialised to the default value *)

(** Type of persistent data *)
type 'a t

(** Data are divided into stores. 
   Create one store for your project, where you will save all your data. *)
type store

(** Open a store (and create it if it does not exist)  *)
val open_store : string -> store

val make_persistent :
    store:store -> name:string -> default:'a -> 'a t Lwt.t
(** [make_persistent store name default] creates a persistent value
    named [name] in store [store]
    from database or create it with the default value [default] if it
    does not exist. *)

val make_persistent_lazy : 
    store:store -> name:string -> default:(unit -> 'a) -> 'a t Lwt.t
(** Same as make_persistent but the default value is evaluated only
    if needed
*)

val get : 'a t -> 'a Lwt.t
(** [get pv] gives the value of [pv] *)

val set : 'a t -> 'a -> unit Lwt.t
(** [set pv value] sets a persistent value [pv] to [value] *)

(*****************************************************************************)
(** {2 Persistent tables} *)

(** Type of persistent table *)
type 'value table

(** Open a table (and create it if it does not exist)  *)
val open_table : string -> 'value table

val find : 'value table -> string -> 'value Lwt.t
(** [find table key] gives the value associated to [key] *)

val add : 'value table -> string -> 'value -> unit Lwt.t
(** [add table key value] associates the value [value] to key [key]. 
   If the database already contains data associated with key, 
   that data is discarded and silently replaced by the new data.
 *)

val remove : 'value table -> string -> unit Lwt.t
(** [remove table key] removes the entry in the table *)

val remove_from_all_tables : string -> unit Lwt.t
(** removes the entry from all opened tables *)

val iter_table : (string -> 'a -> unit Lwt.t) -> 'a table -> unit Lwt.t
(** Important warning: this iterator may not iter on all data of the table
   if several iterator are running simultanously on the same table *)

val number_of_tables : unit -> int
(** Number of tables opened *)

val length : string -> int Lwt.t
(** Size of a table.
   Because of Dbm implementation, the result may be less thann the expected
   result in some case (with a version of ocsipersist based on Dbm) *)

val number_of_persistent_table_elements : unit -> (string * int) list Lwt.t
(** Whole number of elements in all tables, table by table *)
