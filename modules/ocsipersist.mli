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
(** When launching the program, if the value exists on hard disk,
    it is loaded, otherwise it is initialised to the default value *)


(*****************************************************************************)
(** {2 Persistent references} *)

(** Type of persistent data *)
type 'a t

(** Data are divided into stores. 
   Create one store for your project, where you will save all your data. *)
type store

(** Open a store (and create it if it does not exist)  *)
val open_store : name:string list -> store

val make_persistent :
    store:store -> name:string -> default:'a -> 'a t
(** [make_persistent store name default] creates a persistent value
    named [name] in store [store]
    from database or create it with the default value [default] if it
    does not exist. *)

val make_persistent_lazy : 
    store:store -> name:string -> default:(unit -> 'a) -> 'a t
(** Same as make_persistent but the default value is evaluated only
    if needed
*)

val get : 'a t -> 'a
(** [get pv] gives the value of [pv] *)

val set : 'a t -> 'a -> unit
(** [set pv value] sets a persistent value [pv] to [value] *)

(*****************************************************************************)
(** {2 Persistent tables} *)

(** Type of persistent table *)
type ('key, 'value) table

(** Open a table (and create it if it does not exist)  *)
val open_table_ : 
    name:string list -> key_to_string:('key -> string) -> ('key, 'value) table

(** Open a table (and create it if it does not exist)  *)
val open_table : name:string list -> (string, 'value) table

val find : ('key, 'value) table -> 'key -> 'value
(** [find table key] gives the value associated to [key] *)

val add : ('key, 'value) table -> 'key -> 'value -> unit
(** [add table key value] associates the value [value] to key [key]. 
   If the database already contains data associated with key, 
   that data is discarded and silently replaced by the new data.
 *)

val remove : ('key, 'value) table -> 'key -> unit
(** [remove table key] removes the entry in the table *)

val remove_from_all_tables : key:string -> unit
(** removes the entry from all opened tables *)

val iter_table : (string -> 'b -> 'a) -> (string, 'b) table -> unit
(** removes the entry from all opened tables *)


(*
val list_tables : dir:string list -> string list
*)
