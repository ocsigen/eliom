(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)


(** Module Persistant: persistant data *)
(** Data are kept in memory but all modifications are stored in the database *)
(** When launching the program, if the value exists in the database,
    it is loaded, otherwise it is initialised to the default value *)

(** Type of persistant data *)
type 'a t = string * 'a ref

open Db_create

(** Database access: *)
let db_add key value =
  let s =dbh#prepare "INSERT INTO globalstore (key,value) VALUES (?,?)" in
    s#execute [`String key; `Binary (Marshal.to_string value [])]; 
    dbh#commit ()

let db_get key = 
  let sth = dbh#prepare "SELECT value FROM globalstore WHERE key = ?" in
    sth#execute [`String key];
    match sth#fetch1() with
      | [`Binary name] -> Marshal.from_string name 0
      | _ -> assert false 

let db_store key value = 
  (dbh#prepare ("UPDATE globalstore SET value = ? WHERE key = ?"))#execute [`Binary (Marshal.to_string value []); `String key]; 
  dbh#commit ()



(** Public functions: *)
let make_persistant_lazy ~name ~default =
  try
    (name, ref (db_get name))
  with
      Not_found -> let def = default ()
      in db_add name def; (name, ref def)

let make_persistant ~name ~default = 
  make_persistant_lazy name (fun () -> default)

let get ((pvname, pvref) : 'a t) : 'a = !pvref

let set (pvname, pvref) v =
  pvref := v;
  db_store pvname v
