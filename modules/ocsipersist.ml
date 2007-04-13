(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsipersist.ml
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


(** Module Ocsipersist: persistent data *)
(** Warning! These functions are not cooperative! *)

open Dbm




(** Data are divided into stores. 
   Create one store for your project, where you will save all your data.
 *)
type store = Dbm.t

(*****************************************************************************)
(** Internal functions: storage directory *)

open Simplexmlparser.ExprOrPatt
(** getting the directory from config file *)
let rec parse_global_config d = function
      PLEmpty -> d
    | PLCons 
        (EPanytag 
           ("store", 
            (PLCons
               ((EPanyattr (EPVstr("dir"), EPVstr(s))), 
                PLEmpty)),
            PLEmpty), ll) -> 
              (match d with
              | None -> parse_global_config (Some s) ll
              | Some _ -> raise (Extensions.Error_in_config_file 
                                   ("Ocsipersist: Duplicate <store> tag")))
    | PLCons ((EPcomment _), l) -> parse_global_config d l
    | PLCons ((EPwhitespace _), l) -> parse_global_config d l
    | PLCons ((EPanytag (tag,_,_)),l) -> d
    | _ -> raise (Extensions.Error_in_config_file ("Unexpected content inside Ocsipersist config"))

let create_dirs l = 
  let rec aux beg = function
    | [] -> ()
    | [a] -> ()
    | a::l -> 
        let dirname= beg^"/"^a in
        (try
          Unix.access dirname [Unix.R_OK; Unix.W_OK; Unix.X_OK; Unix.F_OK]
        with
        | Unix.Unix_error (Unix.ENOENT, _, _) -> Unix.mkdir dirname 0o700);
        aux dirname l       
  in aux "" l

let directory = 
  match parse_global_config None (Extensions.get_config ()) with
    None -> (Ocsiconfig.get_datadir ())^"/ocsipersist"
  | Some d -> d

(*****************************************************************************)
(** Internal functions: storage in files using DBM *)

let open_db name =
  create_dirs (directory::name);
  opendbm 
    (Ocsimisc.string_of_url_path (directory::name))
    [Dbm_rdwr; Dbm_create] 0o644

let db_add (store, name) value =
  add store name value

let db_get (store, name) =
  find store name

let db_remove (store, name) =
  remove store name

let db_replace (store, name) value = 
  replace store name value



(*****************************************************************************)
(** Public functions: *)

(** Type of persistent data *)
type 'a t = Dbm.t * string
      
let open_store ~name : store = open_db name

let make_persistent_lazy ~store ~name ~default =
  let pvname = (store, name) in
  (try
    ignore (db_get pvname)
  with
    Not_found -> 
      let def = Marshal.to_string (default ()) []
      in db_add pvname def); 
  pvname
      
let make_persistent ~store ~name ~default = 
  make_persistent_lazy ~store ~name ~default:(fun () -> default)
    
let get (pvname : 'a t) : 'a =
  Marshal.from_string (db_get pvname) 0
    
let set pvname v =
  let data = Marshal.to_string v [] in
  db_replace pvname data



(** Type of persistent tables *)
type ('key, 'value) table = Dbm.t * ('key -> string)
      
module Tableoftables = 
  struct 
    let empty = []
    let add v t = v::t
    let iter = List.iter
  end

let tableoftables = ref Tableoftables.empty

let open_table_ ~name ~key_to_string = 
  let t = open_db name in
  tableoftables := Tableoftables.add t !tableoftables;
  (t, key_to_string)
    
let open_table ~name = open_table_ name Ocsimisc.id
    
let find (table, to_string) key =
  Marshal.from_string (db_get (table, to_string key)) 0
    
let add (table, to_string) key value =
  let data = Marshal.to_string value [] in
  db_replace (table, (to_string key)) data

let remove (table, to_string) key =
  try
    db_remove (table, to_string key)
  with _ -> ()

let remove_from_all_tables ~key =
  Tableoftables.iter (fun t -> db_remove (t, key)) !tableoftables

let iter_table f (t, to_string) = 
  Dbm.iter (fun k v -> f k (Marshal.from_string v 0)) t

(*
let list_tables ~dir =
  let d = Unix.opendir (directory^(Ocsimisc.string_of_url_path dir)) in
  let rec aux () =
    try
      (Unix.readdir d)::(aux ())
    with End_of_file -> []
  in aux ()
  
*)
