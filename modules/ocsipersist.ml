(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsipersist.ml
 * Copyright (C) 2007 Vincent Balat - Gabriel Kerneis
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


(** Module Ocsipersist: persistent data *)

open Sqlite3
open Lwt
open Printf

(** Data are divided into stores. 
   Create one store for your project, where you will save all your data.
 *)
type store = string

exception Ocsipersist_error


(*****************************************************************************)
(** Internal functions: storage directory *)

open Simplexmlparser
(** getting the directory from config file *)
let rec parse_global_config d = function
  | [] -> d
  | (Element ("database", [("file", s)], []))::ll -> 
      (match d with
      | None  -> parse_global_config (Some s) ll
      | (Some _) -> raise (Extensions.Error_in_config_file 
                             ("Ocsipersist: Duplicate <store> tag")))
  | (Element (tag,_,_))::ll -> parse_global_config d ll
  | _ -> raise (Extensions.Error_in_config_file
                  ("Unexpected content inside Ocsipersist config"))
        
let db_file = 
  match parse_global_config None (Extensions.get_config ()) with
  | None -> (Ocsiconfig.get_datadir ())^"/ocsidb"
  | Some d -> d



(*****************************************************************************)
(** Useful functions on database *)

let yield () = 
  Thread.yield ()

let rec bind_safely stmt = function
  | [] -> stmt
  | (value,name)::q as l ->
      match Sqlite3.bind stmt (bind_parameter_index stmt name) value with
      | Rc.OK -> bind_safely stmt q
      | Rc.BUSY | Rc.LOCKED -> yield () ; bind_safely stmt l 
      | rc -> ignore(finalize stmt) ; failwith (Rc.to_string rc)

let close_safely db = 
 if not (db_close db) then Messages.errlog "Couldn't close database"

let m = Mutex.create()

let exec_safely f = 
  let aux () = 
   let db = (Mutex.lock m ;db_open db_file) in 
    (try
      let r = f db in
      close_safely db ;
      Mutex.unlock m ;
      r
    with e -> (
      close_safely db ;
      Mutex.unlock m ;
      raise e))
  in
  Preemptive.detach aux ()

(* Référence indispensable pour les codes de retours et leur signification :
 * http://sqlite.org/capi3ref.html 
 * Langage compris par SQLite : http://www.sqlite.org/lang.html
 *)

let db_create table = 
  let sql = sprintf "CREATE TABLE IF NOT EXISTS %s (key TEXT, value BLOB,  PRIMARY KEY(key) ON CONFLICT REPLACE)" table in
  let create db = 
    let stmt = prepare db sql in
    let rec aux () = 
      match step stmt with 
      | Rc.DONE -> ignore(finalize stmt)  
      | Rc.BUSY | Rc.LOCKED ->  yield () ; aux ()
      | rc -> ignore(finalize stmt) ; failwith (Rc.to_string rc)
    in 
    aux ()
  in
  ignore (exec_safely create);
  table

let db_remove (table, key) =
  let sql =  sprintf "DELETE FROM %s WHERE key = :key " table in
  let remove db = 
    let stmt =  bind_safely (prepare db sql) [Data.TEXT key,":key"] in
    let rec aux () = 
      match step stmt with 
      | Rc.DONE -> ignore(finalize stmt)  
      | Rc.BUSY | Rc.LOCKED ->  yield () ; aux ()
      | rc -> ignore(finalize stmt) ; failwith (Rc.to_string rc)
    in aux ()
  in
  exec_safely remove

let (db_get, db_replace, db_replace_if_exists) =
  let get (table, key) db =
    let sqlget = sprintf "SELECT value FROM %s WHERE key = :key " table in
    let stmt = bind_safely (prepare db sqlget) [Data.TEXT key,":key"] in
    let rec aux () = 
      match step stmt with 
      | Rc.ROW -> 
          let value = match column stmt 0 with 
	  | Data.BLOB s -> s
	  | _ -> assert false 
	  in
  	  ignore (finalize stmt);  
 	  value
      | Rc.DONE -> ignore(finalize stmt) ;  raise Not_found
      | Rc.BUSY | Rc.LOCKED ->  yield () ; aux ()
      | rc -> ignore(finalize stmt) ; failwith (Rc.to_string rc)
    in aux ()
  in
  let replace (table, key) value db =
    let sqlreplace = sprintf "INSERT INTO %s VALUES ( :key , :value )" table in
    let stmt = 
      bind_safely
        (prepare db sqlreplace)
        [Data.TEXT key,":key"; Data.BLOB value, ":value"] 
    in
    let rec aux () = 
      match step stmt with 
      | Rc.DONE -> ignore(finalize stmt)
      | Rc.BUSY | Rc.LOCKED ->  yield () ; aux ()
      | rc -> ignore(finalize stmt) ; failwith (Rc.to_string rc)
    in aux ()
  in
  ((fun tablekey -> exec_safely (get tablekey)),
   (fun tablekey value -> exec_safely (replace tablekey value)),
   (fun tablekey value -> exec_safely 
       (fun db -> ignore (get tablekey db); replace tablekey value db)))


let db_iter_step table rowid =
  let sql = 
    sprintf "SELECT key , value , ROWID FROM %s WHERE ROWID > :rowid" table in
  let iter db = 
    let stmt = bind_safely (prepare db sql) [Data.INT rowid, ":rowid"] in
    let rec aux () = 
      match step stmt with 
      | Rc.ROW ->
          (match (column stmt 0,column stmt 1, column stmt 2) with 
	  | (Data.TEXT k, Data.BLOB v, Data.INT rowid) -> 
              ignore(finalize stmt) ; 
              Some (k, v, rowid)
	  | _ -> assert false )
      | Rc.DONE -> ignore(finalize stmt) ; None
      | Rc.BUSY | Rc.LOCKED -> yield () ; aux ()
      | rc -> ignore(finalize stmt) ; failwith (Rc.to_string rc)
    in aux ()
  in
  exec_safely iter
    
let db_iter_block table f =
  let sql = sprintf "SELECT key , value FROM %s " table in
  let iter db = 
    let stmt = prepare db sql in
    let rec aux () = 
      match step stmt with 
      | Rc.ROW -> 
          (match (column stmt 0,column stmt 1) with 
	  | (Data.TEXT k, Data.BLOB v) -> f k (Marshal.from_string v 0); aux()
	  | _ -> assert false )
      | Rc.DONE -> ignore(finalize stmt)
      | Rc.BUSY | Rc.LOCKED ->  yield () ; aux ()
      | rc -> ignore(finalize stmt) ; failwith (Rc.to_string rc)
    in aux ()
  in
  exec_safely iter
    
let db_length table =
  let sql = sprintf "SELECT count(*) FROM %s " table in
  let length db = 
    let stmt = prepare db sql in
    let rec aux () = 
      match step stmt with 
      | Rc.ROW -> 
          let  value = match column stmt 0 with 
	  | Data.INT s -> Int64.to_int s
	  | _ -> assert false 
          in
  	  ignore (finalize stmt);  
 	  value
      | Rc.DONE -> ignore(finalize stmt) ;  raise Not_found
      | Rc.BUSY | Rc.LOCKED ->  yield () ; aux ()
      | rc -> ignore(finalize stmt) ; failwith (Rc.to_string rc)
    in aux ()
  in
  exec_safely length




    
(*****************************************************************************)
(** Public functions: *)

(** Type of persistent data *)
type 'a t = store * string
      
let open_store name : store = 
  let s = "store___"^name in
  db_create s 

let make_persistent_lazy ~store ~name ~default =
  let pvname = (store, name) in
  (catch
     (fun () -> db_get pvname >>= (fun _ -> return ()))
     (function
       | Not_found -> 
           let def = Marshal.to_string (default ()) []
           in db_replace pvname def
       | e -> fail e)) >>=
  (fun () -> return pvname)
    
let make_persistent ~store ~name ~default = 
  make_persistent_lazy ~store ~name ~default:(fun () -> default)
    
let get (pvname : 'a t) : 'a =
  db_get pvname >>=
  (fun r -> return (Marshal.from_string r 0))
    
let set pvname v =
  let data = Marshal.to_string v [] in
  db_replace pvname data
    
(** Type of persistent tables *)
type 'value table = string
      
(** name SHOULD NOT begin with "store___" *)
let open_table name =  db_create name
    
let find table key =
  db_get (table, key) >>= fun v -> 
  return (Marshal.from_string v 0)

let add table key value =
  let data = Marshal.to_string value [] in
  db_replace (table, key) data

let replace_if_exists table key value =
  let data = Marshal.to_string value [] in
  db_replace_if_exists (table, key) data

let remove table key =
  db_remove (table, key)
  
let iter_step f table =
  let rec aux rowid =
    db_iter_step table rowid >>=
    (function
      | None -> return ()
      | Some (k,v,rowid') -> 
          f k (Marshal.from_string v 0) >>= (fun () -> aux rowid'))
  in
  aux Int64.zero
    
let fold_step f table beg =
  let rec aux rowid beg =
    db_iter_step table rowid >>=
    (function
      | None -> return beg
      | Some (k, v, rowid') -> 
          f k (Marshal.from_string v 0) beg >>= (fun res -> aux rowid' res))
  in
  aux Int64.zero beg
    
let iter_block f table =
  db_iter_block table f

let iter_table = iter_step

let fold_table = fold_step
    
let length table = 
  db_length table






