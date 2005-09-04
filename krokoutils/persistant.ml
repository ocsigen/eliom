(* Vincent Balat 2005 *)  

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
let make_persistant ~name ~default =
  try
    (name, ref (db_get name))
  with
      Not_found -> db_add name default; (name, ref default)

let get ((pvname, pvref) : 'a t) : 'a = !pvref

let set (pvname, pvref) v =
  pvref := v;
  db_store pvname v
