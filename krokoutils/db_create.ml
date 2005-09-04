(* Vincent Balat 2005 *)    

(** Module Db_create *)
(** Initialize the connection to the db. *)

module DB = Dbi_postgres

let dbh = new DB.connection "krokobase";;
