
open Db_create

exception Cache_error of string

module Make = 
  functor (A : sig 
	     type tkey 
	     type tvalue 
	     val tkey_to_sql_t : tkey -> Dbi.sql_t
	     val sql_t_list_to_tvalue : Dbi.sql_t list -> tvalue
	     val tvalue_to_sql_t_list_without_key : tvalue -> Dbi.sql_t list
	     val tvalue_sql_set_string : string
	     val tvalue_sql_values_string : string
	     val table : string 
	     val key : string
	   end) ->
struct

  let get ~key =
    let sth = dbh#prepare ("SELECT * FROM "^A.table^" WHERE "^A.key^" = ?") in
      sth#execute [A.tkey_to_sql_t key];
      A.sql_t_list_to_tvalue (sth#fetch1 ())

  let get_by_field ~field ~value =
    let sth = dbh#prepare ("SELECT * FROM "^A.table^" WHERE "^field^" = ?") in
      sth#execute [value];
      A.sql_t_list_to_tvalue (sth#fetch1 ())

  let insert ~key ~value =
    let s = dbh#prepare ("INSERT INTO "^A.table^" VALUES "^A.tvalue_sql_values_string) in
    s#execute ((A.tkey_to_sql_t key)::(A.tvalue_to_sql_t_list_without_key value));
    dbh#commit ()

  let update ~key ~value =
    (dbh#prepare ("UPDATE "^A.table^" SET "^A.tvalue_sql_set_string^" WHERE "^A.key^" = ?"))#execute ((A.tvalue_to_sql_t_list_without_key value)@[A.tkey_to_sql_t key]);
    dbh#commit ()

end


module Make_with_automatic_key 
  (A : sig 
     type tvalue
     val sql_t_list_to_tvalue : Dbi.sql_t list -> tvalue
     val tvalue_to_sql_t_list_without_key : tvalue -> Dbi.sql_t list
     val tvalue_sql_set_string : string
     val tvalue_sql_values_string : string
     val table : string 
     val key : string 
   end) =
struct 
  module C = 
    Make (
      struct
	type tkey = int
	type tvalue = A.tvalue
	let tkey_to_sql_t = fun i -> `Int i
	let sql_t_list_to_tvalue = A.sql_t_list_to_tvalue
	let tvalue_to_sql_t_list_without_key = 
	  A.tvalue_to_sql_t_list_without_key
	let tvalue_sql_set_string = A.tvalue_sql_set_string
	let tvalue_sql_values_string = A.tvalue_sql_values_string
	let table = A.table
	let key = A.key
      end)

  let count = Persistant.make_persistant ("number_of_messages_"^C.table) 0

  let get = C.get

  let get_by_field = C.get_by_field

  let insert ~value = 
    let lastcount = Persistant.get count in
      print_int lastcount;
      C.insert lastcount value;
      Persistant.set count (lastcount + 1);
      lastcount

  let update = C.update

  let size () = Persistant.get count

end

