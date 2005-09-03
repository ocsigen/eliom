(* Copyright Vincent Balat 2005 *)  


(** module Cache: access to database with cached data *)

(** The key must be the first field in the database *)

exception Cache_error of string

module Make (A : sig 
	       type tkey
	       type tvalue
	       val tkey_to_sql_t : tkey -> Dbi.sql_t
	       val sql_t_list_to_tvalue : Dbi.sql_t list -> tvalue
	       val tvalue_to_sql_t_list_without_key : tvalue -> Dbi.sql_t list
	       val tvalue_sql_set_string : string
	       val tvalue_sql_values_string : string
	       val table : string 
	       val key : string 
	     end) :
sig
  val get : key:A.tkey -> A.tvalue
  val get_by_field : field:string -> value:Dbi.sql_t -> A.tvalue
  val insert : key:A.tkey -> value:A.tvalue -> unit
  val update : key:A.tkey -> value:A.tvalue -> unit
end
(** [tvalue_sql_set_string] has the shape ["field1 = ?, field2 = ?"] in the
case the table has a key field and two fields named [field1] and [field2].
It is used in the Dbi module for UPDATE sql query.
[tvalue_sql_values_string] has the shape ["(?, ?, ?)"] in the
case the table has a key field and two others fields.
It is used in the Dbi module for INSERT sql query.
*)


module Make_with_automatic_key 
  (A : sig 
     type tvalue
     val sql_t_list_to_tvalue : Dbi.sql_t list -> tvalue
     val tvalue_to_sql_t_list_without_key : tvalue -> Dbi.sql_t list
     val tvalue_sql_set_string : string
     val tvalue_sql_values_string : string
     val table : string 
     val key : string 
   end) :
sig
  val get : key:int -> A.tvalue
  val get_by_field : field:string -> value:Dbi.sql_t -> A.tvalue
  val insert : value:A.tvalue -> int
  val update : key:int -> value:A.tvalue -> unit
  val size : unit -> int
end
