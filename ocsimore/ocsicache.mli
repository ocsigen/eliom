(* Copyright Vincent Balat 2005 *)  


(** module Cache: access to database with cached data *)

(** The key must be the first field in the database *)

exception Ocsigen_Cache_error of string

module Make (A : sig 
	       type tvalue
	       val sql_t_list_to_tvalue : Dbi.sql_t list -> tvalue
	       val tvalue_to_sql_t_list : tvalue -> Dbi.sql_t list
	       val sql_fields_list_without_key : string list
	       val table : string 
	       val key : string 
	     end) :
sig
  val get : key:int -> A.tvalue
  val get_by_field : field:string -> value:Dbi.sql_t -> int * A.tvalue
  val insert : value:A.tvalue -> int
  val update : key:int -> value:A.tvalue -> unit
  val size : unit -> int
end
