(* Kroko
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

(* Warning: the cache is NOT implemented... *)

open Db_create

exception Cache_error of string

module Make = 
  functor (A : sig 
	     type tvalue 
	     val sql_t_list_to_tvalue : Dbi.sql_t list -> tvalue
	     val tvalue_to_sql_t_list : tvalue -> Dbi.sql_t list
	     val sql_fields_list_without_key : string list
	     val table : string 
	     val key : string
	   end) ->
struct

  let fields_string = 
    let rec aux = function
	[] -> ""
      | [a] -> a
      | a::l -> (a^", "^(aux l))
    in aux A.sql_fields_list_without_key

  let set_string = 
    let rec aux = function
	[] -> ""
      | [a] -> a^" = ?"
      | a::l -> (a^" = ?, "^(aux l))
    in aux A.sql_fields_list_without_key

  let question_marks_string =
    let rec aux = function
	[] -> ""
      | [_] -> "?"
      | _::l -> ("?, "^(aux l))
    in aux A.sql_fields_list_without_key

  let get ~key =
    let sth = dbh#prepare ("SELECT * FROM "^A.table^" WHERE "^A.key^" = ?") in
      sth#execute [`Int key];
      A.sql_t_list_to_tvalue (List.tl (sth#fetch1 ()))

  let get_by_field ~field ~value =
    let sth = dbh#prepare ("SELECT * FROM "^A.table^" WHERE "^field^" = ?") in
      sth#execute [value];
      match (sth#fetch1 ()) with
	  (`Int a)::l -> (a, (A.sql_t_list_to_tvalue l))
	| _ -> raise (Cache_error "get_by_field")

  let size () = 
    let s = dbh#prepare ("SELECT currval('"^A.table^"_"^A.key^"_seq"^"')") in
      s#execute [];
      match s#fetch1 () with
      | [`Bigint i] -> Big_int.int_of_big_int i
      | _ -> raise (Cache_error "size")

  let insert ~value =
    let s = dbh#prepare ("INSERT INTO "^A.table^" ("^fields_string^") VALUES ("^question_marks_string^")") in
      s#execute (A.tvalue_to_sql_t_list value);
      dbh#commit ();
      size ()
	
  let update ~key ~value =
    (dbh#prepare ("UPDATE "^A.table^" SET "^set_string^" WHERE "^A.key^" = ?"))#execute ((A.tvalue_to_sql_t_list value)@[`Int key]);
    dbh#commit ()

end

