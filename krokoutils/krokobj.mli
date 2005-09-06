open Krokodata

class virtual ['a] generic_item : object method virtual print : 'a end

class box : object method print : Xhtmlpp.xhtmlcont end

class savable_box :
  'a ->
  ('a -> saved_obj) ->
  object method print : Xhtmlpp.xhtmlcont method save : saved_obj end

(*
module RegisterBox :
  sig
    type t = savable_box
    exception Duplicate_registering
    val register :
      name:string ->
      decode:('a -> ('a -> saved_obj) -> t) -> 'a -> saved_obj
    val get : saved_obj -> t
  end
*)
val new_savable_box : unit -> savable_box

class data_box :
  'a ->
  ('a -> Xhtmlpp.xhtmlcont) -> object method print : Xhtmlpp.xhtmlcont end

class savable_data_box :
  'a ->
  ('a -> Xhtmlpp.xhtmlcont) ->
  ('a -> saved_obj) ->
  object method print : Xhtmlpp.xhtmlcont method save : saved_obj end

val constructor_for_new_savable_data_box :
  string -> ('a -> Xhtmlpp.xhtmlcont) -> 'a -> savable_data_box

val new_title_box : string -> savable_data_box

val new_error_box : string -> savable_data_box

val get_box : saved_obj -> savable_box

val dbget_box : key:int -> savable_box

class page :
  (< print : Xhtmlpp.xhtmlcont; .. > as 'a) list ->
  object val boxlist : 'a list method print : Xhtmlpp.xhtml end

class savable_page :
  savable_box list ->
  (saved_obj list -> saved_obj) ->
  object
    val boxlist : savable_box list
    method print : Xhtmlpp.xhtml
    method save : saved_obj
  end

val new_savable_page : savable_box list -> savable_page

val dbinsert_page : savable_box list -> int

val get_page : saved_obj -> savable_page

val dbget_page : key:int -> savable_page

module MakeNewMessage :
  functor (A : sig type t val name : string val default_content : t end) ->
    sig
      val dbinsert_message : A.t -> int
      val dbupdate_message : key:int -> value:A.t -> unit
      val dbget_message : key:int -> A.t
    end

module StringMessage :
  sig
    val dbinsert_message : string -> int
    val dbupdate_message : key:int -> value:string -> unit
    val dbget_message : key:int -> string
  end

val new_string_message_box : int -> savable_data_box
