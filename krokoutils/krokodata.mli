module Dyn :
  sig
    type t
    exception Dyn_duplicate_registering
    exception Dyn_typing_error_while_unfolding
    val register : string -> ('a -> t) * (t -> 'a)
    val tag : t -> string
  end

module DyntCache :
  sig
    val get : key:int -> Dyn.t
    val insert : value:Dyn.t -> int
    val update : key:int -> value:Dyn.t -> unit
    val size : unit -> int
  end

val dbinsertdyn : value:Dyn.t -> int
val dbupdatedyn : key:int -> value:Dyn.t -> unit
val dbgetdyn : key:int -> Dyn.t

module MakeSaver :
  functor (A : sig type t val name : string val default_content : t end) ->
    sig
      val dbinsert : A.t -> int
      val dbupdate : key:int -> value:A.t -> unit
      val dbget : key:int -> A.t
    end



type saved_obj

module ObjCache :
  sig
    val get : key:int -> saved_obj
    val insert : value:saved_obj -> int
    val update : key:int -> value:saved_obj -> unit
    val size : unit -> int
  end

module type REGISTER =
  sig
    type t
    exception Duplicate_registering
    val register :
      name:string ->
      decode:('a -> ('a -> saved_obj) -> t) -> 'a -> saved_obj
    val get : saved_obj -> t
  end

module MakeRegister :
  functor (A : sig type t end) -> REGISTER with type t = A.t

val dbinsertobj : value:< save : saved_obj; .. > -> int
val dbupdateobj : key:int -> value:< save : saved_obj; .. > -> unit

class virtual savable :
  ('a -> saved_obj) -> object method virtual save : saved_obj end

class savable_data :
  'a -> ('a -> saved_obj) -> object method save : saved_obj end


module StringMessage :
  sig
    val dbinsert : string -> int
    val dbupdate : key:int -> value:string -> unit
    val dbget : key:int -> string
  end

module MessagesList :
  sig
    val dbinsert : int list -> int
    val dbupdate : key:int -> value:int list -> unit
    val dbget : key:int -> int list
  end

