type 'a index

module Dyn :
sig
  type t
  exception Dyn_duplicate_registering of string
  exception Dyn_typing_error_while_unfolding of (string * string)
end

module Rights :
sig
  type user
  type group
  type rights = (user * bool * bool) * (group * bool * bool) * (bool * bool)

  exception Read_Forbidden
  exception Write_Forbidden
  exception Permission_Denied
  exception Wrong_Password
  exception No_such_user

  val anonymoususer : user
  val root : user

  val connect : user:string -> password:string -> user
  val create_user : user:user -> login:string -> name:string -> 
    password:string -> user
  val create_group : user:user -> name:string -> description:string -> group
  val get_user_info : user:user -> string * string * (group list)
  val in_group : user:user -> group:group -> bool
  val put_user_in_group : user:user -> u:user -> group:group -> unit

end

module type SAVER =
sig
  type t
  val dbinsert : user:Rights.user -> ?rights:Rights.rights -> t -> t index
  val dbupdate : user:Rights.user -> key:t index -> value:t -> unit
  val dbget : user:Rights.user -> key:t index -> t
  val _index : 
    string ->
    (t index -> 'a, 'a, (t index Omlet.name -> 'b) -> 'b, 
      t index -> Omlet.formorlink) 
	Omlet.parameters
  val int_of_index : t index -> int
  val intname_of_indexname : t index Omlet.name -> int Omlet.name
end

module MakeSaver :
  functor (A : sig type t val name : string end) ->
    SAVER with type t = A.t

exception Box_not_available of string
exception Unfolds_not_registered of string

type tfolded

module type REGISTER =
sig
  type content
  type 'a t
  type box
  type boxes
  type tfolded_list

  exception Duplicate_registering of string
  
  val register : 
    name:string -> 
    constructor:(box_param:'boxparam -> content t) -> 'boxparam -> box

  val unfold : box ->content t
  val unfolds : boxes ->content t

  val dbget : user:Rights.user -> key:content t index -> content t

  val dbinsert : user:Rights.user -> ?rights:Rights.rights -> box -> 
    content t index
  val dbupdate : user:Rights.user -> key:content t index -> value:box -> unit

  val register_unfolds : box_constructor:(boxes -> content t) -> 
    boxes list -> tfolded_list

  val tfoldedlist_to_contentlistt : tfolded_list -> content list t

  val dbgetlist : user:Rights.user -> key:tfolded_list index -> 
    content list t
  val dbinsertlist : user:Rights.user -> ?rights:Rights.rights ->
    tfolded_list -> tfolded_list index
  val dbupdatelist : user:Rights.user -> key:tfolded_list index ->
    value:tfolded_list -> unit

end
  
module MakeRegister :
  functor (A : sig
      type content
      type 'a t
      type box
      type boxes
      val name : string
      val tag : tfolded -> box
      val untag : box -> tfolded
      val default_handler : exn -> content t
      val make_boxofboxes : filter:('a -> content t) -> 
	'a list -> content list t
  end) -> 
    REGISTER
with type 'a t = 'a A.t 
and type content = A.content
and type box = A.box
and type boxes = A.boxes

module StringMessage : SAVER with type t = string


