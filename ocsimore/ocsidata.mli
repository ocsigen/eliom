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
  type resource
  type rights = user list * user list * resource list * resource list
  type 'a protected

  exception Read_Forbidden_for_user
  exception Write_Forbidden_for_user
  exception Read_Forbidden_for_resource
  exception Write_Forbidden_for_resource
  exception Permission_Denied
  exception Wrong_Password
  exception No_such_user

  val anonymoususer : user
  val root : user
  val anyresource : resource
  val users : user

  val connect : user:string -> password:string -> user
  val create_user : login:string -> name:string -> 
    password:string -> ?groups:user list -> unit -> user
  val create_group : name:string -> description:string -> 
    ?groups:user list -> unit -> user
  val create_resource : ?rgroups:resource list -> unit -> resource
  val get_user_info : user:user -> string * string * (user list)
  val in_group : user:user -> group:user -> bool
  val put_user_in_group : user:user -> group:user -> unit

  val default_rights : user:user -> resource:resource -> rights
  val protect : rights:rights -> 'a -> 'a protected
  val get_protected : user:user -> resource:resource -> data:'d protected -> 'd

end

module type SAVER =
sig
  type t
  val dbinsert : rights:Rights.rights -> t -> t index
  val dbupdate : user:Rights.user -> resource:Rights.resource -> 
    ?rights:Rights.rights -> key:t index -> t -> unit
  val dbget : user:Rights.user -> resource:Rights.resource -> key:t index -> t
  val _index : 
    string ->
    (t index -> 'a, 'a, (t index Ocsigen.name -> 'b) -> 'b, 
      t index -> [>Xhtml.xhalink] Xhtml.t, t index -> [>Xhtml.xhform] Xhtml.t, 
	t index -> [>Xhtml.xhimg] Xhtml.t, 
	t index -> [>Xhtml.xhheadlink] Xhtml.t, t index -> [>Xhtml.xhscript] Xhtml.t) 
	Ocsigen.parameters
  val int_of_index : t index -> int
  val intname_of_indexname : t index Ocsigen.name -> int Ocsigen.name
end

module MakeSaver :
  functor (A : sig type t val name : string end) ->
    SAVER with type t = A.t

exception Box_not_available of string
exception Unfolds_not_registered of string

type 'a tfolded
type 'a tfolded_list

module type REGISTER =
sig
  type content
  type 'a t
  type box
  type boxes
  type container_param

  exception Duplicate_registering of string
  
  val register : 
    name:string -> 
    constructor:(box_param:'boxparam -> content t) -> 'boxparam -> box

  val unfold : box -> content t
  val unfolds : boxes -> content t

  val dbget : user:Rights.user -> resource:Rights.resource -> 
    key:content t index -> content t

  val dbinsert : rights:Rights.rights -> box -> content t index
  val dbupdate : user:Rights.user -> resource:Rights.resource ->
    ?rights:Rights.rights -> key:content t index -> box -> unit

  val register_unfolds : box_constructor:(boxes -> content t) -> 
    boxes list -> content t tfolded_list

  val dbgetlist : user:Rights.user -> resource:Rights.resource -> 
    key:content t tfolded_list index -> content list t
  val dbinsertlist : rights:Rights.rights ->
    content t tfolded_list -> content t tfolded_list index
  val dbupdatelist : user:Rights.user -> resource:Rights.resource ->
    ?rights:Rights.rights -> key:content t tfolded_list index ->
      content t tfolded_list -> unit

  val fold_container : container_param * content t tfolded_list -> box
  val fold_subpage : container_param * content t tfolded_list index -> box

end
  
module MakeRegister :
  functor (A : sig
      type content
      type 'a t
      type box
      type boxes
      val name : string
      val tag : content t tfolded -> box
      val untag : box -> content t tfolded
      val default_handler : exn -> content t
      val make_boxofboxes : filter:('a -> content t) -> 
	'a list -> content list t
      type container_param
      val container : (user:Rights.user -> resource:Rights.resource
	-> 'a -> content list t) 
	-> box_param:(container_param * 'a) -> content t
  end) -> 
    REGISTER
with type 'a t = 'a A.t 
and type content = A.content
and type box = A.box
and type boxes = A.boxes
and type container_param = A.container_param

module StringMessage : SAVER with type t = string
module StringMessageIndexList : SAVER with type t = StringMessage.t index list


