(* Ocsigen
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

(* A module to save and load data of any type to and from the database *)

type 'a index = int

module Dyn : sig
  type t
  exception Dyn_duplicate_registering of string
  exception Dyn_typing_error_while_unfolding of (string * string)
  val register : string -> ('a -> t) * (t -> 'a)
  val tag : t -> string
end = struct

  type t = string * Obj.t
      (* The string is the name of the class, 
	 the Obj.t is a data of any type
      *)

  exception Dyn_duplicate_registering of string
  exception Dyn_typing_error_while_unfolding of (string * string)

  let table = ref []

  let register name = 
    if List.mem name !table
    then raise (Dyn_duplicate_registering name)
    else
      table := name::!table;
      ((fun v -> (name, Obj.repr v)),
       (fun (name', rv) ->
	  if name = name' then Obj.magic rv 
	  else raise (Dyn_typing_error_while_unfolding (name, name'))))

  let tag (n,_) = n

end

let ((fold_string : string -> Dyn.t), unfold_string) = Dyn.register "string"
let ((fold_int : int -> Dyn.t), unfold_int) = Dyn.register "int"



module Rights : sig

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
  val dbinsert : rights:rights -> Dyn.t -> int
  val dbupdate : user:user -> resource:resource -> ?rights:rights
    -> key:int -> Dyn.t -> unit
  val dbget : user:user -> resource:resource -> key:int -> Dyn.t

  val default_rights : user:user -> resource:resource -> rights
  val protect : rights:rights -> 'a -> 'a protected
  val get_protected : user:user -> resource:resource -> data:'d protected -> 'd

end = struct

  type user = int (* (the uid). abstract in the interface for security reasons
		     The only means to get it is to connect with a password
		     (except for anonymous) *)
  type resource = int
  type rights = user list * user list * resource list * resource list
  type 'a protected = rights * 'a
  type data = Dyn.t protected

  exception Read_Forbidden_for_user
  exception Write_Forbidden_for_user
  exception Read_Forbidden_for_resource
  exception Write_Forbidden_for_resource
  exception Permission_Denied
  exception Wrong_Password
  exception No_such_user

  module DataCache =
    Ocsicache.Make(
      struct
	type tvalue = data

	let sql_t_list_to_tvalue = function
            [`Binary data
            ] -> ((Marshal.from_string data 0) : tvalue)
          | _ -> raise (Ocsicache.Cache_error "content (probably database table wrong?)")

	let tvalue_to_sql_t_list o =
          [`Binary (Marshal.to_string o [])
          ]

	let sql_fields_list_without_key = ["data"]

	let table = "content"

	let key = "content_key"

      end)

  type userdata =
      {login: string;
       password: string option; (* When there is no password, 
				 it means that it is a group.
				 Not possible to connect *)
       real_name: string;(*
       user_creation_date: Dbi.datetime;
       last_connection_date: Dbi.datetime;*)
       groups: int list
      }

  module UsersCache = Ocsicache.Make(
    struct
      type tvalue = userdata

      let sql_t_list_to_tvalue = function
          [`String login;
           `Binary password;
           `String real_name;(*
           `Timestamp user_creation_date;
           `Timestamp last_connection_date;*)
           `Binary groups
          ] ->
            {login=login;
             password=((Marshal.from_string password 0) : string option);
             real_name=real_name;(*
             user_creation_date= user_creation_date;
             last_connection_date= last_connection_date;*)
             groups=(Marshal.from_string groups 0)}
	| _ -> raise (Ocsicache.Cache_error "users (probably database table wrong?)")

      let tvalue_to_sql_t_list
          {login=login;
           password=password;
           real_name=real_name;(*
           user_creation_date= user_creation_date;
           last_connection_date= last_connection_date;*)
           groups=groups} =
        [`String login;
         `Binary (Marshal.to_string password []);
         `String real_name;(*
         `Timestamp user_creation_date;
         `Timestamp last_connection_date;*)
         `Binary (Marshal.to_string groups [])
        ]

      let sql_fields_list_without_key =
	["login";
	 "password";
	 "real_name";(*
	 "user_creation_date";
	 "last_connection_date";*)
	 "groups"]

      let table = "users"

      let key = "uid"

    end)

  module ResourceCache = Ocsicache.Make(
    struct
      type tvalue = resource list

      let sql_t_list_to_tvalue = function
          [`Binary groups
          ] -> (Marshal.from_string groups 0 : resource list)
	| _ -> raise (Ocsicache.Cache_error "resources (probably database table wrong?)")

      let tvalue_to_sql_t_list l
          =
	[`Binary (Marshal.to_string l [])
	]

      let sql_fields_list_without_key =
	["rgroups"]

      let table = "ressources"

      let key = "rid"

    end)

  let rec get_passwd () = 
    let pwd1 =
      Printf.printf "Enter Ocsimore admin password:%!";
      Scanf.scanf "%s" (fun x -> x)
    in
    let pwd2 =
      Printf.printf "Enter Ocsimore admin password once again:%!";
      Scanf.scanf "%s" (fun x -> x)
    in if pwd1 = pwd2 
    then pwd1 
    else begin
      print_endline "Passwords do not match.";
      get_passwd ();
    end

  let root_uid =
    Ocsipersist.make_persistant_lazy 
      "root_uid"
      (fun () -> 
	UsersCache.insert 
	  {login="root";
	   real_name="Admin";
	   password=Some (get_passwd ());
	   groups=[]})

  let root = Ocsipersist.get root_uid

  let create_user_aux ~login ~name ~password ?(groups=[]) () =
    (* I remove root from groups to forbid circularities *)
    let g = List.filter (fun a -> not (a = root)) groups in
    UsersCache.insert {login=login;real_name=name;password=password;groups=g}

  let create_user ~login ~name ~password ?groups () =
    create_user_aux ~login ~name ~password:(Some password) ?groups ()

  let get_user_info ~user = 
    let ui = UsersCache.get ~key:user
    in (ui.login, ui.real_name, ui.groups)

  let create_group ~name ~description ?groups () =
    create_user_aux ~login:name ~name:description ~password:None ?groups ()

  let anonymoususer_uid =
    Ocsipersist.make_persistant_lazy 
      "anonymoususer_uid"
      (fun () -> create_user 
	  ~login:"anonymous" 
	  ~name:"Anonymous User" 
	  ~password:"" ())

  let anonymoususer = Ocsipersist.get anonymoususer_uid

  let users = 
    Ocsipersist.get
      (Ocsipersist.make_persistant_lazy 
	 "users_uid"
	 (fun () -> create_group
	     ~name:"users" 
	     ~description:"All Users" 
	     ~groups:[anonymoususer] ()))

  let create_resource ?(rgroups=[]) () = ResourceCache.insert rgroups

  let anyresource = create_resource ()

  let default_rights ~user ~resource = 
    ([anonymoususer],[user],[resource],[resource])

  let connect ~(user : string) ~password : user = 
    try
      let uid,userdata = UsersCache.get_by_field "login" (`String user)
	(* Vérification du mot de passe *)
      in if (userdata.password = Some password)
	then uid
	else raise Wrong_Password
    with Not_found -> raise No_such_user

  let rec in_group_aux getgroups (user : user) group = 
    let groups = getgroups user
    in try List.mem group groups
    with Not_found -> 
      List.exists (fun g -> in_group_aux getgroups g group) groups

  let in_group ~(user : user) ~group = 
    (group = anonymoususer) || (user = root) || (user = group) ||
    (in_group_aux (fun a -> (UsersCache.get a).groups) user group)

  let in_rgroup ~(resource : resource) ~rgroup = 
    (rgroup = anyresource) || (resource = rgroup) ||
    (in_group_aux (fun a -> ResourceCache.get a) resource rgroup)

  let rec listadd a = function
      [] -> [a]
    | b::l when b=a -> b::l
    | b::l -> b::(listadd a l)

  let put_user_in_group ~user ~group =
    let ui = UsersCache.get user in
    if not (group = root) 
    then UsersCache.update 
	~key:user ~value:{ui with groups = listadd group ui.groups}

  let put_resource_in_rgroup ~resource ~rgroup =
    let ui = ResourceCache.get resource in
    ResourceCache.update 
      ~key:resource ~value:(listadd rgroup ui)

  let has_user_write_access ~(user : user) (_,ul,_,_) =
    (user = root) ||
    List.exists (fun g -> in_group user g) ul

  let has_user_read_access ~(user : user) (ul,_,_,_) =
    (user = root) ||
    List.exists (fun g -> in_group user g) ul

  let has_resource_write_access ~(resource : resource) (_,_,_,rl) =
    List.exists (fun g -> in_rgroup resource g) rl

  let has_resource_read_access ~(resource : resource) (_,_,rl,_) =
    List.exists (fun g -> in_rgroup resource g) rl

  let get_protected ~(user : user)  ~(resource : resource) ~data:(r,d) =
    if (has_user_read_access user r) 
	then begin
	  if (has_resource_read_access resource r) 
	  then d 
	  else raise Read_Forbidden_for_resource
	end
    else raise Read_Forbidden_for_user

  let protect ~rights dyn = (rights, dyn)

  let chmod_data
      ~rights
      (_,oldd) = (rights,oldd)

  let chmod ~user ~rights key = 
    let data = (DataCache.get key) 
    in DataCache.update key (chmod_data rights data)

  let dbget ~user ~resource ~key = 
    get_protected ~user:user ~resource:resource ~data:(DataCache.get key)

  let dbinsert ~rights value = DataCache.insert (protect rights value)

  let dbupdate ~user ~resource ?rights ~key value = 
    let (r,d) = (DataCache.get key) 
    in if (has_user_write_access user r)
      then begin
	if (has_resource_write_access resource r)
	then DataCache.update key 
	    (protect 
	       ~rights:(match rights with
		 None -> r
	       | Some rr -> rr) 
	       value)
	else raise Write_Forbidden_for_resource
      end
    else raise Write_Forbidden_for_user

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
      t index -> [>Xhtmlpp.xhalink], t index -> [>Xhtmlpp.xhform], 
	t index -> [>Xhtmlpp.xhheadlink], t index -> [>Xhtmlpp.xhscript]) 
	Ocsigen.parameters
  val int_of_index : t index -> int
  val intname_of_indexname : t index Ocsigen.name -> int Ocsigen.name
end



module MakeSaver (A: sig 
		    type t
		    val name : string
		  end) : SAVER with type t = A.t =
struct

  type t = A.t

  let fold,unfold = Dyn.register A.name

  let dbinsert ~rights value = 
    Rights.dbinsert ~rights:rights (fold value)

  let dbupdate ~user ~resource ?rights ~key value = 
    Rights.dbupdate ~user:user ~resource:resource ?rights ~key:key (fold value)

  let dbget ~user ~resource ~key = 
    unfold (Rights.dbget user resource key)

  let _index = Ocsigen._int

  let int_of_index x = x
  let intname_of_indexname x = x
end

(** Now a module to save constructors for a particular datatype
    For example for sons of a particular ancestor class, 
    But it works without objects, for several representation on the same data,
    or to save a representation of functions constructing datas.

    Actually the first parameter of the constructor is saved in the database
    and the following are not.

    We create a module for each class type.
    Something like boxparam -> sessionparam -> pageparam -> xhtml
    Here t is (sessionparam -> pageparam -> xhtml)
    We associate each function of this type to its name
    using the register function, that will give the function fold
    for this class type as result.

    unfold is common for all the module (that is, for one type).
*)
module Table=Map.Make(struct type t = string let compare = compare end)

type 'a tfolded = Dyn.t
type 'a tfolded_list = Dyn.t

exception Unfolds_not_registered of string

module type REGISTER =
sig

  (** The type we want to save is 'boxparam -> content t *)
  type content
  type 'a t
  type box
  type boxes
  type container_param

  exception Duplicate_registering of string
  
  val register : 
    name:string -> 
    constructor:(box_param:'boxparam -> content t) -> 'boxparam -> box

  val unfold : box ->content t
  val unfolds : boxes ->content t

  val dbget : user:Rights.user -> resource:Rights.resource -> 
    key:content t index -> content t

  val dbinsert : rights:Rights.rights -> box -> content t index
  val dbupdate : user:Rights.user -> resource:Rights.resource ->
    ?rights:Rights.rights -> key:content t index -> box -> unit

  val register_unfolds : box_constructor:(boxes -> content t) -> 
    boxes list -> content t tfolded_list

(*  val tfoldedlist_to_contentlistt : tfolded_list -> content list t *)

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

exception Box_not_available of string

module MakeRegister
    (A: sig 
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
    end) 
  : REGISTER 
with type 'a t = 'a A.t 
and type content = A.content
and type box = A.box
and type boxes = A.boxes
and type container_param = A.container_param
=
 
struct

  type 'a t = 'a A.t
  type content = A.content
  type box = A.box
  type boxes = A.boxes
  type container_param = A.container_param

  exception Duplicate_registering of string

  let constr_table = ref Table.empty

  let register ~name ~constructor = 
    let name = ("__"^A.name^"_"^name) in
    if Table.mem name !constr_table
    then raise (Duplicate_registering name)
    else let fold,unfold = Dyn.register name
    in (constr_table:=
	  (Table.add name
	     (fun data -> constructor ~box_param:(unfold data)) !constr_table);
	(fun a -> A.tag (fold a)))

  let unfold_tfolded saved_data =
    try 
      Table.find (Dyn.tag saved_data) !constr_table saved_data
    with ex -> A.default_handler ex

  let unfold box = unfold_tfolded (A.untag box)

  let dbget ~user ~resource ~key = 
    try
      unfold_tfolded (Rights.dbget user resource key)
    with ex -> A.default_handler ex

  let dbinsert ~rights value = 
    Rights.dbinsert ~rights:rights (A.untag value)

  let dbupdate ~user ~resource ?rights ~key value = 
    Rights.dbupdate 
      ~user:user ~resource:resource ?rights ~key:key (A.untag value)

  let foldlist,unfoldlist = Dyn.register A.name

  let unfolds_ref = ref (fun boxes -> 
    A.default_handler (Unfolds_not_registered A.name))

  let unfolds a = !unfolds_ref a

  let register_unfolds ~box_constructor =
    unfolds_ref := box_constructor;
    foldlist

  let unfold_with_exn d =
    try !unfolds_ref d
    with ex -> A.default_handler ex

  let tfoldedlist_to_contentlistt l = 
    A.make_boxofboxes 
      ~filter:unfold_with_exn
      (unfoldlist l)
      
  let dbgetlist ~user ~resource ~key =
    tfoldedlist_to_contentlistt (Rights.dbget user resource key)
      
  let dbinsertlist ~rights v = 
    Rights.dbinsert ~rights:rights v

  let dbupdatelist ~user ~resource ?rights ~key value = 
    Rights.dbupdate ~user:user ~resource:resource ?rights ~key:key value

  let fold_container = 
    register 
      ~name:"__container"
      ~constructor:(A.container (fun ~user ~resource -> 
	tfoldedlist_to_contentlistt))

  let fold_subpage = 
    register 
      ~name:"__subpage"
      ~constructor:
      (A.container (fun ~user ~resource k -> dbgetlist ~user ~resource ~key:k))

end


(*****************************************************************************)
(** Now the messages *)
(** We can save messages in the database. 
    (Some boxes are used print these messages) 
    Messages have no print method, as the printing depends on the kind of
    box it is in.
    Actually for messages we don't use objects.
    We use the Dyn module and dbinsert dbupdate to save in the db.
*)

(****)
(** A simple string message *)
module StringMessage = 
  MakeSaver (struct 
	       type t = string
	       let name = "string_message"
	     end)


(** A list of messages numbers *)
module StringMessageIndexList = 
  MakeSaver (struct 
	       type t = StringMessage.t index list
	       let name = "string_message_index_list"
	     end)

