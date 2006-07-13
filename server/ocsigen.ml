(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsigen.ml
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

open Http_frame
open Http_com

let _ = Random.self_init ()

let rec list_remove a = function
    [] -> []
  | b::l when a = b -> l
  | b::l -> b::(list_remove a l)

let rec list_assoc_remove a = function
    [] -> raise Not_found
  | (b,c)::l when a = b -> c,l
  | b::l -> let v,ll = list_assoc_remove a l in v,b::ll





(** type of URL, without parameter *)
type url_path = string list
type current_url = string list
type current_dir = string list
      
let string_list_of_current_url x = x
    
    
(** various functions for URLs *)
let remove_slash = function
    [] -> []
  | ""::l -> l
  | l -> l
	
let defaultpagename = "index"
    
    
let change_empty_list = function
    [] -> [""] (* It is not possible to register an empty URL *)
  | l -> l


(* En fait cette fonction est dans Neturl (split_path)
   let rec cut_url s = 
   try
   let length = String.length s in
   if length = 0 then []
   else
   let pos_slash = String.index s '/' in
   if pos_slash = 0 
   then cut_url (String.sub s 1 (length-1))
   else 
   let prefix = String.sub s 0 pos_slash in
   (*  if length > (pos_slash+1)
      then *)
   prefix::(cut_url (String.sub s (pos_slash+1) (length - pos_slash - 1)))
   (* else [prefix] *)
   with _ -> [s]
 *)


let rec reconstruct_url_path = function
    [] -> ""
  | [a] -> a
  | a::l -> a^"/"^(reconstruct_url_path l)

let rec reconstruct_url_path_suff u = function
    None -> reconstruct_url_path u
  | Some suff -> let deb = (reconstruct_url_path u) in
    if deb = "" then suff else deb^"/"^suff

let reconstruct_absolute_url_path current_url = reconstruct_url_path_suff

let reconstruct_relative_url_path current_url u suff =
  let rec drop cururl desturl = match cururl, desturl with
  | a::l, [b] -> l, desturl
  | [a], m -> [], m
  | a::l, b::m when a = b -> drop l m
  | a::l, m -> l, m
  | [], m -> [], m
  in let rec makedotdot = function
    | [] -> ""
(*    | [a] -> "" *)
    | _::l -> "../"^(makedotdot l)
  in 
  let aremonter, aaller = drop current_url u
  in let s = (makedotdot aremonter)^(reconstruct_url_path_suff aaller suff) in
(*  Messages.debug ((reconstruct_url_path current_url)^"->"^(reconstruct_url_path u)^"="^s);*)
  if s = "" then defaultpagename else s



(** state is a parameter to differenciate
   several instances of the same URL.
   (for internal use)
 *)
type internal_state = int

let counter = let c = ref (Random.int 1000000) in fun () -> c := !c + 1 ; !c

let new_state =
  let c : internal_state ref = ref (Random.int 1000000) in
  fun () -> c := !c + 1 ; Some !c


exception Ocsigen_Internal_Error of string
exception Ocsigen_Typing_Error of (string * exn) list
exception Ocsigen_Wrong_parameter
exception Ocsigen_Is_a_directory
exception Ocsigen_404
exception Ocsigen_page_erasing of string
exception Ocsigen_service_created_outside_site_loading
exception Ocsigen_there_are_unregistered_services of string
exception Ocsigen_duplicate_registering of string
exception Ocsigen_register_for_session_outside_session

let id x = x

(** Type of names in a formular *)
type 'a param_name = string

type ('a,'b) binsum = Inj1 of 'a | Inj2 of 'b;;

let ocsigen_suffix_name = "__ocsigen_suffix"

(* This is a generalized algebraic datatype *)
type ('a,'tipo,'names) params_type =
    (* 'tipo is [`WithSuffix] or [`WithoutSuffix] *)
    TProd of (* 'a1 *) ('a,'tipo,'names) params_type * (* 'a2 *) ('a,'tipo,'names) params_type (* 'a = 'a1 * 'a2 ; 'names = 'names1 * 'names2 *)
  | TOption of (* 'a1 *) ('a,'tipo,'names) params_type (* 'a = 'a1 option *)
  | TList of 'a param_name * (* 'a1 *) ('a,'tipo,'names) params_type (* 'a = 'a1 list *)
  | TSum of (* 'a1 *) ('a,'tipo,'names) params_type * (* 'a2 *) ('a,'tipo,'names) params_type (* 'a = ('a1, 'a2) binsum *)
  | TString of string param_name (* 'a = string *)
  | TInt of int param_name (* 'a = int *)
  | TFloat of float param_name (* 'a = float *)
  | TBool of bool param_name (* 'a = bool *)
  | TUserType of ('a param_name * (string -> 'a) * ('a -> string)) (* 'a = 'a *)
  | TSuffix (* 'a = string *)
  | TUnit (* 'a = unit *);;

type 'an listnames = 
    {it:'el 'a. ('an -> 'el -> 'a list) -> 'el list -> 'a list -> 'a list}

(* As GADT are not implemented in OCaml for the while, we define our own
   constructors for params_type *)
let int (n : string) : (int,[`WithoutSuffix], int param_name) params_type = TInt n
let float (n : string) : (float,[`WithoutSuffix], float param_name) params_type = TFloat n
let bool (n : string) : (bool,[`WithoutSuffix], bool param_name) params_type= TBool n
let string (n : string) : (string,[`WithoutSuffix], string param_name) params_type = 
  TString n
let radio_answer (n : string) : (string option,[`WithoutSuffix], string option param_name) params_type= TString n
let unit : (unit,[`WithoutSuffix], unit param_name) params_type = TUnit
let user_type
    (of_string : string -> 'a) (from_string : 'a -> string) (n : string)
    : ('a,[`WithoutSuffix], 'a param_name) params_type =
  Obj.magic (TUserType (n,of_string,from_string))
let sum (t1 : ('a,[`WithoutSuffix], 'an) params_type) 
    (t2 : ('b,[`WithoutSuffix], 'bn) params_type) 
    : (('a,'b) binsum,[`WithoutSuffix], 'an * 'bn) params_type =
  Obj.magic (TSum (t1, t2))
let prod (t1 : ('a,[`WithoutSuffix], 'an) params_type) 
    (t2 : ('b,[`WithoutSuffix], 'bn) params_type)
    : (('a * 'b),[`WithoutSuffix], 'an * 'bn) params_type =
  Obj.magic (TProd ((Obj.magic t1), (Obj.magic t2)))
let opt (t : ('a,[`WithoutSuffix], 'an) params_type) 
    : ('a option,[`WithoutSuffix], 'an) params_type = 
  Obj.magic (TOption t)
let list (n : string) (t : ('a,[`WithoutSuffix], 'an) params_type) 
    : ('a list,[`WithoutSuffix], 'an listnames) params_type = 
  Obj.magic (TList (n,t))
let ( ** ) = prod

let suffix_only : (string, [`WithSuffix], string param_name) params_type = 
  (Obj.magic TSuffix)
let suffix (t : ('a,[`WithoutSuffix], 'an) params_type) : 
    ((string * 'a), [`WithSuffix], string param_name * 'an) params_type = 
  (Obj.magic (TProd (Obj.magic TSuffix, Obj.magic t)))

let make_list_suffix i = "["^(string_of_int i)^"]"

let add_to_string s1 sep = function
    "" -> s1
  | s2 -> s1^sep^s2

let concat_strings s1 sep s2 = match s1,s2 with
  _,"" -> s1
| "",_ -> s2
| _ -> s1^sep^s2

(* The following function reconstruct the value of parameters
   from expected type and GET or POST parameters *)
type 'a res_reconstr_param = 
    Res_ of ('a * (string * string) list)
  | Errors_ of (string * exn) list
let reconstruct_params
    (typ : ('a,[<`WithSuffix|`WithoutSuffix],'b) params_type)
    params urlsuffix : 'a = 
  let rec aux_list t params name pref suff =
    let rec aa i lp pref suff =
      try 
	match aux t lp pref (suff^(make_list_suffix i)) with
	  Res_ (v,lp2) ->
	    (match aa (i+1) lp2 pref suff with
	      Res_ (v2,lp3) -> Res_ ((Obj.magic (v::v2)),lp3)
	    | err -> err)
	| Errors_ errs ->
	    (match aa (i+1) lp pref suff with
	      Res_ _ -> Errors_ errs
	    | Errors_ errs2 -> Errors_ (errs@errs2))
      with Not_found -> Res_ ((Obj.magic []),lp)
    in 
    aa 0 params (pref^name^".") suff
  and aux (typ : ('a,[<`WithSuffix|`WithoutSuffix],'b) params_type)
      params pref suff : 'a res_reconstr_param =
    match typ with
      TProd (t1, t2) ->
	(match aux t1 params pref suff with
	  Res_ (v1,l1) ->
	    (match aux t2 l1 pref suff with
	      Res_ (v2,l2) -> Res_ ((Obj.magic (v1,v2)),l2)
	    | err -> err)
	| Errors_ errs ->
	    (match aux t2 params pref suff with
	      Res_ _ -> Errors_ errs
	    | Errors_ errs2 -> Errors_ (errs2@errs)))
    | TOption t -> 
	(try 
	  (match aux t params pref suff with
	    Res_ (v,l) -> Res_ ((Obj.magic (Some v)),l)
	  | err -> err)
	with Not_found -> Res_ ((Obj.magic None), params))
    | TBool name -> 
	(try 
	  let v,l = (list_assoc_remove (pref^name^suff) params) in
	  Res_ ((Obj.magic true),l)
	with Not_found -> Res_ ((Obj.magic false), params))
    | TList (n,t) -> Obj.magic (aux_list t params n pref suff)
    | TSum (t1, t2) -> 
	(try 
	  match aux t1 params pref suff with
	    Res_ (v,l) -> Res_ ((Obj.magic (Inj1 v)),l)
	  | err -> err
	with Not_found -> 
	  (match aux t2 params pref suff with
	    Res_ (v,l) -> Res_ ((Obj.magic (Inj2 v)),l)
	  | err -> err))
    | TString name -> 
	let v,l = list_assoc_remove (pref^name^suff) params in
	Res_ ((Obj.magic v),l)
    | TInt name -> 
	let v,l = (list_assoc_remove (pref^name^suff) params) in 
	(try (Res_ ((Obj.magic (int_of_string v)),l))
	with e -> Errors_ [(pref^name^suff),e])
    | TFloat name -> 
	let v,l = (list_assoc_remove (pref^name^suff) params) in 
	(try (Res_ ((Obj.magic (float_of_string v)),l))
	with e -> Errors_ [(pref^name^suff),e])
    | TUserType (name, of_string, string_of) ->
	let v,l = (list_assoc_remove (pref^name^suff) params) in 
	(try (Res_ ((Obj.magic (of_string v)),l))
	with e -> Errors_ [(pref^name^suff),e])
    | TUnit -> Res_ ((Obj.magic ()), params)
    | TSuffix -> raise (Ocsigen_Internal_Error "Bad use of suffix")
  in
  let aux2 typ =
    match Obj.magic (aux typ params "" "") with
      Res_ (v,l) -> if l = [] then v else raise Ocsigen_Wrong_parameter
    | Errors_ errs -> raise (Ocsigen_Typing_Error errs)
  in
  try 
    match typ with
      TProd(TSuffix,t) -> Obj.magic ((reconstruct_url_path urlsuffix), aux2 t)
    | TSuffix -> Obj.magic (reconstruct_url_path urlsuffix)
    | _ -> Obj.magic (aux2 typ)
  with Not_found -> raise Ocsigen_Wrong_parameter

(* The following function takes a 'a params_type and a 'a and
   constructs the string of parameters (GET or POST) 
   (This is a marshalling function towards HTTP parameters format) *)
let construct_params (typ : ('a, [<`WithSuffix|`WithoutSuffix],'b) params_type)
    (params : 'a) : string * string =
  let rec aux typ params pref suff =
    match typ with
      TProd (t1, t2) ->
	let s1 = aux t1 (fst (Obj.magic params)) pref suff
	and s2 = aux t2 (snd (Obj.magic params)) pref suff in
	(concat_strings s1 "&" s2)
    | TOption t -> (match ((Obj.magic params) : 'zozo option) with None -> "" 
      | Some v -> aux t v pref suff)
    | TBool name -> 
	(if ((Obj.magic params) : bool)
	then pref^name^suff^"="^"on"
	else "")
    | TList (list_name, t) -> 
	let pref2 = pref^list_name^suff^"." in
	fst 
	  (List.fold_left
	     (fun (s,i) p -> 
	       let ss = 
		 aux t p pref2 (suff^(make_list_suffix i)) in
	       ((concat_strings s "&" ss),(i+1))) ("",0) (Obj.magic params))
    | TSum (t1, t2) -> (match Obj.magic params with
	Inj1 v -> aux t1 v pref suff
      | Inj2 v -> aux t2 v pref suff)
    | TString name -> pref^name^suff^"="^(Obj.magic params)
    | TInt name -> pref^name^suff^"="^(string_of_int (Obj.magic params))
    | TFloat name -> pref^name^suff^"="^(string_of_float (Obj.magic params))
    | TUserType (name, of_string, string_of) ->
	pref^name^suff^"="^(string_of (Obj.magic params))
    | TUnit -> ""
    | TSuffix -> raise (Ocsigen_Internal_Error "Bad use of suffix")
  in
  match typ with
    TProd(TSuffix,t) ->
      (fst (Obj.magic params)),(aux t (snd (Obj.magic params)) "" "")
  | TSuffix -> (Obj.magic params),""
  | _ -> "",(aux typ params "" "")


type 'a server_params1 = {full_url: string;
			  user_agent: string;
			  ip: Unix.inet_addr;
			  get_params: (string * string) list;
			  post_params: (string * string) list;
			  current_url: current_url;
			  current_dir: current_dir;
			  session_table: 'a ref
			}
      
type 'a server_params2 = url_path * 'a server_params1
      


(** Typed services *)
type internal_service_kind = [`Public_Service | `Local_Service]
type service_kind = [`Internal_Service of internal_service_kind | `External_Service]

type ('get,'post,'kind,'tipo,'getnames,'postnames) service = 
    {url: url_path; (* name of the service without parameters *)
     unique_id: int;
     url_prefix: bool;
     external_service: bool;
     url_state: internal_state option;
     (* 'kind is just a type information: it can be only 
	`Internal_Service `Public_Service or `Internal_Service `Local_Service
	or `External_Service, so that we can't use session services as fallbacks for
	other session services. If it is a session service, it contains a value
	(internal state) that will allow to differenciate between
	services that have the same url.
      *)
     get_params_type: ('get,'tipo,'getnames) params_type;
     post_params_type: ('post,[`WithoutSuffix],'postnames) params_type;
   }


(* The current registration directory *)
let absolute_change_dir, get_current_dir, end_current_dir =
  let current_dir : url_path ref = ref [] in
  let f1 = ref (fun dir -> current_dir := remove_slash dir) in
  let f2 = ref (fun () -> !current_dir) in
  let exn1 _ = 
    raise (Ocsigen_Internal_Error "absolute_change_dir after init") in
  let exn2 () = 
    raise (Ocsigen_Internal_Error "get_current_dir after init") in
  ((fun dir -> !f1 dir),
   (fun () -> !f2 ()),
   (fun () -> f1 := exn1; f2 := exn2))
(* Warning: these functions are used only during the initialisation
   phase, which is not threaded ... That's why it works, but ...
   it is not really clean ... public registration relies on this
   directory (defined for each site in the config file) 
 *)

let add_unregistered, remove_unregistered, verify_all_registered =
  let l = ref [] in
  ((fun a -> l := a::!l),
   (fun a -> l := list_remove a !l),
   (fun () -> 
     match !l with [] -> () 
     | (a,_)::_ -> raise (Ocsigen_there_are_unregistered_services (reconstruct_url_path a))))

let during_initialisation, end_initialisation =
  let init = ref true in
  ((fun () -> !init), 
   (fun () -> 
     init := false;
     end_current_dir ();
     verify_all_registered ()))

let during_ocsigen_module_loading, 
  begin_load_ocsigen_module, 
  end_load_ocsigen_module =
  let during_ocsigen_module_loading = ref false in
  ((fun () -> !during_ocsigen_module_loading),
   (fun () -> during_ocsigen_module_loading := true),
   (fun () -> during_ocsigen_module_loading := false))

let global_register_allowed () = 
  (during_initialisation ()) && (during_ocsigen_module_loading ())


(********)

let action_prefix = "__ocsigen_action__"
let action_name = "name"
let action_reload = "reload"
let state_param_name = "__ocsigen_etat__"




(*****************************************************************************)
(*****************************************************************************)
(* Tables of services (global and session tables)                            *)
(*****************************************************************************)
(*****************************************************************************)


(** We associate to a service a function server_params2 -> page *)
module type DIRECTORYTREE =
  sig
    type tables

    type page_table_key =
	{prefix:bool;
	 state: internal_state option}

    val make_server_params :
	current_dir -> tables ref -> current_url -> string ->
	  (string * string) list -> (string * string) list -> 
	    string -> Unix.inet_addr -> tables server_params1

    val empty_tables : unit -> tables
    val are_empty_tables : tables -> bool
    val add_service : tables -> current_dir -> bool -> url_path ->
      Sender_helpers.create_sender_type ->
	page_table_key * (int * (tables server_params2 -> 
	  Sender_helpers.send_page_type)) -> unit
    val add_action :
	tables -> current_dir
	  -> string -> (tables server_params1 -> unit) -> unit
    val find_service :
	tables ->
	  tables ref * 
	    current_url * internal_state option * (string * string) list *
	    (string * string) list * string * Unix.inet_addr * string -> 
	      Sender_helpers.send_page_type * 
		Sender_helpers.create_sender_type * url_path
    val find_action :
	tables -> string -> (tables server_params1 -> unit) * url_path
  end

module Directorytree : DIRECTORYTREE = struct
  (* Each node contains either a list of nodes (case directory)
     or a table of "answers" (functions that will generate the page) *)

  type page_table_key =
      {prefix:bool;
       state: internal_state option}
	(* action: tables server_params2 -> page *)

	(* module Page_Table = Map.Make(struct type t = page_table_key 
	   let compare = compare end) *)

  module String_Table = Map.Make(struct type t = string
    let compare = compare end)

  type page_table = 
      (page_table_key * 
	 ((int * ((tables server_params2 -> Sender_helpers.send_page_type)
		    * Sender_helpers.create_sender_type * url_path)) list)) list
	(* Here, the url_path is the working directory.
	   That is, the directory in which we are when we register
	   dynamically the pages.
	   Each time we load a page, we change to this directory
	   (in case the page registers new pages).
	 *)

  and action_table = 
      AVide 
    | ATable of ((tables server_params1 -> unit) * url_path) String_Table.t

  and dircontent = 
      Vide
    | Table of direlt ref String_Table.t

  and direlt = 
      Dir of dircontent ref
    | File of page_table ref

  and tables = dircontent ref * action_table ref


	(** Create server parameters record *)
  let make_server_params
      dir st url fullurl get_params post_params useragent ip = 
    {full_url= fullurl;
     user_agent=useragent;
     current_url=url;
     ip=ip;
     get_params = get_params;
     post_params = post_params;
     current_dir = dir;
     session_table = st;
   }

  let empty_page_table () = []
  let empty_action_table () = AVide
  let empty_dircontent () = Vide

  let find_page_table t (str,url,getp,postp,ua,ip,fullurl,urlsuffix) k = 
    let sp = make_server_params [] str url fullurl getp postp ua ip in
    let rec aux = function
	[] -> raise Ocsigen_Wrong_parameter
      | (_,(funct,create_sender,working_dir))::l ->
	  try
	    Messages.debug "I'm trying a service";
	    let p = funct (urlsuffix, {sp with current_dir = working_dir}) in
	    Messages.debug "Page found";
	    p,create_sender,working_dir
	  with Ocsigen_Wrong_parameter -> aux l
    in 
    let r = try List.assoc k t with Not_found -> raise Ocsigen_404 in
    aux r

  let add_page_table session url_act t (key,(id,elt)) = 
    (* Duplicate registering forbidden in global table *)
    try
      let l,newt = list_assoc_remove key t in
      try
(********** Vérifier ici qu'il n'y a pas qqchose similaire déjà enregistré ! *)
	let _,oldl = list_assoc_remove id l in
	if not session then
	  raise (Ocsigen_duplicate_registering (reconstruct_url_path url_act))
	else (key,((id,elt)::oldl))::newt
      with Not_found -> (key,((id,elt)::l))::newt
    with Not_found -> (key,[(id,elt)])::t

  let add_dircontent dc (key,elt) =
    match dc with
      Vide -> Table (String_Table.add key elt String_Table.empty)
    | Table t -> Table (String_Table.add key elt t)

  let find_dircontent dc k =
    match dc with
      Vide -> raise Not_found
    | Table t -> String_Table.find k t

  let add_action_table at (key,elt) = 
    match at with
      AVide -> ATable (String_Table.add key elt String_Table.empty)
    | ATable t -> ATable (String_Table.add key elt t)

  let find_action_table at k = 
    match at with
      AVide -> raise Not_found
    | ATable t -> String_Table.find k t

  let empty_tables () = 
    (ref (empty_dircontent ()), ref (empty_action_table ()))

  let are_empty_tables (dcr,atr) = 
    (!dcr = Vide && !atr = AVide)

  let add_action (_,actiontableref) current_dir name action =
    actiontableref :=
      add_action_table !actiontableref
	(name,(action,current_dir))

  let find_action (_,atr) name =
    find_action_table !atr name

  let add_service (dircontentref,_) current_dir session url_act
      create_sender
      (page_table_key, (unique_id, action)) =
    let aux search dircontentref a l =
      try 
	let direltref = find_dircontent !dircontentref a in
	match !direltref with
	  Dir dcr -> search dcr l
	| File ptr -> raise (Ocsigen_page_erasing a)
	      (* Messages.warning ("Ocsigen page registering: Page "^
		 a^" has been replaced by a directory");
		 let newdcr = ref (empty_dircontent ()) in
		 (direltref := Dir newdcr;
		 search newdcr l) *)
      with
	Not_found -> 
	  let newdcr = ref (empty_dircontent ()) in
	  (dircontentref := 
	    add_dircontent !dircontentref (a, ref (Dir newdcr));
	   search newdcr l)
    in 
    let rec search_page_table_ref dircontentref = function
	[] | [""] -> search_page_table_ref dircontentref [defaultpagename]
      | [a] -> 
	  (try 
	    let direltref = find_dircontent !dircontentref a in
	    (match !direltref with
	      Dir _ -> raise (Ocsigen_page_erasing a)
		  (* Messages.warning ("Ocsigen page registering: Directory "^
		     a^" has been replaced by a page");
		     let newpagetableref = ref (empty_page_table ()) in
		     (direltref := File newpagetableref;
		     newpagetableref) *)
	    | File ptr -> ptr)
	  with
	    Not_found ->
	      let newpagetableref = ref (empty_page_table ()) in
	      (dircontentref := 
		add_dircontent !dircontentref (a,ref (File newpagetableref));
	       newpagetableref))
      | ""::l -> search_page_table_ref dircontentref l
      | a::l -> aux search_page_table_ref dircontentref a l
	    (* and search_dircontentref dircontentref = function
	       [] -> dircontentref
	       | ""::l -> search_dircontentref dircontentref l
	       | a::l -> aux search_dircontentref a l *)
    in
    let content = ({prefix = page_table_key.prefix;
		    state = page_table_key.state},
		   (unique_id, (action, create_sender, current_dir))) in
    (* let current_dircontentref = 
       search_dircontentref dircontentref current_dir) in *)
    let page_table_ref = 
      search_page_table_ref (*current_*) dircontentref url_act in
    page_table_ref := add_page_table session url_act !page_table_ref content

	
  let find_service 
      (dircontentref,_)
      (session_table_ref, 
       string_list, state_option, get_param_list, post_param_list, 
       ua, ip, fullurl) =
    let rec search_page_table dircontent =
      let aux a l =
	(match !(find_dircontent dircontent a) with
	  Dir dircontentref2 -> search_page_table !dircontentref2 l
	| File page_table_ref -> !page_table_ref, l)
      in function
	  [] -> raise Ocsigen_Is_a_directory
	| [""] -> aux defaultpagename []
	| ""::l -> search_page_table dircontent l
	| a::l -> aux a l
    in
    let page_table, suffix = 
      try 
	(search_page_table !dircontentref (change_empty_list string_list)) 
      with Not_found -> raise Ocsigen_404
    in
    let suffix,get_param_list = 
      if  suffix = []
      then try
	let s,l = list_assoc_remove ocsigen_suffix_name get_param_list in
	[s],l
      with Not_found -> suffix,get_param_list
      else suffix,get_param_list in
    let pref = suffix <> [] in
    find_page_table 
      page_table
      (session_table_ref,
       string_list,
       get_param_list,
       post_param_list,
       ua,
       ip,
       fullurl,
       suffix)
      {prefix = pref;
       state = state_option}

end

open Directorytree

type session_table = Directorytree.tables

(** Type of http parameters *)
(* type server_params = Directorytree.server_params it doesn't work *)
type server_params = session_table server_params1

let global_tables = empty_tables ()

let new_session_tables = empty_tables


(* The table of tables for each session. Keys are (hostname,cookie) *)
module Cookies = Hashtbl.Make(struct 
  type t = Unix.inet_addr * string
  let equal = (=)
  let hash = Hashtbl.hash
end)

let cookie_table = Cookies.create 100

let rec new_cookie table ip = 
  let c = Int64.to_string (Random.int64 Int64.max_int) in
  try 
    Cookies.find table (ip,c);
    new_cookie table ip
  with Not_found -> c





(*****************************************************************************)
(*****************************************************************************)
(* Page registration, handling of links and forms                            *)
(*****************************************************************************)
(*****************************************************************************)

module type PAGES = 
  sig
(** Type of answers from modules (web pages) *)
    type page
    type form_content_elt
    type form_elt
    type a_content_elt
    type a_elt
    type div_content_elt
    type uri
    type link_elt
    type script_elt
    type textarea_elt
    type input_elt
    type pcdata_elt

    val create_sender : Sender_helpers.create_sender_type
    val send : content:page -> Sender_helpers.send_page_type

    type a_attrib_t
    type form_attrib_t
    type input_attrib_t
    type textarea_attrib_t
    type link_attrib_t
    type script_attrib_t
    type input_type_t

    val hidden : input_type_t
    val text : input_type_t
    val password : input_type_t
    val checkbox : input_type_t
    val radio : input_type_t
    val submit : input_type_t

    val make_a : ?a:a_attrib_t -> href:string -> a_content_elt list -> a_elt
    val make_get_form : ?a:form_attrib_t -> 
      action:string -> 
	form_content_elt -> form_content_elt list -> form_elt
    val make_post_form : ?a:form_attrib_t ->
      action:string -> ?id:string -> ?inline:bool -> 
	form_content_elt -> form_content_elt list -> form_elt
    val make_hidden_field : input_elt -> form_content_elt
    val make_empty_form_content : unit -> form_content_elt
    val make_input : ?a:input_attrib_t -> ?checked:bool ->
      typ:input_type_t -> ?name:string -> 
	?value:string -> unit -> input_elt
    val make_textarea : ?a:textarea_attrib_t -> 
      name:string -> rows:int -> cols:int ->
	pcdata_elt -> 
	  textarea_elt
    val make_div : classe:(string list) -> a_elt list -> form_content_elt
    val make_uri_from_string : string -> uri


    val make_css_link : ?a:link_attrib_t -> uri -> link_elt

    val make_js_script : ?a:script_attrib_t -> uri -> script_elt

  end

module type OCSIGENSIG =
  sig
    type page
    type form_content_elt
    type form_elt
    type a_content_elt
    type a_elt
    type div_content_elt
    type uri
    type link_elt
    type script_elt
    type textarea_elt
    type input_elt
    type pcdata_elt
	  
    type a_attrib_t
    type form_attrib_t
    type input_attrib_t
    type textarea_attrib_t
    type link_attrib_t
    type script_attrib_t
    type input_type_t

    val new_external_service :
	url:url_path ->
	  ?prefix:bool ->
	    get_params:('a, [< `WithSuffix | `WithoutSuffix ] as 'b, 'c)
	      params_type ->
		post_params:('d, [ `WithoutSuffix ], 'e) params_type ->
		  unit -> ('a, 'd, [ `External_Service ], 'b, 'c, 'e) service
    val new_service :
	url:url_path ->
	  ?prefix:bool ->
	    get_params:('a, [< `WithSuffix | `WithoutSuffix ] as 'b, 'c)
	      params_type ->
		unit ->
		  ('a, unit, [ `Internal_Service of [ `Public_Service ] ], 'b, 'c,
		   unit param_name)
		    service
    val new_auxiliary_service :
	fallback:('a, unit, [ `Internal_Service of [ `Public_Service ] ], 'b,
		  'c, 'd)
	service ->
	  ('a, unit, [ `Internal_Service of [ `Local_Service ] ], 'b, 'c, 'd)
	    service
    val register_service :
	service:('a, 'b, [ `Internal_Service of 'c ],
		 [< `WithSuffix | `WithoutSuffix ], 'd, 'e) service ->
		   ?error_handler:(server_params -> (string * exn) list -> page) ->
		     (server_params -> 'a -> 'b -> page) -> unit
    val register_service_for_session :
	Directorytree.tables server_params1 ->
	  service:('a, 'b, [ `Internal_Service of 'c ],
		   [< `WithSuffix | `WithoutSuffix ], 'd, 'e)
	    service ->
	      ?error_handler:(server_params -> (string * exn) list -> page) ->
		(server_params -> 'a -> 'b -> page) -> unit
    val register_new_service :
	url:url_path ->
	  ?prefix:bool ->
	    get_params:('a, [< `WithSuffix | `WithoutSuffix ] as 'b, 'c)
	      params_type ->
		?error_handler:(server_params -> (string * exn) list -> page) ->
		  (server_params -> 'a -> unit -> page) ->
		    ('a, unit, [ `Internal_Service of [ `Public_Service ] ], 'b, 'c,
		     unit param_name)
		      service
    val register_new_auxiliary_service :
	fallback:('a, unit, [ `Internal_Service of [ `Public_Service ] ],
		  [< `WithSuffix | `WithoutSuffix ] as 'b, 'c, 'd)
	service ->
	  ?error_handler:(server_params -> (string * exn) list -> page) ->
	    (server_params -> 'a -> unit -> page) ->
	      ('a, unit, [ `Internal_Service of [ `Local_Service ] ], 'b, 'c, 'd)
		service
    val register_new_auxiliary_service_for_session :
	Directorytree.tables server_params1 ->
	  fallback:('a, unit, [ `Internal_Service of [ `Public_Service ] ],
		    [< `WithSuffix | `WithoutSuffix ] as 'b, 'c, 'd)
	    service ->
	      ?error_handler:(server_params -> (string * exn) list -> page) ->
		(server_params -> 'a -> unit -> page) ->
		  ('a, unit, [ `Internal_Service of [ `Local_Service ] ], 'b, 'c, 'd)
		    service
    val new_post_service :
	fallback:('a, unit, [ `Internal_Service of [ `Public_Service ] ], 'b,
		  'c, unit param_name)
	service ->
	  post_params:('d, [ `WithoutSuffix ], 'e) params_type ->
	    ('a, 'd, [ `Internal_Service of [ `Public_Service ] ], 'b, 'c, 'e)
	      service
    val new_post_auxiliary_service :
	fallback:('a, 'b, [ `Internal_Service of [ `Public_Service ] ], 'c,
		  'd, 'e)
	service ->
	  post_params:('f, [ `WithoutSuffix ], 'g) params_type ->
	    ('a, 'f, [ `Internal_Service of [ `Local_Service ] ], 'c, 'd, 'g)
	      service
    val register_new_post_service :
	fallback:('a, unit, [ `Internal_Service of [ `Public_Service ] ],
		  [< `WithSuffix | `WithoutSuffix ] as 'b, 'c,
		  unit param_name)
	service ->
	  post_params:('d, [ `WithoutSuffix ], 'e) params_type ->
	    ?error_handler:(server_params -> (string * exn) list -> page) ->
	      (server_params -> 'a -> 'd -> page) ->
		('a, 'd, [ `Internal_Service of [ `Public_Service ] ], 'b, 'c, 'e)
		  service
    val register_new_post_auxiliary_service :
	fallback:('a, 'b, [ `Internal_Service of [ `Public_Service ] ],
		  [< `WithSuffix | `WithoutSuffix ] as 'c, 'd, 'e)
	service ->
	  post_params:('f, [ `WithoutSuffix ], 'g) params_type ->
	    ?error_handler:(server_params -> (string * exn) list -> page) ->
	      (server_params -> 'a -> 'f -> page) ->
		('a, 'f, [ `Internal_Service of [ `Local_Service ] ], 'c, 'd, 'g)
		  service
    val register_new_post_auxiliary_service_for_session :
	Directorytree.tables server_params1 ->
	  fallback:('a, 'b, [ `Internal_Service of [ `Public_Service ] ],
		    [< `WithSuffix | `WithoutSuffix ] as 'c, 'd, 'e)
	    service ->
	      post_params:('f, [ `WithoutSuffix ], 'g) params_type ->
		?error_handler:(server_params -> (string * exn) list -> page) ->
		  (server_params -> 'a -> 'f -> page) ->
		    ('a, 'f, [ `Internal_Service of [ `Local_Service ] ], 'c, 'd, 'g)
		      service
    type ('a, 'b) action
    val new_action :
	post_params:('a, [ `WithoutSuffix ], 'b) params_type ->
	  ('a, 'b) action
    val register_action :
	action:('a, 'b) action -> (server_params -> 'a -> unit) -> unit
    val register_new_action :
	post_params:('a, [ `WithoutSuffix ], 'b) params_type ->
	  (server_params -> 'a -> unit) -> ('a, 'b) action
    val register_action_for_session :
	Directorytree.tables server_params1 ->
	  action:('a, 'b) action ->
	    (Directorytree.tables server_params1 -> 'a -> unit) -> unit
    val register_new_action_for_session :
	Directorytree.tables server_params1 ->
	  post_params:('a, [ `WithoutSuffix ], 'b) params_type ->
	    (Directorytree.tables server_params1 -> 'a -> unit) ->
	      ('a, 'b) action
    val static_dir :
	'a server_params1 ->
	  (string, unit, [ `Internal_Service of [ `Public_Service ] ],
	   [ `WithSuffix ], string param_name, unit param_name)
	    service
    val close_session : Directorytree.tables server_params1 -> unit
    val a :
	?a:a_attrib_t ->
	  ('a, unit, 'b, [< `WithSuffix | `WithoutSuffix ], 'c, 'd) service ->
	    server_params -> a_content_elt list -> 'a -> a_elt
    val get_form :
	?a:form_attrib_t ->
	  ('a, unit, 'b, 'c, 'd, unit param_name) service ->
	    server_params ->
	      ('d -> form_content_elt list) -> form_elt
    val post_form :
	?a:form_attrib_t ->
	  ('a, 'b, 'c, [< `WithSuffix | `WithoutSuffix ], 'd, 'e) service ->
	    server_params ->
	      ('e -> form_content_elt list) -> 'a -> form_elt
    val make_uri :
	('a, unit, 'b, [< `WithSuffix | `WithoutSuffix ], 'c, 'd) service ->
	  'e server_params1 -> 'a -> uri
    val action_a :
	?a:a_attrib_t ->
	  ?reload:bool ->
	    ('a, 'b) action ->
	      'c server_params1 -> a_content_elt list -> form_elt
    val action_form :
	?a:form_attrib_t ->
	  ?reload:bool ->
	    ('a, 'b) action ->
	      'c server_params1 ->
		('b -> form_content_elt list) -> form_elt
    val js_script :
	?a:script_attrib_t -> uri -> script_elt
    val css_link : ?a:link_attrib_t -> uri -> link_elt

    val int_input :
	?a:input_attrib_t -> ?value:int -> int param_name -> input_elt
    val float_input :
	?a:input_attrib_t -> ?value:float -> float param_name -> input_elt
    val string_input :
	?a:input_attrib_t -> ?value:string -> string param_name -> input_elt
    val user_type_input :
	?a:input_attrib_t -> ?value:'a -> ('a -> string) -> 
	  'a param_name -> input_elt
    val int_password_input :
	?a:input_attrib_t -> ?value:int -> int param_name -> input_elt
    val float_password_input :
	?a:input_attrib_t -> ?value:float -> float param_name -> input_elt
    val string_password_input :
	?a:input_attrib_t -> ?value:string -> string param_name -> input_elt
    val user_type_password_input :
	?a:input_attrib_t -> ?value:'a -> ('a -> string) -> 
	  'a param_name -> input_elt
    val hidden_int_input :
	?a:input_attrib_t -> int param_name -> int -> input_elt
    val hidden_float_input :
	?a:input_attrib_t -> float param_name -> float -> input_elt
    val hidden_string_input :
	?a:input_attrib_t -> string param_name -> string -> input_elt
    val hidden_user_type_input :
	?a:input_attrib_t -> ('a -> string) -> 'a param_name -> 'a -> input_elt
    val bool_checkbox :
	?a:input_attrib_t -> ?checked:bool -> bool param_name -> input_elt
    val string_radio :
        ?a:input_attrib_t -> ?checked:bool -> 
	  string option param_name -> string -> input_elt
    val int_radio :
        ?a:input_attrib_t -> ?checked:bool -> 
	   int option param_name -> int -> input_elt
    val float_radio :
        ?a:input_attrib_t -> ?checked:bool -> 
	   float option param_name -> float -> input_elt
    val user_type_radio :
        ?a:input_attrib_t -> ?checked:bool -> ('a -> string) ->
	   'a option param_name -> 'a -> input_elt
    val textarea :
	?a:textarea_attrib_t ->
	  string param_name ->
	    rows:int -> cols:int -> pcdata_elt -> textarea_elt
    val submit_input : ?a:input_attrib_t -> string -> input_elt
  end


module Make = functor
  (Pages : PAGES) ->
    (struct

      type page = Pages.page
      type form_content_elt = Pages.form_content_elt
      type form_elt = Pages.form_elt
      type a_content_elt = Pages.a_content_elt
      type a_elt = Pages.a_elt
      type div_content_elt = Pages.div_content_elt
      type uri = Pages.uri
      type link_elt = Pages.link_elt
      type script_elt = Pages.script_elt
      type textarea_elt = Pages.textarea_elt
      type input_elt = Pages.input_elt
      type pcdata_elt = Pages.pcdata_elt
	    
      type a_attrib_t = Pages.a_attrib_t
      type form_attrib_t = Pages.form_attrib_t
      type input_attrib_t = Pages.input_attrib_t
      type textarea_attrib_t = Pages.textarea_attrib_t
      type link_attrib_t = Pages.link_attrib_t
      type script_attrib_t = Pages.script_attrib_t
      type input_type_t = Pages.input_type_t

(** Create a service *)
      let new_service_aux_aux
	  ~(url : url_path)
	  ~prefix
	  ~external_service
	  ~(get_params : ('get,[<`WithoutSuffix|`WithSuffix] as 'tipo,'gn) params_type)
	  ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
	  : ('get,'post,'kind,'tipo,'gn,'pn) service =
(* ici faire une vérification "duplicate parameter" ? *) 
	{url = url;
	 unique_id = counter ();
	 url_prefix = prefix;
	 url_state = None;
	 external_service = external_service;
	 get_params_type = get_params;
	 post_params_type = post_params;
       }

      let new_service_aux
	  ~(url : url_path)
	  ~prefix
	  ~(get_params : ('get,[<`WithoutSuffix|`WithSuffix] as 'tipo,'gn) params_type)
	  : ('get,unit,[`Internal_Service of 'popo],'tipo,'gn,unit param_name) service =
	if global_register_allowed () then
	  let full_path = (get_current_dir ())@(change_empty_list url) in
	  let u = new_service_aux_aux
	      ~url:full_path
	      ~prefix
	      ~external_service:false
	      ~get_params
	      ~post_params:unit
	  in
	  add_unregistered (u.url,u.unique_id); u
	else raise Ocsigen_service_created_outside_site_loading

      let new_external_service
	  ~(url : url_path)
	  ?(prefix=false)
	  ~(get_params : ('get,[<`WithoutSuffix|`WithSuffix] as 'tipo,'gn) params_type)
	  ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
	  ()
	  : ('get,'post,[`External_Service],'tipo,'gn,'pn) service =
	new_service_aux_aux
	  ~url
	  ~prefix
	  ~external_service:true
	  ~get_params 
	  ~post_params

      let new_service
	  ~(url : url_path)
	  ?(prefix=false)
	  ~(get_params : ('get,[<`WithoutSuffix|`WithSuffix] as 'tipo,'gn) params_type)
	  ()
	  : ('get,unit,[`Internal_Service of [`Public_Service]],'tipo,'gn, unit param_name) service =
	new_service_aux ~url ~prefix ~get_params

      let new_auxiliary_service
	  ~(fallback : ('get,unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service)
	  : ('get,unit,[`Internal_Service of [`Local_Service]],'tipo,'gn,'pn) service =
	{fallback with url_state = new_state ()}

      let register_service_aux
	  current_dir
	  tables
	  session
	  state
	  ~(service : ('get,'post,[`Internal_Service of 'popo],'tipo,'gn,'pn) service)
	  ?(error_handler = fun sp l -> raise (Ocsigen_Typing_Error l))
	  (page_generator : server_params -> 'get -> 'post -> 'fin) =
	add_service tables current_dir session service.url 
	  Pages.create_sender
	  ({prefix = service.url_prefix;
	    state = state},
	   (service.unique_id,
	    (fun (suff,h) -> 
	      Pages.send 
		~content:(try 
		  page_generator h 
		    (reconstruct_params 
		       service.get_params_type h.get_params suff)
		    (reconstruct_params
		       service.post_params_type h.post_params suff)
		with Ocsigen_Typing_Error l -> error_handler h l))))

      let register_service 
	  ~(service : ('get,'post,[`Internal_Service of 'g],'tipo,'gn,'pn) service)
	  ?error_handler
	  (page_gen : server_params -> 'get -> 'post -> 'fin) =
	if global_register_allowed () then begin
	  remove_unregistered (service.url,service.unique_id);
	  register_service_aux 
	    (get_current_dir ()) global_tables false (service.url_state)
	    ~service ?error_handler page_gen; 
	end
	else Messages.warning "Public service registration after init forbidden! Please correct your module! (ignored)"

(* WARNING: if we create a new service without registering it,
   we can have a link towards a page that does not exist!!! :-(
   That's why I impose to register all service during init.
   The only other way I see to avoid this is to impose a syntax extension
   like "let rec" for service...
 *)

      let register_service_for_session
	  sp
	  ~(service : ('get,'post,[`Internal_Service of 'g],'tipo,'gn,'pn) service) 	?error_handler
	  page =
	register_service_aux ?error_handler sp.current_dir
	  !(sp.session_table) true service.url_state ~service page

      let register_new_service 
	  ~url
	  ?(prefix=false)
	  ~(get_params : ('get,[<`WithoutSuffix|`WithSuffix] as 'tipo,'gn) params_type)
	  ?error_handler
	  page
	  : ('get,unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,unit param_name) service =
	let u = new_service ~prefix ~url ~get_params () in
	register_service ~service:u ?error_handler page;
	u
	  
      let register_new_auxiliary_service
	  ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service) 	
	  ?error_handler
	  page
	  : ('get, unit, [`Internal_Service of [`Local_Service]],'tipo,'gn,'pn) service =
	let u = (new_auxiliary_service fallback) in
	register_service ~service:u ?error_handler page;
	u

      let register_new_auxiliary_service_for_session
	  sp
	  ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service)
	  ?error_handler
	  page
	  : ('get, unit, [`Internal_Service of [`Local_Service]],'tipo,'gn,'pn) service =
	let u = (new_auxiliary_service fallback) in
	register_service_for_session sp ~service:u ?error_handler page;
	u


(** Register an service with post parameters in the server *)
      let new_post_service_aux
	  ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,unit param_name) service)
	  ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
	  : ('get, 'post, [`Internal_Service of [`Public_Service]], 'tipo,'gn,'pn) service = 
(* ici faire une vérification "duplicate parameter" ? *) 
	{url = fallback.url;
	 unique_id = counter ();
	 url_prefix = fallback.url_prefix;
	 external_service = false;
	 url_state = None;
	 get_params_type = fallback.get_params_type;
	 post_params_type = post_params;
       }

      let new_post_service
	  ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,unit param_name) service)
	  ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
	  : ('get, 'post, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service = 
	if global_register_allowed () then
	  let u = new_post_service_aux fallback post_params in
	  add_unregistered (u.url,u.unique_id); u
	else raise Ocsigen_service_created_outside_site_loading
	    
      let new_post_auxiliary_service
	  ~(fallback : ('get, 'post1, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn1) service)
	  ~(post_params : ('post,[`WithoutSuffix],'pn2) params_type)
	  : ('get, 'post, [`Internal_Service of [`Local_Service]],'tipo,'gn,'pn2) service = 
	{fallback with 
	 url_state = new_state ();
	 post_params_type = post_params;
       }

      let register_new_post_service 
	  ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,unit param_name) service)
	  ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
	  ?error_handler
	  (page_gen : server_params -> 'get -> 'post -> 'fin)
	  : ('get,'post, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service =
	let u = new_post_service ~fallback:fallback ~post_params:post_params in
	register_service ~service:u ?error_handler page_gen;
	u

      let register_new_post_auxiliary_service
	  ~(fallback : ('get, 'post1, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn1) service)
	  ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
	  ?error_handler
	  page_gen
	  : ('get, 'post, [`Internal_Service of [`Local_Service]],'tipo,'gn,'pn) service = 
	let u = new_post_auxiliary_service ~fallback:fallback ~post_params:post_params in
	register_service ~service:u ?error_handler page_gen;
	u

      let register_new_post_auxiliary_service_for_session
	  sp
	  ~(fallback : ('get, 'post1, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn1) service)
	  ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
	  ?error_handler
	  page_gen
	  : ('get, 'post, [`Internal_Service of [`Local_Service]],'tipo,'gn,'pn) service = 
	let u = new_post_auxiliary_service ~fallback:fallback ~post_params:post_params in
	register_service_for_session sp ~service:u ?error_handler page_gen;
	u


(** actions (new 10/05) *)
      type ('post,'pn) action =
	  {action_name: string;
	   action_params_type: ('post,[`WithoutSuffix],'pn) params_type}

      let new_action_name () = string_of_int (counter ())

      let new_action
	  ~(post_params : ('post,[`WithoutSuffix],'pn) params_type) =
	{
	 action_name = new_action_name ();
	 action_params_type = post_params;
       }

      let register_action_aux current_dir tables ~action actionfun =
	add_action tables current_dir 
	  action.action_name
	  (fun h -> actionfun h 
	      (reconstruct_params action.action_params_type h.post_params []))

      let register_action
	  ~(action : ('post,'pn) action)
	  (actionfun : (server_params -> 'post -> unit)) : unit =
	if global_register_allowed () then begin
	  register_action_aux (get_current_dir ()) global_tables action actionfun
	end
	else Messages.warning "Public action registration after init forbidden! Please correct your module! (ignored)"

      let register_new_action ~post_params actionfun = 
	let a = new_action post_params in
	register_action a actionfun;
	a

      let register_action_for_session sp ~action actionfun =
	register_action_aux sp.current_dir !(sp.session_table) action actionfun

      let register_new_action_for_session sp ~post_params actionfun =
	let a = new_action post_params in
	register_action_for_session sp a actionfun;
	a

(** Satic directories **)
      let static_dir sp : (string, unit, [`Internal_Service of [`Public_Service]],[`WithSuffix],string param_name, unit param_name) service =
	{url = sp.current_dir;
	 unique_id = counter ();
	 url_state = None;
	 url_prefix = true;
	 external_service = false;
	 get_params_type = suffix_only;
	 post_params_type = unit
       }



(** Close a session *)
      let close_session sp = sp.session_table := empty_tables ()


(** Functions to construct web pages: *)

      let a ?a
	  (service : ('get, unit, 'kind, 'tipo,'gn,'pn) service) 
	  (sp : server_params) content
	  (getparams : 'get) =
	let suff,params_string = construct_params service.get_params_type getparams in
	let suff = (if service.url_prefix then Some suff else None) in
	let uri = 
	  (if service.external_service 
	  then (reconstruct_absolute_url_path sp.current_url service.url suff)
	  else (reconstruct_relative_url_path sp.current_url service.url suff))
	in
	match service.url_state with
	  None ->
	    Pages.make_a ?a ~href:(add_to_string uri "?" params_string) content
	| Some i -> 
	    Pages.make_a ?a
	      ~href:(add_to_string 
		       (uri^"?"^state_param_name^"="^(string_of_int i))
		       "&" params_string)
	      content

(* avec un formulaire caché (ça marche mais ce n'est pas du xhtml valide
   let stateparam = string_of_int i in
   let formname="hiddenform"^(string_of_int (counter ())) in
   let href="javascript:document."^formname^".submit ()" in
   << <a href=$href$>$str:name$<form name=$formname$ method="post" action=$v$ style="display:none">
   <input type="hidden" name=$state_param_name$
   value=$stateparam$/>
   </form></a> >>) *)

(* 	   let stateparam = string_of_int i in
   << <form name="hiddenform" method="post" action=$v$>
   <input type="hidden" name=$state_param_name$
   value=$stateparam$/>
   <a href="javascript:document.hiddenform.submit ()">$str:name$</a>
   </form> >>)

   À VOIR ! IMPORTANT ! :

   Pour les form get on peut faire pareil, du style :
   <input type="button"
   onClick="document.form1.submit();document.form2.submit()">
   (problème : on n'a pas accès au bouton)

 *)

      let make_params_names (params : ('t,'tipo,'n) params_type) : 'n =
	let rec aux prefix suffix = function
	    TProd (t1, t2) -> Obj.magic (aux prefix suffix t1, aux prefix suffix t2)
	  | TInt name -> Obj.magic (prefix^name^suffix)
	  | TFloat name -> Obj.magic (prefix^name^suffix)
	  | TString name -> Obj.magic (prefix^name^suffix)
	  | TUserType (name,o,t) -> Obj.magic (prefix^name^suffix)
	  | TUnit -> Obj.magic ("")
	  | TSuffix -> Obj.magic ocsigen_suffix_name
	  | TOption t -> Obj.magic (aux prefix suffix t)
	  | TBool name -> Obj.magic (prefix^name^suffix)
	  | TSum (t1,t2) -> Obj.magic (aux prefix suffix t1, aux prefix suffix t2)
	  | TList (name,t1) -> Obj.magic 
		{it =
		 (fun f l endlist ->
		   let length = List.length l in
		   snd
		     (List.fold_right 
			(fun el (i,l2) -> 
			  let i'= i-1 in
			  (i',(f (aux (prefix^name^".") (make_list_suffix i') t1) el)
			   @l2))
			l
			(length,endlist)))}
	in aux "" "" params
	  
      let get_form ?a
	  (service : ('get,unit,'kind,'tipo,'gn,unit param_name) service) 
	  (sp : server_params)
	  (f : 'gn -> Pages.form_content_elt list) =
	let urlname =
	  (if service.external_service
	  then (reconstruct_absolute_url_path sp.current_url service.url None)
	  else (reconstruct_relative_url_path sp.current_url service.url None)) in
	let state_param =
	  (match  service.url_state with
	    None -> None
	  | Some i -> 
	      let i' = string_of_int i in
	      Some (Pages.make_input ~typ:Pages.hidden
		      ~name:state_param_name ~value:i' ()))
	in
	let inside = f (make_params_names service.get_params_type) in
	let i1, i =
	  match state_param, inside with
	    Some s, i -> (Pages.make_hidden_field s),i
	  | None, i1::i -> i1, i
	  | None, [] -> (Pages.make_empty_form_content ()),[]
	in Pages.make_get_form ?a ~action:urlname i1 i

      let post_form ?a
	  (service : ('get,'form,'kind,'tipo,'gn,'pn) service) 
	  (sp : server_params)
	  (f : 'pn -> Pages.form_content_elt list) (getparams : 'get) =
	let suff,params_string = construct_params service.get_params_type getparams in
	let suff = (if service.url_prefix then Some suff else None) in
	let urlname = 
	  (if service.external_service 
	  then (reconstruct_absolute_url_path sp.current_url service.url suff)
	  else (reconstruct_relative_url_path sp.current_url service.url suff))
	in
	let state_param =
	  (match  service.url_state with
	    None -> None
	  | Some i -> 
	      let i' = string_of_int i in
	      Some (Pages.make_input ~typ:Pages.hidden
		      ~name:state_param_name ~value:i' ()))
	in
	let inside = f (make_params_names service.post_params_type) in
	let i1, i =
	  match state_param, inside with
	    Some s, i -> (Pages.make_hidden_field s),i
	  | None, i1::i -> i1, i
	  | None, [] -> (Pages.make_empty_form_content ()),[]
	in Pages.make_post_form ?a
	  ~action:(add_to_string urlname "?" params_string)
	  i1 i

      let make_uri 
	  (service : ('get, unit, 'kind, 'tipo,'gn,'pn) service) sp
	  (getparams : 'get) : Pages.uri =
	let suff,params_string = construct_params service.get_params_type getparams in
	let suff = (if service.url_prefix then Some suff else None) in
	let uri = 
	  (if service.external_service 
	  then (reconstruct_absolute_url_path sp.current_url service.url suff)
	  else (reconstruct_relative_url_path sp.current_url service.url suff))
	in
	match service.url_state with
	  None ->
	    Pages.make_uri_from_string (add_to_string uri "?" params_string)
	| Some i -> 
	    Pages.make_uri_from_string 
	      (add_to_string (uri^"?"^state_param_name^"="^(string_of_int i))
		 "&" params_string)

(* actions : *)
      let action_a ?a ?(reload=true) action h content =
	let formname="hiddenform"^(string_of_int (counter ())) in
	let href="javascript:document.getElementById(\""^formname^"\").submit ()" in
	let action_param_name = action_prefix^action_name in
	let action_param = (action.action_name) in
	let reload_name = action_prefix^action_reload in
	let reload_param = 
	  if reload 
	  then [Pages.make_hidden_field
		  (Pages.make_input ~typ:Pages.hidden
		     ~name:reload_name ~value:reload_name ())]
	  else [] in
	let v = h.full_url in
	Pages.make_post_form ~inline:true 
	  ~id:formname ~action:v
	  (Pages.make_div ~classe:["inline"]
	     [Pages.make_a ?a ~href:href content])
	  ((Pages.make_hidden_field
	      (Pages.make_input ~typ:Pages.hidden ~name:action_param_name
		 ~value:action_param ()))
	   ::reload_param)
	  
      let action_form ?a
	  ?(reload=true) (action : ('a,'pn) action) h 
	  (f : 'pn -> Pages.form_content_elt list) = 
	let action_param_name = action_prefix^action_name in
	let action_param = (action.action_name) in
	let reload_name = action_prefix^action_reload in
	let action_line = Pages.make_input ~typ:Pages.hidden ~name:action_param_name ~value:action_param () in
	let v = h.full_url in
	let inside = f (make_params_names action.action_params_type) in
	let inside_reload = 
	  if reload 
	  then (Pages.make_hidden_field 
		  (Pages.make_input ~typ:Pages.hidden ~name:reload_name ~value:reload_name ()))
	    ::inside
	  else inside 
	in
	Pages.make_post_form ?a ~action:v
	  (Pages.make_hidden_field action_line)
	  inside_reload

	  
	  
	  
      let js_script = Pages.make_js_script
      let css_link = Pages.make_css_link


      let gen_input ?a ?value ?(pwd = false)
	  (string_of : 'a -> string) (name : 'a param_name) =
	let typ = if pwd then Pages.password else Pages.text in
	match value with
	  None ->
	    Pages.make_input ?a ~typ:typ ~name:name ()
	| Some v -> 
	    Pages.make_input
	      ?a
	      ~value:(string_of v)
	      ~typ:typ ~name:name ()

      let int_input ?a ?value (name : int param_name) = 
	gen_input ?a ?value string_of_int name
      let float_input ?a ?value (name : float param_name) =
	gen_input ?a ?value string_of_float name
      let string_input ?a ?value (name : string param_name) =
	gen_input ?a ?value id name
      let user_type_input = gen_input ~pwd:false

      let int_password_input ?a ?value (name : int param_name) = 
	gen_input ~pwd:true ?a ?value string_of_int name
      let float_password_input ?a ?value (name : float param_name) =
	gen_input ~pwd:true ?a ?value string_of_float name
      let string_password_input ?a ?value (name : string param_name) =
	gen_input ~pwd:true ?a ?value id name
      let user_type_password_input = gen_input ~pwd:true

      let hidden_gen_input ?a string_of (name : 'a param_name) v = 
	let vv = string_of v in
	Pages.make_input ?a ~typ:Pages.hidden ~name:name ~value:vv ()

      let hidden_int_input ?a (name : int param_name) v = 
	hidden_gen_input ?a string_of_int name v
      let hidden_float_input ?a (name : float param_name) v =
	hidden_gen_input ?a string_of_float name v
      let hidden_string_input ?a (name : string param_name) v =
	hidden_gen_input ?a id name v
      let hidden_user_type_input = hidden_gen_input

      let bool_checkbox ?a ?checked (name : bool param_name) =
	Pages.make_input ?a ?checked ~typ:Pages.checkbox ~name:name ()

      let string_radio ?a ?checked (name : string option param_name) value =
	Pages.make_input
	  ?a ?checked ~typ:Pages.radio ~name:name ~value:value ()
      let int_radio ?a ?checked (name : int option param_name) value =
	Pages.make_input
	  ?a ?checked ~typ:Pages.radio ~name:name 
	  ~value:(string_of_int value) ()
      let float_radio ?a ?checked (name : float option param_name) value =
	Pages.make_input
	  ?a ?checked ~typ:Pages.radio ~name:name 
	  ~value:(string_of_float value) ()
      let user_type_radio ?a ?checked string_of
	  (name : 'a option param_name) (value : 'a) =
	Pages.make_input
	  ?a ?checked ~typ:Pages.radio ~name:name ~value:(string_of value) ()

      let textarea ?a (name : string param_name) =
	Pages.make_textarea ?a ~name:name

      let submit_input ?a s =
	Pages.make_input ?a ~typ:Pages.submit ~value:s ()


    end : OCSIGENSIG with 
     type page = Pages.page
     and type form_content_elt = Pages.form_content_elt
     and type form_elt = Pages.form_elt
     and type a_content_elt = Pages.a_content_elt
     and type a_elt = Pages.a_elt
     and type div_content_elt = Pages.div_content_elt
     and type uri = Pages.uri
     and type link_elt = Pages.link_elt
     and type script_elt = Pages.script_elt
     and type textarea_elt = Pages.textarea_elt
     and type input_elt = Pages.input_elt
     and type pcdata_elt = Pages.pcdata_elt
     and type a_attrib_t = Pages.a_attrib_t
     and type form_attrib_t = Pages.form_attrib_t
     and type input_attrib_t = Pages.input_attrib_t
     and type textarea_attrib_t = Pages.textarea_attrib_t
     and type link_attrib_t = Pages.link_attrib_t
     and type script_attrib_t = Pages.script_attrib_t
     and type input_type_t = Pages.input_type_t)


(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)


module Xhtml_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = xhtml elt Lwt.t
  type form_content_elt = form_content elt
  type uri = XHTML.M.uri
  type a_content_elt = a_content elt
  type div_content_elt = div_content elt

  type a_elt = a elt
  type form_elt = form elt

  type textarea_elt = textarea elt
  type input_elt = input elt

  type link_elt = link elt
  type script_elt = script elt

  type pcdata_elt = pcdata elt

  type a_attrib_t = Xhtmltypes.a_attrib XHTML.M.attrib list
  type form_attrib_t = Xhtmltypes.form_attrib XHTML.M.attrib list
  type input_attrib_t = Xhtmltypes.input_attrib XHTML.M.attrib list
  type textarea_attrib_t = Xhtmltypes.textarea_attrib XHTML.M.attrib list
  type link_attrib_t = Xhtmltypes.link_attrib XHTML.M.attrib list
  type script_attrib_t = Xhtmltypes.script_attrib XHTML.M.attrib list

  type input_type_t = 
      [`Button | `Checkbox | `File | `Hidden | `Image
    | `Password | `Radio | `Reset | `Submit | `Text]

  let hidden = `Hidden
  let text = `Text
  let password = `Password
  let checkbox = `Checkbox
  let radio = `Radio
  let submit = `Submit

  let make_uri_from_string = XHTML.M.make_uri_from_string

  let add_css (a : 'a) : 'a = 
    let css = 
      XHTML.M.toelt 
	(XHTML.M.style ~contenttype:"text/css"
	   [pcdata "\n.inline {display: inline}\n.nodisplay {display: none}\n"])
    in
    let rec aux = function
      | (XML.Element ("head",al,el))::l -> (XML.Element ("head",al,css::el))::l
      | (XML.BlockElement ("head",al,el))::l -> 
	  (XML.BlockElement ("head",al,css::el))::l
      | (XML.SemiBlockElement ("head",al,el))::l -> 
	  (XML.SemiBlockElement ("head",al,css::el))::l
      | (XML.Node ("head",al,el))::l -> (XML.Node ("head",al,css::el))::l
      | e::l -> e::(aux l)
      | [] -> []
    in
    XHTML.M.tot
      (match XHTML.M.toelt a with
      | XML.Element ("html",al,el) -> XML.Element ("html",al,aux el) 
      | XML.BlockElement ("html",al,el) -> XML.BlockElement ("html",al,aux el) 
      | XML.SemiBlockElement ("html",al,el) -> 
	  XML.SemiBlockElement ("html",al,aux el)
      | XML.Node ("html",al,el) -> XML.Node ("html",al,aux el)
      | e -> e)

  let create_sender = Sender_helpers.create_xhtml_sender
  let send ~content = 
    Sender_helpers.send_xhtml_page ~content:content

  let make_a ?(a=[]) ~href l : a_elt = 
    XHTML.M.a ~a:((a_href (make_uri_from_string href))::a) l

  let make_get_form ?(a=[]) ~action elt1 elts : form_elt = 
    form ~a:((a_method `Get)::a) 
      ~action:(make_uri_from_string action) elt1 elts

  let make_post_form ?(a=[]) ~action ?id ?(inline = false) elt1 elts 
      : form_elt = 
    let aa = (match id with
      None -> a
    | Some i -> (a_id i)::a) 
    in
    form ~a:((a_method `Post)::
	     (if inline then (a_class ["inline"])::aa else aa))
      ~action:(make_uri_from_string action) elt1 elts

  let make_hidden_field content = 
    div ~a:[a_class ["nodisplay"]] [content]

  let make_div ~classe (c : a_elt list) =
    div ~a:[a_class classe] (c :> div_content_elt list)

  let make_empty_form_content () = p [pcdata ""] (**** à revoir !!!!! *)

  let make_input ?(a=[]) ?(checked=false) ~typ ?name ?value () = 
    let a2 = match value with
      None -> a
    | Some v -> (a_value v)::a
    in
    let a3 = match name with
      None -> a2
    | Some v -> (a_name v)::a2
    in
    let a4 = if checked then (a_checked `Checked)::a3 else a3 in
    input ~a:((a_input_type typ)::a4) ()

  let make_textarea ?(a=[]) ~name:name = 
    let a3 = (a_name name)::a in
    textarea ~a:a3

  let make_css_link ?(a=[]) uri =
    link ~a:((a_href uri)::
	     (a_type "text/css")::(a_rel [`Stylesheet])::a) ()
      
  let make_js_script ?(a=[]) uri =
    script ~a:((a_src uri)::a) ~contenttype:"text/javascript" (pcdata "")

end



(****************************************************************************)
(*****************************************************************************)

module Xhtml__ = Make(Xhtml_)

(* As we want -> [> a ] elt and not -> [ a ] elt, we define a new module: *)
module Xhtml = struct
  open XHTML.M
  open Xhtmltypes
  include Xhtml__
  let a = (a : ?a:([< Xhtmltypes.a_attrib > `Href ] XHTML.M.attrib list) ->
    ('get, unit, 'b, [< `WithSuffix | `WithoutSuffix ], 'c, unit param_name) 
      service ->
	server_params -> 
	  Xhtmltypes.a_content XHTML.M.elt list -> 
	    'get -> [Xhtmltypes.a] XHTML.M.elt
		:> ?a:([< Xhtmltypes.a_attrib > `Href ] XHTML.M.attrib list) ->
		  ('get, unit, 'b, [< `WithSuffix | `WithoutSuffix ], 'c, unit param_name) 
		    service ->
		      server_params -> 
			Xhtmltypes.a_content XHTML.M.elt list -> 
			  'get -> [> Xhtmltypes.a] XHTML.M.elt)

  let css_link = 
    (css_link : ?a:([< link_attrib > `Href `Rel `Type ] attrib list) ->
      uri -> [ link ] elt
	  :> ?a:([< link_attrib > `Href `Rel `Type ] attrib list) ->
	    uri -> [> link ] elt)

  let js_script = (js_script
		     : ?a:([< script_attrib > `Src ] attrib list) ->
		       uri -> [ script ] elt
			   :> ?a:([< script_attrib > `Src ] attrib list) ->
			     uri -> [> script ] elt)

  let get_form = 
    (get_form
       : ?a:([< form_attrib > `Method ] attrib list) ->
	 ('get, unit, 'c, 'd, 'getnames, unit param_name) service ->
	   server_params -> ('getnames -> form_content elt list) -> [form] elt
	       :> ?a:([< form_attrib > `Method ] attrib list) ->
		 ('get, unit, 'c, 'd, 'getnames, unit param_name) service ->
		   server_params -> ('getnames -> form_content elt list) -> [>form] elt)

  let post_form = 
    (post_form
       : ?a:([< form_attrib > `Class `Id `Method ] attrib list) ->
	 ('get, 'post, 'c, [< `WithSuffix | `WithoutSuffix ], 'getnames, 'postnames) service ->
	   server_params ->
	     ('postnames -> form_content elt list) -> 'get -> [form] elt
		 :> ?a:([< form_attrib > `Class `Id `Method ] attrib list) ->
		   ('get, 'post, 'c, [< `WithSuffix | `WithoutSuffix ], 'getnames, 'postnames) service ->
		     server_params ->
		       ('postnames -> form_content elt list) -> 'get -> [>form] elt)

  let int_input = 
    (int_input 
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 ?value:int ->
	   int param_name -> [ input ] elt
	       :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
		 ?value:int ->
		 int param_name -> [> input ] elt)
      
  let float_input = 
    (float_input 
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 ?value:float ->
	 float param_name -> [ input ] elt
	     :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	       ?value:float ->
		 float param_name -> [> input ] elt)

  let user_type_input = 
    (user_type_input
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 ?value:'a ->
	   ('a -> string) ->
	     'a param_name -> [ input ] elt
		 :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
		   ?value:'a ->
		     ('a -> string) ->
		       'a param_name -> [> input ] elt)
      
  let string_input = 
    (string_input 
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) ->
	 ?value:string -> string param_name -> [ input ] elt
	   :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) ->
	     ?value:string -> string param_name -> [> input ] elt)

  let int_password_input = 
    (int_password_input 
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 ?value:int ->
	   int param_name -> [ input ] elt
	       :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
		 ?value:int ->
		 int param_name -> [> input ] elt)
      
  let float_password_input = 
    (float_password_input 
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 ?value:float ->
	 float param_name -> [ input ] elt
	     :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	       ?value:float ->
		 float param_name -> [> input ] elt)

  let user_type_password_input = 
    (user_type_password_input
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 ?value:'a ->
	   ('a -> string) ->
	     'a param_name -> [ input ] elt
		 :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
		   ?value:'a ->
		     ('a -> string) ->
		       'a param_name -> [> input ] elt)
      
  let string_password_input = 
    (string_password_input 
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) ->
	 ?value:string -> string param_name -> [ input ] elt
	   :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) ->
	     ?value:string -> string param_name -> [> input ] elt)

  let hidden_int_input = 
    (hidden_int_input
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 int param_name -> int -> [ input ] elt
	     :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	       int param_name -> int -> [> input ] elt)
  let hidden_float_input = 
    (hidden_float_input
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 float param_name -> float -> [ input ] elt
	     :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	       float param_name -> float -> [> input ] elt)
  let hidden_string_input = 
    (hidden_string_input
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 string param_name -> string -> [ input ] elt
	     :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	       string param_name -> string -> [> input ] elt)
  let hidden_user_type_input = 
    (hidden_user_type_input
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 ('a -> string) ->
	   'a param_name -> 'a -> [ input ] elt
	       :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
		 ('a -> string) ->
		   'a param_name -> 'a -> [> input ] elt)
      


  let bool_checkbox = 
    (bool_checkbox
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 ?checked:bool ->
	   bool param_name -> [ input ] elt
	       :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
		 ?checked:bool ->
		   bool param_name -> [> input ] elt)

  let string_radio = 
    (string_radio
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 ?checked:bool ->
	    string option param_name -> string -> [ input ] elt
	     :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	       ?checked:bool ->
		  string option param_name -> string -> [> input ] elt)
  let int_radio = 
    (int_radio
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 ?checked:bool ->
	    int option param_name -> int -> [ input ] elt
	     :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	       ?checked:bool ->
		  int option param_name -> int -> [> input ] elt)
  let float_radio = 
    (float_radio
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 ?checked:bool ->
	    float option param_name -> float -> [ input ] elt
	     :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	       ?checked:bool ->
		  float option param_name -> float -> [> input ] elt)
  let user_type_radio = 
    (user_type_radio
       : ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
	 ?checked:bool ->
	   ('a -> string) ->
	     'a option param_name -> 'a -> [ input ] elt
		 :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
		   ?checked:bool ->
		     ('a -> string) ->
		       'a option param_name -> 'a -> [> input ] elt)

  let textarea = (textarea
		    : ?a:([< textarea_attrib > `Name ] attrib list ) -> 
		      string param_name -> rows:number -> cols:number -> 
			[ `PCDATA ] XHTML.M.elt ->
			  [ textarea ] elt
			    :> ?a:([< textarea_attrib > `Name ] attrib list ) -> 
			      string param_name -> rows:number -> cols:number -> 
				[ `PCDATA ] XHTML.M.elt ->
				  [> textarea ] elt)

  let submit_input = (submit_input
			: ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
			  string -> [ input ] elt
			      :> ?a:([< input_attrib > `Input_Type `Name `Value ] attrib list ) -> 
				string -> [> input ] elt)

  let action_a = (action_a
		    : ?a:([< a_attrib > `Href ] attrib list) ->
		      ?reload:bool ->
			('a,'b) action -> 
			  server_params -> 
			    a_content elt list -> 
			      [ form ] elt
				:> ?a:([< a_attrib > `Href ] attrib list) ->
				  ?reload:bool ->
				    ('a,'b) action -> 
				      server_params -> 
					a_content elt list -> 
					  [> form ] elt)

  let action_form = (action_form
		       : ?a:([< form_attrib > `Class `Id `Method ] attrib list) ->
			 ?reload:bool ->
			   ('a, 'b) action ->
			     server_params -> 
			       ('b -> form_content elt list) ->
				 [ form ] elt
				   :> ?a:([< form_attrib > `Class `Id `Method ] attrib list) ->
				     ?reload:bool ->
				       ('a, 'b) action ->
					 server_params -> 
					   ('b -> form_content elt list) ->
					     [> form ] elt)

end

(****************************************************************************)
(****************************************************************************)
(****************************************************************************)
(****************************************************************************)

module Text_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = string
  type form_content_elt = string
  type uri = string
  type a_content_elt = string
  type div_content_elt = string

  type a_elt = string
  type form_elt = string

  type textarea_elt = string
  type input_elt = string

  type link_elt = string
  type script_elt = string

  type pcdata_elt = string

  let create_sender = Sender_helpers.create_xhtml_sender
  let send = Sender_helpers.send_text_page

  type a_attrib_t = string
  type form_attrib_t = string
  type input_attrib_t = string
  type textarea_attrib_t = string
  type link_attrib_t = string
  type script_attrib_t = string

  type input_type_t = string

  let hidden = "hidden"
  let text = "text"
  let password = "password"
  let checkbox = "checkbox"
  let radio = "radio"
  let submit = "submit"

  let make_uri_from_string x = x

  let make_a ?(a="") ~href l : a_elt = 
    "<a href=\""^href^"\""^a^">"^(List.fold_left (^) "" l)^"</a>"

  let make_get_form ?(a="") ~action elt1 elts : form_elt = 
    "<form method=\"get\" action=\""^(make_uri_from_string action)^"\""^a^">"^
    elt1^(List.fold_left (^) "" elts)^"</form>"

  let make_post_form ?(a="") ~action ?id ?(inline = false) elt1 elts 
      : form_elt = 
    let aa = (match id with
      None -> a
    | Some i -> " id="^i^" "^a)
    in
    "<form method=\"post\" action=\""^(make_uri_from_string action)^"\""^
    (if inline then "style=\"display: inline\"" else "")^aa^">"^
    elt1^(List.fold_left (^) "" elts)^"</form>"

  let make_hidden_field content = 
    "<div style=\"display: none\""^content^"</div>"

  let make_div ~classe c =
    "<div class=\""^(List.fold_left (fun a b -> a^" "^b) "" classe)^"\""^
    (List.fold_left (^) "" c)^"</div>"

  let make_empty_form_content () = ""

  let make_input ?(a="") ?(checked=false) ~typ ?name ?value () = 
    let a2 = match value with
      None -> a
    | Some v -> " value="^v^" "^a
    in
    let a3 = match name with
      None -> a2
    | Some v -> " name="^v^" "^a2
    in
    let a4 = if checked then " checked=\"checked\" "^a3 else a3 in
    "<input type=\""^typ^"\" "^a4^"/>"

  let make_textarea ?(a="") ~name:name ~rows ~cols s = 
    "<textarea name=\""^name^"\" rows=\""^(string_of_int rows)^
    "\" cols=\""^(string_of_int cols)^"\" "^a^">"^s^"</textarea>"

  let make_css_link ?(a="") uri =
    "<link href=\""^uri^" type=\"text/css\" rel=\"stylesheet\" "^a^"/>"
								      
  let make_js_script ?(a="") uri =
    "<script src=\""^uri^" contenttype=\"text/javascript\" "^a^"></script>"

end



(****************************************************************************)
(****************************************************************************)

module Text = Make(Text_)


(****************************************************************************)
(****************************************************************************)
(****************************************************************************)
(****************************************************************************)

let localhost = Unix.inet_addr_of_string "127.0.0.1"

let execute generate_page sockaddr cookie = 
  let ip = match sockaddr with
    Unix.ADDR_INET (ip,port) -> ip
  | _ -> localhost
  in
  let (tablesref, new_session) = 
    (match cookie with
      None -> (ref (new_session_tables ()), true)
    | Some c -> try (ref (Cookies.find cookie_table (ip,c)), false)
    with Not_found -> (ref (new_session_tables ()), true))
  in
  let send_page,sender,working_dir = generate_page ip tablesref in
  let cookie2 = 
    if are_empty_tables !tablesref
    then ((if not new_session 
    then match cookie with
      Some c -> Cookies.remove cookie_table (ip,c)
    | None -> ());None)
    else (if new_session 
    then let c = new_cookie cookie_table ip in
    (Cookies.add cookie_table (ip,c) !tablesref;
     Some c)
    else cookie)
  in (cookie2, send_page, sender, ("/"^(reconstruct_url_path working_dir)))


let get_page 
    (url, path, params, internal_state, get_params, post_params, useragent)
    sockaddr cookie = 
  let fullurl = path^params in
  let generate_page ip session_tables_ref =
    try (* D'abord recherche dans la table de session *)
      Messages.debug ("--- I search "^(reconstruct_url_path url)^" in the session table:");
      (find_service
	 !session_tables_ref
	 (session_tables_ref,
	  url,
	  internal_state,
	  get_params,
	  post_params,
	  useragent,
	  ip,
	  fullurl))
    with Ocsigen_404 | Ocsigen_Wrong_parameter -> 
      try (* ensuite dans la table globale *)
	Messages.debug "--- I search in the global table:";
	(find_service 
	   global_tables
	   (session_tables_ref,
	    url,
	    internal_state,
	    get_params,
	    post_params,
	    useragent,
	    ip,
	    fullurl))
      with exn ->
	match exn with 
	  Ocsigen_404 | Ocsigen_Wrong_parameter -> 
            (* si pas trouvé avec, on essaie sans l'état *)
	    (match internal_state with
	      None -> raise exn
	    | _ -> try (* d'abord la table de session *)
		Messages.debug "--- I search in the session table, without state parameter:";
		(find_service 
		   !session_tables_ref
		   (session_tables_ref,
		    url,
		    None,
		    get_params,
		    post_params,
		    useragent,
		    ip,
		    fullurl))
	    with Ocsigen_404 | Ocsigen_Wrong_parameter -> 
              (* ensuite dans la table globale *)
	      Messages.debug "--- I search in the global table, without state parameter:";
	      (find_service 
		 global_tables
		 (session_tables_ref,
		  url,
		  None,
		  get_params,
		  post_params,
		  useragent,
		  ip,
		  fullurl)))
	| _ -> raise exn
  in try 
    execute generate_page sockaddr cookie
  with 
    Ocsigen_Typing_Error l -> 
      (cookie, (Sender_helpers.send_xhtml_page 
		  ~content:(Lwt.return (Error_pages.page_error_param_type l))),
       Sender_helpers.create_xhtml_sender, "/")
  | Ocsigen_Wrong_parameter -> 
      (cookie, (Sender_helpers.send_xhtml_page 
		  ~content:(Lwt.return Error_pages.page_bad_param)),
       Sender_helpers.create_xhtml_sender, "/")


let make_action action_name action_params 
    (url, path, params, _, _, _, useragent) sockaddr cookie =
  let fullurl = path^params in
  let generate_page ip session_tables_ref =
    let action,working_dir = 
      (try 
	find_action !session_tables_ref action_name
      with Not_found ->
	(find_action global_tables action_name))
    in 
    (action 
       (make_server_params 
	  working_dir session_tables_ref url fullurl [] 
	  action_params useragent ip)),
    (),
    working_dir
  in try
    let (c,(),(),wd) = execute 
	generate_page
	sockaddr cookie in
    Messages.debug "Action executed";
    (c,wd)
  with 
    Ocsigen_Typing_Error _ -> (cookie, "/")
  | Ocsigen_Wrong_parameter -> (cookie, "/")


(** Module loading *)
exception Ocsigen_error_while_loading of string
    
let load_ocsigen_module ~dir ~cmo =
  let save_current_dir = get_current_dir () in
  try
    begin_load_ocsigen_module ();
    absolute_change_dir dir;
    Dynlink.loadfile cmo;
    absolute_change_dir save_current_dir;
    end_load_ocsigen_module ()
  with Dynlink.Error e -> 
    absolute_change_dir save_current_dir;
    end_load_ocsigen_module ();
    raise
      (Ocsigen_error_while_loading (cmo^" ("^(Dynlink.error_message e)^")"))
  | e -> 
      absolute_change_dir save_current_dir;
      end_load_ocsigen_module ();
      raise e (*Ocsigen_error_while_loading cmo*)


let number_of_sessions () = Cookies.length cookie_table

(* This is used by server.ml. 
   I put that here because I need it to be accessible for profiling. *)
let get_number_of_connected, 
  incr_connected, 
  decr_connected =
  let connected = ref 0 in
  ((fun () -> !connected),
   (fun () -> connected := !connected + 1),
   (fun () -> connected := !connected - 1))

