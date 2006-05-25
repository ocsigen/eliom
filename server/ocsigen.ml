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
open XHTML.M
open Xhtmltypes

let _ = Random.self_init ()

let rec list_remove a = function
    [] -> []
  | b::l when a = b -> l
  | b::l -> b::(list_remove a l)

let rec list_assoc_remove a = function
    [] -> raise Not_found
  | (b,c)::l when a = b -> c,l
  | b::l -> let v,ll = list_assoc_remove a l in v,b::ll


(** Type of answers from modules (web pages) *)
type page = xhtml elt

(** Type of formulars *)
type form_content_l = form_content elt list

(** type of URL, without parameter *)
type url_path = string list
type current_url = string list

let string_list_of_current_url x = x


(** various functions for URLs *)
let remove_slash = function
    [] -> []
  | ""::l -> l
  | l -> l

(* The current working directory *)
let absolute_change_dir, get_current_dir =
  let current_dir : url_path ref = ref [] in
  ((fun dir -> current_dir := remove_slash dir), 
   (fun () -> !current_dir))

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
  (* print_endline ((reconstruct_url_path current_url)^"->"^(reconstruct_url_path u)^"="^s); *)
  if s = "" then defaultpagename else s



(** Type of http parameters *)
type server_params = {full_url: string;
		      current_url: current_url;
		      user_agent: string;
		      ip: Unix.inet_addr;
		      get_params: (string * string) list;
		      post_params: (string * string) list}

(* abstract *)
type server_params2 = url_path * server_params

(** Create server parameters record *)
let make_server_params
    url fullurl get_params post_params useragent ip = 
  {full_url= fullurl;
   user_agent=useragent;
   current_url=url;
   ip=ip;
   get_params = get_params;
   post_params = post_params}

let make_server_params2
    url fullurl get_params post_params useragent ip urlsuffix = 
  urlsuffix,
  (make_server_params url fullurl get_params post_params useragent ip)




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
exception Ocsigen_Typing_Error of string
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
type 'a name = string

type ('a,'b) binsum = Inj1 of 'a | Inj2 of 'b;;

let ocsigen_suffix_name = "__ocsigen_suffix"

(* This is a generalized algebraic datatype *)
type ('a,'tipo,'names) params_type =
    (* 'tipo is [`WithSuffix] or [`WithoutSuffix] *)
    TProd of (* 'a1 *) ('a,'tipo,'names) params_type * (* 'a2 *) ('a,'tipo,'names) params_type (* 'a = 'a1 * 'a2 ; 'names = 'names1 * 'names2 *)
  | TOption of (* 'a1 *) ('a,'tipo,'names) params_type (* 'a = 'a1 option *)
  | TList of 'a name * (* 'a1 *) ('a,'tipo,'names) params_type (* 'a = 'a1 list *)
  | TSum of (* 'a1 *) ('a,'tipo,'names) params_type * (* 'a2 *) ('a,'tipo,'names) params_type (* 'a = ('a1, 'a2) binsum *)
  | TString of string name (* 'a = string *)
  | TInt of int name (* 'a = int *)
  | TBool of bool name (* 'a = bool *)
  | TUserType of ('a name * (string -> 'a) * ('a -> string)) (* 'a = 'a *)
  | TSuffix (* 'a = string *)
  | TUnit (* 'a = unit *);;

type 'an listnames = 
    {it:'el 'a. ('an -> 'el -> 'a list) -> 'el list -> 'a list -> 'a list}

(* As GADT are not implemented in OCaml for the while, we define our own
   constructors for params_type *)
let int (n : string) : (int,[`WithoutSuffix], int name) params_type = TInt n
let bool (n : string) : (bool,[`WithoutSuffix], bool name) params_type= TBool n
let string (n : string) : (string,[`WithoutSuffix], string name) params_type = 
  TString n
let unit : (unit,[`WithoutSuffix], unit name) params_type = TUnit
let user_type
    (of_string : string -> 'a) (from_string : 'a -> string) (n : string)
    : ('a,[`WithoutSuffix], 'a name) params_type =
  Obj.magic (TUserType (n,of_string,from_string))
let sum (t1 : ('a,[`WithoutSuffix], 'an) params_type) 
    (t2 : ('b,[`WithoutSuffix], 'bn) params_type) 
    : (('a,'b) binsum,[`WithoutSuffix], 'an * 'bn) params_type =
  Obj.magic (TSum (t1, t2))
let prod (t1 : ('a,[`WithoutSuffix], 'an) params_type) 
    (t2 : ('b,[`WithoutSuffix], 'bn) params_type)
    : (('a * 'b),[`WithoutSuffix], 'an * 'bn) params_type =
  Obj.magic (TProd ((Obj.magic t1), (Obj.magic t2)))
let option (t : ('a,[`WithoutSuffix], 'an) params_type) 
    : ('a option,[`WithoutSuffix], 'an) params_type = 
  Obj.magic (TOption t)
let list (n : string) (t : ('a,[`WithoutSuffix], 'an) params_type) 
    : ('a list,[`WithoutSuffix], 'an listnames) params_type = 
  Obj.magic (TList (n,t))
let ( ** ) = prod

let suffix_only : (string, [`WithSuffix], string name) params_type = 
  (Obj.magic TSuffix)
let suffix (t : ('a,[`WithoutSuffix], 'an) params_type) : 
    ((string * 'a), [`WithSuffix], string name * 'an) params_type = 
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
let reconstruct_params 
    (typ : ('a,[<`WithSuffix|`WithoutSuffix],'b) params_type) 
    params urlsuffix : 'a = 
(* AEFFFFFFFFFFFFFFF
  let rec aux_list t params name pref suff =
    let length,l = list_assoc_remove (pref^name^suff) params in
    let long = try int_of_string length 
    with _ -> raise (Ocsigen_Typing_Error (pref^name^suff)) in
    let rec aa i p pref suff =
      if i=long 
      then (Obj.magic []), p
      else
	let v,l = Obj.magic (aux t p pref (suff^(make_list_suffix i))) in
	let v2,l2 = aa (i+1) l pref suff in
	(Obj.magic (v::v2)),l2
    in 
    aa 0 params (pref^name^".") suff *)
  let rec aux_list t params name pref suff =
    let rec aa i lp pref suff =
      try
	let v,lp2 = Obj.magic (aux t lp pref (suff^(make_list_suffix i))) in
	let v2,lp3 = aa (i+1) lp2 pref suff in
	(Obj.magic (v::v2)),lp3
      with Not_found -> ((Obj.magic []),lp)
    in 
    aa 0 params (pref^name^".") suff
  and aux typ params pref suff =
    match typ with
      TProd (t1, t2) ->
	let v1,l1 = Obj.magic (aux t1 params pref suff) in
	let v2,l2 = Obj.magic (aux t2 l1 pref suff) in
	(Obj.magic (v1,v2)),l2
    | TOption t -> 
	(try 
	  let v,l = Obj.magic (aux t params pref suff) in
	  (Obj.magic (Some v)),l
	with Not_found -> (Obj.magic None), params)
    | TBool name -> 
	(try 
	  let v,l = (list_assoc_remove (pref^name^suff) params) in
	  (Obj.magic true),l
	with Not_found -> (Obj.magic false), params)
    | TList (n,t) -> Obj.magic (aux_list t params n pref suff)
    | TSum (t1, t2) -> 
	(try let v,l = Obj.magic (aux t1 params pref suff) in
	(Obj.magic (Inj1 v)),l
	with Not_found -> 
	  let v,l = Obj.magic (aux t2 params pref suff) in 
	  (Obj.magic (Inj2 v)),l)
    | TString name -> let v,l = list_assoc_remove (pref^name^suff) params in
      (Obj.magic v),l
    | TInt name -> 
	let v,l = (list_assoc_remove (pref^name^suff) params) in 
	(Obj.magic (try int_of_string v
	with _ -> raise (Ocsigen_Typing_Error (pref^name^suff)))),l
    | TUserType (name, of_string, string_of) ->
	let v,l = (list_assoc_remove (pref^name^suff) params) in 
	(Obj.magic (try of_string v
	with _ -> raise (Ocsigen_Typing_Error (pref^name^suff)))),l
    | TUnit -> (Obj.magic ()), params
    | TSuffix -> raise (Ocsigen_Internal_Error "Bad use of suffix")
  in
  let aux2 typ =
    let v,l = Obj.magic (aux typ params "" "") in
    if l = [] then v else raise Ocsigen_Wrong_parameter
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
(* AEFFFFFFFF
	let long = List.length ((Obj.magic params) : 'a list) in
	let beg = (pref^list_name^suff^"="^(string_of_int long)) in
	let pref2 = pref^list_name^suff^"." in
	fst 
	  (List.fold_left
	     (fun (s,i) p -> 
	       let ss = 
		 aux t p pref2 (suff^(make_list_suffix i)) in
	       ((concat_strings s "&" ss),(i+1))) (beg,0) (Obj.magic params))
*)
    | TSum (t1, t2) -> (match Obj.magic params with
	Inj1 v -> aux t1 v pref suff
      | Inj2 v -> aux t2 v pref suff)
    | TString name -> pref^name^suff^"="^(Obj.magic params)
    | TInt name -> pref^name^suff^"="^(string_of_int (Obj.magic params))
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
	
(** We associate to a service a function server_params2 -> page *)
module type DIRECTORYTREE =
  sig
    type tables
    type page_table_key =
	{prefix:bool;
	 state: internal_state option}
    val empty_tables : unit -> tables
    val are_empty_tables : tables -> bool
    val add_service : tables -> bool -> url_path -> 
      page_table_key * (int * (server_params2 -> page)) -> unit
    val add_action :
	tables -> string -> (server_params -> unit) -> unit
    val find_service :
	tables ->
	  current_url * internal_state option * (string * string) list *
	    (string * string) list * string * Unix.inet_addr * string -> 
	      page * url_path
    val find_action :
	tables -> string -> (server_params -> unit) * url_path
  end

module Directorytree : DIRECTORYTREE = struct
  (* Each node contains either a list of nodes (case directory)
     or a table of "answers" (functions that will generate the page) *)

  type page_table_key =
      {prefix:bool;
       state: internal_state option}
       (* action: server_params2 -> page *)

  (* module Page_Table = Map.Make(struct type t = page_table_key 
				      let compare = compare end) *)

  module String_Table = Map.Make(struct type t = string
					let compare = compare end)

  type page_table = 
      (page_table_key * ((int * ((server_params2 -> page) * url_path)) list)) list
	(* Here, the url_path is the working directory.
	   That is, the directory in which we are when we register
	   dynamically the pages.
	   Each time we load a page, we change to this directory
	   (in case the page registers new pages).
	 *)

  type action_table = 
      AVide 
    | ATable of ((server_params -> unit) * url_path) String_Table.t

  type dircontent = 
      Vide
    | Table of direlt ref String_Table.t

  and direlt = 
      Dir of dircontent ref
    | File of page_table ref

  type tables = dircontent ref * action_table ref

  let empty_page_table () = []
  let empty_action_table () = AVide
  let empty_dircontent () = Vide

  let find_page_table t (url,getp,postp,ua,ip,fullurl,urlsuffix) k = 
    let sp = (make_server_params2 url fullurl getp postp ua ip urlsuffix) in
    let rec aux = function
      [] -> raise Ocsigen_Wrong_parameter
    | (_,(funct,working_dir))::l ->
	try 
	  absolute_change_dir working_dir;
	  let p = funct sp in
	  Messages.debug "Page found";
	  p,working_dir
	with Ocsigen_Wrong_parameter -> aux l
    in aux (List.assoc k t)

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

  let add_action (_,actiontableref) name action =
    actiontableref :=
      add_action_table !actiontableref
	(name,(action,get_current_dir ()))

  let find_action (_,atr) name =
    find_action_table !atr name

  let add_service (dircontentref,_) session url_act 
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
		    (unique_id, (action, get_current_dir ()))) in
    (* let current_dircontentref = 
      search_dircontentref dircontentref (get_current_dir ()) in *)
    let page_table_ref = 
      search_page_table_ref (*current_*) dircontentref url_act in
    page_table_ref := add_page_table session url_act !page_table_ref content

	 
  let find_service 
      (dircontentref,_) 
      (string_list, state_option, get_param_list, post_param_list, 
       ua, ip, fullurl) =
    try
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
	(search_page_table !dircontentref (change_empty_list string_list)) in
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
	(string_list,
	 get_param_list,
	 post_param_list,
	 ua,
	 ip,
	 fullurl,
	 suffix)
	{prefix = pref;
	 state = state_option}
    with Not_found -> raise Ocsigen_404

end

open Directorytree

type url_table = tables

let global_tables = empty_tables ()

let get_session_tables, set_session_tables =
  let session_tables = ref (empty_tables ()) in
  let err () = raise Ocsigen_register_for_session_outside_session in
  let ok () = !session_tables in
  let get = ref err in
  ((fun () -> !get ()),
   (fun st -> session_tables := st; get := ok))

let new_session_tables = empty_tables
let are_empty_tables = are_empty_tables


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

(** Typed services *)
type internal_service_kind = [`Public_Service | `State_Service]
type service_kind = [`Internal_Service of internal_service_kind | `External_Service]

type ('get,'post,'kind,'tipo,'getnames,'postnames) service = 
    {url: url_path; (* name of the service without parameters *)
     unique_id: int;
     url_prefix: bool;
     external_service: bool;
     url_state: internal_state option;
       (* 'kind is just a type information: it can be only 
	  `Internal_Service `Public_Service or `Internal_Service `State_Service
	  or `External_Service, so that we can't use session services as fallbacks for
	  other session services. If it is a session service, it contains a value
	  (internal state) that will allow to differenciate between
	  services that have the same url.
	*)
     get_params_type: ('get,'tipo,'getnames) params_type;
     post_params_type: ('post,[`WithoutSuffix],'postnames) params_type;
    }

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
    : ('get,unit,[`Internal_Service of 'popo],'tipo,'gn,unit name) service =
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
    : ('get,unit,[`Internal_Service of [`Public_Service]],'tipo,'gn, unit name) service =
  new_service_aux ~url ~prefix ~get_params

let new_local_service
   ~(fallback : ('get,unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service)
    : ('get,unit,[`Internal_Service of [`State_Service]],'tipo,'gn,'pn) service =
  {fallback with url_state = new_state ()}

let register_service_aux
    tables
    session
    state
    ~(service : ('get,'post,[`Internal_Service of 'popo],'tipo,'gn,'pn) service)
    (page_generator : server_params -> 'get -> 'post -> 'fin) =
  add_service tables session service.url
    ({prefix = service.url_prefix;
      state = state},
     (service.unique_id,
      (fun (suff,h) -> page_generator h 
	  (reconstruct_params service.get_params_type h.get_params suff)
	  (reconstruct_params service.post_params_type h.post_params suff))))

let register_service 
    ~(service : ('get,'post,[`Internal_Service of 'g],'tipo,'gn,'pn) service)
    (page_gen : server_params -> 'get -> 'post -> 'fin) =
  if global_register_allowed () then begin
    remove_unregistered (service.url,service.unique_id);
    register_service_aux global_tables false (service.url_state) service page_gen; end
  else Messages.warning "Public service registration after init forbidden! Please correct your module! (ignored)"

(* WARNING: if we create a new service without registering it,
   we can have a link towards a page that does not exist!!! :-(
   That's why I impose to register all service during init.
   The only other way I see to avoid this is to impose a syntax extension
   like "let rec" for service...
 *)

let register_service_for_session
    ~(service : ('get,'post,[`Internal_Service of 'g],'tipo,'gn,'pn) service)
    page =
  register_service_aux (get_session_tables ()) true service.url_state service page

let register_new_service 
    ~url
    ?(prefix=false)
    ~(get_params : ('get,[<`WithoutSuffix|`WithSuffix] as 'tipo,'gn) params_type)
    page
    : ('get,unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,unit name) service =
  let u = new_service ~prefix ~url ~get_params () in
  register_service u page;
  u
    
let register_new_local_service
    ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service)
    page
    : ('get, unit, [`Internal_Service of [`State_Service]],'tipo,'gn,'pn) service =
  let u = (new_local_service fallback) in
  register_service u page;
  u

let register_new_local_service_for_session
    ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service)
    page
    : ('get, unit, [`Internal_Service of [`State_Service]],'tipo,'gn,'pn) service =
  let u = (new_local_service fallback) in
  register_service_for_session u page;
  u


(** Register an service with post parameters in the server *)
let new_post_service_aux
    ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,unit name) service)
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
    ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,unit name) service)
    ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
    : ('get, 'post, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service = 
  if global_register_allowed () then
    let u = new_post_service_aux fallback post_params in
    add_unregistered (u.url,u.unique_id); u
  else raise Ocsigen_service_created_outside_site_loading
  
let new_post_local_service
    ~(fallback : ('get, 'post1, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn1) service)
    ~(post_params : ('post,[`WithoutSuffix],'pn2) params_type)
    : ('get, 'post, [`Internal_Service of [`State_Service]],'tipo,'gn,'pn2) service = 
  {fallback with 
   url_state = new_state ();
   post_params_type = post_params;
  }

let register_new_post_service 
    ~(fallback : ('get, unit, [`Internal_Service of [`Public_Service]],'tipo,'gn,unit name) service)
    ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
    (page_gen : server_params -> 'get -> 'post -> 'fin)
    : ('get,'post, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn) service =
  let u = new_post_service ~fallback:fallback ~post_params:post_params in
  register_service u page_gen;
  u

let register_new_post_local_service
    ~(fallback : ('get, 'post1, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn1) service)
    ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
    page_gen
    : ('get, 'post, [`Internal_Service of [`State_Service]],'tipo,'gn,'pn) service = 
  let u = new_post_local_service ~fallback:fallback ~post_params:post_params in
  register_service u page_gen;
  u

let register_new_post_local_service_for_session
    ~(fallback : ('get, 'post1, [`Internal_Service of [`Public_Service]],'tipo,'gn,'pn1) service)
    ~(post_params : ('post,[`WithoutSuffix],'pn) params_type)
    page_gen
    : ('get, 'post, [`Internal_Service of [`State_Service]],'tipo,'gn,'pn) service = 
  let u = new_post_local_service ~fallback:fallback ~post_params:post_params in
  register_service_for_session u page_gen;
  u


(** actions (new 10/05) *)
type ('post,'pn) action =
    {action_name: string;
     action_params_type: ('post,[`WithoutSuffix],'pn) params_type}

let action_prefix = "__ocsigen_action__"
let action_name = "name"
let action_reload = "reload"

let new_action_name () = string_of_int (counter ())

let new_action
    ~(post_params : ('post,[`WithoutSuffix],'pn) params_type) =
    {
     action_name = new_action_name ();
     action_params_type = post_params;
   }

let register_action_aux tables ~action actionfun =
  add_action tables 
    action.action_name
    (fun h -> actionfun h 
	(reconstruct_params action.action_params_type h.post_params []))

let register_action
    ~(action : ('post,'pn) action)
    (actionfun : (server_params -> 'post -> unit)) : unit =
  register_action_aux global_tables action actionfun

let register_new_action ~post_params actionfun = 
  let a = new_action post_params in
    register_action a actionfun;
    a

let register_action_for_session ~action actionfun =
  register_action_aux (get_session_tables ()) action actionfun


let register_new_action_for_session ~params actionfun =
  let a = new_action params in
    register_action_for_session a actionfun;
    a

(** Satic directories **)
let static_dir : (string, unit, [`Internal_Service of [`Public_Service]],[`WithSuffix],string name, unit name) service =
  {url = [];
   unique_id = counter ();
   url_state = None;
   url_prefix = true;
   external_service = false;
   get_params_type = suffix_only;
   post_params_type = unit
  }



(** Close a session *)
let close_session () = set_session_tables (empty_tables ())

let state_param_name = "__ocsigen_etat__"


(** Functions to construct web pages: *)

let make_a ?(a=[]) l = XHTML.M.a ~a:a l

let a ?(a=[])
    (service : ('get, unit, 'kind, 'tipo,'gn,'pn) service) 
    (current_url : current_url) content
    (getparams : 'get) : [>a] elt =
  let suff,params_string = construct_params service.get_params_type getparams in
  let suff = (if service.url_prefix then Some suff else None) in
  let uri = 
    (if service.external_service 
    then (reconstruct_absolute_url_path current_url service.url suff)
    else (reconstruct_relative_url_path current_url service.url suff))
  in
  match service.url_state with
    None ->
      make_a ~a:((a_href (make_uri_from_string 
			    (add_to_string uri "?" params_string)))::a) 
	content
  | Some i -> 
      make_a ~a:((a_href (make_uri_from_string 
			    (add_to_string 
			       (uri^"?"^state_param_name^"="^(string_of_int i))
			       "&" params_string)))::a)
	content
	
let a_ = a
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

let css_link ?(a=[]) uri =
      link ~a:((a_href uri)::
	       (a_type "text/css")::(a_rel [`Stylesheet])::a) ()

let js_script ?(a=[]) uri =
  script ~a:((a_src uri)::a) ~contenttype:"text/javascript" (pcdata "")

(*
let css_link ?(a=[]) (service : ('a, form_content_l,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) service) current_url =
  service.create_link_url current_url
    (fun v -> 
      link ~a:((a_href (make_uri_from_string  v))::(a_type "text/css")::(a_rel [`Stylesheet])::a) ())

let script ?(a=[]) (service : ('a, form_content_l,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) service) current_url =
  service.create_script_url current_url
    (fun v -> 
      script ~a:((a_src v)::a) ~contenttype:"text/javascript" (pcdata ""))
*)

let make_params_names (params : ('t,'tipo,'n) params_type) : 'n =
  let rec aux prefix suffix = function
      TProd (t1, t2) -> Obj.magic (aux prefix suffix t1, aux prefix suffix t2)
    | TInt name -> Obj.magic (prefix^name^suffix)
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
(* AEFFFFFFFFFFFFFFFF
	     let length = List.length l in
	     snd
	       (List.fold_right 
		  (fun el (i,l2) -> 
		    let i'= i-1 in
		    (i',(f (aux (prefix^name^".") (make_list_suffix i') t1) el)
		     @l2))
		  l
		  (length,endlist)))} *)
  in aux "" "" params
    
let get_form ?(a=[])
    (service : ('get,unit,'kind,'tipo,'gn,unit name) service) 
    (current_url : current_url)
    (f : 'gn -> form_content_l) : [>form] elt =
  let urlname =
    (if service.external_service
    then (reconstruct_absolute_url_path current_url service.url None)
    else (reconstruct_relative_url_path current_url service.url None)) in
  let state_param =
    (match  service.url_state with
      None -> []
    | Some i -> 
	let i' = string_of_int i in
	[<< <input type="hidden" name=$state_param_name$ value=$i'$/> >>])
  in
  let inside = f (make_params_names service.get_params_type) in
  form ~a:((a_method `Get)::a) ~action:(make_uri_from_string urlname)
    << <p style="display:none">
      $list:state_param$
      </p> >>
    inside

let post_form ?(a=[])
    (service : ('get,'form,'kind,'tipo,'gn,'pn) service) (current_url : current_url)
    (f : 'pn -> form_content_l) (getparams : 'get) : [>form] elt =
  let suff,params_string = construct_params service.get_params_type getparams in
  let suff = (if service.url_prefix then Some suff else None) in
  let urlname = 
    (if service.external_service 
    then (reconstruct_absolute_url_path current_url service.url suff)
    else (reconstruct_relative_url_path current_url service.url suff))
  in
  let state_param =
    (match  service.url_state with
      None -> []
    | Some i -> 
       let i' = string_of_int i in
       <:xmllist< <input type="hidden" name=$state_param_name$ value=$i'$/> >>)
  in
  let inside = f (make_params_names service.post_params_type) in
  form ~a:((a_method `Post)::a) 
    ~action:(make_uri_from_string (add_to_string urlname "?" params_string))
    << <p style="display:none">
      $list:state_param$
      </p> >>
    inside

let make_uri 
    (service : ('get, unit, 'kind, 'tipo,'gn,'pn) service) current_url
    (getparams : 'get) : uri =
  let suff,params_string = construct_params service.get_params_type getparams in
  let suff = (if service.url_prefix then Some suff else None) in
  let uri = 
    (if service.external_service 
    then (reconstruct_absolute_url_path current_url service.url suff)
    else (reconstruct_relative_url_path current_url service.url suff))
  in
  match service.url_state with
    None ->
      make_uri_from_string (add_to_string uri "?" params_string)
  | Some i -> 
      make_uri_from_string 
	(add_to_string (uri^"?"^state_param_name^"="^(string_of_int i))
	   "&" params_string)

(* actions : *)
let action_a ?(a=[]) ?(reload=true) action h content : [>form] elt  =
  let formname="hiddenform"^(string_of_int (counter ())) in
  let href="javascript:document."^formname^".submit ()" in
  let action_param_name = action_prefix^action_name in
  let action_param = (action.action_name) in
  let reload_name = action_prefix^action_reload in
  let reload_param = 
    if reload 
    then <:xmllist< <input type="hidden" name=$reload_name$ value=$reload_name$/> >> 
    else [] in
  let v = h.full_url in
  << <form name=$formname$ method="post" action=$v$ >
    <p>$XHTML.M.a ~a:((a_href (make_uri_from_string href))::a) content$
    <input type="hidden" name=$action_param_name$ value=$action_param$/>
	$list:reload_param$
	</p>
	</form> >>
	
let action_form ?(a=[])
    ?(reload=true) (action : ('a,'pn) action) h 
    (f : 'pn -> form_content_l) : [>form] elt = 
  let action_param_name = action_prefix^action_name in
  let action_param = (action.action_name) in
  let reload_name = action_prefix^action_reload in
  let action_line =
    << <input type="hidden" name=$action_param_name$ value=$action_param$/> >>
  in
  let v = h.full_url in
  let inside = f (make_params_names action.action_params_type) in
  let inside_reload = 
    if reload 
    then <:xmllist< <p><input type="hidden" name=$reload_name$ value=$reload_name$/></p>$list:inside$ >>
    else inside in
  form ~a:((a_method `Post)::a) ~action:(make_uri_from_string v)
    << <p>$action_line$</p> >>
  inside_reload





let gen_input ?(a=[]) name = 
  input ~a:((a_name name)::(a_input_type `Text)::a) ()

let password_input ?(a=[]) (name : string name) = 
  input ~a:((a_name name)::(a_input_type `Password)::a) ()

let int_input ?a (name : int name) = gen_input ?a name
let string_input ?a (name : string name) = gen_input ?a name

let hidden_int_input ?(a=[]) (name : int name) v = 
  let vv = string_of_int v in
  input ~a:((a_name name)::(a_input_type `Hidden)::(a_value vv)::a) ()

let checkbox_input ?(a=[]) (name : bool name) =
  input ~a:((a_name name)::(a_input_type `Checkbox)::a) ()

let radio_input ?(a=[]) (name : string name) =
  input ~a:((a_name name)::(a_input_type `Radio)::a) ()

let textarea ?(a=[]) (name : string name) =
  textarea ~a:((a_name name)::a)

let submit_input ?(a=[]) s =
  input ~a:((a_input_type `Submit)::(a_value s)::a) ()






let localhost = Unix.inet_addr_of_string "127.0.0.1"

let execute generate_page sockaddr cookie = 
  let ip = match sockaddr with
      Unix.ADDR_INET (ip,port) -> ip
    | _ -> localhost
  in
  let save_current_dir = get_current_dir () in
  let answer =
    let (tables, new_session) = 
      (match cookie with
	   None -> (new_session_tables (), true)
	 | Some c -> try (Cookies.find cookie_table (ip,c), false)
	   with Not_found -> (new_session_tables (), true))
    in
    set_session_tables tables;
    let page,working_dir = generate_page ip in
    let cookie2 = 
      if are_empty_tables (get_session_tables ())
      then ((if not new_session 
      then match cookie with
	Some c -> Cookies.remove cookie_table (ip,c)
      | None -> ());None)
      else (if new_session 
      then let c = new_cookie cookie_table ip in
      (Cookies.add cookie_table (ip,c) (get_session_tables ());
       Some c)
      else cookie)
    in (cookie2, page, ("/"^(reconstruct_url_path working_dir)))
  in absolute_change_dir save_current_dir; 	
  answer


let get_page 
    (url, path, params, internal_state, get_params, post_params, useragent)
    sockaddr cookie = 
  let fullurl = path^params in
  let generate_page ip =
    try (* D'abord recherche dans la table de session *)
      Messages.debug ("--- recherche "^(reconstruct_url_path url)^" dans la table de session :");
      (find_service
	 (get_session_tables ())
	 (url,
	  internal_state,
	  get_params,
	  post_params,
	  useragent,
	  ip,
	  fullurl))
    with Ocsigen_404 | Ocsigen_Wrong_parameter -> 
      try (* ensuite dans la table globale *)
      Messages.debug "--- recherche dans la table globale :";
      (find_service 
	 global_tables
	 (url,
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
	      Messages.debug "--- recherche dans la table de session, sans état :";
	      (find_service 
		 (get_session_tables ())
		 (url,
		  None,
		  get_params,
		  post_params,
		  useragent,
		  ip,
		  fullurl))
	  with Ocsigen_404 | Ocsigen_Wrong_parameter -> 
          (* ensuite dans la table globale *)
	    Messages.debug "--- recherche dans la table globale, sans état :";
	    (find_service 
	       global_tables
	       (url,
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
    Ocsigen_Typing_Error n -> 
      (cookie, (Error_pages.page_error_param_type n), "/")
  | Ocsigen_Wrong_parameter -> (cookie, Error_pages.page_bad_param, "/")


let make_action action_name action_params 
    (url, path, params, _, _, _, useragent) sockaddr cookie =
  let fullurl = path^params in
  let generate_page ip =
    let action,working_dir = 
      (try 
	find_action (get_session_tables ()) action_name
      with Not_found ->
	(find_action global_tables action_name))
    in 
    absolute_change_dir working_dir;
    (action (make_server_params url fullurl [] action_params useragent ip)),
    working_dir
  in try
    let r = execute 
	generate_page
	sockaddr cookie in
    Messages.debug "Action executed";
    r
    with 
	Ocsigen_Typing_Error _ -> (cookie, (), "/")
      | Ocsigen_Wrong_parameter -> (cookie, (), "/")


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










(* DEPRECATED FUNCTIONS : *)

let new_url ~path ?prefix ~get_params () =
  new_service ~url:path ?prefix ~get_params () 

let new_external_url ~path ?prefix ~get_params ~post_params () =
  new_external_service ~url:path ?prefix ~get_params ~post_params () 

let new_state_url = new_local_service

let register_url ~url f = register_service ~service:url f

let register_url_for_session ~url f = 
  register_service_for_session ~service:url f

let register_new_url ~path ?prefix ~get_params f =
  register_new_service ~url:path ?prefix ~get_params f

let register_new_state_url = register_new_local_service

let register_new_state_url_for_session =
  register_new_local_service_for_session

let new_post_url = new_post_service

let new_post_state_url = new_post_local_service

let register_new_post_url = register_new_post_service

let register_new_post_state_url = register_new_post_local_service

let register_new_post_state_url_for_session =
  register_new_post_local_service_for_session

let new_actionurl = new_action

let register_actionurl ~actionurl ~action =
  register_action ~action:actionurl action 

let register_actionurl_for_session ~actionurl ~action =
  register_action_for_session ~action:actionurl action 

let register_new_actionurl ~post_params ~action =
  register_new_action ~post_params action

let register_new_actionurl_for_session ~post_params ~action =
  register_new_action ~post_params action

