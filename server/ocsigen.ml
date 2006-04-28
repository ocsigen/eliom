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

let reconstruct_absolute_url_path current_url = reconstruct_url_path

let reconstruct_relative_url_path current_url u =
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
  in let s = (makedotdot aremonter)^(reconstruct_url_path aaller) in
  (* print_endline ((reconstruct_url_path current_url)^"->"^(reconstruct_url_path u)^"="^s); *)
  if s = "" then defaultpagename else s



(** Type of http parameters *)
type server_params = {url_suffix: string;
		    full_url: string;
		    current_url: current_url;
		    user_agent: string;
		    ip: Unix.inet_addr;
		    get_params: (string * string) list;
		    post_params: (string * string) list}

(** Create server parameters record *)
let make_server_params 
    url fullurl url_suffix get_params post_params useragent ip = 
  {url_suffix = (reconstruct_url_path url_suffix);
   full_url= fullurl;
   user_agent=useragent;
   current_url=url;
   ip=ip;
   get_params = get_params;
   post_params = post_params}




(** state is a parameter to differenciate
    several instances of the same URL.
	(for internal use)
 *)
type internal_state = int

let counter = let c = ref (Random.int 1000000) in fun () -> c := !c + 1 ; !c

let new_state =
  let c : internal_state ref = ref (Random.int 1000000) in
  fun () -> c := !c + 1 ; Some !c


exception Ocsigen_Typing_Error of string
exception Ocsigen_Wrong_parameter
exception Ocsigen_Is_a_directory
exception Ocsigen_404
exception Ocsigen_page_erasing of string
exception Ocsigen_url_created_outside_site_loading
exception Ocsigen_there_are_unregistered_url of string
exception Ocsigen_duplicate_registering of string
exception Ocsigen_register_for_session_outside_session

let id x = x

(** Type of names in a formular *)
type 'a name = string

type ('a,'b) binsum = Inj1 of 'a | Inj2 of 'b;;

type tlist (* used only for tlist name *)

(* This is a generalized algebraic datatype *)
type 'a params_type =
    TProd of (* 'b *) 'a params_type * (* 'c *) 'a params_type (* 'a = 'b * 'c *)
  | TOption of (* 'b *) 'a params_type (* 'a = 'b option *)
  | TList of tlist name * (* 'b *) 'a params_type (* 'a = 'b list *)
  | TSum of (* 'b *) 'a params_type * (* 'c *) 'a params_type (* 'a = ('b, 'c) binsum *)
  | TString of string name (* 'a = string *)
  | TInt of int name (* 'a = int *)
  | TUserType of ('a name * (string -> 'a) * ('a -> string)) (* 'a = 'a *)
  | TUnit (* 'a = unit *);;

(* As GADT are not implemented in OCaml for the while, we define our own
   constructors for params_type *)
let int (n : string) : int params_type = TInt n
let string (n : string) : string params_type = TString n
let unit : unit params_type = TUnit
let user_type
    (of_string : string -> 'a) (from_string : 'a -> string) (n : string)
    : 'a params_type =
  Obj.magic (TUserType (n,of_string,from_string))
let sum (t1 : 'a params_type) (t2 : 'b params_type) : ('a,'b) binsum params_type =
  Obj.magic (TSum (t1, t2))
let prod (t1 : 'a params_type) (t2 : 'b params_type)
    : ('a * 'b) params_type =
  Obj.magic (TProd ((Obj.magic t1), (Obj.magic t2)))
let option (t : 'a params_type) : 'a option params_type = 
  Obj.magic (TOption t)
let list (n : string) (t : 'a params_type) : 'a list params_type = 
  Obj.magic (TList (n,t))
let ( ** ) = prod

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
let reconstruct_params (typ : 'a params_type) params : 'a = 
  let rec aux_list t p name pref suff =
    let length,l = list_assoc_remove (pref^name^suff) params in
    let long = int_of_string length in
    let rec aa i p pref suff =
      if i=long 
      then (Obj.magic []), p
      else
	let v,l = Obj.magic (aux t p pref (suff^(make_list_suffix i))) in
	let v2,l2 = aa (i+1) l pref suff in
	(Obj.magic (v::v2)),l2
    in 
    aa 0 l (pref^name^".") suff
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
	(Obj.magic (int_of_string v)),l
    | TUserType (name, of_string, string_of) ->
	let v,l = (list_assoc_remove (pref^name^suff) params) in 
	(Obj.magic (of_string v)),l
    | TUnit -> (Obj.magic ()), params
  in 
  let v,l = Obj.magic (aux typ params "" "") in
  if l = [] then v else raise Not_found

(* The following function takes a 'a params_type and a 'a and
   constructs the string of parameters (GET or POST) 
   (This is a marshalling function towards HTTP parameters format) *)
let construct_params (typ : 'a params_type) (params : 'a) : string =
  let rec aux typ params pref suff =
    match typ with
      TProd (t1, t2) ->
	let s1 = aux t1 (fst (Obj.magic params)) pref suff
	and s2 = aux t2 (snd (Obj.magic params)) pref suff in
	(concat_strings s1 "&" s2)
    | TOption t -> (match ((Obj.magic params) : 'zozo option) with None -> "" 
      | Some v -> aux t v pref suff)
    | TList (list_name, t) -> 
	let long = List.length ((Obj.magic params) : 'a list) in
	let beg = (pref^list_name^suff^"="^(string_of_int long)) in
	let pref2 = pref^list_name^suff^"." in
	fst 
	  (List.fold_left
	     (fun (s,i) p -> 
	       let ss = 
		 aux t p pref2 (suff^(make_list_suffix i)) in
	       ((concat_strings s "&" ss),(i+1))) (beg,0) (Obj.magic params))
    | TSum (t1, t2) -> (match Obj.magic params with
	Inj1 v -> aux t1 v pref suff
      | Inj2 v -> aux t2 v pref suff)
    | TString name -> pref^name^suff^"="^(Obj.magic params)
    | TInt name -> pref^name^suff^"="^(string_of_int (Obj.magic params))
    | TUserType (name, of_string, string_of) ->
	pref^name^suff^"="^(string_of (Obj.magic params))
    | TUnit -> ""
  in
  aux typ params "" ""
	

(** We associate to an URL a function server_params -> page *)
module type DIRECTORYTREE =
  sig
    type tables
    type page_table_key =
	{prefix:bool;
	 state: internal_state option}
    val empty_tables : unit -> tables
    val are_empty_tables : tables -> bool
    val add_url : tables -> bool -> url_path -> 
      page_table_key * (server_params -> page) -> unit
    val add_action :
	tables -> string -> (server_params -> unit) -> unit
    val find_url :
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
      {(* *********** AEFFFFFFFFF get_names: 'a params_type;
	  post_names: 'a params_type; *)
       prefix:bool;
       state: internal_state option}
       (* action: server_params -> page *)

  module Page_Table = Map.Make(struct type t = page_table_key 
				      let compare = compare end)

  module String_Table = Map.Make(struct type t = string
					let compare = compare end)

  type page_table = ((server_params -> page) * url_path) list Page_Table.t
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

  let empty_page_table () = Page_Table.empty
  let empty_action_table () = AVide
  let empty_dircontent () = Vide

  let find_page_table t (url,getp,postp,ua,ip,fullurl) k url_suffix = 
    let rec aux = function
      [] -> raise Not_found
    | (funct,working_dir)::l ->
	try 
	  absolute_change_dir working_dir;
	  let p = funct
	      (make_server_params url fullurl url_suffix getp postp ua ip)
	  in
	  Messages.debug "Page found";
	  p,working_dir
	with _ -> aux l
    in aux (Page_Table.find k t)

  let add_page_table session url_act t (key,elt) = 
    (* Duplicate registering forbidden in global table *)
    if not session then
      try
	let l = Page_Table.find key t in
        if (* mettre ici le test duplicate *) false (***************) 
        then
          raise (Ocsigen_duplicate_registering (reconstruct_url_path url_act))
        else Page_Table.add key (elt::l) t
      with Not_found -> Page_Table.add key [elt] t
    else Page_Table.add key [elt] t

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

  let add_url (dircontentref,_) session url_act (page_table_key, action) =
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
		    (action, get_current_dir ())) in
    (* let current_dircontentref = 
      search_dircontentref dircontentref (get_current_dir ()) in *)
    let page_table_ref = 
      search_page_table_ref (*current_*) dircontentref url_act in
    page_table_ref := add_page_table session url_act !page_table_ref content

	 
  let find_url 
      (dircontentref,_) 
      (string_list, state_option, get_param_list, post_param_list, 
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
      (search_page_table !dircontentref (change_empty_list string_list)) in
    find_page_table 
      page_table
      (string_list,
       get_param_list,
       post_param_list,
       ua,
       ip,
       fullurl)
      {prefix = (suffix <> []);
       state = state_option}
      suffix

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

(** Typed URLs *)
type internal_url_kind = [`Public_Url | `State_Url]
type url_kind = [`Internal_Url of internal_url_kind | `External_Url]

type ('get,'post,'kind) url = 
    {url: url_path; (* name of the URL without parameters *)
     unique_id: int;
     url_prefix: bool;
     external_url: bool;
     url_state: internal_state option;
       (* 'kind is just a type information: it can be only 
	  `Internal_Url `Public_Url or  `Internal_Url `State_Url
	  or `External_Url, so that we can't use session urls as fallbacks for
	  other session urls. If it is a session url, it contains a value
	  (internal state) that will allow to differenciate between
	  url that have the same name.
	*)
     get_params_type: 'get params_type;
     post_params_type: 'post params_type;
    }

let add_unregistered, remove_unregistered, verify_all_registered =
  let l = ref [] in
  ((fun a -> l := a::!l),
   (fun a -> l := list_remove a !l),
   (fun () -> 
     match !l with [] -> () 
     | (a,_)::_ -> raise (Ocsigen_there_are_unregistered_url (reconstruct_url_path a))))

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

(** Create an url *)
let new_url_aux_aux
    ~(path : url_path)
    ~prefix
    ~externalurl
    ~(get_params : 'get params_type)
    ~(post_params : 'post params_type)
    : ('get,'post,'kind) url =
(* ici faire une vérification "duplicate parameter" ? *) 
  {url = path;
   unique_id = counter ();
   url_prefix = prefix;
   url_state = None;
   external_url = externalurl;
   get_params_type = get_params;
   post_params_type = post_params;
  }

let new_url_aux
    ~(path : url_path)
    ~prefix
    ~(get_params : 'get params_type)
    : ('get,unit,[`Internal_Url of 'popo]) url =
  if global_register_allowed () then
    let full_path = (get_current_dir ())@(change_empty_list path) in
    let u = new_url_aux_aux
	~path:full_path
	~prefix
	~externalurl:false
	~get_params
	~post_params:unit
    in
    add_unregistered (u.url,u.unique_id); u
  else raise Ocsigen_url_created_outside_site_loading

let new_external_url
    ~(path : url_path)
    ?(prefix=false)
    ~(get_params : 'get params_type)
    ~(post_params : 'post params_type)
    ()
    : ('get,unit,[`External_Url]) url =
  new_url_aux_aux
    ~path
    ~prefix
    ~externalurl:true
    ~get_params 
    ~post_params

let new_url
    ~(path : url_path)
    ?(prefix=false)
    ~(get_params : 'get params_type)
    ()
    : ('get,unit,[`Internal_Url of [`Public_Url]]) url =
  new_url_aux ~path ~prefix ~get_params

let new_state_url
   ~(fallback : ('get,unit, [`Internal_Url of [`Public_Url]])url)
    : ('get,unit,[`Internal_Url of [`State_Url]]) url =
  {fallback with url_state = new_state ()}

let register_url_aux
    tables
    session
    state
    ~(url : ('get,'post,[`Internal_Url of 'popo]) url)
    (page_generator : server_params -> 'get -> 'post -> 'fin) =
  add_url tables session url.url
    ({prefix = url.url_prefix;
      state = state},
     (fun h -> page_generator h 
	 (reconstruct_params url.get_params_type h.get_params)
	 (reconstruct_params url.post_params_type h.post_params)))

let register_url 
    ~(url : ('get,unit,[`Internal_Url of 'g]) url)
    (page_gen : server_params -> 'get -> 'post -> 'fin) =
  if global_register_allowed () then begin
    remove_unregistered (url.url,url.unique_id);
    register_url_aux global_tables false (url.url_state) url page_gen; end
  else Messages.warning "Public URL registration after init forbidden! Please correct your module! (ignored)"

(* WARNING: if we create a new URL without registering it,
   we can have a link towards a page that does not exist!!! :-(
   That's why I impose to register all url during init.
   The only other way I see to avoid this is to impose a syntax extension
   like "let rec" for url...
 *)

let register_url_for_session
    ~(url : ('get,'post,[`Internal_Url of 'g]) url)
    page =
  register_url_aux (get_session_tables ()) true url.url_state url page

let register_new_url 
    ~path
    ?(prefix=false)
    ~get_params
    page
    : ('get,unit, [`Internal_Url of [`Public_Url]]) url =
  let u = new_url ~prefix ~path ~get_params () in
  register_url u page;
  u
    
let register_new_state_url
    ~(fallback : ('get, unit, [`Internal_Url of [`Public_Url]]) url)
    page
    : ('get, unit, [`Internal_Url of [`State_Url]]) url =
  let u = (new_state_url fallback) in
  register_url u page;
  u

let register_new_state_url_for_session
    ~(fallback : ('get, unit, [`Internal_Url of [`Public_Url]])url)
    page
    : ('get, unit, [`Internal_Url of [`State_Url]]) url =
  let u = (new_state_url fallback) in
  register_url_for_session u page;
  u


(** Register an url with post parameters in the server *)
let new_post_url_aux
    ~(fallback : ('get, unit, [`Internal_Url of [`Public_Url]]) url)
    ~(post_params : 'post params_type)
    : ('get, 'post, [`Internal_Url of [`Public_Url]]) url = 
(* ici faire une vérification "duplicate parameter" ? *) 
  {url = fallback.url;
   unique_id = counter ();
   url_prefix = fallback.url_prefix;
   external_url = false;
   url_state = None;
   get_params_type = fallback.get_params_type;
   post_params_type = post_params;
  }

let new_post_url
    ~(fallback : ('get, unit, [`Internal_Url of [`Public_Url]]) url)
    ~(post_params : 'post params_type)
    : ('get, 'post, [`Internal_Url of [`Public_Url]]) url = 
  if global_register_allowed () then
    let u = new_post_url_aux fallback post_params in
    add_unregistered (u.url,u.unique_id); u
  else raise Ocsigen_url_created_outside_site_loading
  
let new_post_state_url
    ~(fallback : ('get, 'post1, [`Internal_Url of [`Public_Url]]) url)
    ~(post_params : 'post params_type)
    : ('get, 'post, [`Internal_Url of [`State_Url]]) url = 
  {fallback with 
   url_state = new_state ();
   post_params_type = post_params;
  }

let register_new_post_url 
    ~(fallback : ('get, unit, [`Internal_Url of [`Public_Url]]) url)
    ~(post_params : 'post params_type)
    (page_gen : server_params -> 'get -> 'post -> 'fin)
    : ('get,'post, [`Internal_Url of [`Public_Url]]) url =
  let u = new_post_url ~fallback:fallback ~post_params:post_params in
  register_url u page_gen;
  u

let register_new_post_state_url
    ~(fallback : ('get, 'post1, [`Internal_Url of [`Public_Url]]) url)
    ~(post_params : 'post params_type)
    page_gen
    : ('get, 'post, [`Internal_Url of [`State_Url]]) url = 
  let u = new_post_state_url ~fallback:fallback ~post_params:post_params in
  register_url u page_gen;
  u

let register_new_post_state_url_for_session
    ~(fallback : ('get, 'post1, [`Internal_Url of [`Public_Url]]) url)
    ~(post_params : 'post params_type)
    page_gen
    : ('get, 'post, [`Internal_Url of [`State_Url]]) url = 
  let u = new_post_state_url ~fallback:fallback ~post_params:post_params in
  register_url_for_session u page_gen;
  u


(** actions (new 10/05) *)
type 'post actionurl =
    {action_name: string;
     action_params_type: 'post params_type}

let action_prefix = "__ocsigen_action__"
let action_name = "name"
let action_reload = "reload"

let new_action_name () = string_of_int (counter ())

let new_actionurl
    ~(params: 'post params_type) =
  {
    action_name = new_action_name ();
    action_params_type = params;
  }

let register_actionurl_aux tables ~actionurl ~action =
  add_action tables 
    actionurl.action_name
    (fun h -> action h 
	(reconstruct_params actionurl.action_params_type h.post_params))

let register_actionurl ~actionurl ~action =
  register_actionurl_aux global_tables actionurl action

let register_new_actionurl ~params ~action = 
  let a = new_actionurl params in
    register_actionurl a action;
    a

let register_actionurl_for_session ~actionurl ~action =
  register_actionurl_aux (get_session_tables ()) actionurl action


let register_new_actionurl_for_session ~params ~action =
  let a = new_actionurl params in
    register_actionurl_for_session a action;
    a

(** Satic directories **)
let static_dir : (unit, unit, [`Internal_Url of [`Public_Url]]) url =
  {url = [""];
   unique_id = counter ();
   url_state = None;
   url_prefix = true;
   external_url = false;
   get_params_type = unit;
   post_params_type = unit
  }



(** Close a session *)
let close_session () = set_session_tables (empty_tables ())

let state_param_name = "__ocsigen_etat__"


(** Functions to construct web pages: *)

let make_a ?(a=[]) l = XHTML.M.a ~a:a l

let a ?(a=[])
    (url : ('get, unit, 'kind) url) current_url content (getparams : 'get) =
  let params_string = construct_params url.get_params_type getparams in
  let uri = 
    (if url.external_url 
    then (reconstruct_absolute_url_path current_url url.url)
    else (reconstruct_relative_url_path current_url url.url)) in
  match url.url_state with
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
let css_link ?(a=[]) (url : ('a, form_content_l,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url) current_url =
  url.create_link_url current_url
    (fun v -> 
      link ~a:((a_href (make_uri_from_string  v))::(a_type "text/css")::(a_rel [`Stylesheet])::a) ())

let script ?(a=[]) (url : ('a, form_content_l,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url) current_url =
  url.create_script_url current_url
    (fun v -> 
      script ~a:((a_src v)::a) ~contenttype:"text/javascript" (pcdata ""))
*)

(*	   let stateparam = string_of_int i in
   << <span class="link" onclick="document.hiddenform.submit ()"><form name="hiddenform" method="post" action=$v$>
   <input type="hidden" name=$state_param_name$
   value=$stateparam$/>
   </form>$str:name$</span> >>) *)
(* Pas vraiment de moyen simple pour passer un paramètre POST dans un lien...
   let stateparam = string_of_int i in
   let link = "submit()" in
   << <form method="post" action=$v$> 
   <input type="hidden" name=$state_param_name$
   value=$stateparam$/>
   <input type="submit" style="background:none; border:none; cursor:pointer; color:red; text-align: left; line-height: 1" value=$name$/>
   </form> >>) *)



(*
   let get_form (url : ('a,form_content_l,'c,'d,'e,'f,'g) url) current_url (f : 'a) =
   let urlname = (match url.url with Url_Prefix s | Url s -> 
   reconstruct_relative_url_path current_url s) in
   let inside = url.create_get_form f in
   (match  url.url_state with
   None ->   << <form method="get" action=$urlname$>
   $list:inside$
   </form> >>
   | Some i -> 
   let i' = string_of_int i in
   let formname="hiddenform"^(string_of_int (counter ())) in
   let onsubmit="document."^formname^".submit();" in
   << <form method="get" action=$urlname$ onsubmit=$onsubmit$>
   <form name=$formname$ method="post" action=$urlname$ 
   style="display:none">
   <input type="hidden" name=$state_param_name$ value=$i'$/>
   </form>
   $list:inside$
   </form> >>)

   ou alors faire un form POST et du javascript qui va mettre 
   la chaîne ?blbla=truc&etc
 *)


(*	   let i' = string_of_int i in
   let formname="hiddenform"^(string_of_int (counter ())) in
   let onsubmit="document."^formname^".submit();window.open('"^urlname^"','indexWindow',''); return true;" in
   << <form method="get" action=$urlname$ onsubmit=$onsubmit$>
   <form name=$formname$ method="post" action=$urlname$ 
   style="display:none">
   <input type="hidden" name=$state_param_name$ value=$i'$/>
   </form>
   $list:inside$
   </form> >>)
 *)

(*
let get_form ?(a=[])
    (url : ('get,'form,'kind) url) current_url (getparams : 'get) =
  let urlname = reconstruct_relative_url_path current_url url.url in
  let state_param =
    (match  url.url_state with
      None -> []
    | Some i -> 
	let i' = string_of_int i in
	[<< <input type="hidden" name=$state_param_name$ value=$i'$/> >>])
  in
  let inside = url.create_get_form f in
(* `Form ([(`Method, "get"); (`Action, urlname)], state_param::inside) *)
  form ~a:((a_method `Get)::a) ~action:(make_uri_from_string urlname)
    << <p style="display:none">
      $list:state_param$
      </p> >>
    inside


    let post_form ?(a=[])
	(url : ('a,'b,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url) current_url (f : 'b) = 
      let state_param =
	(match  url.url_state with
	  None -> []
	| Some i -> 
	    let i' = string_of_int i in
	    <:xmllist< <input type="hidden" name=$state_param_name$ value=$i'$/> >>)
      in
      url.create_form_url current_url
	(fun v -> 
	  let inside = url.create_post_form f in
(*	 `Form ([(`Method, "post"); (`Action, v)], state_param::inside)) *)
	  form ~a:((a_method `Post)::a) ~action:(make_uri_from_string v)
	    << <p style="display:none">
          	  $list:state_param$
	       </p> >>
	  inside)

let make_uri 
    (url : ('a, form_content_l,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url) current_url =
  match url.url_state with
    None -> url.create_uri current_url make_uri_from_string
  | Some i -> url.create_uri current_url
	(fun v -> make_uri_from_string 
	    (v^"?"^state_param_name^"="^(string_of_int i)))


(*
let img ?a ~alt
    (url : ('a, form_content_l,'ca,'cform,'curi(*'cimg,'clink,'cscript*),'d,'e,'f,'g) url) current_url =
  match url.url_state with
    None -> url.create_img_url current_url
	(fun v -> img ~src:v ~alt ?a ())
  | Some i -> url.create_img_url current_url
	(fun v -> 
	  let vstateparam = (v^"?"^state_param_name^"="^(string_of_int i)) in
	  img ~src:vstateparam ~alt ?a ())
*)

(* actions : *)
let action_a ?(a=[]) ?(reload=true) actionurl h content =
  let formname="hiddenform"^(string_of_int (counter ())) in
  let href="javascript:document."^formname^".submit ()" in
  let action_param_name = action_prefix^action_name in
  let action_param = (actionurl.action_name) in
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
    ?(reload=true) (actionurl : ('a,'b) actionurl) h (f : 'a) = 
  let action_param_name = action_prefix^action_name in
  let action_param = (actionurl.action_name) in
  let reload_name = action_prefix^action_reload in
  let action_line =
    << <input type="hidden" name=$action_param_name$ value=$action_param$/> >>
  in
  let v = h.full_url in
  let inside = actionurl.create_action_form f in
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




*)




(** return a page from an url and parameters *)
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
      (find_url 
	 (get_session_tables ())
	 (url,
	  internal_state,
	  get_params,
	  post_params,
	  useragent,
	  ip,
	  fullurl))
    with Not_found -> try (* ensuite dans la table globale *)
      Messages.debug "--- recherche dans la table globale :";
      (find_url 
	 global_tables
	 (url,
	  internal_state,
	  get_params,
	  post_params,
	  useragent,
	  ip,
	  fullurl))
    with Not_found -> (* si pas trouvé avec, on essaie sans l'état *)
      match internal_state with
	  None -> raise Ocsigen_404
	| _ -> try (* d'abord la table de session *)
	    Messages.debug "--- recherche dans la table de session, sans état :";
	    (find_url 
	       (get_session_tables ())
	       (url,
		None,
		get_params,
		post_params,
		useragent,
		ip,
		fullurl))
	  with Not_found -> (* ensuite dans la table globale *)
	    try 
	      Messages.debug "--- recherche dans la table globale, sans état :";
	      (find_url 
		 global_tables
		 (url,
		  None,
		  get_params,
		  post_params,
		  useragent,
		  ip,
		  fullurl))
	    with Not_found -> raise Ocsigen_404
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
    (action 
       (make_server_params 
	  url fullurl [] [] action_params useragent ip)),
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

