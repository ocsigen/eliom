(* Ocsigen
 * http://www.ocsigen.org
 * Module pagesearch.ml
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
(*****************************************************************************)
(*****************************************************************************)
(* Tables of services (global and session tables)                            *)
(* Store and load pages, static or dynamic                                   *)
(*****************************************************************************)
(*****************************************************************************)


open Lwt
open Ocsimisc

exception Ocsigen_Wrong_parameter
exception Ocsigen_404
exception Ocsigen_duplicate_registering of string
exception Ocsigen_register_for_session_outside_session
exception Ocsigen_page_erasing of string
exception Ocsigen_Is_a_directory
exception Ocsigen_service_created_outside_site_loading
exception Ocsigen_there_are_unregistered_services of string
exception Ocsigen_error_while_loading of string
exception Ocsigen_Typing_Error of (string * exn) list
exception Ocsigen_Internal_Error of string

let ocsigen_suffix_name = "__ocsigen_suffix"

(*****************************************************************************)
(** type of URL, without parameter *)
type url_path = string list
type current_url = string list
type current_dir = string list

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
      
(** state is a parameter to differenciate
   several instances of the same URL.
   (for internal use)
 *)
type internal_state = int

(*****************************************************************************)
(* The current registration directory *)
let absolute_change_hostdir, get_current_hostdir, end_current_hostdir =
  let current_dir : (Ocsiconfig.virtual_hosts * url_path) ref = ref ([],[]) in
  let f1 = ref (fun host,dir -> 
    current_dir := host,remove_slash dir) in
  let f2 = ref (fun () -> !current_dir) in
  let exn1 _ = 
    raise (Ocsigen_Internal_Error "absolute_change_hostdir after init") in
  let exn2 () = 
    raise (Ocsigen_Internal_Error "get_current_hostdir after init") in
  ((fun hostdir -> !f1 hostdir),
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
     | (a,_)::_ -> raise (Ocsigen_there_are_unregistered_services (string_of_url_path a))))

let during_initialisation, end_initialisation =
  let init = ref true in
  ((fun () -> !init), 
   (fun () -> 
     init := false;
     end_current_hostdir ();
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


(** We associate to a service a function server_params2 -> page *)

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
       ((int * 
	   ((tables server_params2 -> Sender_helpers.send_page_type Lwt.t)
	      * Sender_helpers.create_sender_type * url_path)) list)) list
      (* Here, the url_path is the working directory.
	 That is, the directory in which we are when we register
	 dynamically the pages.
	 Each time we load a page, we change to this directory
	 (in case the page registers new pages).
       *)

and action_table = 
    AVide 
  | ATable of ((tables server_params1 -> unit Lwt.t) * url_path)
	String_Table.t

and dircontent = 
    Vide
  | Table of direlt ref String_Table.t

and direlt = 
    Dir of dircontent ref
  | File of page_table ref

and tables = (Ocsiconfig.virtual_hosts * dircontent ref) list ref 
      * action_table ref


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

let find_page_table t (str,host,url,getp,postp,ua,ip,fullurl,urlsuffix) k = 
  let sp = make_server_params [] str url fullurl getp postp ua ip in
  let rec aux = function
      [] -> fail Ocsigen_Wrong_parameter
    | (_,(funct,create_sender,working_dir))::l ->
	catch (fun () ->
	  Messages.debug "I'm trying a service";
	  funct (urlsuffix, {sp with current_dir = working_dir}) >>=
	  (fun p -> 
	    Messages.debug "Page found";
	    Lwt.return (p,create_sender,working_dir)))
	  (function Ocsigen_Wrong_parameter -> aux l
	    | e -> fail e)
  in 
  (catch 
     (fun () -> return (List.assoc k t))
     (function Not_found -> fail Ocsigen_404 | e -> fail e)) >>=
  (fun r -> aux r)

let add_page_table session url_act t (key,(id,elt)) = 
  (* Duplicate registering forbidden in global table *)
  try
    let l,newt = list_assoc_remove key t in
    try
(********** Vérifier ici qu'il n'y a pas qqchose similaire déjà enregistré ! *)
      let _,oldl = list_assoc_remove id l in
      if not session then
	raise (Ocsigen_duplicate_registering (string_of_url_path url_act))
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

let empty_tables () : tables = 
  (ref [], ref (empty_action_table ()))

let are_empty_tables (lr,atr) = 
  (!lr = [] && !atr = AVide)

let add_action (_,actiontableref) current_dir name action =
  actiontableref :=
    add_action_table !actiontableref
      (name,(action,current_dir))

let find_action (_,atr) name =
  find_action_table !atr name

let add_service tables host current_dir session url_act
    create_sender
    (page_table_key, (unique_id, action)) =
  let find_dircontent_for_host (listref,_) = 
    let rec aux = function
	[] -> let dcr = ref (empty_dircontent ()) in
	listref := !listref@[(host,dcr)] (* at the end *); dcr 
      | (h,d)::l when h == host (* physical equality? *) -> d
      | _::l -> aux l
    in aux !listref
  in
  let dircontentref = find_dircontent_for_host tables in
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
    (listref,_)
    (session_table_ref, host, 
     string_list, state_option, get_param_list, post_param_list, 
     ua, ip, fullurl) =
  let find_dircontent_for_host hlist = 
    let rec aux host = function
	[] -> raise Ocsigen_404
      | (h,d)::l when Ocsiconfig.host_match host h -> 
	  Messages.debug ("host found: "^host^" matches "^
			  (Ocsiconfig.string_of_host h)); 
	  (d,l)
      | (h,_)::l -> Messages.debug ("host = "^host^" does not match "^
				    (Ocsiconfig.string_of_host h)); 
	  aux host l
    in match host with 
      None -> (match hlist with
	[] -> raise Ocsigen_404
      | (_,d)::l -> (d,l))
    | Some hh -> aux hh hlist
  in
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
  let string_list = change_empty_list string_list in
  let rec find_pagetablesuf = function
      [] -> raise Ocsigen_404
    | hostlist ->
	let dcr,othershosts = find_dircontent_for_host hostlist in
	try 
	  search_page_table !dcr string_list 
	with Not_found -> find_pagetablesuf othershosts
  in
  let page_table, suffix = find_pagetablesuf !listref in
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
     host,
     string_list,
     get_param_list,
     post_param_list,
     ua,
     ip,
     fullurl,
     suffix)
    {prefix = pref;
     state = state_option}


(****************************************************************************)
(****************************************************************************)
(****************************************************************************)
(****************************************************************************)

type session_table = tables

(** Type of http parameters *)
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
  generate_page ip tablesref >>=
  (fun send_page,sender,working_dir ->
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
    in return 
      (cookie2, send_page, sender, ("/"^(string_of_url_path working_dir))))


let get_page 
    (host, url, path, params, internal_state, 
     get_params, post_params, useragent)
    sockaddr cookie = 
  let fullurl = path^params in
  let generate_page ip session_tables_ref =
    catch (* D'abord recherche dans la table de session *)
      (fun () -> 
	Messages.debug ("--- I search "^(string_of_url_path url)^" in the session table:");
	(find_service
	   !session_tables_ref
	   (session_tables_ref,
	    host,
	    url,
	    internal_state,
	    get_params,
	    post_params,
	    useragent,
	    ip,
	    fullurl)))
      (function Ocsigen_404 | Ocsigen_Wrong_parameter -> 
	catch (* ensuite dans la table globale *)
	  (fun () -> 
	    Messages.debug "--- I search in the global table:";
	    (find_service 
	       global_tables
	       (session_tables_ref,
		host,
		url,
		internal_state,
		get_params,
		post_params,
		useragent,
		ip,
		fullurl)))
	  (function
	      Ocsigen_404 | Ocsigen_Wrong_parameter as exn -> 
		(* si pas trouvé avec, on essaie sans l'état *)
		(match internal_state with
		  None -> fail exn
		| _ -> catch (* d'abord la table de session *)
		      (fun () ->
			Messages.debug "--- I search in the session table, without state parameter:";
			(find_service 
			   !session_tables_ref
			   (session_tables_ref,
			    host,
			    url,
			    None,
			    get_params,
			    post_params,
			    useragent,
			    ip,
			    fullurl)))
		      (function
			  Ocsigen_404 | Ocsigen_Wrong_parameter -> 
			    (* ensuite dans la table globale *)
			    Messages.debug "--- I search in the global table, without state parameter:";
			    (find_service 
			       global_tables
			       (session_tables_ref,
				host,
				url,
				None,
				get_params,
				post_params,
				useragent,
				ip,
				fullurl))
			| e -> fail e))
	    | e -> fail e)
    | e -> fail e)
  in catch 
    (fun () -> execute generate_page sockaddr cookie)
    (function
	Ocsigen_Typing_Error l -> 
	  return (cookie, (Sender_helpers.send_xhtml_page 
			     ~content:(Error_pages.page_error_param_type l)),
		  Sender_helpers.create_xhtml_sender, "/")
      | Ocsigen_Wrong_parameter -> return 
	    (cookie, (Sender_helpers.send_xhtml_page 
			~content:(Error_pages.page_bad_param)),
	     Sender_helpers.create_xhtml_sender, "/")
      | e -> fail e)


let make_action action_name action_params 
    (host, url, path, params, _, _, _, useragent) sockaddr cookie =
  let fullurl = path^params in
  let generate_page ip session_tables_ref =
    let action,working_dir = 
      (try
	find_action !session_tables_ref action_name
      with
	Not_found -> (find_action global_tables action_name)
      | e -> raise e)
    in 
    (action 
       (make_server_params 
	  working_dir session_tables_ref url fullurl [] 
	  action_params useragent ip)) >>=
    (fun r -> return (r,(), working_dir))
  in catch
    (fun () -> execute generate_page sockaddr cookie >>=
      (fun (c,(),(),wd) ->
	Messages.debug "Action executed";
	return (c,wd)))
    (function
	Ocsigen_Typing_Error _ -> return (cookie, "/")
      | Ocsigen_Wrong_parameter -> return (cookie, "/")
      | e -> fail e)


(** Module loading *)
exception Ocsigen_error_while_loading of string
    
let load_ocsigen_module ~host ~dir ~cmo =
  let save_current_dir = get_current_hostdir () in
  try
    begin_load_ocsigen_module ();
    absolute_change_hostdir (host, dir);
    Dynlink.loadfile cmo;
    absolute_change_hostdir save_current_dir;
    end_load_ocsigen_module ()
  with Dynlink.Error e -> 
    absolute_change_hostdir save_current_dir;
    end_load_ocsigen_module ();
    raise
      (Ocsigen_error_while_loading (cmo^" ("^(Dynlink.error_message e)^")"))
  | e -> 
      absolute_change_hostdir save_current_dir;
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



