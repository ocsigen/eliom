(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsigenmod.ml
 * Copyright (C) 2005 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception; 
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(*****************************************************************************)
(*****************************************************************************)
(* Tables of services (global and session tables)                            *)
(* Store and load dynamic pages                                              *)
(*****************************************************************************)
(*****************************************************************************)


open Lwt
open Ocsimisc
open Extensions

exception Ocsigen_Wrong_parameter
exception Ocsigen_Typing_Error of (string * exn) list

exception Ocsigen_duplicate_registering of string
exception Ocsigen_there_are_unregistered_services of string
exception Ocsigen_service_or_action_created_outside_site_loading
exception Ocsigen_page_erasing of string
exception Ocsigen_register_for_session_outside_session
exception Ocsigen_error_while_loading_site of string

(*****************************************************************************)
type 'a server_params1 = 
    request_info * current_dir * 'a ref
      
type 'a server_params2 = url_path * 'a server_params1
      
(** state is a parameter to differenciate
   several instances of the same URL.
   (for internal use)
 *)
type internal_state = int

(********)

let ocsigen_suffix_name = "__ocsigen_suffix"
let action_prefix = "__ocsigen_action__"
let action_name = "name"
let action_reload = "reload"
let state_param_name = "__ocsigen_etat__"
let cookiename = "ocsigensession"



(*****************************************************************************)
(* Finding special ocsigenmod parameters (for actions, state, suffix ...)    *)

let getcookie cookies = 
  try 
    Some (List.assoc cookiename cookies)
  with Not_found -> None


let change_request_info ri =
  let cookie = getcookie ri.ri_cookies in
  let internal_state, post_params2 = 
    try (Some (int_of_string (List.assoc state_param_name ri.ri_post_params)),
         List.remove_assoc state_param_name ri.ri_post_params)
    with Not_found -> (None, ri.ri_post_params)
  in
  let internal_state2, get_params2 = 
    try 
      match internal_state with
        None ->
          (Some (int_of_string (List.assoc state_param_name ri.ri_get_params)),
           List.remove_assoc state_param_name ri.ri_get_params)
      | _ -> (internal_state, ri.ri_get_params)
    with Not_found -> (internal_state, ri.ri_get_params)
  in
  let action_info, post_params3 =
    try
      let action_name, pp = 
        ((List.assoc (action_prefix^action_name) post_params2),
         (List.remove_assoc (action_prefix^action_name) post_params2)) in
      let reload,pp2 =
        try
          ignore (List.assoc (action_prefix^action_reload) pp);
          (true, (List.remove_assoc (action_prefix^action_reload) pp))
        with Not_found -> false, pp in
      let ap,pp3 = pp2,[] in
      (Some (action_name, reload, ap), pp3)
    with Not_found -> None, post_params2 
  in
  {ri with 
   ri_get_params = get_params2; 
   ri_post_params = post_params3},
  (cookie, action_info, internal_state2)




(*****************************************************************************)
(* The table of dynamic pages for each virtual server, and actions           *)
(* Each node contains either a list of nodes (case directory)
    or a table of "answers" (functions that will generate the page) *)

(* The table of tables for each session. Keys are (hostname,cookie) *)
module Cookies = Hashtbl.Make(struct 
  type t = Unix.inet_addr * string
  let equal = (=)
  let hash = Hashtbl.hash
end)

(* table cookie -> session table *)
let new_cookie_table () = Cookies.create 100

let rec new_cookie table ip = 
  let c = Int64.to_string (Random.int64 Int64.max_int) in
  try
    Cookies.find table (ip,c);
    new_cookie table ip
  with Not_found -> c

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
           ((tables server_params2 -> Predefined_senders.send_page_type Lwt.t)
              * Predefined_senders.create_sender_type * url_path)) list)) list
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

and tables = dircontent ref * action_table ref

type cookiestable = tables Cookies.t

type pages_tree = 
    tables (* global tables of continuations/actions *)
      * (tables Cookies.t) (* session tables *)

let empty_page_table () = []
let empty_action_table () = AVide
let empty_dircontent () = Vide
let empty_tables () =
  (ref (empty_dircontent ()), ref (empty_action_table ()))
    
let are_empty_tables (lr,atr) = 
  (!lr = Vide && !atr = AVide)

let new_pages_tree () =
  ((empty_tables ()),
   (new_cookie_table ()))

(*****************************************************************************)
(* The current registration directory *)
let absolute_change_hostdir, get_current_hostdir, end_current_hostdir =
  let current_dir : ((unit -> pages_tree) * url_path) ref = 
    ref ((fun () ->
      raise (Ocsigen_Internal_Error "No pages tree available")), []) 
  in
  let f1 = ref (fun (pagetree,dir) -> 
    current_dir := (fun () -> pagetree), remove_slash dir) in
  let f2 = ref (fun () -> let (cd1,cd2) = !current_dir in (cd1 (), cd2)) in
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

let during_ocsigen_module_loading, 
  begin_load_ocsigen_module, 
  end_load_ocsigen_module =
  let during_ocsigen_module_loading = ref false in
  ((fun () -> !during_ocsigen_module_loading),
   (fun () -> during_ocsigen_module_loading := true),
   (fun () -> during_ocsigen_module_loading := false))

let global_register_allowed () = 
  (during_initialisation ()) && (during_ocsigen_module_loading ())



(*****************************************************************************)
(* dynamic pages *)
(** We associate to a service a function server_params2 -> page *)


    (** Create server parameters record *)
let make_server_params dir str ri =
  (ri,
   dir,
   str)


let find_page_table 
    t str 
    ri
    urlsuffix k = 
  let (sp0,_,b) = make_server_params [] str ri in
  let rec aux = function
      [] -> fail Ocsigen_Wrong_parameter
    | (_,(funct,create_sender,working_dir))::l ->
        catch (fun () ->
          Messages.debug "- I'm trying a service";
          funct (urlsuffix, (sp0,working_dir,b)) >>=
          (fun p -> 
            Messages.debug "- Page found";
            Lwt.return (p,create_sender,working_dir)))
          (function
              Ocsigen_Wrong_parameter -> aux l
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
(********* Vérifier ici qu'il n'y a pas qqchose similaire déjà enregistré ?! *)
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

let add_action (_,actiontableref) current_dir name action =
  (* synchronize (fun () -> *)
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
  (* synchronize 
    (fun () -> *)
  page_table_ref := add_page_table session url_act !page_table_ref content

      
let find_service 
    (dircontentref,_)
    (session_table_ref, 
     ri,
     state_option) =
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
    try search_page_table !dircontentref (change_empty_list ri.ri_path)
    with Not_found -> raise Ocsigen_404
  in
  let suffix,get_param_list = 
    if  suffix = []
    then try
      let s,l = list_assoc_remove ocsigen_suffix_name ri.ri_get_params in
      [s],l
    with Not_found -> suffix, ri.ri_get_params
    else suffix, ri.ri_get_params in
  let pref = suffix <> [] in
  find_page_table 
    page_table
    session_table_ref
    ri
    suffix
    {prefix = pref;
     state = state_option}


(****************************************************************************)
(****************************************************************************)
(****************************************************************************)
(****************************************************************************)

type session_table = tables

(** Type of http parameters *)
type server_params = session_table server_params1

let new_session_tables = empty_tables


(*****************************************************************************)
(* Generation of the page or action                                          *)

let execute generate_page ip cookie (globtable, cookie_table) =
  let (sessiontablesref, new_session) = 
    (match cookie with
      None -> (ref (new_session_tables ()), true)
    | Some c -> try (ref (Cookies.find cookie_table (ip,c)), false)
    with Not_found -> (ref (new_session_tables ()), true))
  in
  generate_page globtable sessiontablesref >>=
  (fun ((send_page,sender,working_dir),lastmod,etag) ->
    let cookie2 = 
      if are_empty_tables !sessiontablesref
      then ((if not new_session 
      then match cookie with
        Some c -> Cookies.remove cookie_table (ip,c)
      | None -> ());None)
      else (if new_session 
      then let c = new_cookie cookie_table ip in
      (Cookies.add cookie_table (ip,c) !sessiontablesref;
       Some c)
      else cookie)
    in
    let cookie3 = 
      if cookie2 <> cookie then 
        (if cookie2 = None 
        then Some remove_cookie_str
        else cookie2)
      else None
    in return 
      ((cookie3, 
        send_page, 
        sender, 
        ("/"^(string_of_url_path working_dir))),
       lastmod,
       etag))


let get_page
    page_tree
    ri
    cookie internal_state =
  let generate_page
      global_tables
      session_tables_ref =
    ((catch
        (fun () -> 
          Messages.debug 
            ("-- I'm looking for "^(string_of_url_path ri.ri_path)^
             " in the session table:");
          (find_service
             !session_tables_ref
             (session_tables_ref,
              ri,
              internal_state)))
        (function 
            Ocsigen_404 | Ocsigen_Wrong_parameter -> 
              catch (* ensuite dans la table globale *)
                (fun () -> 
                  Messages.debug "-- I'm searching in the global table:";
                  (find_service 
                     global_tables
                     (session_tables_ref,
                      ri,
                      internal_state)))
                (function
                    Ocsigen_404 | Ocsigen_Wrong_parameter as exn -> 
                      (* si pas trouvé avec, on essaie sans l'état *)
                      (match internal_state with
                        None -> fail exn
                      | _ -> catch (* d'abord la table de session *)
                            (fun () ->
                              Messages.debug 
                                "-- I'm searching in the session table, without state parameter:";
                              (find_service 
                                 !session_tables_ref
                                 (session_tables_ref,
                                  ri,
                                  None)))
                            (function
                                Ocsigen_404 | Ocsigen_Wrong_parameter -> 
                                  (* ensuite dans la table globale *)
                                  Messages.debug "-- I'm searching in the global table, without state parameter:";
                                  (find_service 
                                     global_tables
                                     (session_tables_ref,
                                      ri,
                                      None))
                              | e -> fail e))
                  | e -> fail e)
          | e -> fail e)) >>= (fun r -> return (r,None,None)))
  in catch 
    (fun () ->
      execute 
        generate_page
        ri.ri_inet_addr
        cookie 
        page_tree >>=
      fun ((cook, sp, s, path),lm,etag) -> 
        return 
          (Ext_found 
             {res_cookies=
              (match cook with
                None -> []
              | Some c -> [(cookiename, c)]);
              res_send_page=sp;
              res_create_sender=s;
              res_code=None;
              res_path=path;
              res_lastmodified=lm;
              res_etag=etag})
    )
    (function
        Ocsigen_Typing_Error l -> 
          return (Ext_found
                    {res_cookies=[];
                     res_send_page=
                     (Predefined_senders.send_xhtml_page 
                        ~content:(Error_pages.page_error_param_type l));
                     res_create_sender=Predefined_senders.create_xhtml_sender;
                     res_path="/";
                     res_code=None;
                     res_lastmodified=None;
                     res_etag=None})
      | Ocsigen_Wrong_parameter -> 
	  return (Ext_found 
                    {res_cookies=[];
                     res_send_page=
                     (Predefined_senders.send_xhtml_page 
                        ~content:(Error_pages.page_bad_param));
                     res_create_sender=Predefined_senders.create_xhtml_sender;
                     res_path="/";
                     res_code=None;
                     res_lastmodified=None;
                     res_etag=None})
      | Ocsigen_404 | Ocsigen_Wrong_parameter -> return Ext_not_found
      | e -> fail e)



let make_action page_tree action_name action_params
    ri
    cookie =
  let generate_page global_tables session_tables_ref =
    let action, working_dir = 
      try
        try
          find_action !session_tables_ref action_name
        with
          Not_found -> (find_action global_tables action_name)
      with
        Not_found -> raise Ocsigen_404
    in 
    (action
       (make_server_params 
          working_dir session_tables_ref 
          {ri with ri_get_params=[]; ri_post_params = action_params})) >>=
    (fun r -> return ((r,(), working_dir), None, None))
  in catch
    (fun () ->
      execute 
        generate_page ri.ri_inet_addr cookie page_tree >>=
      (fun ((c,(),(),wd),_,_) ->
        Messages.debug "- Action executed";
        return (c,wd)))
    (function
        Ocsigen_Typing_Error _ -> return (None, "/")
      | Ocsigen_Wrong_parameter -> return (None, "/")
      | e -> fail e)


let gen page_tree ri =
  let ri, (cookie, action_info, internal_state) = change_request_info ri in

  match action_info with
    None ->

      (* page generation *)
      get_page page_tree ri cookie internal_state

  | Some (action_name, reload, action_params) ->

      (* action *)
      make_action 
        page_tree action_name action_params ri cookie
        >>= (fun (cookie2,path) ->
	  let cookie3 = match cookie2 with
	    None -> cookie
	  | Some c -> Some c
	  in
          (if reload then
            (get_page page_tree ri cookie3 internal_state >>=
	     (function
		 Ext_found r -> return 
		     (Ext_found
			{r with 
			 res_cookies=
			 (match cookie2, r.res_cookies with
			 | (Some c), [] -> [(cookiename, c)]
			 | _,cl -> cl)})
	       | _ -> return (Ext_found
				{res_cookies=
				 (match cookie2 with
				   None -> []
				 | Some c -> [(cookiename, c)]);
				 res_send_page=
				 (Predefined_senders.send_xhtml_page 
				    ~content:(Error_pages.error_page "Error: redirection after action is experimental (it works only for ocsigenmod pages for now, and I didn't find any)"));
				 res_create_sender=Predefined_senders.create_xhtml_sender;
				 res_path="/";
				 res_code=None;
				 res_lastmodified=None;
				 res_etag=None})))
          
          else
	    return
	      (Ext_found
                 {res_cookies=
                  (match cookie2 with
                    None -> []
                  | Some c -> [(cookiename, c)]);
                  res_send_page=Predefined_senders.send_empty;
                  res_create_sender=Predefined_senders.create_empty_sender;
                  res_code=Some 204;
                  res_path=path;
                  res_lastmodified=None;
                  res_etag=None})
          )
            )

(*****************************************************************************)
(** Module loading *)
open Simplexmlparser.ExprOrPatt
let config = ref PLEmpty

let load_ocsigen_module pages_tree path cmo content =
  config := content;
  begin_load_ocsigen_module ();
  absolute_change_hostdir (pages_tree, path);
  (try
    Dynlink.loadfile cmo
  with Dynlink.Error e -> 
    end_load_ocsigen_module ();
    raise (Ocsigen_error_while_loading_site
             ("(ocsigenmod extension) "^cmo^": "^
              (Dynlink.error_message e))));
  (* absolute_change_hostdir save_current_dir; *)
  end_load_ocsigen_module ();
  config := PLEmpty


(*****************************************************************************)
(** Parsing of config file *)
let parse_config page_tree path = function
    EPanytag 
      ("module", atts, content) -> 
        let mo = match atts with
        | PLEmpty -> 
            raise (Error_in_config_file
                     "file attribute expected for <module>")
        | PLCons ((EPanyattr (EPVstr("file"), EPVstr(s))), PLEmpty) -> s
        | _ -> raise (Error_in_config_file "Wrong attribute for <module>")
        in
        load_ocsigen_module page_tree path mo content
  | EPanytag (t, _, _) -> 
      raise (Extensions.Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "(Ocsigenmod extension)")


(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase *)
let start_init () =
  ()

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  end_current_hostdir ();
  verify_all_registered ()                                

(** Function that will handle exceptions during the initialisation phase *)
let handle_init_exn = function
  Ocsigen_duplicate_registering s -> 
    ("Fatal - Duplicate registering of url \""^s^
     "\". Please correct the module.")
| Ocsigen_there_are_unregistered_services s ->
    ("Fatal - Some public url have not been registered. \
              Please correct your modules. (ex: "^s^")")
| Ocsigen_service_or_action_created_outside_site_loading ->
    ("Fatal - An action or a service is created outside \
              site loading phase")
| Ocsigen_page_erasing s ->
    ("Fatal - You cannot create a page or directory here: "^s^
            ". Please correct your modules.")
| Ocsigen_register_for_session_outside_session ->
    ("Fatal - Register session during initialisation forbidden.")
| Ocsigen_error_while_loading_site s ->
    ("Fatal - Error while loading site: "^s)
| e -> raise e


(*****************************************************************************)
(** extension registration *)
let _ = register_extension
    ((fun hostpattern -> 
      let page_tree = new_pages_tree () in
      (gen page_tree, 
       parse_config page_tree)),
     start_init,
     end_init,
     handle_init_exn)


(*****************************************************************************)

(* à refaire
let number_of_sessions () = 
  List.fold_left 
    (fun d t -> 
      let (_,_,cookie_table) = get_table t in
      d + (Cookies.length cookie_table))
    0 !pages_trees
*)


