(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod.ml
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
open Lazy

type 'a server_params1 = 
    request_info * 
      (current_dir * 'a ref * ((string * string) list) * url_path)
      
(** state is a parameter to differenciate
   several instances of the same URL.
   (for internal use)
 *)
type internal_state = int

(********)

exception Eliom_Wrong_parameter
exception Eliom_Typing_Error of (string * exn) list

exception Eliom_duplicate_registering of string
exception Eliom_there_are_unregistered_services of string
exception Eliom_service_created_outside_site_loading
exception Eliom_page_erasing of string
exception Eliom_register_for_session_outside_session
exception Eliom_error_while_loading_site of string

(*****************************************************************************)
let eliom_suffix_name = "__eliom_suffix"
let anservice_prefix = "__eliom_na__"
let anservice_name = "name"
let get_state_param_name = "__eliom__"
let post_state_param_name = "__eliom_p__"
let cookiename = "eliomsession"
let co_param_prefix = "__co_eliom_"
let na_co_param_prefix = "__na_eliom_"



(*****************************************************************************)
(* Finding special eliommod parameters (for anservices, state, suffix ...)   *)

let getcookie cookies = 
  try 
    Some (List.assoc cookiename cookies)
  with Not_found -> None

(* Split parameter list, removing those whose name starts with pref *)
let split_prefix_param pref l =
  let len = String.length pref in
  List.partition (fun (n,_) -> 
    try 
      (String.sub n 0 len) = pref 
    with _ -> false) l

let change_request_info ri =
  force ri.ri_post_params >>=
  (fun post_params -> 
    let get_params = force ri.ri_get_params in
    let cookie = getcookie (force ri.ri_cookies) in
    let (anservice_info, (get_state, post_state),
      (get_params, other_get_params), post_params) =
      let post_anservice_name, na_post_params = 
        try
          let n, pp =
            list_assoc_remove (anservice_prefix^anservice_name) post_params
          in (Some n, pp)
        with Not_found -> (None, []) 
        (* Not possible to have POST parameters without anservice_name
           if there is a GET anservice_name
         *)
      in
      let get_anservice_name, (na_get_params, other_get_params) = 
        try
          let n, gp =
            list_assoc_remove (anservice_prefix^anservice_name) get_params
          in (Some n, (split_prefix_param na_co_param_prefix gp))
        with Not_found -> (None, ([], get_params))
      in
      match get_anservice_name, post_anservice_name with
        _, Some _
      | Some _, None -> (* non attached coservice *)
          ((get_anservice_name, post_anservice_name),
           (None, None), (na_get_params, other_get_params), na_post_params)
      | None, None ->
          let post_state, post_params = 
            try 
              let s, pp =
                list_assoc_remove post_state_param_name post_params
              in (Some (int_of_string s), pp)
            with 
              Not_found -> (None, post_params)
          in
          let get_state, (get_params, other_get_params) = 
            try 
              let s, gp =
                list_assoc_remove get_state_param_name get_params
              in ((Some (int_of_string s)), 
                  (split_prefix_param co_param_prefix gp))
            with Not_found -> (None, (get_params, []))
          in ((None, None), (get_state, post_state), 
          (get_params, other_get_params), post_params)
    in
    return 
      ({ri with 
        ri_get_params = lazy get_params; 
        ri_post_params = lazy (return post_params)},
       (cookie, anservice_info, (get_state, post_state), other_get_params)))







(*****************************************************************************)
(* The table of dynamic pages for each virtual server, and anservices        *)
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
    {suffix:bool;
     state: (internal_state option * internal_state option)}
      (* action: tables server_params1 -> page *)

      (* module Page_Table = Map.Make(struct type t = page_table_key 
         let compare = compare end) *)

module String_Table = Map.Make(struct 
  type t = string
  let compare = compare 
end)

module NAserv_Table = Map.Make(struct 
  type t = string option * string option
  let compare = compare 
end)

type page_table = 
    (page_table_key * 
       ((int * 
           ((tables server_params1 -> Predefined_senders.send_page_type Lwt.t)
              * Predefined_senders.create_sender_type option 
	      * url_path)) list)) list
      (* Here, the url_path is the working directory.
         That is, the directory in which we are when we register
         dynamically the pages.
         Each time we load a page, we change to this directory
         (in case the page registers new pages).
       *)

and anservice_table = 
    AVide 
  | ATable of 
      ((tables server_params1 -> Predefined_senders.send_page_type Lwt.t)
	 * Predefined_senders.create_sender_type option
	 * url_path)
        NAserv_Table.t

and dircontent = 
    Vide
  | Table of direlt ref String_Table.t

and direlt = 
    Dir of dircontent ref
  | File of page_table ref

and tables = dircontent ref * anservice_table ref

type cookiestable = tables Cookies.t

type pages_tree = 
    tables (* global tables of continuations/anservices *)
      * (tables Cookies.t) (* session tables *)

let empty_page_table () = []
let empty_anservice_table () = AVide
let empty_dircontent () = Vide
let empty_tables () =
  (ref (empty_dircontent ()), ref (empty_anservice_table ()))
    
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
     | (a,_)::_ -> 
         raise (Eliom_there_are_unregistered_services 
                  (match a with
                    None -> "<Non-attached service>"
                  | Some a -> string_of_url_path a))))

let during_eliom_module_loading, 
  begin_load_eliom_module, 
  end_load_eliom_module =
  let during_eliom_module_loading = ref false in
  ((fun () -> !during_eliom_module_loading),
   (fun () -> during_eliom_module_loading := true),
   (fun () -> during_eliom_module_loading := false))

let global_register_allowed () = 
  (during_initialisation ()) && (during_eliom_module_loading ())



(*****************************************************************************)
(* dynamic pages *)
(** We associate to a service a function server_params -> page *)


    (** Create server parameters record *)
let make_server_params dir str ri other_get_params suffix =
  (ri,
   (dir,
    str,
    other_get_params,
    suffix))


let find_page_table 
    (t : page_table)
    str 
    ri
    urlsuffix
    k
    other_get_params = 
  let (sp0,(_,b,o,u)) = 
    make_server_params [] str ri other_get_params urlsuffix in
  let rec aux = function
      [] -> fail Eliom_Wrong_parameter
    | (_,(funct, create_sender,working_dir))::l ->
        catch (fun () ->
          Messages.debug "- I'm trying a service";
          funct (sp0, (working_dir, b, o, u)) >>=
          (fun p -> 
            Messages.debug "- Page found";
            Lwt.return (p, create_sender, working_dir)))
          (function
              Eliom_Wrong_parameter -> aux l
            | e -> fail e)
  in 
  (catch 
     (fun () -> return (List.assoc k t))
     (function Not_found -> fail Ocsigen_404 | e -> fail e)) >>=
  aux



let add_page_table session url_act t (key,(id,elt)) = 
  (* Duplicate registering forbidden in global table *)
  try
    let l,newt = list_assoc_remove key t in
    try
(********* Vérifier ici qu'il n'y a pas qqchose similaire déjà enregistré ?! *)
      let _,oldl = list_assoc_remove id l in
      if not session then
        raise (Eliom_duplicate_registering (string_of_url_path url_act))
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

let add_anservice_table at (key,elt) = 
  match at with
    AVide -> ATable (NAserv_Table.add key elt NAserv_Table.empty)
  | ATable t -> ATable (NAserv_Table.add key elt t)

let find_anservice_table at k = 
  match at with
    AVide -> raise Not_found
  | ATable t -> NAserv_Table.find k t

let add_anservice 
    (_,anservicetableref) current_dir session name create_sender anservice =
  (if not session
  then
    try
      ignore (find_anservice_table !anservicetableref name);
      raise (Eliom_duplicate_registering "Anonymous service")
    with Not_found -> ());
  anservicetableref :=
    add_anservice_table !anservicetableref
      (name, (anservice, create_sender, current_dir))

let find_anservice (_,atr) name =
  find_anservice_table !atr name

let add_service (dircontentref,_) current_dir session url_act
    create_sender
    (page_table_key, (unique_id, action)) =

  let aux search dircontentref a l =
    try 
      let direltref = find_dircontent !dircontentref a in
      match !direltref with
        Dir dcr -> search dcr l
      | File ptr -> raise (Eliom_page_erasing "<non-attached service>")
            (* Messages.warning ("Eliom page registering: Page "^
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
            Dir _ -> raise (Eliom_page_erasing a)
                (* Messages.warning ("Eliom page registering: Directory "^
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

  let content = ({suffix = page_table_key.suffix;
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
     ri,
     states, 
     other_get_params) =
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
  let (suffix, get_param_list) = 
    if suffix = []
    then try
      let s,l = 
        list_assoc_remove eliom_suffix_name (force ri.ri_get_params) in
      [s],l
    with Not_found -> suffix, (force ri.ri_get_params)
    else suffix, (force ri.ri_get_params) in
  let pref = suffix <> [] in
  find_page_table 
    page_table
    session_table_ref
    {ri with ri_get_params = lazy get_param_list}
    suffix
    {suffix = pref;
     state = states}
    other_get_params


(****************************************************************************)
(****************************************************************************)
(****************************************************************************)
(****************************************************************************)

type session_table = tables

(** Type of http parameters *)
type server_params = session_table server_params1

let new_session_tables = empty_tables


(*****************************************************************************)
(* Generation of the page or anservice                                          *)

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

exception Eliom_retry_with of 
  (request_info * 
     (string option * 
        (string option * string option) *
        (internal_state option * internal_state option) *
        (string * string) list))

let get_page
    page_tree
    ri
    charset
    cookie 
    internal_state 
    other_get_params =
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
              internal_state,
              other_get_params)))
        (function 
            Ocsigen_404 | Eliom_Wrong_parameter -> 
              catch (* ensuite dans la table globale *)
                (fun () -> 
                  Messages.debug "-- I'm searching in the global table:";
                  (find_service 
                     global_tables
                     (session_tables_ref,
                      ri,
                      internal_state,
                      other_get_params)))
                (function
                    Ocsigen_404 (* | Eliom_Wrong_parameter *) as exn -> 
                      (* si pas trouvé avec, on essaie sans l'état *)
                      (match internal_state with
                        (None, None) -> fail exn
                      | (g, Some _) -> 
                          (* There was a POST state. 
                             We remove it, and remove POST parameters.
                           *)
                          Messages.debug 
                            "-- Session expired. I will try without POST parameters:";
                          fail (Eliom_retry_with 
                                  ({ri with 
                                    ri_post_params = lazy (return [])
(***************************** ++++++++++++++++++++ exn *)
                                  }, 
                                   (cookie, (None, None), 
                                    (g, None), other_get_params)))
                      | (Some _, None) -> 
                          (* There was a GET state, but no POST state. 
                             We remove it with its parameters, 
                             and remove POST parameters.
                           *)
                          Messages.debug 
                            "-- Session expired. I will try without GET state parameters and POST parameters:";
                          fail (Eliom_retry_with 
                                  ({ri with 
                                    ri_get_params = lazy other_get_params;
                                    ri_post_params = lazy (return [])
(***************************** ++++++++++++++++++++ exn *)
                                  },
                                   (cookie, (None, None), (None, None), []))))
                  | e -> fail e)
          | e -> fail e)) >>= (fun r -> return (r,None,None)))
  in (generate_page, ri.ri_inet_addr, cookie, page_tree)


let make_anservice
    page_tree
    anservice_info
    ri
    cookie 
    other_get_params =
  let generate_page global_tables session_tables_ref =
    (try
      try
        return (find_anservice !session_tables_ref anservice_info)
      with
        Not_found -> return (find_anservice global_tables anservice_info)
    with
      Not_found ->
        (* It was an non-attached service.
           We call the same URL without non-attached parameters.
         *)
        match anservice_info with
          None, None -> assert false
        | Some _ as g, Some _ ->
            Messages.debug 
              "-- Session expired. I will try with only GET non-attached parameters:";
            fail (Eliom_retry_with
                    ({ri with 
                      ri_post_params = lazy (return [])
(***************************** ++++++++++++++++++++ exn *)
                    },
                     (cookie, (g, None), 
                      (None, None), other_get_params)))
        | _ ->
            Messages.debug 
              "-- Session expired. I will try without non-attached parameters:";
            change_request_info
              {ri with 
               ri_get_params = lazy other_get_params;
               ri_post_params = lazy (return [])
(***************************** ++++++++++++++++++++ exn *)
             } >>=
            (fun r -> fail (Eliom_retry_with r))
    ) >>=
    (fun (anservice, create_sender, working_dir) ->
      (anservice
         (make_server_params 
            working_dir session_tables_ref 
            ri
            other_get_params
            [])) >>=
      (fun r -> return ((r,create_sender, working_dir), None, None)))
  in (generate_page, ri.ri_inet_addr, cookie, page_tree)
    

let gen page_tree charset ri =
  let rec gen_aux 
      (ri, (cookie, anservice_info, 
            (get_state, post_state), other_get_params)) =
    let (gen,ia,c,pt) = 
      match anservice_info with
	None, None ->
          
          (* page generation *)
          get_page 
            page_tree ri charset cookie
            (get_state, post_state) other_get_params
            
      | _ ->
          
          (* anonymous service *)
          make_anservice page_tree anservice_info ri cookie other_get_params
    in
    
    catch 
      (fun () ->
	execute gen ia c pt >>=
	fun ((cook, sp, s, path),lm,etag) ->
          let cookie =
	    match cook with
              None -> []
	    | Some c -> [(cookiename, c)]
          in
          match s with
            None -> (* Nothing to send, we retry without POST params
                       (it was an action, we reload the page)
                       If it was not coservice
                       or a service with POST parameters, we do not reload,
                       otherwise it will loop.
                     *)
              force ri.ri_post_params >>=
              (fun ripp ->
                (match anservice_info, get_state, post_state, ripp
                with
                  (None, None),None,None,[] ->
                    return
                      (Ext_found
                         {res_cookies=cookie;
                          res_send_page=
                          Predefined_senders.send_empty ~content:();
                          res_create_sender=
                          Predefined_senders.create_empty_sender;
                          res_code=Some 204;
                          res_path=path;
                          res_lastmodified=None;
                          res_etag=None;
                          res_charset=charset})
                      
                | _ ->
                    return
                      (* Ext_retry_with or Eliom_retry_with? *)
                      (Ext_retry_with 
                         ({ri with
                           ri_post_params = lazy (return []);
                           ri_cookies=(match cook with
                             None -> ri.ri_cookies
                           | Some c -> lazy ((cookiename,c)::
                                             (force ri.ri_cookies)))},
                          Some ((Some path),cookie)))))
          | Some s ->
              return 
                (Ext_found 
                   {res_cookies=cookie;
		    res_send_page=sp;
		    res_create_sender=s;
		    res_code=None;
		    res_path=path;
		    res_lastmodified=lm;
		    res_etag=etag;
		    res_charset=charset})
      )
      (function
          Eliom_Typing_Error l -> 
            return (Ext_found
                      {res_cookies=[];
                       res_send_page=
                       (Predefined_senders.send_xhtml_page 
                          ~content:(Error_pages.page_error_param_type l));
                       res_create_sender=
                       Predefined_senders.create_xhtml_sender;
                       res_path="/";
                       res_code=None;
                       res_lastmodified=None;
                       res_etag=None;
                       res_charset=charset})
	| Eliom_Wrong_parameter -> 
	    return (Ext_found 
                      {res_cookies=[];
                       res_send_page=
                       (Predefined_senders.send_xhtml_page 
                          ~content:(Error_pages.page_bad_param));
                       res_create_sender=
                       Predefined_senders.create_xhtml_sender;
                       res_path="/";
                       res_code=None;
                       res_lastmodified=None;
                       res_etag=None;
                       res_charset=charset})
	| Ocsigen_404 -> return Ext_not_found
        | Eliom_retry_with a -> gen_aux a
	| e -> fail e)

  in
  change_request_info ri >>= gen_aux





(*****************************************************************************)
(** Module loading *)
open Simplexmlparser.ExprOrPatt
let config = ref PLEmpty

let load_eliom_module pages_tree path cmo content =
  config := content;
  begin_load_eliom_module ();
  absolute_change_hostdir (pages_tree, path);
  (try
    Dynlink.loadfile cmo
  with Dynlink.Error e -> 
    end_load_eliom_module ();
    raise (Eliom_error_while_loading_site
             ("(eliommod extension) "^cmo^": "^
              (Dynlink.error_message e))));
  (* absolute_change_hostdir save_current_dir; *)
  end_load_eliom_module ();
  config := PLEmpty


(*****************************************************************************)
(** Parsing of config file *)
let parse_config page_tree path = 
  let rec parse_module_attrs file = function
    | PLEmpty -> (match file with
        None -> 
          raise (Error_in_config_file
                   ("Missing module attribute in <eliom>"))
      | Some s -> s)
    | PLCons ((EPanyattr (EPVstr("module"), EPVstr(s))), suite) ->
        (match file with
          None -> parse_module_attrs (Some s) suite
        | _ -> raise (Error_in_config_file
                        ("Duplicate attribute file in <eliom>")))
    | PLCons ((EPanyattr (EPVstr(s), _)), _) ->
        raise
          (Error_in_config_file ("Wrong attribute for <eliom>: "^s))
    | _ ->
        raise
          (Error_in_config_file ("Error in attributes for <eliom>"))
  in function
      EPanytag 
        ("eliom", atts, content) -> 
          let file = parse_module_attrs None atts in
          load_eliom_module page_tree path file content
    | EPanytag (t, _, _) -> 
        raise (Extensions.Bad_config_tag_for_extension t)
    | _ -> raise (Error_in_config_file "(Eliommod extension)")


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
  Eliom_duplicate_registering s -> 
    ("Fatal - Eliom: Duplicate registering of url \""^s^
     "\". Please correct the module.")
| Eliom_there_are_unregistered_services s ->
    ("Fatal - Eliom: Some public url have not been registered. \
              Please correct your modules. (ex: "^s^")")
| Eliom_service_created_outside_site_loading ->
    ("Fatal - Eliom: A service is created outside \
              site loading phase")
| Eliom_page_erasing s ->
    ("Fatal - Eliom: You cannot create a page or directory here: "^s^
            ". Please correct your modules.")
| Eliom_register_for_session_outside_session ->
    ("Fatal - Eliom: Register session during initialisation forbidden.")
| Eliom_error_while_loading_site s ->
    ("Fatal - Eliom: Error while loading site: "^s)
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


