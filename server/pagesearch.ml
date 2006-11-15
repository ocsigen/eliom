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
exception Ocsigen_malformed_url
exception Ocsigen_service_or_action_created_outside_site_loading
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
                          hostname: string option;
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
(* The table of static and dynamic page for each virtual server, and actions *)
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

and tables = dircontent ref * action_table ref

type cookiestable = tables Cookies.t

type pages_tree = 
    static_dir ref (* static pages *)
      * tables (* global tables of continuations/actions *)
      * (tables Cookies.t) (* session tables *)

let empty_page_table () = []
let empty_action_table () = AVide
let empty_dircontent () = Vide
let empty_tables () =
  (ref (empty_dircontent ()), ref (empty_action_table ()))
    
let are_empty_tables (lr,atr) = 
  (!lr = Vide && !atr = AVide)

let pages_trees : (virtual_hosts * pages_tree) list ref
    = ref []

let new_pages_tree () =
  ((ref (Static_dir (None, []))),
   (empty_tables ()),
   (new_cookie_table ()))

(*****************************************************************************)
(* The current registration directory *)
let absolute_change_hostdir, get_current_hostdir, end_current_hostdir =
  let current_dir : ((unit -> pages_tree) * url_path) ref = 
    ref ((fun () ->
      raise (Ocsigen_Internal_Error "No pages tree available")), []) 
  in
  let f1 = ref (fun pagetree,dir -> 
    current_dir := (fun () -> pagetree),remove_slash dir) in
  let f2 = ref (fun () -> let cd1,cd2 = !current_dir in (cd1 (), cd2)) in
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
(* static pages *)
let set_static_dir staticdirref s path =
  let rec assoc_and_remove a = function
      [] -> raise Not_found
    | (b,v)::l when a = b -> (v,l)
    | e::l -> let v,ll = assoc_and_remove a l
          in v,(e::ll)
  in
  let rec add_path = function
      [] -> Static_dir (Some s,[])
    | a::l -> Static_dir (None, [(a,add_path l)])
  in
  let rec aux (Static_dir (s1,l1)) = function
      [] -> Static_dir (Some s,l1)
    | a::l -> 
        try
          let sd1,l2 = assoc_and_remove a l1 in
          let sd = aux sd1 l in
          Static_dir (s1,(a,sd)::l2)
        with Not_found -> Static_dir (s1,(a,(add_path l))::l1)
  in
  staticdirref := aux !staticdirref path


let host_match host port =
  let port_match = function
      None -> true
    | Some p -> p = port
  in
  let rec aux host =
    let hostlen = String.length host in
    let rec host_match1 beg =
      let rec aux1 t len l p0 =
        try 
          let (p,_) = 
            Netstring_str.search_forward (Netstring_str.regexp t) host p0 in
          let beg2 = p + len in
          (host_match1 beg2 l) || (aux1 t len l (p+1))
        with _ -> false
      in
      function
          [] -> beg = hostlen
        | [Wildcard] -> true
        | (Wildcard)::(Wildcard)::l -> 
            host_match1 beg ((Wildcard)::l)
        | (Wildcard)::(Text (t,len))::l -> aux1 t len l beg
        | (Text (t,len))::l -> 
            try
              (t = String.sub host beg len) && (host_match1 (beg+len) l)
            with _ -> false
    in
    function
        [] -> false
      | (a, p)::l -> ((port_match p) && (host_match1 0 a)) || aux host l
  in match host with
    None -> List.exists (fun (_, p) -> port_match p)
      (* Warning! For HTTP/1.0 we take the first one,
         even if it doesn't match! 
         To be changed! *)
  | Some host -> aux host


let string_of_host h = 
  let aux1 (hh, port) = 
    let p = match port with
      None -> ""
    | Some a -> ":"^(string_of_int a)
    in
    let rec aux2 = function
        [] -> ""
      | Wildcard::l -> "*"^(aux2 l)
      | (Text (t,_))::l -> t^(aux2 l)
    in (aux2 hh)^p
  in List.fold_left (fun d hh -> d^(aux1 hh)^" ") "" h


exception Serv_no_host_match
let do_for_host_matching host port ip f =
  let string_of_host_option = function
    None -> "<no host>:"^(string_of_int port)
  | Some h -> h^":"^(string_of_int port)
  in
  let rec aux e = function
      [] -> fail e
    | (h, pt)::l when host_match host port h -> 
        Messages.debug ("---- host found: "^(string_of_host_option host)^
                        " matches "^(string_of_host h));
        catch (fun () -> f pt) 
          (function
              Ocsigen_404 | Ocsigen_Wrong_parameter as e -> aux e l
            | e -> fail e)
    | (h,_)::l -> 
        Messages.debug ("---- host = "^(string_of_host_option host)^
                        " does not match "^(string_of_host h)); 
        aux e l
  in aux Serv_no_host_match !pages_trees


let find_static_page staticdirref path =
  let rec aux dir (Static_dir (dir_option, subdir_list)) = function
      [] -> (match dir_option with
        None -> dir
      | s -> s)
    | [""] -> (match dir, dir_option with
        None, None -> None
      | Some dir, None -> Some (dir^"/")
      | _, Some s -> Some (s^"/"))
    | ""::l
    | ".."::l -> raise Ocsigen_malformed_url
          (* For security reasons, .. is not allowed in paths *)
    | a::l -> 
        try 
          let e = (List.assoc a subdir_list) in
          match dir with
            None -> aux None e l
          | Some dir -> aux (Some (dir^"/"^a)) e l
        with 
          Not_found -> 
            (match dir, dir_option with
              None, None -> None
            | (Some d), None -> 
                Some (d^"/"^(Ocsimisc.string_of_url_path (a::l)))
            | _, Some s -> 
                Some (s^"/"^(Ocsimisc.string_of_url_path (a::l))))
  in 
  let find_file = function
      None -> raise Ocsigen_404
    | Some filename ->
        ignore (Unix.LargeFile.lstat filename);
        let filename = 
          Messages.debug ("Testing \""^filename^"\".");
          if ((Unix.LargeFile.lstat filename).Unix.LargeFile.st_kind
                = Unix.S_DIR)
          then 
            (if (filename.[(String.length filename) - 1]) = '/'
            then filename^"index.html"
            else
              (if (path = [""])
              then filename^"/index.html"
              else (Messages.debug (filename^" is a directory");
                    raise Ocsigen_Is_a_directory)))
          else filename
        in
        Messages.debug ("Looking for ("^filename^")");

        if ((Unix.LargeFile.lstat filename).Unix.LargeFile.st_kind 
              = Unix.S_REG)
        then begin
          Unix.access filename [Unix.R_OK];
          filename
        end
        else raise Ocsigen_404 (* ??? *)
  in
  find_file (aux None !staticdirref path)



(*****************************************************************************)
(* dynamic pages *)
(** We associate to a service a function server_params2 -> page *)


    (** Create server parameters record *)
let make_server_params
    dir str ((url,host,getp,postp,ua),ip,fullurl) = 
  {full_url= fullurl;
   hostname=host;
   user_agent=ua;
   current_url=url;
   ip=ip;
   get_params = getp;
   post_params = postp;
   current_dir = dir;
   session_table = str;
 }

let find_page_table 
    t str 
    (((url,host,getp,postp,ua),ip,fullurl) as sp1) urlsuffix k = 
  let sp = make_server_params [] str sp1 in
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
     (((path, _, get_param_list, post_param_list, ua), ip, fullurl) as sp), 
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
    try search_page_table !dircontentref (change_empty_list path)
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
    session_table_ref
    sp
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

let localhost = Unix.inet_addr_of_string "127.0.0.1"

let execute generate_page ip cookie (globtable,cookie_table) =
  let (sessiontablesref, new_session) = 
    (match cookie with
      None -> (ref (new_session_tables ()), true)
    | Some c -> try (ref (Cookies.find cookie_table (ip,c)), false)
    with Not_found -> (ref (new_session_tables ()), true))
  in
  generate_page ip globtable sessiontablesref >>=
  (fun (send_page,sender,working_dir),lastmod,etag ->
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
    in return 
      ((cookie2, send_page, sender, ("/"^(string_of_url_path working_dir))),
      lastmod,etag))


let get_page 
    (path, params, internal_state, 
     ((url, host, get_params, post_params, useragent) as sp))
    port sockaddr cookie =
  let fullurl = path^params in
  let ip = match sockaddr with
    Unix.ADDR_INET (ip,_) -> ip
  | _ -> assert false
  in
  let generate_page
      staticdirref
      ip
      global_tables
      session_tables_ref =
    catch
      (* Is it a static page? *)
      (fun () -> 
        if params = "" (* static pages do not have parameters *)
        then begin
          Messages.debug ("--- Is it a static file?");
          let filename = 
            find_static_page staticdirref (change_empty_list url) in
          return 
            (((Sender_helpers.send_file filename),
              Sender_helpers.create_file_sender,
              []),
             Some ((Unix.LargeFile.stat filename).Unix.LargeFile.st_mtime),
             Some (Sender_helpers.File_content.get_etag filename))
        end
        else fail Ocsigen_404)
      (fun e -> 
        match e with
          (Unix.Unix_error (Unix.EACCES,_,_))
        | Ocsigen_Is_a_directory
        | Ocsigen_malformed_url -> fail e
        | _ -> 
            ((catch (* Generate a dynamic page *)
               (fun () -> 
                 Messages.debug 
                   ("-- I'm looking for "^(string_of_url_path url)^
                    " in the session table:");
                 (find_service
                    !session_tables_ref
                    (session_tables_ref,
                     (sp,
                      ip,
                      fullurl),
                     internal_state)))
               (function 
                   Ocsigen_404 | Ocsigen_Wrong_parameter -> 
                     catch (* ensuite dans la table globale *)
                       (fun () -> 
                         Messages.debug "-- I'm searching in the global table:";
                         (find_service 
                            global_tables
                            (session_tables_ref,
                             (sp,
                              ip,
                              fullurl),
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
                                         (sp,
                                          ip,
                                          fullurl),
                                         None)))
                                   (function
                                       Ocsigen_404 | Ocsigen_Wrong_parameter -> 
                                         (* ensuite dans la table globale *)
                                         Messages.debug "-- I'm searching in the global table, without state parameter:";
                                         (find_service 
                                            global_tables
                                            (session_tables_ref,
                                             (sp,
                                              ip,
                                              fullurl),
                                             None))
                                     | e -> fail e))
                         | e -> fail e)
                 | e -> fail e)) >>= (fun r -> return (r,None,None))))
  in catch 
    (fun () ->
      do_for_host_matching 
        host 
        port
        ip
        (fun (staticdirref, global_tables, session_tables) -> 
          execute (generate_page staticdirref)
            ip cookie 
            (global_tables, session_tables)))
    (function
        Ocsigen_Typing_Error l -> 
          return ((cookie, (Sender_helpers.send_xhtml_page 
                             ~content:(Error_pages.page_error_param_type l)),
                   Sender_helpers.create_xhtml_sender, "/"),None,None)
      | Ocsigen_Wrong_parameter -> return 
            ((cookie, (Sender_helpers.send_xhtml_page 
                         ~content:(Error_pages.page_bad_param)),
              Sender_helpers.create_xhtml_sender, "/"),None,None)
      | e -> fail e)




let make_action action_name action_params 
    (path, params, _, (url, host, _, _, useragent)) sockaddr cookie =
  let fullurl = path^params in
  let port,ip = match sockaddr with
    Unix.ADDR_INET (ip,port) -> port,ip
  | _ -> assert false
  in
  let generate_page ip global_tables session_tables_ref =
    let action,working_dir = 
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
          ((url, host, [], action_params, useragent), ip, fullurl))) >>=
    (fun r -> return ((r,(), working_dir),None,None))
  in catch
    (fun () ->
      do_for_host_matching 
        host
        port
        ip
        (fun (staticdirref, global_tables, session_tables) -> 
          execute 
            generate_page ip cookie (global_tables, session_tables) >>=
          (fun ((c,(),(),wd),_,_) ->
            Messages.debug "Action executed";
            return (c,wd))))
    (function
        Ocsigen_Typing_Error _ -> return (cookie, "/")
      | Ocsigen_Wrong_parameter -> return (cookie, "/")
      | e -> fail e)


(** Module loading *)
exception Ocsigen_error_while_loading of string
    
let load_ocsigen_module host sites =
  let rec load_sites ((statdirref, _, _) as pages_tree) = 
    function
        [] -> ()
      | (path, (cmos, static))::l ->
          (match static with
            None -> ()
          | Some static -> set_static_dir statdirref static path);
          (* let save_current_dir = get_current_hostdir () in *)
          (try
            begin_load_ocsigen_module ();
            absolute_change_hostdir (pages_tree, path);
            List.iter 
              (fun cmo -> 
                try
                  Dynlink.loadfile cmo
                with Dynlink.Error e -> 
                  raise
                    (Ocsigen_error_while_loading 
                       (cmo^" ("^(Dynlink.error_message e)^")")))
              cmos;
            (* absolute_change_hostdir save_current_dir; *)
            end_load_ocsigen_module ()
          with 
          | e -> 
              (* absolute_change_hostdir save_current_dir; *)
              end_load_ocsigen_module ();
              raise e (*Ocsigen_error_while_loading cmo*));
          load_sites pages_tree l
  in 
  let pages_tree = new_pages_tree () in
  load_sites pages_tree sites;
  pages_trees := !pages_trees@[(host, pages_tree)]
                                

let number_of_sessions () = 
  List.fold_left 
    (fun d (_,(_,_,cookie_table)) -> d + (Cookies.length cookie_table))
    0 !pages_trees
  

(* This is used by server.ml. 
   I put that here because I need it to be accessible for profiling. *)
let get_number_of_connected, 
  incr_connected, 
  decr_connected =
  let connected = ref 0 in
  ((fun () -> !connected),
   (fun () -> connected := !connected + 1),
   (fun () -> connected := !connected - 1))



