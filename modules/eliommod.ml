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

(** state is a parameter to differenciate
   several instances of the same URL.
   (for internal use)
 *)
type internal_state = int

type sess_info =
    {si_other_get_params: (string * string) list;
     si_cookie: string option;
     si_nonatt_info: (string option * string option);
     si_state_info: (internal_state option * internal_state option);
     si_exn: exn list;
     si_config_file_charset: string option}

type 'a server_params1 = 
    request_info * sess_info * 
      (current_dir (* main directory of the site *) *
         'a ref (* session table ref *) * 
         float option option ref (* user timeout *) *
         url_path (* suffix *))
      
(********)

exception Eliom_Wrong_parameter
exception Eliom_link_to_old
exception Eliom_Typing_Error of (string * exn) list

exception Eliom_duplicate_registering of string
exception Eliom_there_are_unregistered_services of string
exception Eliom_function_forbidden_outside_site_loading
exception Eliom_page_erasing of string
exception Eliom_error_while_loading_site of string

(*****************************************************************************)
let eliom_suffix_name = "__eliom_suffix"
let naservice_prefix = "__eliom_na__"
let naservice_name = "name"
let get_state_param_name = "__eliom__"
let post_state_param_name = "__eliom_p__"
let cookiename = "eliomsession"
let co_param_prefix = "__co_eliom_"
let na_co_param_prefix = "__na_eliom_"

(*****************************************************************************)
type result_to_send = 
    EliomResult of Extensions.result
  | EliomExn of (exn list * cookieslist)


(*****************************************************************************)
(* Finding special eliommod parameters (for naservices, state, suffix ...)   *)

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

let change_request_info ri charset =
  force ri.ri_post_params >>=
  (fun post_params -> 
    let get_params = force ri.ri_get_params in
    let cookie = getcookie (force ri.ri_cookies) in
    let (naservice_info, (get_state, post_state),
      (get_params, other_get_params), post_params) =
      let post_naservice_name, na_post_params = 
        try
          let n, pp =
            list_assoc_remove (naservice_prefix^naservice_name) post_params
          in (Some n, pp)
        with Not_found -> (None, []) 
        (* Not possible to have POST parameters without naservice_name
           if there is a GET naservice_name
         *)
      in
      let get_naservice_name, (na_get_params, other_get_params) = 
        try
          let n, gp =
            list_assoc_remove (naservice_prefix^naservice_name) get_params
          in (Some n, (split_prefix_param na_co_param_prefix gp))
        with Not_found -> (None, ([], get_params))
      in
      match get_naservice_name, post_naservice_name with
        _, Some _
      | Some _, None -> (* non attached coservice *)
          ((get_naservice_name, post_naservice_name),
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
       {si_cookie=cookie;
        si_nonatt_info=naservice_info;
        si_state_info=(get_state, post_state);
        si_other_get_params=other_get_params;
        si_exn=[];
        si_config_file_charset=charset},
      [] (* no cookie to set *)))







(*****************************************************************************)
(* The table of dynamic pages for each virtual server, and naservices        *)
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
       ((int (* unique_id *) * 
           (int ref option (* max_use *) *
              (tables server_params1 -> result_to_send Lwt.t)
	      * url_path)) list)) list
      (* Here, the url_path is the working directory.
         That is, the directory in which we are when we register
         dynamically the pages.
         Each time we load a page, we change to this directory
         (in case the page registers new pages).
       *)

and naservice_table = 
    AVide 
  | ATable of 
      (int ref option (* max_use *) *
         (tables server_params1 -> result_to_send Lwt.t)
	 * url_path)
        NAserv_Table.t

and dircontent = 
    Vide
  | Table of direlt ref String_Table.t

and direlt = 
    Dir of dircontent ref
  | File of page_table ref

and tables = dircontent ref * naservice_table ref

type cookiestable = (tables * 
                       float option(* expiration date *) *
                       float option option ref (* timeout *)) Cookies.t

type pages_tree = 
    tables (* global tables of continuations/naservices *)
      * cookiestable (* session tables *)

let empty_page_table () = []
let empty_naservice_table () = AVide
let empty_dircontent () = Vide
let empty_tables () =
  (ref (empty_dircontent ()), ref (empty_naservice_table ()))
    
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
    current_dir := (fun () -> pagetree), dir) in
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
  if (during_initialisation ()) && (during_eliom_module_loading ())
  then Some get_current_hostdir
  else None



(*****************************************************************************)
(* dynamic pages *)
(** We associate to a service a function server_params -> page *)

    (** Create server parameters record *)
let make_server_params dir str user_timeout_optref ri suffix si =
  (ri,
   si,
   (dir,
    str,
    user_timeout_optref,
    suffix))


let find_page_table 
    (t : page_table ref)
    str 
    user_timeout_optref
    ri
    urlsuffix
    k
    si
    = 
  let (sp0, si, (_, s, tim, u)) = 
    make_server_params [] str user_timeout_optref ri urlsuffix si in
  let rec aux = function
      [] -> fail Eliom_Wrong_parameter
    | (((_, (max_use, funct, working_dir)) as a)::l) as ll ->
        catch 
          (fun () ->
            Messages.debug "- I'm trying a service";
            funct (sp0, si, (working_dir, s, tim, u)) >>=
            (fun p -> 
              Messages.debug "- Page found and generated successfully";
              let newlist =
                (match max_use with
                  Some r -> 
                    if !r = 1
                    then l
                    else (r := !r - 1; ll)
                | _ -> ll)
              in
              Lwt.return ((p, working_dir), newlist)))
          (function
              Eliom_Wrong_parameter -> 
                aux l >>= (fun (r,ll) -> Lwt.return (r, a::ll))
            | e -> fail e)
  in 
  (catch 
     (fun () -> return (list_assoc_remove k !t))
     (function Not_found -> fail Ocsigen_404 | e -> fail e)) >>=
  (fun (liste, newt) -> aux liste >>=
    (fun (r, newlist) -> 
      (if newlist = []
      then t := newt
      else t := (k, newlist)::newt);
      Lwt.return r))




let add_page_table session url_act t (key,((id, _) as v)) = 
  (* Duplicate registering forbidden in global table *)
  try
    let l,newt = list_assoc_remove key t in
    try
(********* Vérifier ici qu'il n'y a pas qqchose similaire déjà enregistré ?! *)
      let _,oldl = list_assoc_remove id l in
      if not session then
        raise (Eliom_duplicate_registering (string_of_url_path url_act))
      else (key,(oldl@[v]))::newt (* At the end! 
                                     services are tried in creation order *)
    with Not_found -> (key,(l@[v]))::newt
  with Not_found -> (key,[v])::t

let add_dircontent dc (key,elt) =
  match dc with
    Vide -> Table (String_Table.add key elt String_Table.empty)
  | Table t -> Table (String_Table.add key elt t)

let find_dircontent dc k =
  match dc with
    Vide -> raise Not_found
  | Table t -> String_Table.find k t

let add_naservice_table at (key,elt) = 
  match at with
    AVide -> ATable (NAserv_Table.add key elt NAserv_Table.empty)
  | ATable t -> ATable (NAserv_Table.add key elt t)

let find_naservice_table at k = 
  match at with
    AVide -> raise Not_found
  | ATable t -> NAserv_Table.find k t

let remove_naservice_table at k = 
  try
    match at with
      AVide -> AVide
    | ATable t -> ATable (NAserv_Table.remove k t)
  with _ -> at

let add_naservice 
    (_,naservicetableref) current_dir session name (max_use, naservice) =
  (if not session
  then
    try
      ignore (find_naservice_table !naservicetableref name);
      raise (Eliom_duplicate_registering "<non-attached coservice>")
    with Not_found -> ());
  naservicetableref :=
    add_naservice_table !naservicetableref
      (name, (max_use, naservice, current_dir))

let find_naservice (_,atr) name =
  find_naservice_table !atr name

let remove_naservice (_,atr) name =
  atr := remove_naservice_table !atr name

let add_service (dircontentref,_) current_dir session url_act
    (page_table_key, (unique_id, max_use, action)) =

  let aux search dircontentref a l =
    try 
      let direltref = find_dircontent !dircontentref a in
      match !direltref with
        Dir dcr -> search dcr l
      | File ptr -> raise (Eliom_page_erasing 
                             ((string_of_url_path current_dir)^"/"^a))
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
                 (unique_id, (max_use, action, current_dir))) in
  (* let current_dircontentref = 
     search_dircontentref dircontentref current_dir) in *)
  let page_table_ref = 
    search_page_table_ref (*current_*) dircontentref url_act in
  page_table_ref := add_page_table session url_act !page_table_ref content

      
exception Exn1

let find_service 
    (dircontentref,_)
    (session_table_ref, 
     user_timeout_optref,
     ri,
     si) =
  let rec search_page_table dircontent =
    let aux a l =
      try
        let dc = 
          try !(find_dircontent dircontent a) 
          with Not_found -> raise Exn1
        in
        (match dc with
          Dir dircontentref2 -> search_page_table !dircontentref2 l
        | File page_table_ref -> page_table_ref, l)
      with Exn1 -> 
        (match !(find_dircontent dircontent defaultpagename) with
          Dir _ -> raise Not_found
        | File page_table_ref -> page_table_ref, a::l)
    in function
        [] -> raise Ocsigen_Is_a_directory
      | [""] -> aux defaultpagename []
      | ""::l -> search_page_table dircontent l
      | a::l -> aux a l
  in
  let page_table_ref, suffix = 
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
    page_table_ref
    session_table_ref
    user_timeout_optref
    {ri with ri_get_params = lazy get_param_list}
    suffix
    {suffix = pref;
     state = si.si_state_info}
    si


(****************************************************************************)
(****************************************************************************)
(****************************************************************************)
(****************************************************************************)

type session_table = tables

(** Type of http parameters *)
type server_params = session_table server_params1

let new_session_tables = empty_tables


(*****************************************************************************)
(* Table of timeouts for sessions *)
let (set_default_timeout, get_default_timeout) =
  let t = ref (Some 3600.) in (* 1 hour by default *)
  ((fun timeout -> t := timeout),
  (fun () -> !t))

type timeout_table =
    Tt of (float option option * (string * timeout_table) list)

let (find_global_timeout, set_global_timeout) =
  let table = ref (Tt (None, [])) in
  let rec find = function
      (Tt (t, _), []) -> t
    | (ta, ""::l) -> find (ta, l)
    | (Tt (t, r), a::l) ->
        try
          find ((List.assoc a r), l)
        with Not_found -> None
  in
  let rec add timeout = function
      (Tt (t, l), []) -> Tt ((Some timeout), l)
    | (ta, ""::l) -> add timeout (ta, l)
    | (Tt (t, r), a::l) ->
        try
          let (Tt (tt, ll), rr) = list_assoc_remove a r in
          Tt (t, (a, add timeout (Tt (tt, ll), l))::rr)
        with Not_found -> Tt (t, (a, (add timeout (Tt (None, []), l)))::r)
  in
  ((fun working_dir -> match find (!table, working_dir) with
    None -> get_default_timeout ()
  | Some t -> t),
   (fun working_dir s -> 
     table := add s (!table, working_dir)))


(*****************************************************************************)
(** Parsing global configuration for Eliommod: *)
open Simplexmlparser.ExprOrPatt

let sessiongcfrequency = ref (Some 3600.)
let set_sessiongcfrequency i = sessiongcfrequency := i
let get_sessiongcfrequency () = !sessiongcfrequency

let rec parse_global_config = function
      PLEmpty -> ()
    | PLCons 
        (EPanytag 
           ("timeout", 
            (PLCons
               ((EPanyattr (EPVstr("value"), EPVstr(s))), 
                PLEmpty)),
            PLEmpty), ll) -> 
              (try
                set_default_timeout (Some (float_of_string s))
              with Failure _ -> 
                if (s = "infinity")
                then set_default_timeout None
                else
                  raise (Error_in_config_file "Eliom: Wrong value for value attribute of <timeout> tag"));
              parse_global_config ll
    | PLCons 
        ((EPanytag ("sessiongcfrequency",             
                    (PLCons
                       ((EPanyattr (EPVstr("value"), EPVstr(s))), 
                        PLEmpty)),
                    p)), ll) ->
          (try
            set_sessiongcfrequency (Some (float_of_string s))
          with Failure _ -> 
            if s = "infinity"
            then set_sessiongcfrequency None
            else raise (Error_in_config_file
                          "Eliom: Wrong value for <sessiongcfrequency>"));
          parse_global_config ll
    | PLCons ((EPcomment _), l) -> parse_global_config l
    | PLCons ((EPwhitespace _), l) -> parse_global_config l
    | PLCons ((EPanytag (tag,_,_)),l) -> 
        raise (Error_in_config_file ("<"^tag^"> tag unexpected inside eliom config"))
    | _ -> raise (Error_in_config_file ("Unexpected content inside eliom config"))


let _ = parse_global_config (Extensions.get_config ())

(*****************************************************************************)
(* Generation of the page or naservice                                          *)

let execute generate_page ip cookie (globtable, cookie_table) =
  let (sessiontablesref, new_session, user_timeout_optref) = 
    (match cookie with
      None -> ((ref (new_session_tables ())), None, ref None)
    | Some c -> 
        try 
          let ta, exp, timeout_optref = Cookies.find cookie_table (ip,c) in
          match exp with
            Some t when t < (Unix.time ()) -> (* session expired *)
              Cookies.remove cookie_table (ip,c);
              raise Not_found
          | _ -> ((ref ta), cookie, timeout_optref)
        with Not_found -> ((ref (new_session_tables ())), None, ref None))
  in
  generate_page globtable sessiontablesref user_timeout_optref >>=
  (fun (result, working_dir) ->
    let cookie2 = 
      if are_empty_tables !sessiontablesref
      then (
        (match new_session with
          Some c -> Cookies.remove cookie_table (ip, c)
        | _ -> ());
        None)
      else 
        let c =
          (match new_session with
            None -> new_cookie cookie_table ip
          | Some c -> c)
        in
        let timeout = match !user_timeout_optref with
          Some t -> t
        | None -> find_global_timeout working_dir
        in
        let exp = match timeout with
          None -> None
        | Some t -> Some ((Unix.time ())+. t)
        in
        Cookies.replace
          cookie_table (ip,c) 
          (!sessiontablesref, exp, user_timeout_optref);
        (* If the key does not exist, replace just adds it.
           If it exists, we put a new expiration date *)
        Some c
    in
    let cookie3 = 
      if cookie2 <> cookie then 
        (if cookie2 = None 
        then Some remove_cookie_str
        else cookie2)
      else None
    in return 
      (cookie3, 
       result, 
       working_dir))

exception Eliom_retry_with of 
  (request_info * 
     sess_info * 
     cookieslist (* cookies to set *)) 

let get_page
    page_tree
    (ri, si, cookies_to_set) =
  let generate_page
      global_tables
      session_tables_ref
      user_timeout_optref 
      =
    ((catch
        (fun () -> 
          Messages.debug 
            ("-- I'm looking for "^(string_of_url_path ri.ri_path)^
             " in the session table:");
          (find_service
             !session_tables_ref
             (session_tables_ref,
              user_timeout_optref,
              ri,
              si)))
        (function 
            Ocsigen_404 | Eliom_Wrong_parameter -> 
              catch (* ensuite dans la table globale *)
                (fun () -> 
                  Messages.debug "-- I'm searching in the global table:";
                  (find_service 
                     global_tables
                     (session_tables_ref,
                      user_timeout_optref,
                      ri,
                      si)))
                (function
                    Ocsigen_404 | Eliom_Wrong_parameter as exn -> 
                      (* si pas trouvé avec, on essaie sans l'état *)
                      (match si.si_state_info with
                        (None, None) -> fail exn
                      | (g, Some _) -> 
                          (* There was a POST state. 
                             We remove it, and remove POST parameters.
                           *)
                          Messages.debug 
                            "-- Link to old. I will try without POST parameters:";
                          fail (Eliom_retry_with 
                                  ({ri with 
                                    ri_post_params = lazy (return [])
                                  }, 
                                   {si with
                                    si_nonatt_info= (None, None);
                                    si_state_info= (g, None);
                                    si_exn= Eliom_link_to_old::si.si_exn
                                  },
                                   cookies_to_set (* no new cookie *)
                                  ))
                      | (Some _, None) -> 
                          (* There was a GET state, but no POST state. 
                             We remove it with its parameters, 
                             and remove POST parameters.
                           *)
                          Messages.debug 
                            "-- Link to old. I will try without GET state parameters and POST parameters:";
                          fail (Eliom_retry_with 
                                  ({ri with 
                                    ri_get_params = 
                                    lazy si.si_other_get_params;
                                    ri_post_params = lazy (return [])
                                  },
                                   {si with
                                    si_nonatt_info=(None, None);
                                    si_state_info=(None, None);
                                    si_other_get_params=[];
                                    si_exn= Eliom_link_to_old::si.si_exn
                                  },
                                   cookies_to_set (* no new cookie *))))
                  | e -> fail e)
          | e -> fail e)) >>= (fun r -> return r))
  in (generate_page, ri.ri_inet_addr, si.si_cookie, page_tree)


let make_naservice
    page_tree
    (ri, si, cookies_to_set) =
  let generate_page
      global_tables session_tables_ref user_timeout_optref =
    (try
      try
        return (find_naservice !session_tables_ref si.si_nonatt_info)
      with
        Not_found -> return 
            (find_naservice global_tables si.si_nonatt_info)
    with
      Not_found ->
        (* It was an non-attached service.
           We call the same URL without non-attached parameters.
         *)
        match si.si_nonatt_info with
          None, None -> assert false
        | Some _ as g, Some _ ->
            Messages.debug 
              "-- Link to old. I will try with only GET non-attached parameters:";
            fail (Eliom_retry_with
                    ({ri with 
                      ri_post_params = lazy (return [])
                    },
                     {si with
                      si_nonatt_info=(g, None);
                      si_state_info=(None, None);
                      si_exn= Eliom_link_to_old::si.si_exn
                    },
                     cookies_to_set (* no new cookie *)))
        | _ ->
            Messages.debug 
              "-- Link to old. I will try without non-attached parameters:";
            change_request_info
              {ri with 
               ri_get_params = lazy si.si_other_get_params;
               ri_post_params = lazy (return [])
             } 
              si.si_config_file_charset >>=
            (fun (ri,si,_) -> 
              fail (Eliom_retry_with 
                      (ri,
                       {si with
                        si_exn= Eliom_link_to_old::si.si_exn
                      },
                       cookies_to_set (* no new cookie *))))
    ) >>=
    (fun (max_use, naservice, working_dir) ->
      (naservice
         (make_server_params 
            working_dir
            session_tables_ref 
            user_timeout_optref
            ri
            []
            si)) >>=
      (fun r -> 
        Messages.debug "- Non attached page found and generated successfully";
        (match max_use with
          None -> ()
        | Some r -> 
            if !r = 1
            then remove_naservice !session_tables_ref si.si_nonatt_info
            else r := !r - 1);
        return (r, working_dir)))
  in (generate_page, ri.ri_inet_addr, si.si_cookie, page_tree)
    

let gen page_tree charset ri =
  let rec gen_aux ((ri, si, old_cookies_to_set) as info) =
    let (gen,ia,c,pt) = 
      match si.si_nonatt_info with
	None, None ->
          
          (* page generation *)
          get_page page_tree info
            
      | _ ->
          
          (* anonymous service *)
          make_naservice page_tree info
    in
    
    catch 
      (fun () ->
        execute gen ia c pt >>=
	fun (new_cookie, result_to_send, path) ->
          
          let compute_cookies cookies_set_by_page =
            let cookies_set_by_page =
              List.map (fun (pathopt,cl) -> 
                ((match pathopt with
                  None -> Some path (* Not possible to set a cookie for another site (?) *)
                | Some p -> Some (path@p)
                 ),cl)) cookies_set_by_page
            in
            let cookies_to_set = cookies_set_by_page@old_cookies_to_set in
            let all_new_cookies =
              match new_cookie with
                None -> cookies_to_set
              | Some c -> (Some path, [(cookiename, c)])::cookies_to_set
            in (cookies_set_by_page, all_new_cookies)
          in


          match result_to_send with
            EliomExn (exnlist, cookies_set_by_page) -> 
                     (* Nothing to send, we retry without POST params
                       (it was an action, we reload the page).
                       If it was an action without POST parameters, 
                       we do not reload, otherwise it will loop.
                     *)
              let cookies_set_by_page, all_new_cookies =
                compute_cookies cookies_set_by_page
              in
              force ri.ri_post_params >>=
              (fun ripp ->
                (match si.si_nonatt_info, si.si_state_info, ripp with
                  (_, None), (_, None), [] ->
                    return
                      (Ext_found
                         {res_cookies= all_new_cookies;
                          res_send_page=
                          Predefined_senders.send_empty ~content:();
                          res_create_sender=
                          Predefined_senders.create_empty_sender;
                          res_code=Some 204;
                          res_lastmodified=None;
                          res_etag=None;
                          res_charset=None})
                      
                | _ ->
                    fail
                      (* Ext_retry_with or Eliom_retry_with? *)
                      (Eliom_retry_with 
                         (let cookies_presents =
                           List.fold_left
                             (fun l (_, cl) -> cl@l)
                             (force ri.ri_cookies)
                             cookies_set_by_page
                         in
                         ({ri with
                           ri_post_params = lazy (return []);
                           ri_cookies= lazy
                             (match new_cookie with
                               None -> cookies_presents
                             | Some c -> ((cookiename,c)::cookies_presents))},
                          {si_other_get_params= si.si_other_get_params;
                           si_cookie= (match new_cookie with
                             Some c -> new_cookie
                           | None -> si.si_cookie);
                           si_nonatt_info= ((fst si.si_nonatt_info), None);
                           si_state_info= ((fst si.si_state_info), None);
                           si_exn = exnlist@si.si_exn;
                           si_config_file_charset = si.si_config_file_charset},
                          all_new_cookies)))))
          | EliomResult res ->
              let cookies_set_by_page, all_new_cookies =
                compute_cookies res.res_cookies
              in
              return 
                (Ext_found 
                   {res with
                    res_cookies= all_new_cookies
                  })
      )
      (function
          Eliom_Typing_Error l -> 
            return (Ext_found
                      {res_cookies= old_cookies_to_set;
                       res_send_page=
                       (Predefined_senders.send_xhtml_page 
                          ~content:(Error_pages.page_error_param_type l));
                       res_create_sender=
                       Predefined_senders.create_xhtml_sender;
                       res_code=None;
                       res_lastmodified=None;
                       res_etag=None;
                       res_charset= Error_pages.charset})
	| Eliom_Wrong_parameter -> 
	    return (Ext_found 
                      {res_cookies= old_cookies_to_set;
                       res_send_page=
                       (Predefined_senders.send_xhtml_page 
                          ~content:(Error_pages.page_bad_param));
                       res_create_sender=
                       Predefined_senders.create_xhtml_sender;
                       res_code=None;
                       res_lastmodified=None;
                       res_etag=None;
                       res_charset= Error_pages.charset})
	| Ocsigen_404 -> return Ext_not_found
        | Eliom_retry_with a -> gen_aux a
	| e -> fail e)

  in
  change_request_info ri charset >>= gen_aux


(*****************************************************************************)
(* garbage collection of timeouted sessions *)
(* This is a thread that will work every hour *)
let session_gc (_, cookie_table) =
  match get_sessiongcfrequency () with
    None -> () (* No garbage collection *)
  | Some t ->
      let rec f () = 
        Lwt_unix.sleep t >>= 
        (fun () ->
          let now = Unix.time () in
          Messages.debug "GC of sessions";
          Cookies.fold
            (fun k (_, exp, _) thr -> 
              (match exp with
                Some exp when exp < now -> Cookies.remove cookie_table k
              | _ -> ());
              Lwt_unix.yield ()
            )
            cookie_table
            (return ()))
          >>=
        f
      in ignore (f ())
      

(*****************************************************************************)
(** Module loading *)
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
(** Parsing of config file for each site: *)
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
| Eliom_function_forbidden_outside_site_loading ->
    ("Fatal - Eliom: Use of forbidden function outside site loading. \
              (creation of public service for example)")
| Eliom_page_erasing s ->
    ("Fatal - Eliom: You cannot create a page or directory here. "^s^
            " already exists. Please correct your modules.")
| Eliom_error_while_loading_site s ->
    ("Fatal - Eliom: Error while loading site: "^s)
| e -> raise e


(*****************************************************************************)
(** extension registration *)
let _ = register_extension
    ((fun hostpattern -> 
      let page_tree = new_pages_tree () in
      session_gc page_tree;
      (gen page_tree, 
       parse_config page_tree)),
     start_init,
     end_init,
     handle_init_exn)


(*****************************************************************************)

(* à refaire
let number_of_sessions () = Cookies.length
*)


