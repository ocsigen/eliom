(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod.ml
 * Copyright (C) 2007 Vincent Balat
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
     si_cookie: string option ref;
     si_persistent_cookie: (string * int64) option ref;
     si_nonatt_info: (string option * string option);
     si_state_info: (internal_state option * internal_state option);
     si_exn: exn list;
     si_config_file_charset: string option}

(* The table of tables for each session. Keys are cookies *)
module Cookies = Hashtbl.Make(struct 
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
end)

type 'a server_params1 = 
    request_info * sess_info * 
      (current_dir (* main directory of the site *) *
         ('a (* global table *) * 
            ('a * string list 
               (* only to put expiration date on the right working dir
                  To be removed if the management of cookies paths is improved *) * 
               float option * float option option ref)
            Cookies.t (* cookies table *) * 
            (string -> unit) ref) * (* remove_session_data *)
         'a ref (* session table ref *) * 
         (float option option ref * float option ref *
            float option option ref * float option ref) 
         (* user timeout for this site (None -> see global config)
            and expiration date for the cookie (None -> browser) 
            then the same for persistent session
          *) *
         url_path (* suffix *))
      
type anon_params_type = int

(********)

exception Eliom_Wrong_parameter
exception Eliom_Link_too_old
exception Eliom_Session_expired
exception Eliom_Persistent_session_expired
exception Eliom_Typing_Error of (string * exn) list

exception Eliom_duplicate_registration of string
exception Eliom_there_are_unregistered_services of string
exception Eliom_function_forbidden_outside_site_loading
exception Eliom_page_erasing of string
exception Eliom_error_while_loading_site of string

(*****************************************************************************)
let eliom_suffix_name = "__eliom_suffix"
let eliom_suffix_internal_name = "__eliom_suffix**"
let naservice_prefix = "__eliom_na__"
let naservice_name = "name"
let get_state_param_name = "__eliom__"
let post_state_param_name = "__eliom_p__"
let cookiename = "eliomsession"
let persistentcookiename = "eliompersistentsession"
let co_param_prefix = "__co_eliom_"
let na_co_param_prefix = "__na_eliom_"
let eliom_persistent_cookie_table = "eliom_persist_cookies"

(*****************************************************************************)
type result_to_send = 
    EliomResult of Extensions.result
  | EliomExn of (exn list * cookieslist)


(*****************************************************************************)
(* Finding special eliommod parameters (for naservices, state, suffix ...)   *)

let getcookie cookiename cookies = 
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
    let cookie = getcookie cookiename (force ri.ri_cookies) in
    let persistent_cookie = 
      match getcookie persistentcookiename (force ri.ri_cookies) with
      | None -> None
      | Some c -> Some (c, Int64.zero)
    in
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
       {si_cookie= ref cookie;
        si_persistent_cookie= ref persistent_cookie;
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

let rec new_cookie table = 
  let c = Int64.to_string (Random.int64 Int64.max_int) in
  try
    ignore (Cookies.find table c);
    new_cookie table
  with Not_found -> c

let remove_session_table cookie_table = function
  | None -> ()
  | Some c -> Cookies.remove cookie_table c

type page_table_key =
    {state: (internal_state option * internal_state option)}
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
       (((anon_params_type * anon_params_type) (* unique_id *) * 
           (int * (* generation (= number of reloads of sites
                     that after which that service has been created) *)
              (int ref option (* max_use *) *
                 (float * float ref) option
                 (* timeout and expiration date for the service *) *
                 (tables server_params1 -> result_to_send Lwt.t)
	         * url_path))) list)) list
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
         (float * float ref) option (* timeout and expiration date *) *
         (tables server_params1 -> result_to_send Lwt.t)
	 * url_path)
        NAserv_Table.t

and dircontent = 
    Vide
  | Table of direlt ref String_Table.t

and direlt = 
    Dir of dircontent ref
  | File of page_table ref

and tables = dircontent ref * naservice_table ref *
      (* Information for the GC: *)
      bool ref (* true if dircontent contains services with timeout *) *
      bool ref (* true if naservice_table contains services with timeout *)

(* non persistent cookies 
      (persistent cookies are always called persistent_cookies in the code) *)
type cookiestable = (tables * 
                       url_path
        (* only to put expiration date on the right working dir
           To be removed if the management of cookies paths is improved *) * 
                       float option (* expiration date by timeout *) *
                       float option option ref (* timeout *)) Cookies.t
(* the table contains:
   - the table of services
   - the expiration date (by timeout), changed at each access to the table
     (float option) None -> no expiration
   - the timeout for the user (float option option) None -> see global config
     Some None -> no timeout
 *)
(* table cookie -> session table *)
let new_cookie_table () : cookiestable = Cookies.create 100



type pages_tree = 
    tables (* global tables of continuations/naservices *)
      * cookiestable (* session tables *)
      * (string -> unit) ref (* remove_session_data *)

let empty_page_table () = []
let empty_naservice_table () = AVide
let empty_dircontent () = Vide
let empty_tables () =
  (ref (empty_dircontent ()), 
   ref (empty_naservice_table ()),
   ref false,
   ref false)
    
let are_empty_tables (lr,atr,_,_) = 
  (!lr = Vide && !atr = AVide)

let new_pages_tree () =
  ((empty_tables ()),
   (new_cookie_table ()),
   ref (fun cookie -> ())) (* remove_session_data *)

(*****************************************************************************)
(* The current registration directory *)
let absolute_change_hostdir, get_current_hostdir, 
  begin_current_host_dir, end_current_hostdir =
  let current_dir : ((unit -> pages_tree) * url_path) ref = 
    ref ((fun () ->
      raise (Ocsigen_Internal_Error "No pages tree available")), []) 
  in
  let f1' (pagetree, dir) = current_dir := ((fun () -> pagetree), dir) in
  let f2' () = let (cd1, cd2) = !current_dir in (cd1 (), cd2) in
  let f1 = ref f1' in
  let f2 = ref f2' in
  let exn1 _ = 
    raise Eliom_function_forbidden_outside_site_loading in
  let exn2 () = 
    raise Eliom_function_forbidden_outside_site_loading in
  ((fun hostdir -> !f1 hostdir),
   (fun () -> !f2 ()),
   (fun () -> f1 := f1'; f2 := f2'),
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
     | a::_ -> 
         raise (Eliom_there_are_unregistered_services 
                  (match a with
                    None -> "<Non-attached service>"
                  | Some a -> string_of_url_path a))))

let during_eliom_module_loading, 
  begin_load_eliom_module, 
  end_load_eliom_module =
  let during_eliom_module_loading_ = ref false in
  ((fun () -> !during_eliom_module_loading_),
   (fun () -> during_eliom_module_loading_ := true),
   (fun () -> during_eliom_module_loading_ := false))

let global_register_allowed () =
  if (during_initialisation ()) && (during_eliom_module_loading ())
  then Some get_current_hostdir
  else None



(*****************************************************************************)
(* dynamic pages *)
(** We associate to a service a function server_params -> page *)

    (** Create server parameters record *)
let make_server_params dir tables str session_exp_info ri suffix si =
  (ri,
   si,
   (dir,
    tables,
    str,
    session_exp_info,
    suffix))


type ('a, 'b) leftright = Left of 'a | Right of 'b

let find_page_table 
    now
    (t : page_table ref)
    tables
    str 
    session_exp_info
    ri
    urlsuffix
    k
    si
    = 
  let (sp0, si, (_, tab, s, tim, u)) = 
    make_server_params [] tables str session_exp_info ri urlsuffix si in
  let rec aux = function
      [] -> Lwt.return ((Right Eliom_Wrong_parameter), [])
    | (((_, (_, (max_use, expdate, funct, working_dir))) as a)::l) as ll ->
        match expdate with
        | Some (_, e) when !e < now ->
            (* Service expired. Removing it. *)
            Messages.debug "Service expired. I'm removing it";
            aux l >>= (fun (r, ll) -> Lwt.return (r, ll (* without a *)))
        | _ ->
            catch 
              (fun () ->
                Messages.debug "- I'm trying a service";
                funct (sp0, si, (working_dir, tab, s, tim, u)) >>=
                (fun p -> 
                  Messages.debug "- Page found and generated successfully";
                  (match expdate with
                  | Some (timeout, e) -> e := timeout +. now
                  | None -> ());
                  let newlist =
                    (match max_use with
                    | Some r -> 
                        if !r = 1
                        then l
                        else (r := !r - 1; ll)
                    | _ -> ll)
                  in
                  Lwt.return ((Left 
                                (p, 
                                 working_dir, 
                                 !(si.si_cookie),
                                 !(si.si_persistent_cookie))), 
                              newlist)))
              (function
                  Eliom_Wrong_parameter -> 
                    aux l >>= (fun (r, ll) -> Lwt.return (r, a::ll))
                | e -> Lwt.return ((Right e), a::ll))
  in 
  (catch 
     (fun () -> return (list_assoc_remove k !t))
     (function Not_found -> fail Ocsigen_404 | e -> fail e)) >>=
  (fun (liste, newt) -> aux liste >>=
    (fun (r, newlist) -> 
      (if newlist = []
      then t := newt
      else t := (k, newlist)::newt);
      match r with
      | Left r -> Lwt.return r
      | Right e -> fail e))



let rec insert_as_last_of_generation generation x = function
  | [] -> [x]
  | ((_, (g, _))::l) as ll when g < generation -> x::ll
  | a::l -> a::(insert_as_last_of_generation generation x l)

let add_page_table duringsession url_act t (key,(id, va)) = 
  (* Duplicate registration forbidden in global table *)
  let generation = Extensions.get_numberofreloads () in
  let v = (id, (generation, va)) in
  try
    let l,newt = list_assoc_remove key t in
    try
      if key = {state = (None, None)}
      then begin
(********* Vérifier ici qu'il n'y a pas qqchose similaire déjà enregistré ?! *)
        let (oldgen, n), oldl = list_assoc_remove id l in
        if not duringsession && (generation = oldgen)
        then
          raise (Eliom_duplicate_registration (string_of_url_path url_act))
        else (key, (insert_as_last_of_generation generation v oldl))::newt 
      end
      else (key, (insert_as_last_of_generation generation v l))::newt
    with Not_found -> 
      (key, (insert_as_last_of_generation generation v l))::newt
  with Not_found -> (key,[v])::t

let add_dircontent dc (key,elt) =
  match dc with
  | Vide -> Table (String_Table.add key elt String_Table.empty)
  | Table t -> Table (String_Table.add key elt t)

let find_dircontent dc k =
  match dc with
  | Vide -> raise Not_found
  | Table t -> String_Table.find k t

let add_naservice_table at (key,elt) = 
  match at with
  | AVide -> ATable (NAserv_Table.add key elt NAserv_Table.empty)
  | ATable t -> ATable (NAserv_Table.add key elt t)

let find_naservice_table at k = 
  match at with
  | AVide -> raise Not_found
  | ATable t -> NAserv_Table.find k t

let remove_naservice_table at k = 
  try
    match at with
    | AVide -> AVide
    | ATable t -> ATable (NAserv_Table.remove k t)
  with _ -> at

let add_naservice 
    (_,naservicetableref,_,containstimeouts) current_dir session name 
    (max_use, expdate, naservice) =
  (if not session
  then
    try
      ignore (find_naservice_table !naservicetableref name);
      raise (Eliom_duplicate_registration "<non-attached coservice>")
    with Not_found -> ());

  (match expdate with
  | Some _ -> containstimeouts := true
  | _ -> ());
  
  naservicetableref :=
    add_naservice_table !naservicetableref
      (name, (max_use, expdate, naservice, current_dir))

let remove_naservice (_,atr,_,_) name =
  atr := remove_naservice_table !atr name

let find_naservice now ((_,atr,_,_) as str) name =
  let ((_, expdate, _, _) as p) = find_naservice_table !atr name in
  match expdate with
  | Some (_, e) when !e < now ->
      (* Service expired. Removing it. *)
      Messages.debug "Non attached service expired. I'm removing it";
      remove_naservice str name;
      raise Not_found
  | _ -> p

let add_service 
    (dircontentref,_,containstimeouts,_)
    current_dir
    duringsession
    url_act
    (page_table_key, 
     ((unique_id1, unique_id2), max_use, expdate, action)) =

  let aux search dircontentref a l =
    try 
      let direltref = find_dircontent !dircontentref a in
      match !direltref with
        Dir dcr -> search dcr l
      | File ptr -> raise (Eliom_page_erasing 
                             ((string_of_url_path current_dir)^"/"^a))
            (* Messages.warning ("Eliom page registration: Page "^
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
                (* Messages.warning ("Eliom page registration: Directory "^
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

  (match expdate with
  | Some _ -> containstimeouts := true
  | _ -> ());

  let content = (page_table_key,
                 ((unique_id1, unique_id2), 
                  (max_use, expdate, action, current_dir))) in
  (* let current_dircontentref = 
     search_dircontentref dircontentref current_dir) in *)
  let page_table_ref = 
    search_page_table_ref (*current_*) dircontentref url_act in
  page_table_ref := 
    add_page_table duringsession url_act !page_table_ref content

      
exception Exn1

let find_service 
    now
    (dircontentref,_,_,_)
    (tables,
     session_table_ref, 
     session_exp_info,
     ri,
     si) =
  let rec search_page_table dircontent =
    let aux a l =
      let aa = match a with
        None -> defaultpagename
      | Some aa -> aa
      in
      try
        let dc = 
          try !(find_dircontent dircontent aa) 
          with Not_found -> raise Exn1
        in
        (match dc with
          Dir dircontentref2 -> search_page_table !dircontentref2 l
        | File page_table_ref -> page_table_ref, l)
      with Exn1 -> 
        (match !(find_dircontent dircontent eliom_suffix_internal_name) with
          Dir _ -> raise Not_found
        | File page_table_ref -> 
            (page_table_ref, (if a = None then [""] else aa::l)))
    in function
        [] -> raise Ocsigen_Is_a_directory
      | [""] -> aux None []
      | ""::l -> search_page_table dircontent l
      | a::l -> aux (Some a) l
  in
  let page_table_ref, suffix = 
    try search_page_table !dircontentref (change_empty_list ri.ri_path)
    with Not_found -> raise Ocsigen_404
  in
  find_page_table 
    now
    page_table_ref
    tables
    session_table_ref
    session_exp_info
    ri
    suffix
    {state = si.si_state_info}
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
let (set_default_timeout, set_default_persistent_timeout, 
     get_default_timeout, get_default_persistent_timeout) =
  let t = ref (Some 3600.) in (* 1 hour by default *)
  let persistent_t = ref (Some 86400.) in (* 1 day by default *)
  ((fun timeout -> t := timeout),
   (fun timeout -> persistent_t := timeout),
   (fun () -> !t),
   (fun () -> !persistent_t))

type timeout_table =
    Tt of ((float option option * float option option) * 
             (string * timeout_table) list)

let (find_global_timeout, find_global_persistent_timeout, 
     set_global_timeout, set_global_persistent_timeout) =
  let table = ref (Tt ((None, None), [])) in
  let rec find = function
      (Tt (t, _), []) -> t
    | (ta, ""::l) -> find (ta, l)
    | (Tt (t, r), a::l) ->
        try
          find ((List.assoc a r), l)
        with Not_found -> (None, None)
  in
  let rec add fstsnd timeout = function
      (Tt (t, l), []) -> Tt ((fstsnd t (Some timeout)), l)
    | (ta, ""::l) -> add fstsnd timeout (ta, l)
    | (Tt (t, r), a::l) ->
        try
          let (Tt (tt, ll), rr) = list_assoc_remove a r in
          Tt (t, (a, add fstsnd timeout (Tt (tt, ll), l))::rr)
        with Not_found -> 
          Tt (t, (a, (add fstsnd timeout (Tt ((None, None), []), l)))::r)
  in
  let set_aux fstsnd working_dir s =
     table := add fstsnd s (!table, working_dir)
  in
  ((fun wd -> match fst (find (!table, wd)) with
  | None -> get_default_timeout ()
  | Some t -> t),
   (fun wd -> match snd (find (!table, wd)) with
   | None -> get_default_persistent_timeout ()
   | Some t -> t),
   (fun wd s -> set_aux (fun (a,b) t -> (t,b)) wd s),
   (fun wd s -> set_aux (fun (a,b) t -> (a,t)) wd s))


(*****************************************************************************)
(** Persistent sessions: *)
module Perstables = 
  struct 
    let empty = []
    let add v t = v::t
    let fold = List.fold_left
  end

let perstables = ref Perstables.empty

let create_persistent_table name =
  perstables := Perstables.add name !perstables;
  Ocsipersist.open_table name

let persistent_cookies_table = 
  create_persistent_table eliom_persistent_cookie_table
(* the table contains:
   - the expiration date (by timeout), changed at each access to the table
     (float option) None -> no expiration
   - the timeout for the user (float option option) None -> see global config
     Some None -> no timeout
   - a randomly generated key, to be sure that it is not an old cookie that
   has been reused
 *)

(** removes the entry from all opened tables *)
let remove_from_all_persistent_tables key =
  Perstables.fold
    (fun thr t -> thr >>= 
      (fun () -> Ocsipersist.remove (Ocsipersist.open_table t) key))
    (return ())
    !perstables

let number_of_persistent_tables () =
  List.length !perstables

let number_of_persistent_table_elements () = 
  List.fold_left 
    (fun thr t -> 
      thr >>= 
      (fun l -> Ocsipersist.length (Ocsipersist.open_table t) >>=
        (fun e -> return ((t, e)::l)))) (return []) !perstables

let rec new_persistent_cookie ((_, _, (working_dir, _, _, _, _)) as sp) = 
  let c = Int64.to_string (Random.int64 Int64.max_int) in
  catch
    (fun () ->
      Ocsipersist.find persistent_cookies_table c >>=
      (fun _ -> new_persistent_cookie sp))
    (function
      | Not_found -> 
          begin
            let randomkey = Random.int64 Int64.max_int in
            Ocsipersist.add persistent_cookies_table c 
              ((match find_global_persistent_timeout working_dir with
              | None -> None
              | Some t -> Some (t +. Unix.time ())),
               None,
               randomkey,
               working_dir
        (* only to put expiration date on the right working dir
           To be removed if the management of cookies paths is improved *)
              ) >>=
            (fun () -> return (c, randomkey))
          end
      | e -> fail e)

let create_persistent_cookie ((_, si, _) as sp) = 
  match !(si.si_persistent_cookie) with
  | None -> 
      new_persistent_cookie sp >>=
      (fun c ->
        si.si_persistent_cookie := Some c;
        return c)
  | Some c -> return c

let create_cookie (_,si,(_,(_, cookie_table, _), _,_,_)) = 
  match !(si.si_cookie) with
  | None -> 
      let c = new_cookie cookie_table in
      si.si_cookie := Some c;
      c
  | Some c -> c



(*****************************************************************************)
(** session data *)

let counttableelements = ref []
(* Here only for exploration functions *)

let create_table, create_table_during_session =
  let aux remove_session_data =
    let t = Cookies.create 100 in
    let old_remove_session_data = !remove_session_data in
    remove_session_data := 
      (fun cookie ->
        old_remove_session_data cookie;
        Cookies.remove t cookie
      );
    counttableelements := 
      (fun () -> Cookies.length t)::!counttableelements;
    t
  in
  ((fun () ->
    let (_,_, remove_session_data), _ = get_current_hostdir () in
    aux remove_session_data),
   (fun (_,_,(_,(_,_, remove_session_data),_,_,_)) ->
     aux remove_session_data))

let remove_session_data remove_session_data =
  function
    | None -> ()
    | Some cookie -> !remove_session_data cookie

let remove_session (_, si, (_,(_,cook,rem),_,_,_)) = 
  remove_session_data rem !(si.si_cookie);
  remove_session_table cook !(si.si_cookie)



(*****************************************************************************)
(** Parsing global configuration for Eliommod: *)
open Simplexmlparser

let sessiongcfrequency = ref (Some 3600.)
let persistentsessiongcfrequency = ref (Some 86400.)
let set_sessiongcfrequency i = sessiongcfrequency := i
let get_sessiongcfrequency () = !sessiongcfrequency
let set_persistentsessiongcfrequency i = persistentsessiongcfrequency := i
let get_persistentsessiongcfrequency () = !persistentsessiongcfrequency

let rec parse_global_config = function
      [] -> ()
    | (Element ("timeout", [("value", s)], []))::ll -> 
              (try
                set_default_timeout (Some (float_of_string s))
              with Failure _ -> 
                if (s = "infinity")
                then set_default_timeout None
                else
                  raise (Error_in_config_file "Eliom: Wrong value for value attribute of <timeout> tag"));
              parse_global_config ll
    | (Element ("sessiongcfrequency", [("value", s)], p))::ll ->
          (try
            set_sessiongcfrequency (Some (float_of_string s))
          with Failure _ -> 
            if s = "infinity"
            then set_sessiongcfrequency None
            else raise (Error_in_config_file
                          "Eliom: Wrong value for <sessiongcfrequency>"));
          parse_global_config ll
    | (Element ("persistentsessiongcfrequency", 
                    [("value", s)], p))::ll ->
          (try
            set_persistentsessiongcfrequency (Some (float_of_string s))
          with Failure _ -> 
            if s = "infinity"
            then set_persistentsessiongcfrequency None
            else raise (Error_in_config_file
                          "Eliom: Wrong value for <persistentsessiongcfrequency>"));
          parse_global_config ll
    | (Element (tag,_,_))::ll -> 
        parse_global_config ll
    | _ -> raise (Error_in_config_file ("Unexpected content inside eliom config"))


let _ = parse_global_config (Extensions.get_config ())

(*****************************************************************************)
(* Generation of the page or naservice                                          *)

let execute
    generate_page info old_cookie old_persistent_cookie
    ((_, cookie_table, remove_session_data) as tables) =
  
  let now = Unix.time () in
  
  (match !old_persistent_cookie with
    None -> return (None, []) (* By default, global persistent timeout *)
  | Some (c, _) -> 
      catch
        (fun () ->
          Ocsipersist.find persistent_cookies_table c >>=
          fun (persexp, perstimeout, persrandomkey, oldperscookpath) ->
            match persexp with
              Some t when t < now -> 
                (* session expired by timeout *)
                remove_from_all_persistent_tables c >>=
                (fun () -> fail Eliom_Persistent_session_expired)
            | _ -> 
                old_persistent_cookie := Some (c, persrandomkey);
                return (perstimeout, oldperscookpath))
        (fun _ -> fail Eliom_Persistent_session_expired)
        (* ?? If an error occurs with Ocsipersist, assume no data *)
        (* function
          | Not_found -> fail Eliom_Persistent_session_expired
          | e -> fail e*)
  ) >>=
  
  fun (user_persistent_timeout, oldperscookpath) ->
    (* oldperscookpath only to put expiration date on the right working dir
       To be removed if the management of cookies paths is improved *)
    
    try
      let (sessiontablesref, user_timeout_optref, oldcookiepath) = 
    (* oldcookiepath only to put expiration date on the right working dir
       To be removed if the management of cookies paths is improved *)
        (match old_cookie with
          None -> ((ref (new_session_tables ())), ref None, [])
        | Some c -> 
            try 
              let ta, cookiepath, exp, timeout_optref = 
                Cookies.find cookie_table c in
              match exp with
                Some t when t < now -> 
                  (* session expired by timeout *)
                  !remove_session_data c;
                  Cookies.remove cookie_table c;
                  raise Eliom_Session_expired
              | _ -> ((ref ta), timeout_optref, cookiepath)
(*        with Not_found -> ((ref (new_session_tables ())), None, ref None)) *)
            with Not_found -> raise Eliom_Session_expired) 
      in
      
      let cookie_exp_date = ref None in
      let persistent_cookie_exp_date = ref None in
      let user_persistent_timeout_ref = ref user_persistent_timeout in
      generate_page
        now info tables sessiontablesref 
        (user_timeout_optref, cookie_exp_date, 
         user_persistent_timeout_ref, persistent_cookie_exp_date) >>=
      (fun (result, working_dir, the_new_cookie, new_persistent_cookie) ->

        (* Update persistent expiration date (and user timeout) *)
        (catch
           (fun () ->
             match new_persistent_cookie with
             | None -> return []
             | Some (pc, randomkey) ->
                 let newperscookiepath =
                   (if new_persistent_cookie = !old_persistent_cookie
                   then oldperscookpath
                   else working_dir)
                 in
                 (if (!user_persistent_timeout_ref = 
                      user_persistent_timeout) &&
                   ((user_persistent_timeout = Some None) ||
                   ((user_persistent_timeout = None) && 
                    ((find_global_persistent_timeout working_dir) = 
                     (None : float option))))
                 then return () (* Nothing to do *)
                 else begin
                   Ocsipersist.add persistent_cookies_table pc
                     ((match !user_persistent_timeout_ref with
                     | None -> 
                         (match
                           find_global_persistent_timeout newperscookiepath 
                         with
                         | None -> None
                         | Some t -> Some (t +. now))
                     | Some None -> None
                     | Some (Some t) -> Some (t +. now)), 
                      !user_persistent_timeout_ref,
                      randomkey,
                      newperscookiepath)
                 end) >>= (fun () -> return newperscookiepath))
           (fun _ -> return [])
        (* ?? If an error occurs with Ocsipersist, continue *)
           ) >>=

        (fun newperscookiepath ->
          let cookie2 = 
            if are_empty_tables !sessiontablesref
            then the_new_cookie
            else 
              (match the_new_cookie with
              | None -> Some (new_cookie cookie_table)
              | Some _ -> the_new_cookie)
          in
          let newcookiepath =
            (if cookie2 = old_cookie 
            then oldcookiepath
            else working_dir)
          in
          (match cookie2 with
          | None -> ()
          | Some c ->
              let timeout = match !user_timeout_optref with
              | Some t -> t
              | None -> find_global_timeout newcookiepath
              in
              let exp = match timeout with
              | None -> None
              | Some t -> Some (t +. now)
              in
              Cookies.replace
                cookie_table c
                (!sessiontablesref, newcookiepath, exp, user_timeout_optref));
              (* If the key does not exist, replace just adds it.
                 If it exists, we put a new expiration date *)
          let cookie3 = 
            if cookie2 <> old_cookie || !cookie_exp_date <> None then 
              (if cookie2 = None 
              then ((Some ""), (Some 0.), newcookiepath)
              else (cookie2, !cookie_exp_date, newcookiepath))
            else (None, None, [])
          in return 
            (cookie3, 
             (new_persistent_cookie, 
              !persistent_cookie_exp_date,
              newperscookiepath),
             result,
             working_dir)))
  with e -> fail e

exception Eliom_retry_with of 
  (request_info * 
     sess_info * 
     cookieslist (* cookies to set *)) 

let get_page
    now (ri, si, cookies_to_set)
    ((global_tables, _, _) as tables)
    session_tables_ref
    session_exp_info
    =
  ((catch
      (fun () -> 
        Messages.debug 
          ("-- I'm looking for "^(string_of_url_path ri.ri_path)^
           " in the session table:");
        (find_service
           now
           !session_tables_ref
           (tables,
            session_tables_ref,
            session_exp_info,
            ri,
            si)))
      (function 
          Ocsigen_404 | Eliom_Wrong_parameter -> 
            catch (* ensuite dans la table globale *)
              (fun () -> 
                Messages.debug "-- I'm searching in the global table:";
                (find_service 
                   now
                   global_tables
                   (tables,
                    session_tables_ref,
                    session_exp_info,
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
                                  si_exn= Eliom_Link_too_old::si.si_exn
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
                                  si_exn= Eliom_Link_too_old::si.si_exn
                                },
                                 cookies_to_set (* no new cookie *))))
                | e -> fail e)
        | e -> fail e)) >>= (fun r -> return r))


let make_naservice
    now (ri, si, cookies_to_set)
    ((global_tables, _, _) as tables) session_tables_ref session_exp_info =
  (try
    try
      return (find_naservice now !session_tables_ref si.si_nonatt_info)
    with
      Not_found -> return 
          (find_naservice now global_tables si.si_nonatt_info)
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
                    si_exn= Eliom_Link_too_old::si.si_exn
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
            si.si_config_file_charset
            >>=
          (fun (ri,si,_) -> 
            fail (Eliom_retry_with 
                    (ri,
                     {si with
                      si_exn= Eliom_Link_too_old::si.si_exn
                    },
                     cookies_to_set (* no new cookie *))))
  ) >>=
  (fun (max_use, expdate, naservice, working_dir) ->
    (naservice
       (make_server_params 
          working_dir
          tables
          session_tables_ref 
          session_exp_info
          ri
          []
          si)) >>=
    (fun r -> 
      Messages.debug
        "- Non attached page found and generated successfully";
      (match expdate with
      | Some (timeout, e) -> e := timeout +. now
      | None -> ());
      (match max_use with
        None -> ()
      | Some r -> 
          if !r = 1
          then remove_naservice !session_tables_ref si.si_nonatt_info
          else r := !r - 1);
      return (r, working_dir, 
              !(si.si_cookie), !(si.si_persistent_cookie))))


let rec cookies_remove c = function
  | [] -> []
  | (Set (p, e, l))::ll -> 
      (match List.remove_assoc c l with
      | [] -> cookies_remove c ll
      | l -> (Set (p, e, l))::(cookies_remove c ll))
  | (Unset (p, l))::ll -> 
      match list_remove c l with
      | [] -> cookies_remove c ll
      | l -> (Unset (p, l))::(cookies_remove c ll)


let gen page_tree charset ri =
  let rec gen_aux
      close_session close_persistent_session
      ((ri, si, old_cookies_to_set) as info) =
    let genfun = 
      match si.si_nonatt_info with
	None, None ->
          
          (* page generation *)
          get_page
            
      | _ ->
          
          (* anonymous service *)
          make_naservice
    in
    
    catch 
      (fun () ->
        execute genfun
          info !(si.si_cookie) si.si_persistent_cookie page_tree >>=
	fun ((new_cookie, cookie_exp, newcookiepath),
             (new_persistent_cookie, 
              persistent_cookie_exp, 
              newperscookiepath), 
             result_to_send,
             working_dir) ->
          
          let compute_cookies cookies_set_by_page =
            let cookies_set_by_page =
              List.map 
                (function
                  | Set (pathopt, expopt, cl) -> 
                      Set ((match pathopt with
                        None -> Some working_dir (* Not possible to set a cookie for another site (?) *)
                      | Some p -> Some (working_dir@p)
                           ), expopt, cl)
                  | Unset (pathopt, cl) -> 
                      Unset ((match pathopt with
                        None -> Some working_dir (* Not possible to set a cookie for another site (?) *)
                      | Some p -> Some (working_dir@p)
                           ), cl)
                )
                cookies_set_by_page
            in
            let cookies_to_set = cookies_set_by_page@old_cookies_to_set in
            let all_new_cookies =
              match new_cookie with
              | None -> 
                  if close_session
                  then (Unset ((Some working_dir(*?????*)), [cookiename]))::cookies_to_set
                  else cookies_to_set
              | Some c -> Set (Some newcookiepath, cookie_exp, 
                               [(cookiename, c)])::
                  (cookies_remove cookiename cookies_to_set)
            in
            let all_new_cookies =
              match new_persistent_cookie with
              | None -> 
                  if close_persistent_session
                  then (Unset ((Some working_dir(*??????*)), 
                               [persistentcookiename]))::
                    all_new_cookies
                  else all_new_cookies
              | Some (c, _) -> Set (Some newperscookiepath, 
                                    persistent_cookie_exp, 
                               [(persistentcookiename, c)])::
                  (cookies_remove persistentcookiename all_new_cookies)
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
                         (let now = Unix.time () in
                         let cookies_presents =
                           List.fold_left
                             (fun l c ->
                               match c with
                                 Set (p, exp, cl) -> 
                                   (match exp with
                                   | Some t when t < now -> l
                                   | _ -> (match p with
                                     | None -> cl@l
                                     | Some p 
                                       when list_is_prefix p ri.ri_path -> 
                                         cl@l
                                     | _ -> l
                                      ))
                               | Unset (p, cl) ->  
                                   match p with
                                     Some p ->
                                       if list_is_prefix p ri.ri_path
                                       then 
                                         List.fold_left
                                           (fun lll ccc -> 
                                             List.remove_assoc ccc lll)
                                           l
                                           cl
                                       else l
                                   | _ -> List.fold_left
                                         (fun lll ccc -> 
                                           List.remove_assoc ccc lll)
                                         l
                                         cl
                             )
                             (force ri.ri_cookies)
                             cookies_set_by_page
                         in
                         ({ri with
                           ri_post_params = lazy (return []);
                           ri_cookies= lazy
                             (let l =
                               match new_cookie, cookie_exp with
                               | None, _ -> cookies_presents
                               | _, Some d when d < now -> 
                                   List.remove_assoc
                                     cookiename cookies_presents
                               | Some c, _ -> 
                                   ((cookiename, c)::cookies_presents)
                             in
                               match new_persistent_cookie, 
                                 persistent_cookie_exp with
                               | None, _ -> l
                               | _, Some d when d < now -> 
                                   List.remove_assoc persistentcookiename l
                               | Some (c, _), _ -> 
                                   ((persistentcookiename, c)::l)
                             )
                         },
                          {si_other_get_params= si.si_other_get_params;
                           si_cookie= (match new_cookie, cookie_exp with
                           | Some c, Some d when d < now -> ref None
                           | Some c, _ -> ref new_cookie
                           | None, _ -> si.si_cookie);
                           si_persistent_cookie= 
                           (match new_persistent_cookie, 
                             persistent_cookie_exp with
                           | Some c, Some d when d < now -> ref None
                           | Some c, _ -> ref new_persistent_cookie
                           | None, _ -> si.si_persistent_cookie);
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
	| Eliom_Session_expired -> 
            gen_aux 
              true
              false
              (ri, 
               {si with 
                si_cookie= ref None; 
                (* si_persistent_cookie inchangé *)
                si_exn= Eliom_Session_expired::si.si_exn}, 
               old_cookies_to_set)
	| Eliom_Persistent_session_expired -> 
            gen_aux
              false
              true
              (ri, 
               {si with 
                si_persistent_cookie= ref None; 
                (* si_cookie inchangé *)
                si_exn= Eliom_Persistent_session_expired::si.si_exn}, 
               old_cookies_to_set)
        | Eliom_retry_with a -> gen_aux false false a
	| e -> fail e)

  in
  change_request_info ri charset >>= gen_aux false false


(*****************************************************************************)
(* garbage collection of timeouted sessions *)
let rec gc_timeouted_services now t = 
  let rec aux k direltr thr = 
    thr >>=
    (fun table ->
      match !direltr with
      | Dir r -> gc_timeouted_services now r >>= 
          (fun () -> match !r with
          | Vide -> return (String_Table.remove k table)
          | Table t -> return table)
      | File ptr ->
          List.fold_right
            (fun (ptk, l) foll -> 
              foll >>=
              (fun foll ->
                let newl =
                  List.fold_right
                    (fun ((i, (_, (_, expdate, _, _))) as a) foll -> 
                      match expdate with
                      | Some (_, e) when !e < now -> foll
                      | _ -> a::foll
                    )
                    l
                    []
                in
                Lwt_unix.yield () >>=
                (fun () ->
                  match newl with
                  | [] -> return foll
                  | _ -> return ((ptk, newl)::foll))
              )
            )
            !ptr
            (return []) >>=
          (function
            | [] -> return (String_Table.remove k table)
            | r -> ptr := r; return table)
    )
  in
  match !t with
  | Vide -> return ()
  | Table r -> (String_Table.fold aux r (return r)) >>=
      (fun table -> 
        if String_Table.is_empty table
        then begin t := Vide; return () end
        else begin t := Table table; return () end)

let gc_timeouted_naservices now tr = 
  match !tr with
  | AVide -> return ()
  | ATable t -> 
      NAserv_Table.fold
        (fun k (_, expdate,_,_) thr -> 
          thr >>=
          (fun table -> 
            Lwt_unix.yield () >>=
            (fun () ->
              match expdate with
              | Some (_, e) when !e < now -> 
                  return (NAserv_Table.remove k table)
              | _ -> return table)
          ))
        t
        (return t) >>=
      (fun t -> 
        if NAserv_Table.is_empty t
        then tr := AVide
        else tr := ATable t; 
        return ())

        

(* This is a thread that will work for example every hour *)
let session_gc ((servicetable,
                 naservicetable, 
                 contains_services_with_timeout, 
                 contains_naservices_with_timeout), 
                cookie_table, rem) =
  match get_sessiongcfrequency () with
    None -> () (* No garbage collection *)
  | Some t ->
      let rec f () = 
        Lwt_unix.sleep t >>= 
        (fun () ->
          let now = Unix.time () in
          Messages.debug "GC of sessions";
          (if !contains_services_with_timeout
          then gc_timeouted_services now servicetable
          else return ()) >>=
          (fun () -> if !contains_naservices_with_timeout
          then gc_timeouted_naservices now naservicetable
          else return ()) >>=
          (fun () ->
            Cookies.fold
              (fun k (((servicetable,
                       naservicetable, 
                       contains_services_with_timeout, 
                       contains_naservices_with_timeout) as tables), 
                      _, exp, _) thr -> 
                         thr >>=
                         (fun () ->
                           (match exp with
                           | Some exp when exp < now -> 
                               Cookies.remove cookie_table k;
                               !rem k;
                               return ()
                           | _ -> 
                               (if !contains_services_with_timeout
                               then gc_timeouted_services now servicetable
                               else return ()) >>=
                               (fun () -> if !contains_naservices_with_timeout
                               then gc_timeouted_naservices now naservicetable
                               else return ()) >>=
                               (fun () ->
                                 if are_empty_tables tables
                                 then begin
                                   Cookies.remove cookie_table k;
                                   !rem k;
                                   return ()
                                 end
                                 else return ()
                               )
                           ) >>=
                           Lwt_unix.yield
                         )
              )
              cookie_table
              (return ())))
          >>=
        f
      in ignore (f ())
      
(* garbage collection of timeouted persistent sessions *)
(* This is a thread that will work every hour/day *)
let persistent_session_gc () =
  match get_persistentsessiongcfrequency () with
    None -> () (* No garbage collection *)
  | Some t ->
      let rec f () = 
        Lwt_unix.sleep t >>= 
        (fun () ->
          let now = Unix.time () in
          Messages.debug "GC of persistent sessions";
          (Ocsipersist.iter_table
             (fun k (exp, _, _, _) -> 
               (match exp with
               | Some exp when exp < now -> 
                   remove_from_all_persistent_tables k
               | _ -> return ())
             )
             persistent_cookies_table))
          >>=
        f
      in ignore (f ())
      


(*****************************************************************************)
(** Module loading *)
let config = ref []

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
  config := []



(*****************************************************************************)
(** Parsing of config file for each site: *)
let parse_config page_tree path = 
  let rec parse_module_attrs file = function
    | [] -> (match file with
        None -> 
          raise (Error_in_config_file
                   ("Missing module attribute in <eliom>"))
      | Some s -> s)
    | ("module", s)::suite ->
        (match file with
          None -> parse_module_attrs (Some s) suite
        | _ -> raise (Error_in_config_file
                        ("Duplicate attribute file in <eliom>")))
    | (s, _)::_ ->
        raise
          (Error_in_config_file ("Wrong attribute for <eliom>: "^s))
  in function
      Element ("eliom", atts, content) -> 
          let file = parse_module_attrs None atts in
          load_eliom_module page_tree path file content
    | Element (t, _, _) -> 
        raise (Extensions.Bad_config_tag_for_extension t)
    | _ -> raise (Error_in_config_file "(Eliommod extension)")


(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase *)
let start_init () =
  begin_current_host_dir ()

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  end_current_hostdir ();
  verify_all_registered ()                                

(** Function that will handle exceptions during the initialisation phase *)
let handle_init_exn = function
  Eliom_duplicate_registration s -> 
    ("Fatal - Eliom: Duplicate registration of url \""^s^
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
(** table of page trees *)

let page_tree_table = ref []

let find k = List.assoc k !page_tree_table

let add k a = page_tree_table := (k,a)::!page_tree_table

(*****************************************************************************)
(** extension registration *)
let _ = register_extension
    ((fun hostpattern -> 
      let page_tree = 
        try 
          find hostpattern
        with Not_found ->
          let n = new_pages_tree () in
          add hostpattern n;
          session_gc n;
          n
      in
      (gen page_tree, 
       parse_config page_tree)),
     start_init,
     end_init,
     handle_init_exn)

let _ = persistent_session_gc ()


(*****************************************************************************)
(* Exploration *)

let number_of_sessions (_,_,(_,(_, cookie_table, _),_,_,_)) = 
  Cookies.length cookie_table

let number_of_tables () =
  List.length !counttableelements

let number_of_table_elements () =
  List.map (fun f -> f ()) !counttableelements

let number_of_persistent_sessions () = 
  Ocsipersist.length persistent_cookies_table
