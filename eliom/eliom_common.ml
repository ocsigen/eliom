(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_sessions.ml
 * Copyright (C) 2007 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(** Session management                                                     *)

(********)

type na_key =
  | Na_no
  | Na_get_ of string (* service *)
  | Na_post_ of string (* service *)
  | Na_get' of string (* coservice *)
  | Na_post' of string (* coservice *)


exception Eliom_Wrong_parameter (** Service called with wrong parameter names *)
exception Eliom_Link_too_old (** The coservice does not exist any more *)
exception Eliom_Session_expired
exception Eliom_Service_session_expired of (string list)
    (** The service session cookies does not exist any more.
        The string lists are the list of names of expired sessions
     *)
exception Eliom_Typing_Error of (string * exn) list

exception Eliom_duplicate_registration of string
exception Eliom_there_are_unregistered_services of
  (string list * string list list * na_key list)
exception Eliom_function_forbidden_outside_site_loading of string
exception Eliom_page_erasing of string
exception Eliom_error_while_loading_site of string

exception Eliom_404



(*****************************************************************************)
let defaultpagename = "./" 
(* should be "" but this does not work with firefox.
   "index" works but one page may have two different URLs *)

let eliom_suffix_name = "__eliom_suffix"
let eliom_suffix_internal_name = "__eliom_suffix**"
let naservice_num = "__eliom_na__num"
let naservice_name = "__eliom_na__name"
let get_state_param_name = "__eliom__"
let post_state_param_name = "__eliom_p__"
let datacookiename = "eliomdatasession|" 
let servicecookiename = "eliomservicesession|" 
(* must not be a prefix of the following and vice versa (idem for data) *)
let persistentcookiename = "eliompersistentsession|"
let co_param_prefix = "__co_eliom_"
let na_co_param_prefix = "__na_eliom_"

(*VVV Do not forget to change the version number 
  when the internal format change!!! *)
let persistent_cookie_table_version = "_v2" (* v2 introduces session groups *)
let eliom_persistent_cookie_table = 
  "eliom_persist_cookies"^persistent_cookie_table_version

(*****************************************************************************)

(** state is a parameter to differenciate coservices
    (several instances of the same URL).
 *)
type internal_state = string

(** Type used for cookies to set. 
    The float option is the timestamp for the expiration date.
    The strings are names and values.
 *)
type cookie = 
  | Set of Extensions.url_path option * float option * string * string
  | Unset of Extensions.url_path option * string



type sess_info =
    {si_other_get_params: (string * string) list;
     si_all_get_params: (string * string) list;
     si_all_post_params: (string * string) list;

     si_service_session_cookies: string Http_frame.Cookievalues.t;
     (* the session service cookies sent by the request *)
     (* the key is the cookie name (or site dir) *)

     si_data_session_cookies: string Http_frame.Cookievalues.t;
     (* the session data cookies sent by the request *)
     (* the key is the cookie name (or site dir) *)

     si_persistent_session_cookies: string Http_frame.Cookievalues.t;
     (* the persistent session cookies sent by the request *)
     (* the key is the cookie name (or site dir) *)

     si_nonatt_info: na_key;
     si_state_info: (internal_state option * internal_state option);
     si_config_file_charset: string;
     si_previous_extension_error: int;
     (* HTTP error code sent by previous extension (default: 404) *)
   }

(* The table of tables for each session. Keys are cookies *)
module SessionCookies = Hashtbl.Make(struct 
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
end)

type 'a session_cookie =
  | SCNo_data
  | SCData_session_expired
  | SC of 'a

type cookie_exp =
  | CENothing   (** nothing to set *)
  | CEBrowser   (** expires at browser close *)
  | CESome of float (** expiration date *)

type timeout = 
  | TGlobal (** see global setting *)
  | TNone   (** explicitely set no timeout *)
  | TSome of float (** timeout duration in seconds *)



(* cookies information during page generation: *)

type 'a one_service_cookie_info =
    (* service sessions: *)
    {sc_value:string             (* current value *);
     sc_table:'a ref             (* service session table
                                    ref towards cookie table
                                  *);
     sc_timeout:timeout ref      (* user timeout - 
                                    ref towards cookie table
                                  *);
     sc_exp:float option ref     (* expiration date ref
                                    (server side) - 
                                    None = never
                                    ref towards cookie table
                                  *);
     sc_cookie_exp:cookie_exp ref (* cookie expiration date to set *);
     sc_session_group:Eliommod_sessiongroups.sessgrp option ref (* session group *)
   }


type one_data_cookie_info =
    (* in memory data sessions: *)
    {dc_value:string                    (* current value *);
     dc_timeout:timeout ref             (* user timeout - 
                                           ref towards cookie table
                                         *);
     dc_exp:float option ref            (* expiration date ref (server side) - 
                                           None = never
                                           ref towards cookie table
                                         *);
     dc_cookie_exp:cookie_exp ref       (* cookie expiration date to set *);
     dc_session_group: Eliommod_sessiongroups.sessgrp option ref (* session group *)
   }

type one_persistent_cookie_info =
     {pc_value:string                    (* current value *);
      pc_timeout:timeout ref             (* user timeout *); 
      pc_cookie_exp:cookie_exp ref       (* cookie expiration date to set *);
      pc_session_group:Eliommod_sessiongroups.perssessgrp option ref (* session group *)
    }


(*VVV heavy *)
type 'a cookie_info =
    (* service sessions: *)
    (string option            (* value sent by the browser *)
                              (* None = new cookie 
                                 (not sent by the browser) *)
       *
       
       'a one_service_cookie_info session_cookie ref
       (* SCNo_data = the session has been closed
          SCData_session_expired = the cookie has not been found in the table.
          For both of them, ask the browser to remove the cookie.
        *)
    )
      (* This one is not lazy because we must check all service sessions
         at each request to find the services *)
      Http_frame.Cookievalues.t ref (* The key is the full session name *) *
      
    (* in memory data sessions: *)
      (string option            (* value sent by the browser *)
                                (* None = new cookie 
                                   (not sent by the browser) *)
         *
         
         one_data_cookie_info session_cookie ref
         (* SCNo_data = the session has been closed
            SCData_session_expired = the cookie has not been found in the table.
            For both of them, ask the browser to remove the cookie.
          *)
      ) Lazy.t
      (* Lazy because we do not want to ask the browser to unset the cookie 
         if the cookie has not been used, otherwise it is impossible to 
         write a message "Your session has expired" *)
      Http_frame.Cookievalues.t ref (* The key is the full session name *) *
      
      (* persistent sessions: *)
      ((string                  (* value sent by the browser *) *
        timeout                 (* timeout at the beginning of the request *) *
        float option            (* (server side) expdate 
                                   at the beginning of the request
                                   None = no exp *) *
        Eliommod_sessiongroups.perssessgrp option      (* session group at beginning of request *))
         option
                                (* None = new cookie 
                                   (not sent by the browser) *)
         *
         
         one_persistent_cookie_info session_cookie ref
         (* SCNo_data = the session has been closed
            SCData_session_expired = the cookie has not been found in the table.
            For both of them, ask the browser to remove the cookie.
          *)
      ) Lwt.t Lazy.t
      Http_frame.Cookievalues.t ref


(* non persistent cookies for services *)
type 'a servicecookiestablecontent =
    (string              (* session fullsessname *) *
     'a                  (* session table *) * 
     float option ref    (* expiration date by timeout 
                            (server side) *) *
     timeout ref         (* user timeout *) *
     Eliommod_sessiongroups.sessgrp option ref   (* session group *))

type 'a servicecookiestable = 'a servicecookiestablecontent SessionCookies.t
(* the table contains:
   - the table of services
   - the expiration date (by timeout), changed at each access to the table
     (float option) None -> no expiration
   - the timeout for the user (float option option) None -> see global config
     Some None -> no timeout
   - the group to which belongs the session
 *)

(* non persistent cookies for in memory data *)
type datacookiestablecontent = 
    (string                  (* session fullsessname *) *
     float option ref        (* expiration date by timeout 
                                (server side) *) *
     timeout ref             (* user timeout *) *
     Eliommod_sessiongroups.sessgrp option ref   (* session group *))

type datacookiestable = datacookiestablecontent SessionCookies.t




(*****************************************************************************)
type result_to_send = 
  | EliomResult of Http_frame.result
  | EliomExn of (exn list * cookie list)

(*****************************************************************************)


type page_table_key =
    {key_state: (internal_state option * internal_state option);
     key_kind: Http_frame.Http_header.http_method}
      (* action: server_params -> page *)

      (* module Page_Table = Map.Make(struct type t = page_table_key 
         let compare = compare end) *)

module String_Table = Map.Make(struct 
  type t = string
  let compare = compare 
end)

module NAserv_Table = Map.Make(struct 
  type t = na_key
  let compare = compare 
end)

type anon_params_type = int

type server_params = 
    {sp_ri:Extensions.request_info;
     sp_si:sess_info;
     sp_sitedata:sitedata (* data for the whole site *);
     sp_cookie_info:tables cookie_info;
     sp_suffix:Extensions.url_path (* suffix *);
     sp_fullsessname:string option (* the name of the session
                                      to which belong the service
                                      that answered
                                      (if it is a session service) *)}

and page_table = 
    (page_table_key * 
       (((anon_params_type * anon_params_type) (* unique_id *) * 
           (int * (* generation (= number of reloads of sites
                     after which that service has been created) *)
              (int ref option (* max_use *) *
                 (float * float ref) option
                 (* timeout and expiration date for the service *) *
                 (server_params -> result_to_send Lwt.t)
	      ))) list)) list
       (* Here, the url_path is the site directory.
          That is, the directory in which we are when we register
          dynamically the pages.
          Each time we load a page, we change to this directory
          (in case the page registers new pages).
        *)

and naservice_table = 
  | AVide 
  | ATable of 
      (int (* generation (= number of reloads of sites
              after which that service has been created) *) *
       int ref option (* max_use *) *
         (float * float ref) option (* timeout and expiration date *) *
         (server_params -> result_to_send Lwt.t)
      )
        NAserv_Table.t

and dircontent = 
  | Vide
  | Table of direlt ref String_Table.t

and direlt = 
  | Dir of dircontent ref
  | File of page_table ref

and tables = 
    dircontent ref * 
    naservice_table ref *
    (* Information for the GC: *)
    bool ref (* true if dircontent contains services with timeout *) *
    bool ref (* true if naservice_table contains services with timeout *)

and sitedata =
  {site_dir: Extensions.url_path;
   site_dir_string: string;
   mutable servtimeout: (string * float option) list;
   mutable datatimeout: (string * float option) list;
   mutable perstimeout: (string * float option) list;
   global_services: tables; (* global service table *)
   session_services: tables servicecookiestable; (* cookie table for services *)
   session_data: datacookiestable; (* cookie table for in memory session data *)
   mutable remove_session_data: string -> unit;
   mutable not_bound_in_data_tables: string -> bool;
   mutable exn_handler: server_params -> exn -> result_to_send Lwt.t;
   mutable unregistered_services: Extensions.url_path list;
   mutable unregistered_na_services: na_key list;
   mutable max_volatile_data_sessions_per_group: int option;
   mutable max_service_sessions_per_group: int option;
   mutable max_persistent_data_sessions_per_group: int option;
 }


(*****************************************************************************)
    (** Create server parameters record *)
let make_server_params sitedata all_cookie_info ri suffix si fullsessname
    : server_params =
  {sp_ri=ri;
   sp_si=si;
   sp_sitedata=sitedata;
   sp_cookie_info=all_cookie_info;
   sp_suffix=suffix;
   sp_fullsessname= fullsessname}


(*****************************************************************************)
(*****************************************************************************)
(* The table of dynamic pages for each virtual server, and naservices        *)
(* Each node contains either a list of nodes (case directory)
    or a table of "answers" (functions that will generate the page) *)

let empty_page_table () = []
let empty_dircontent () = Vide
let empty_naservice_table () = AVide
    
let service_tables_are_empty (lr,atr,_,_) = 
  (!lr = Vide && !atr = AVide)

let empty_tables () =
  (ref (empty_dircontent ()), 
   ref (empty_naservice_table ()),
   ref false, (* does not contain services with timeout *)
   ref false (* does not contain na_services with timeout *))

let new_service_session_tables = empty_tables


(*****************************************************************************)
open Lwt 

(* Split parameter list, removing those whose name starts with pref *)
let split_prefix_param pref l =
  let len = String.length pref in
  List.partition (fun (n,_) -> 
    try 
      (String.sub n 0 len) = pref 
    with Invalid_argument _ -> false) l


let getcookies cookiename cookies = 
  let length = String.length cookiename in
  let last = length - 1 in
  Http_frame.Cookievalues.fold
    (fun name value beg -> 
      if Ocsigen_lib.string_first_diff cookiename name 0 last = length
      then
        Http_frame.Cookievalues.add
          (String.sub name length ((String.length name) - length))
          value
          beg
      else beg
    )
    cookies
    Http_frame.Cookievalues.empty


let change_request_info ri charset previous_extension_err =
  Lazy.force ri.Extensions.ri_post_params >>=
  (fun post_params -> 
    let get_params = Lazy.force ri.Extensions.ri_get_params in
    let get_params0 = get_params in
    let post_params0 = post_params in
    let data_cookies = getcookies datacookiename 
        (Lazy.force ri.Extensions.ri_cookies) 
    in
    let service_cookies = getcookies servicecookiename 
        (Lazy.force ri.Extensions.ri_cookies) 
    in
    let persistent_cookies = 
      getcookies persistentcookiename (Lazy.force ri.Extensions.ri_cookies)
    in
    let naservice_info, 
      (get_state, post_state),
      (get_params, other_get_params), 
      post_params =
      let post_naservice_name, na_post_params = 
        try
          let n, pp =
            Ocsigen_lib.list_assoc_remove naservice_num post_params
          in (Na_post' n, pp)
        with Not_found ->
          try
            let n, pp =
              Ocsigen_lib.list_assoc_remove naservice_name post_params
            in (Na_post_ n, pp)
          with Not_found -> (Na_no, [])
      in
      match post_naservice_name with
        | Na_post_ _
        | Na_post' _ -> (* POST non attached coservice *)
            (post_naservice_name,
             (None, None), 
             ([], get_params),
             na_post_params)
        | _ ->
            let get_naservice_name, (na_get_params, other_get_params) = 
              try
                let n, gp =
                  Ocsigen_lib.list_assoc_remove naservice_num get_params
                in (Na_get' n, 
                    (split_prefix_param na_co_param_prefix gp))
              with Not_found ->
                try
                  let n, gp =
                    Ocsigen_lib.list_assoc_remove naservice_name get_params
                  in (Na_get_ n, 
                      (split_prefix_param na_co_param_prefix gp))
                with Not_found -> (Na_no, ([], get_params))
            in
            match get_naservice_name with
              | Na_get_ _
              | Na_get' _ -> (* GET non attached coservice *)
                  (get_naservice_name,
                   (None, None), 
                   (na_get_params, other_get_params), 
                   [])
                    (* Not possible to have POST parameters 
                       without naservice_num
                       if there is a GET naservice_num
                    *)
              | _ ->
                  let post_state, post_params = 
                    try 
                      let s, pp =
                        Ocsigen_lib.list_assoc_remove
                          post_state_param_name post_params
                      in (Some s, pp)
                    with 
                        Not_found -> (None, post_params)
                  in
                  let get_state, (get_params, other_get_params) = 
                    try 
                      let s, gp =
                        Ocsigen_lib.list_assoc_remove
                          get_state_param_name get_params
                      in ((Some s), 
                          (split_prefix_param co_param_prefix gp))
                    with Not_found -> (None, (get_params, []))
                  in 
                  (Na_no, 
                   (get_state, post_state), 
                   (get_params, other_get_params), 
                   post_params)
    in

    return 
      ({ri with 
        Extensions.ri_method = 
        (if ri.Extensions.ri_method = Http_frame.Http_header.HEAD
        then Http_frame.Http_header.GET
        else ri.Extensions.ri_method);
        Extensions.ri_get_params = lazy get_params; 
        Extensions.ri_post_params = lazy (return post_params)},
       {si_service_session_cookies= service_cookies;
        si_data_session_cookies= data_cookies;
        si_persistent_session_cookies= persistent_cookies;
        si_nonatt_info= naservice_info;
        si_state_info= (get_state, post_state);
        si_other_get_params= other_get_params;
        si_all_get_params= get_params0;
        si_all_post_params= post_params0;
        si_config_file_charset= charset;
        si_previous_extension_error= previous_extension_err}))



type ('a, 'b) foundornot = Found of 'a | Notfound of 'b

(******************************************************************)
let make_full_cookie_name a b = a^b

let make_fullsessname ~sp = function
  | None -> sp.sp_sitedata.site_dir_string
  | Some s -> sp.sp_sitedata.site_dir_string^"|"^s
(* Warning: do not change this without modifying Eliomsessions.Admin *)

let make_fullsessname2 site_dir_string = function
  | None -> site_dir_string
  | Some s -> site_dir_string^"|"^s
(* Warning: do not change this without modifying Eliomsessions.Admin *)




(*****************************************************************************)
exception Eliom_retry_with of 
  (Extensions.request_info * 
     sess_info * 
     Http_frame.cookieset (* user cookies set by previous pages *) *
     tables cookie_info
     (* current cookie info *)
  ) 


(*****************************************************************************)
(* Each persistent table created by sites correspond to a file on the disk.
   We save the names of the currently opened tables in this table: *)

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

let persistent_cookies_table :
    (string * float option * timeout * 
       Eliommod_sessiongroups.perssessgrp option)
    Ocsipersist.table = 
  create_persistent_table eliom_persistent_cookie_table
(* Another tables, containing the session info for each cookie *)
(* the table contains:
   - the expiration date (by timeout), changed at each access to the table
     (float option) None -> no expiration
   - the timeout for the user (float option option) None -> see global config
     Some None -> no timeout
 *)

(** removes the entry from all opened tables *)
let remove_from_all_persistent_tables key =
  Perstables.fold (* could be replaced by a parallel map *)
    (fun thr t -> thr >>= fun () ->
      Ocsipersist.remove (Ocsipersist.open_table t) key >>= Lwt_unix.yield)
    (return ())
    !perstables

(*****************************************************************************)
(* The current registration directory *)
let absolute_change_sitedata, 
  get_current_sitedata, 
  end_current_sitedata =
  let f2 : sitedata list ref = ref [] in
  let popf2 () =
    match !f2 with
    | _::t -> f2 := t
    | [] -> f2 := [] 
  in
  ((fun sitedata -> f2 := sitedata::!f2) (* absolute_change_sitedata *),
   (fun () ->  match !f2 with
   | [] -> raise (Eliom_function_forbidden_outside_site_loading
                    "get_current_sitedata") 
   | sd::_ -> sd) (* get_current_sitedata *),
   (fun () -> popf2 ()) (* end_current_sitedata *))
(* Warning: these functions are used only during the initialisation
   phase, which is not threaded ... That's why it works, but ...
   it is not really clean ... public registration relies on this
   directory (defined for each site in the config file) 
 *)

(*****************************************************************************)
let add_unregistered sitedata a = 
  sitedata.unregistered_services <- a::sitedata.unregistered_services

let add_unregistered_na sitedata a = 
  sitedata.unregistered_na_services <- a::sitedata.unregistered_na_services

let remove_unregistered sitedata a = 
  sitedata.unregistered_services <- 
    Ocsigen_lib.list_remove_first_if_any a sitedata.unregistered_services

let remove_unregistered_na sitedata a = 
  sitedata.unregistered_na_services <- 
    Ocsigen_lib.list_remove_first_if_any a sitedata.unregistered_na_services

let verify_all_registered sitedata =
  match sitedata.unregistered_services, sitedata.unregistered_na_services with
  | [], [] -> () 
  | l1, l2 -> 
      raise (Eliom_there_are_unregistered_services (sitedata.site_dir, l1, l2))


let during_eliom_module_loading, 
  begin_load_eliom_module, 
  end_load_eliom_module =
  let during_eliom_module_loading_ = ref false in
  ((fun () -> !during_eliom_module_loading_),
   (fun () -> during_eliom_module_loading_ := true),
   (fun () -> during_eliom_module_loading_ := false))

let global_register_allowed () =
  if (Extensions.during_initialisation ()) && (during_eliom_module_loading ())
  then Some get_current_sitedata
  else None


(*****************************************************************************)
let close_service_session2 sitedata fullsessgrp cookie = 
  SessionCookies.remove sitedata.session_services cookie;
  Eliommod_sessiongroups.Serv.remove cookie fullsessgrp
