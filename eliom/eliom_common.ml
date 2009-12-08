(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_common.ml
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

include Eliom_common_obrowser

exception Eliom_Wrong_parameter (** Service called with wrong parameter names *)
exception Eliom_Session_expired
exception Eliom_Typing_Error of (string * exn) list

exception Eliom_duplicate_registration of string
exception Eliom_there_are_unregistered_services of
  (string list * string list list * na_key_serv list)
exception Eliom_function_forbidden_outside_site_loading of string
exception Eliom_page_erasing of string
exception Eliom_error_while_loading_site of string

exception Eliom_404
exception Eliom_Suffix_redirection of string


(*****************************************************************************)
(*VVV Do not forget to change the version number
  when the internal format change!!! *)
let persistent_cookie_table_version = "_v2" (* v2 introduces session groups *)
let eliom_persistent_cookie_table =
  "eliom_persist_cookies"^persistent_cookie_table_version

let datacookiename = "eliomdatasession|"
let servicecookiename = "eliomservicesession|"
(* must not be a prefix of the following and vice versa (idem for data) *)
let persistentcookiename = "eliompersistentsession|"

(* the same, secure: *)
let sdatacookiename = "Seliomdatasession|"
let sservicecookiename = "Seliomservicesession|"
let spersistentcookiename = "Seliompersistentsession|"


(*****************************************************************************)

(** Type used for cookies to set.
    The float option is the timestamp for the expiration date.
    The strings are names and values.
 *)
type cookie =
  | Set of Ocsigen_lib.url_path option * float option * string * string * bool
  | Unset of Ocsigen_lib.url_path option * string


type 'a session_cookie =
  | SCNo_data
  | SCData_session_expired
  | SC of 'a

type cookie_exp =
  | CENothing   (** nothing to set (keep current value) *)
  | CEBrowser   (** expires at browser close *)
  | CESome of float (** expiration date *)

type timeout =
  | TGlobal (** see global setting *)
  | TNone   (** explicitely set no timeout *)
  | TSome of float (** timeout duration in seconds *)




(* The table of tables for each session. Keys are cookies *)
module SessionCookies = 
  Hashtbl.Make(struct
                 type t = string
                 let equal = (=)
                 let hash = Hashtbl.hash
               end)

(* session groups *)
type sessgrp = (string * (string, Unix.inet_addr) Ocsigen_lib.leftright)
    (* The full session group is the pair (site_dir_string, session group name).
       If there is no session group, 
       we limit the number of sessions by IP address *)
type perssessgrp = string (* the same pair, marshaled *)


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
     sc_session_group:sessgrp ref (* session group *);
     mutable sc_session_group_node:string Ocsigen_cache.Dlist.node;
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
     dc_session_group: sessgrp ref (* session group *);
     mutable dc_session_group_node:string Ocsigen_cache.Dlist.node;
   }

type one_persistent_cookie_info =
     {pc_value:string                    (* current value *);
      pc_timeout:timeout ref             (* user timeout *);
      pc_cookie_exp:cookie_exp ref       (* cookie expiration date to set *);
      pc_session_group:perssessgrp option ref (* session group *)
    }


(*VVV heavy *)
type 'a cookie_info1 =
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
      Ocsigen_lib.String_Table.t ref (* The key is the full session name *) *

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
      Ocsigen_lib.String_Table.t ref (* The key is the full session name *) *

      (* persistent sessions: *)
      ((string                  (* value sent by the browser *) *
        timeout                 (* timeout at the beginning of the request *) *
        float option            (* (server side) expdate
                                   at the beginning of the request
                                   None = no exp *) *
        perssessgrp option      (* session group at beginning of request *))
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
      Ocsigen_lib.String_Table.t ref


type 'a cookie_info =
    'a cookie_info1 (* unsecure *) * 
      'a cookie_info1 option (* secure, if https *)



(* non persistent cookies for services *)
type 'a servicecookiestablecontent =
    (string              (* session fullsessname *) *
     'a                  (* session table *) *
     float option ref    (* expiration date by timeout
                            (server side) *) *
     timeout ref         (* user timeout *) *
     sessgrp ref   (* session group *) *
     string Ocsigen_cache.Dlist.node (* session group node *))

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
     sessgrp ref   (* session group *) *
     string Ocsigen_cache.Dlist.node (* session group node *))

type datacookiestable = datacookiestablecontent SessionCookies.t




(*****************************************************************************)


type page_table_key =
    {key_state : att_key_serv * att_key_serv;
     key_kind: Ocsigen_http_frame.Http_header.http_method}

module Serv_Table = Map.Make(struct
  type t = page_table_key
  let compare = compare
end)

module NAserv_Table = Map.Make(struct
  type t = na_key_serv
  let compare = compare
end)

type anon_params_type = int

type server_params =
    {sp_request: Ocsigen_extensions.request;
     sp_si: sess_info;
     sp_sitedata: sitedata (* data for the whole site *);
     sp_cookie_info: tables cookie_info;
     sp_suffix: Ocsigen_lib.url_path option (* suffix *);
     sp_fullsessname: string option (* the name of the session
                                       to which belong the service
                                       that answered
                                       (if it is a session service) *)}

and page_table = page_table_content Serv_Table.t

and page_table_content =
    Ptc of
      (page_table ref * page_table_key, na_key_serv) Ocsigen_lib.leftright
        Ocsigen_cache.Dlist.node option
        (* for limitation of number of dynamic anonymous coservices *) *
        
        ((anon_params_type * anon_params_type)
           (* unique_id, computed from parameters type.
              must be the same even if the actual service reference
              is different (after reloading the site)
              so that it replaces the former one
           *) *
           (int * (* generation (= number of reloads of sites
                     after which that service has been created) *)
              (int ref option (* max_use *) *
                 (float * float ref) option
                 (* timeout and expiration date for the service *) *
                 (bool -> server_params -> Ocsigen_http_frame.result Lwt.t)
              ))) list

and naservice_table_content =
    (int (* generation (= number of reloads of sites
            after which that service has been created) *) *
       int ref option (* max_use *) *
       (float * float ref) option (* timeout and expiration date *) *
       (server_params -> Ocsigen_http_frame.result Lwt.t) *
       (page_table ref * page_table_key, na_key_serv) Ocsigen_lib.leftright
       Ocsigen_cache.Dlist.node option
       (* for limitation of number of dynamic coservices *)
    )

and naservice_table =
  | AVide
  | ATable of naservice_table_content NAserv_Table.t

and dircontent =
  | Vide
  | Table of direlt ref Ocsigen_lib.String_Table.t

and direlt =
  | Dir of dircontent ref
  | File of page_table ref

and tables =
    {table_services : dircontent ref;
     table_naservices : naservice_table ref;
     (* ref, and not mutable field because it simpler to use
        recursively with Dir of dircontent ref *)
    (* Information for the GC: *)
     mutable table_contains_services_with_timeout : bool;
     (* true if dircontent contains services with timeout *)
     mutable table_contains_naservices_with_timeout : bool;
     (* true if naservice_table contains services with timeout *)
     mutable csrf_get_or_na_registration_functions :
       (sp:server_params -> string) Ocsigen_lib.Int_Table.t;
     mutable csrf_post_registration_functions :
       (sp:server_params -> 
         att_key_serv -> string) Ocsigen_lib.Int_Table.t;
      (* These two table are used for CSRF safe services:
         We associate to each service unique id the function that will
         register a new anonymous coservice each time we create a link or form.
         Attached POST coservices may have both a GET and POST 
         registration function. That's why there are two tables.
         The functions associated to each service may be different for
         each session. That's why we use these table, and not a field in
         the service record.
     *)
     service_dlist_add :
       ?sp:server_params -> 
       (page_table ref * page_table_key, na_key_serv) Ocsigen_lib.leftright ->
       (page_table ref * page_table_key, na_key_serv) Ocsigen_lib.leftright
         Ocsigen_cache.Dlist.node
       (* Add in a dlist
          for limiting the number of dynamic anonymous coservices in each table 
          (and avoid DoS).
          There is one dlist for each session, and one for each IP
          in global tables.
          The dlist parameter is the table and coservice number
          for attached coservices,
          and the coservice number for non-attached ones.
       *)
    }

and sitedata =
  {site_dir: Ocsigen_lib.url_path;
   site_dir_string: string;
   mutable servtimeout: (string * float option) list;
   mutable datatimeout: (string * float option) list;
   mutable perstimeout: (string * float option) list;
   global_services: tables; (* global service table *)
   session_services: tables servicecookiestable; (* cookie table for services *)
   session_data: datacookiestable; (* cookie table for in memory session data *)
   mutable remove_session_data: string -> unit;
   mutable not_bound_in_data_tables: string -> bool;
   mutable exn_handler: server_params -> exn -> Ocsigen_http_frame.result Lwt.t;
   mutable unregistered_services: Ocsigen_lib.url_path list;
   mutable unregistered_na_services: na_key_serv list;
   mutable max_volatile_data_sessions_per_group : int;
   mutable max_volatile_data_sessions_per_ip : int;
   mutable max_service_sessions_per_group : int;
   mutable max_service_sessions_per_ip : int;
   mutable max_persistent_data_sessions_per_group : int option;
   mutable max_anonymous_services_per_session : int;
 }


(*****************************************************************************)
    (** Create server parameters record *)
let make_server_params sitedata all_cookie_info ri suffix si fullsessname
    : server_params =
  {sp_request=ri;
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

let empty_page_table () = Serv_Table.empty
let empty_dircontent () = Vide
let empty_naservice_table () = AVide

let service_tables_are_empty t =
  (!(t.table_services) = Vide && !(t.table_naservices) = AVide)

let remove_naservice_table at k =
  match at with
    | AVide -> AVide
    | ATable t -> ATable (NAserv_Table.remove k t)

let dlist_finaliser na_table_ref node =
  (* If the node disappears from the dlist,
     we remove the service from the service table *)
  match Ocsigen_cache.Dlist.value node with
    | Ocsigen_lib.Left (page_table_ref, page_table_key) ->
        page_table_ref := Serv_Table.remove page_table_key !page_table_ref
    | Ocsigen_lib.Right na_key_serv ->
        na_table_ref := remove_naservice_table !na_table_ref na_key_serv

let dlist_finaliser_ip dlist_table ip na_table_ref node =
  dlist_finaliser na_table_ref node;
  match Ocsigen_cache.Dlist.list_of node with
    | Some cl ->
        if Ocsigen_cache.Dlist.size cl = 1
        then
          (try
             Ocsigen_lib.Inet_addr_Hashtbl.remove dlist_table ip
           with Not_found -> ())
    | None -> ()

let add_dlist_ dlist v =
  ignore (Ocsigen_cache.Dlist.add v dlist);
  match Ocsigen_cache.Dlist.newest dlist with
    | Some a -> a
    | None -> assert false

let empty_tables max forsession =
  let dlist_table = Ocsigen_lib.Inet_addr_Hashtbl.create 100 in
    (*VVV One for each tables or a global one? *)
  let t1 = ref (empty_dircontent ()) in
  let t2 = ref (empty_naservice_table ()) in
  {table_services = t1;
   table_naservices = t2;
   table_contains_services_with_timeout = false;
   table_contains_naservices_with_timeout = false;
   csrf_get_or_na_registration_functions = Ocsigen_lib.Int_Table.empty;
   csrf_post_registration_functions = Ocsigen_lib.Int_Table.empty;
   service_dlist_add =
      if forsession
      then
        let dlist = Ocsigen_cache.Dlist.create max in
        Ocsigen_cache.Dlist.set_finaliser (dlist_finaliser t2) dlist;
        fun ?sp v -> add_dlist_ dlist v
      else
        fun ?sp v ->
          let ip =
            match sp with
              | None -> Unix.inet6_addr_loopback
              | Some sp -> sp.sp_request.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_remote_inet_addr
          in
          let dlist =
            try
              Ocsigen_lib.Inet_addr_Hashtbl.find dlist_table ip
            with Not_found ->
              let dlist = Ocsigen_cache.Dlist.create max in
              Ocsigen_cache.Dlist.set_finaliser 
                (dlist_finaliser_ip dlist_table ip t2) dlist;
              dlist
          in
          add_dlist_ dlist v
  }

let new_service_session_tables sitedata = 
  empty_tables sitedata.max_anonymous_services_per_session true

(*****************************************************************************)
open Lwt

(* Split parameter list, removing those whose name starts with pref *)
let split_prefix_param pref l =
  let len = String.length pref in
  List.partition (fun (n,_) ->
    try
      (String.sub n 0 len) = pref
    with Invalid_argument _ -> false) l

(* Special version for non localized parameters *)
let split_nl_prefix_param =
  let prefixlength = String.length nl_param_prefix in
  let prefixlengthminusone = prefixlength - 1 in
  fun l ->
    let rec aux other map = function
      | [] -> (map, other)
      | ((n, v) as a)::l -> 
          if Ocsigen_lib.string_first_diff 
            n nl_param_prefix 0 prefixlengthminusone = prefixlength
          then 
            try
              let last = String.index_from n prefixlength '.' in
              let nl_param_name = 
                String.sub n prefixlength (last - prefixlength) 
              in
              let previous = 
                try Ocsigen_lib.String_Table.find nl_param_name map
                with Not_found -> []
              in
              aux
                other
                (Ocsigen_lib.String_Table.add nl_param_name (a::previous) map)
                l
            with Invalid_argument _ | Not_found -> aux (a::other) map l
          else aux (a::other) map l
    in
    aux [] Ocsigen_lib.String_Table.empty l

let getcookies cookiename cookies =
  let length = String.length cookiename in
  let last = length - 1 in
  Ocsigen_lib.String_Table.fold
    (fun name value beg ->
      if Ocsigen_lib.string_first_diff cookiename name 0 last = length
      then
        Ocsigen_lib.String_Table.add
          (String.sub name length ((String.length name) - length))
          value
          beg
      else beg
    )
    cookies
    Ocsigen_lib.String_Table.empty

(* Remove all parameters whose name starts with pref *)
let remove_prefixed_param pref l =
  let len = String.length pref in
  let rec aux = function
    | [] -> []
    | ((n,v) as a)::l ->
        try
          if (String.sub n 0 len) = pref then
            aux l
          else a::(aux l)
        with Invalid_argument _ -> a::(aux l)
  in aux l

(* After an action, we do not take into account actual get params,
   but these ones: *)
let eliom_params_after_action = Polytables.make_key ()


let get_session_info ri previous_extension_err =
  let ri_whole = ri
  and ri = ri.Ocsigen_extensions.request_info in
  Lazy.force ri.Ocsigen_extensions.ri_post_params >>= fun post_params ->
  let get_params = Lazy.force ri.Ocsigen_extensions.ri_get_params in
  let get_params0 = get_params in
  let post_params0 = post_params in

  let get_params, post_params, 
    (all_get_params, all_post_params,
     nl_get_params, nl_post_params, 
     all_get_but_nl) = 
    try
      (get_params, 
       post_params,
       Polytables.get
         ~table:ri.Ocsigen_extensions.ri_request_cache
         ~key:eliom_params_after_action)
    with Not_found -> 
    let nl_get_params, get_params = split_nl_prefix_param get_params0 in
    let nl_post_params, post_params = split_nl_prefix_param post_params0 in
    let all_get_but_nl = get_params in
    get_params, post_params,
    (get_params0, post_params0, nl_get_params, nl_post_params, all_get_but_nl)
  in

  let data_cookies = getcookies datacookiename
    (Lazy.force ri.Ocsigen_extensions.ri_cookies)
  in
  let service_cookies = getcookies servicecookiename
    (Lazy.force ri.Ocsigen_extensions.ri_cookies)
  in
  let persistent_cookies =
    getcookies
      persistentcookiename
      (Lazy.force ri.Ocsigen_extensions.ri_cookies)
  in
  
  let secure_cookie_info =
    if ri.Ocsigen_extensions.ri_ssl
    then
      let sdata_cookies = getcookies sdatacookiename
        (Lazy.force ri.Ocsigen_extensions.ri_cookies)
      in
      let sservice_cookies = getcookies sservicecookiename
        (Lazy.force ri.Ocsigen_extensions.ri_cookies)
      in
      let spersistent_cookies =
        getcookies
          spersistentcookiename
          (Lazy.force ri.Ocsigen_extensions.ri_cookies)
      in
      Some (sservice_cookies, sdata_cookies, spersistent_cookies)
    else None
  in
  
  let naservice_info,
    (get_state, post_state),
    (get_params, other_get_params),
    na_get_params,
    post_params =
    let post_naservice_name, na_post_params =
      try
        let n, pp =
          Ocsigen_lib.list_assoc_remove naservice_num post_params
        in (RNa_post' n, pp)
      with Not_found ->
        try
          let n, pp =
            Ocsigen_lib.list_assoc_remove naservice_name post_params
          in (RNa_post_ n, pp)
        with Not_found -> (RNa_no, [])
    in
    match post_naservice_name with
      | RNa_post_ _
      | RNa_post' _ -> (* POST non attached coservice *)
          (post_naservice_name,
           (RAtt_no, RAtt_no),
           ([], get_params),
           (lazy
              (try
                 (try
                    (naservice_name, List.assoc naservice_name get_params)
                  with Not_found ->
                    (naservice_num, List.assoc naservice_num get_params))
                 ::(fst (split_prefix_param na_co_param_prefix get_params))
               with Not_found -> [])
           ),
           na_post_params)
      | _ ->
          let get_naservice_name, 
            na_name_num,
            (na_get_params, other_get_params) =
            try
              let n, gp =
                Ocsigen_lib.list_assoc_remove naservice_num get_params
              in (RNa_get' n,
                  [(naservice_num, n)],
                  (split_prefix_param na_co_param_prefix gp))
            with Not_found ->
              try
                let n, gp =
                  Ocsigen_lib.list_assoc_remove naservice_name get_params
                in (RNa_get_ n,
                    [(naservice_name, n)],
                    (split_prefix_param na_co_param_prefix gp))
              with Not_found -> (RNa_no, [], ([], get_params))
          in
          match get_naservice_name with
            | RNa_get_ _
            | RNa_get' _ -> (* GET non attached coservice *)
                (get_naservice_name,
                 (RAtt_no, RAtt_no),
                 (na_get_params, other_get_params),
                 (lazy (na_name_num@na_get_params)),
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
                        post_numstate_param_name post_params
                    in (RAtt_anon s, pp)
                  with
                      Not_found -> 
                        try
                          let s, pp =
                            Ocsigen_lib.list_assoc_remove
                              post_state_param_name post_params
                          in (RAtt_named s, pp)
                        with
                            Not_found -> (RAtt_no, post_params)
                in
                let get_state, (get_params, other_get_params) =
                  try
                    let s, gp =
                      Ocsigen_lib.list_assoc_remove
                        get_numstate_param_name get_params
                    in ((RAtt_anon s),
                        (split_prefix_param co_param_prefix gp))
                  with Not_found ->
                    try
                      let s, gp =
                        Ocsigen_lib.list_assoc_remove
                          get_state_param_name get_params
                      in ((RAtt_named s),
                          (split_prefix_param co_param_prefix gp))
                    with Not_found -> (RAtt_no, (get_params, []))
                in
                (RNa_no,
                 (get_state, post_state),
                 (get_params, other_get_params),
                 (lazy (na_name_num@na_get_params)),
                 post_params)
  in
  let persistent_nl_get_params =
    lazy
      (Ocsigen_lib.String_Table.fold
         (fun k a t -> if nl_is_persistent k
          then Ocsigen_lib.String_Table.add k a t
          else t)
         nl_get_params Ocsigen_lib.String_Table.empty)
  in
  let ri', sess =
    {ri with
       Ocsigen_extensions.ri_method =
        (if ri.Ocsigen_extensions.ri_method = Ocsigen_http_frame.Http_header.HEAD
         then Ocsigen_http_frame.Http_header.GET
         else ri.Ocsigen_extensions.ri_method);
       (* Here we modify ri, instead of putting service parameters in si.
          Thus it works better after actions: 
          the request can be taken by other extensions, with new parameters.
          Initial parameters are kept in si.
       *)
       Ocsigen_extensions.ri_get_params = lazy get_params;
       Ocsigen_extensions.ri_post_params = lazy (return post_params)},
    {si_service_session_cookies= service_cookies;
     si_data_session_cookies= data_cookies;
     si_persistent_session_cookies= persistent_cookies;
     si_secure_cookie_info= secure_cookie_info;
     si_nonatt_info= naservice_info;
     si_state_info= (get_state, post_state);
     si_other_get_params= other_get_params;
     si_all_get_params= all_get_params;
     si_all_post_params= all_post_params;
     si_previous_extension_error= previous_extension_err;
     si_na_get_params= na_get_params;
     si_nl_get_params= nl_get_params;
     si_nl_post_params= nl_post_params;
     si_persistent_nl_get_params= persistent_nl_get_params;
     si_all_get_but_nl= all_get_but_nl;
     si_all_get_but_na_nl= 
        lazy
          (List.remove_assoc naservice_name
             (List.remove_assoc naservice_num
                (remove_prefixed_param na_co_param_prefix get_params0)));
    }
  in
  Lwt.return
    ({ ri_whole with Ocsigen_extensions.request_info = ri' }, sess)




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
  (Ocsigen_extensions.request *
     sess_info *
(*     Ocsigen_http_frame.cookieset (* user cookies set by previous pages *) * *)
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
    (string * float option * timeout * perssessgrp option)
    Ocsipersist.table Lazy.t =
  lazy (create_persistent_table eliom_persistent_cookie_table)
(* Another tables, containing the session info for each cookie *)
(* the table contains:
   - the expiration date (by timeout), changed at each access to the table
     (float option) None -> no expiration
   - the timeout for the user (float option option) None -> see global config
     Some None -> no timeout
 *)
(* It is lazy, because we must delay the creation of the table until
   the initialization of eliom in case we use static linking with 
   sqlite backend ... *)


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
  if (Ocsigen_extensions.during_initialisation ())
    && (during_eliom_module_loading ())
  then Some get_current_sitedata
  else None
