(* Ocsigen
 * http://www.ocsigen.org
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

include Eliom_common_cli

exception Eliom_Wrong_parameter (** Service called with wrong parameter names *)
exception Eliom_Session_expired
exception Eliom_Typing_Error of (string * exn) list

exception Eliom_duplicate_registration of string
exception Eliom_there_are_unregistered_services of
  (string list * string list list * na_key_serv list)
exception Eliom_site_information_not_available of string
exception Eliom_page_erasing of string
exception Eliom_error_while_loading_site of string

exception Eliom_404
exception Eliom_do_redirection of string
exception Eliom_do_half_xhr_redirection of string


(*****************************************************************************)
(*VVV Do not forget to change the version number
  when the internal format change!!! *)
let persistent_cookie_table_version = "_v4" 
(* v2 introduces session groups *)
(* v3 introduces tab sessions *)
(* v4 introduces group tables *)
  
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

let eliom_link_too_old : bool Polytables.key = Polytables.make_key ()
(** The coservice does not exist any more *)

let eliom_service_session_expired : 
    (fullsessionname list * fullsessionname list) Polytables.key = 
  Polytables.make_key ()
(** If present in request data,  means that
    the service session cookies does not exist any more.
    The string lists are the list of names of expired sessions
*)

let found_stop_key = Polytables.make_key ()

(*****************************************************************************)

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
type 'a sessgrp =
    (string * cookie_scope
     * (string, Ocsigen_lib.ip_address) Ocsigen_lib.leftright)
    (* The full session group is the triple
       (site_dir_string, scope, session group name).
       The scope is the scope of group members (`Session by default).
       If there is no session group, 
       we limit the number of sessions by IP address. *)
type perssessgrp = string (* same triple, marshaled *)

let make_persistent_full_group_name ~cookie_scope site_dir_string = function
  | None -> None
  | Some g ->
    Some (Marshal.to_string
            (site_dir_string, cookie_scope, Ocsigen_lib.Left g) [])

let getperssessgrp a = Marshal.from_string a 0

let string_of_perssessgrp = Ocsigen_lib.id

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
     sc_session_group: cookie_scope sessgrp ref
       (* session group *);
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
     dc_session_group: cookie_scope sessgrp ref (* session group *);
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
      Fullsessionname_Table.t ref (* The key is the full session name *) *

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
      Fullsessionname_Table.t ref (* The key is the full session name *) *

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
      Fullsessionname_Table.t ref


type 'a cookie_info =
    'a cookie_info1 (* unsecure *) * 
      'a cookie_info1 option (* secure, if https *)



(* non persistent cookies for services *)
type 'a servicecookiestablecontent =
    (fullsessionname     (* fullsessname *) *
     'a                  (* session table *) *
     float option ref    (* expiration date by timeout
                            (server side) *) *
     timeout ref         (* user timeout *) *
     cookie_scope sessgrp ref   (* session group *) *
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
    (fullsessionname         (* fullsessname *) *
     float option ref        (* expiration date by timeout
                                (server side) *) *
     timeout ref             (* user timeout *) *
     cookie_scope sessgrp ref   (* session group *) *
     string Ocsigen_cache.Dlist.node (* session group node *))

type datacookiestable = datacookiestablecontent SessionCookies.t




(*****************************************************************************)
let ipv4mask = ref 0b11111111111111110000000000000000l    (* /16 *)
let ipv6mask = ref (0b1111111111111111111111111111111111111111111111111111111100000000L, 0L) (* /56 (???) *)

let get_mask4 m = 
  match fst m with
    | Some m -> m
    | None -> !ipv4mask
      
let get_mask6 m =
  match fst m with
    | Some m -> m
    | None -> !ipv6mask

module Net_addr_Hashtbl = 
  (* keys are IP address modulo "network equivalence" *)
  (struct
     include Hashtbl.Make(struct
                            type t = Ocsigen_lib.ip_address
                            let equal = (=)
                            let hash = Hashtbl.hash
                          end)

     let add m4 m6 t k v = 
       add t (Ocsigen_lib.network_of_ip k (get_mask4 m4) (get_mask6 m6)) v
         
     let remove m4 m6 t k = 
       remove t (Ocsigen_lib.network_of_ip k (get_mask4 m4) (get_mask6 m6))

     let find m4 m6 t k = 
       find t (Ocsigen_lib.network_of_ip k (get_mask4 m4) (get_mask6 m6))

     let find_all m4 m6 t k = 
       find_all t (Ocsigen_lib.network_of_ip k (get_mask4 m4) (get_mask6 m6))
         
     let replace m4 m6 t k v = 
       replace t (Ocsigen_lib.network_of_ip k (get_mask4 m4) (get_mask6 m6)) v
         
     let mem m4 m6 t k = 
       mem t (Ocsigen_lib.network_of_ip k (get_mask4 m4) (get_mask6 m6))

   end : sig

     type key = Ocsigen_lib.ip_address
     type 'a t
     val create : int -> 'a t
     val clear : 'a t -> unit
     val copy : 'a t -> 'a t
     val add : 
       int32 option * 'bb -> (int64 * int64) option * 'bb -> 'a t -> key -> 'a -> unit
     val remove : int32 option * 'bb -> (int64 * int64) option * 'bb -> 'a t -> key -> unit
     val find : int32 option * 'bb -> (int64 * int64) option * 'bb -> 'a t -> key -> 'a
     val find_all :
       int32 option * 'bb -> (int64 * int64) option * 'bb -> 'a t -> key -> 'a list
     val replace : 
       int32 option * 'bb -> (int64 * int64) option * 'bb -> 'a t -> key -> 'a -> unit
     val mem : int32 option * 'bb -> (int64 * int64) option * 'bb -> 'a t -> key -> bool
     val iter : (key -> 'a -> unit) -> 'a t -> unit
     val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
     val length : 'a t -> int

   end)

let create_dlist_ip_table = Net_addr_Hashtbl.create
let find_dlist_ip_table = Net_addr_Hashtbl.find
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

type client_process_info = (* information about the client process.
                              Mainly the URL when it has been launched *)
    {
      cpi_ssl : bool;
      cpi_hostname : string;
      cpi_server_port : int;
      cpi_original_full_path : Ocsigen_lib.url_path;
      cpi_references : Polytables.t;
    }

type server_params =
    {sp_request: Ocsigen_extensions.request;
     sp_si: sess_info;
     sp_sitedata: sitedata (* data for the whole site *);
     sp_cookie_info: tables cookie_info;
     sp_tab_cookie_info: tables cookie_info;
     mutable sp_user_cookies: Ocsigen_cookies.cookieset;
     (* cookies (un)set by the user during service *)
     mutable sp_user_tab_cookies: Ocsigen_cookies.cookieset;
     mutable sp_client_appl_name: string option; (* The application name,
                                                    as sent by the browser *)
     sp_suffix: Ocsigen_lib.url_path option (* suffix *);
     sp_fullsessname: fullsessionname option (* the name of the session
                                                to which belong the service
                                                that answered
                                                (if it is a session service) *);
     mutable sp_client_process_info: client_process_info Lazy.t
     (* Contains the base URL information from which the client process
        has been launched (if any). All relative links and forms will be
        created with respect to this information (if present - from current
        URL otherwise).
        It is taken form a client process state if the application has been
        launched before (and not timeouted on server side).
        Otherwise, it is created and registered in a server side state
        the first time we need it.
     *);
    }

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
            (int ref option (* max_use *) *
               (float * float ref) option
                 (* timeout and expiration date for the service *) *
            (bool -> server_params -> Ocsigen_http_frame.result Lwt.t)
            )) list

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
    {mutable table_services : (int (* generation *) * 
                                 int (* priority *) * 
                                 dircontent ref) list;
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
       (sp:server_params -> att_key_serv -> string) Ocsigen_lib.Int_Table.t;
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
   config_info: Ocsigen_extensions.config_info;

   (* Timeouts:
       - default for site (browser sessions)
       - default for site (tab sessions)
       - then default for each full session name
      The booleans means "has been set from config file"
   *)
   mutable servtimeout: 
     (float option * bool) option * 
     (float option * bool) option * 
     ((fullsessionname * (float option * bool)) list);
   mutable datatimeout: 
     (float option * bool) option * 
     (float option * bool) option * 
     ((fullsessionname * (float option * bool)) list);
   mutable perstimeout: 
     (float option * bool) option *
     (float option * bool) option * 
     ((fullsessionname * (float option * bool)) list);

   global_services: tables; (* global service table *)
   session_services: tables servicecookiestable;
   (* cookie table for services (tab and browser sessions) *)
   session_data: datacookiestable; (* cookie table for in memory session data
                                      (tab and browser sessions) 
                                      contains the information about the cookie
                                      (expiration, group ...).
                                   *)
   group_of_groups: [ `Session_group ] sessgrp Ocsigen_cache.Dlist.t; 
   (* Limitation of the number of groups per site *)
   mutable remove_session_data: string -> unit;
   mutable not_bound_in_data_tables: string -> bool;
   mutable exn_handler: exn -> Ocsigen_http_frame.result Lwt.t;
   mutable unregistered_services: Ocsigen_lib.url_path list;
   mutable unregistered_na_services: na_key_serv list;
   mutable max_volatile_data_sessions_per_group : int * bool;
   mutable max_volatile_data_sessions_per_subnet : int * bool;
   mutable max_volatile_data_tab_sessions_per_group : int * bool;
   mutable max_service_sessions_per_group : int * bool;
   mutable max_service_sessions_per_subnet : int * bool;
   mutable max_service_tab_sessions_per_group : int * bool;
   mutable max_persistent_data_sessions_per_group : int option * bool;
   mutable max_persistent_data_tab_sessions_per_group : int option * bool;
   mutable max_anonymous_services_per_session : int * bool;
   mutable max_anonymous_services_per_subnet : int * bool;
   dlist_ip_table : dlist_ip_table;
   mutable ipv4mask : int32 option * bool;
   mutable ipv6mask : (int64 * int64) option * bool;
   mutable get_client_process_info : unit -> client_process_info option;
   mutable set_client_process_info : client_process_info -> unit;
 }

and dlist_ip_table = (page_table ref * page_table_key, na_key_serv)
    Ocsigen_lib.leftright Ocsigen_cache.Dlist.t Net_addr_Hashtbl.t




(*****************************************************************************)

let make_full_cookie_name a b = a^b

let make_fullsessname ~sp cookie_scope = function
  | None -> ((cookie_scope :> cookie_scope), sp.sp_sitedata.site_dir_string)
  | Some s -> ((cookie_scope :> cookie_scope), sp.sp_sitedata.site_dir_string^"|"^s)
(* Warning: do not change this without modifying Eliomsessions.Admin *)

let make_fullsessname2 site_dir_string cookie_scope = function
  | None -> ((cookie_scope :> cookie_scope), site_dir_string)
  | Some s -> ((cookie_scope :> cookie_scope), site_dir_string^"|"^s)
(* Warning: do not change this without modifying Eliomsessions.Admin *)

let get_cookie_info sp = function
  | `Session -> sp.sp_cookie_info
  | `Client_process -> sp.sp_tab_cookie_info



(*****************************************************************************)
(** Create server parameters record *)
let make_server_params_
    sitedata (ri, si, all_cookie_info, all_tab_cookie_info, user_tab_cookies)
    suffix fullsessname
    : server_params =
  let appl_name =
    try
      Some
        (Ocsigen_lib.String_Table.find appl_name_cookie_name si.si_tab_cookies)
    (* It is an XHR from the client application, or an internal form *)
    with Not_found -> None
  in
  {sp_request=ri;
   sp_si=si;
   sp_sitedata=sitedata;
   sp_cookie_info=all_cookie_info;
   sp_tab_cookie_info=all_tab_cookie_info;
   sp_user_cookies= Ocsigen_cookies.empty_cookieset;
   sp_user_tab_cookies= user_tab_cookies;
   sp_client_appl_name= appl_name;
   sp_suffix=suffix;
   sp_fullsessname= fullsessname;
   sp_client_process_info = 
      lazy (failwith "sp_client_process_info called before initialization");
  (* Will be set later from server side state data *)
  }

let sp_key = Lwt.new_key ()

let get_sp_option () = Lwt.get sp_key

let get_sp () =
  match Lwt.get sp_key with
    | Some sp -> sp
    | None -> failwith "sp not initialized"

let sp_of_option sp = 
  match sp with
    | None -> get_sp ()
    | Some sp -> sp

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
   | [] -> raise (Eliom_site_information_not_available
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

(*****************************************************************************)
(*****************************************************************************)
(* The table of dynamic pages for each virtual server, and naservices        *)
(* Each node contains either a list of nodes (case directory)
    or a table of "answers" (functions that will generate the page) *)


let empty_page_table () = Serv_Table.empty
let empty_dircontent () = Vide
let empty_naservice_table () = AVide

let service_tables_are_empty t =
  !(t.table_naservices) = AVide
  && ((* !(t.table_services) = [] <---- probably enough? *)
      List.for_all (fun (_, _, r) -> !r = Vide) t.table_services)

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

let dlist_finaliser_ip sitedata ip na_table_ref node =
  dlist_finaliser na_table_ref node;
  match Ocsigen_cache.Dlist.list_of node with
    | Some cl ->
        if Ocsigen_cache.Dlist.size cl = 1
        then
          (try
             Net_addr_Hashtbl.remove
               sitedata.ipv4mask sitedata.ipv6mask sitedata.dlist_ip_table ip
           with Not_found -> ())
    | None -> ()

let add_dlist_ dlist v =
  ignore (Ocsigen_cache.Dlist.add v dlist);
  match Ocsigen_cache.Dlist.newest dlist with
    | Some a -> a
    | None -> assert false

let empty_tables max forsession =
  let t1 = [] in
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
        Ocsigen_cache.Dlist.set_finaliser_before (dlist_finaliser t2) dlist;
        fun ?sp v -> add_dlist_ dlist v
      else
        fun ?sp v ->
          let ip, max, sitedata =
            match sp with
              | None ->
                  Ocsigen_lib.inet6_addr_loopback, max,
                  (match global_register_allowed () with
                     | None -> 
                         failwith "global tables created outside initialisation"
                     | Some get -> get ())
              | Some sp -> 
                  ((Lazy.force 
                      sp.sp_request.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_remote_ip_parsed),
                   (fst sp.sp_sitedata.max_anonymous_services_per_subnet),
                   sp.sp_sitedata
                  )
          in
          let dlist =
            try
              Net_addr_Hashtbl.find
                sitedata.ipv4mask sitedata.ipv6mask sitedata.dlist_ip_table ip
            with Not_found ->
              let dlist = Ocsigen_cache.Dlist.create max in
              Net_addr_Hashtbl.add 
                sitedata.ipv4mask sitedata.ipv6mask sitedata.dlist_ip_table ip dlist;
              Ocsigen_cache.Dlist.set_finaliser_before 
                (dlist_finaliser_ip sitedata ip t2) 
                dlist;
              dlist
          in
          add_dlist_ dlist v
  }

let new_service_session_tables sitedata = 
  empty_tables
    (fst sitedata.max_anonymous_services_per_session)
    true

let get_mask4 sitedata = get_mask4 sitedata.ipv4mask
let get_mask6 sitedata = get_mask6 sitedata.ipv6mask

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

let getcookies cookie_scope cookiename cookies =
  let length = String.length cookiename in
  let last = length - 1 in
  Ocsigen_lib.String_Table.fold
    (fun name value beg ->
      if Ocsigen_lib.string_first_diff cookiename name 0 last = length
      then
        Fullsessionname_Table.add
          (cookie_scope, (String.sub name length ((String.length name) - length)))
          value
          beg
      else beg
    )
    cookies
    Fullsessionname_Table.empty

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

(* After an ction, we get tab_cookies info from rc: *)
let tab_cookie_action_info_key = Polytables.make_key ()

let get_session_info req previous_extension_err =
  let req_whole = req
  and ri = req.Ocsigen_extensions.request_info
  and ci = req.Ocsigen_extensions.request_config in
  let rc = ri.Ocsigen_extensions.ri_request_cache in
  let no_post_param, p =
    match ri.Ocsigen_extensions.ri_post_params with
      | None -> true, Lwt.return []
      | Some f -> false, f ci
  in
  p >>= fun post_params ->

  let (previous_tab_cookies_info, tab_cookies, post_params) =
    try
      let (tci, utc, tc) = 
        Polytables.get ~table:rc ~key:tab_cookie_action_info_key
      in
      Polytables.remove ~table:rc ~key:tab_cookie_action_info_key;
      (Some (tci, utc), tc, post_params)
    with Not_found ->
      let tab_cookies, post_params =
        try
          let (tc, pp) = 
            Ocsigen_lib.list_assoc_remove tab_cookies_param_name post_params
          in
          (Json.from_string<string Ocsigen_lib.String_Table.t> tc, pp)
          (*Marshal.from_string (Ocsigen_lib.decode tc) 0, pp*)
        with Not_found -> Ocsigen_lib.String_Table.empty, post_params
      in
      (None, tab_cookies, post_params)
  in


  let post_params, get_params, to_be_considered_as_get =
    try
      ([],
       Lazy.force ri.Ocsigen_extensions.ri_get_params
       @snd (Ocsigen_lib.list_assoc_remove 
                         get_request_post_param_name post_params),
       true)
    (* It was a POST request to be considered as GET *)
    with Not_found ->
      (post_params, Lazy.force ri.Ocsigen_extensions.ri_get_params, false)
  in

  let get_params, internal_form =
    try
      (snd (Ocsigen_lib.list_assoc_remove internal_form_full_name get_params),
       true)
    with Not_found -> (get_params, false)
  in

  let get_params0 = get_params in
  let post_params0 = post_params in

  let get_params, post_params, 
    (all_get_params, all_post_params,
     nl_get_params, nl_post_params, 
     all_get_but_nl, internal_form) = 
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
      (get_params0, (if no_post_param then None else Some post_params0), 
       nl_get_params, nl_post_params, all_get_but_nl, internal_form)
  in

  let browser_cookies = Lazy.force ri.Ocsigen_extensions.ri_cookies in

  let data_cookies = getcookies `Session datacookiename browser_cookies in
  let service_cookies = getcookies `Session servicecookiename browser_cookies in
  let persistent_cookies = getcookies `Session persistentcookiename browser_cookies in
  
  let secure_cookie_info =
    if ri.Ocsigen_extensions.ri_ssl
    then
      let sdata_cookies = getcookies `Session sdatacookiename browser_cookies in
      let sservice_cookies = getcookies `Session sservicecookiename browser_cookies in
      let spersistent_cookies =
        getcookies `Session spersistentcookiename browser_cookies
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

  let data_cookies_tab = getcookies `Client_process datacookiename tab_cookies in
  let service_cookies_tab = getcookies `Client_process servicecookiename tab_cookies in
  let persistent_cookies_tab = getcookies `Client_process persistentcookiename tab_cookies in

  let secure_cookie_info_tab =
    if ri.Ocsigen_extensions.ri_ssl
    then
      let sdata_cookies = getcookies `Client_process sdatacookiename tab_cookies in
      let sservice_cookies = getcookies `Client_process sservicecookiename tab_cookies in
      let spersistent_cookies = getcookies `Client_process spersistentcookiename tab_cookies in
      Some (sservice_cookies, sdata_cookies, spersistent_cookies)
    else None
  in
  
  let get_params_string, url_string =
    if internal_form
    then
      let gps = Ocsigen_lib.mk_url_encoded_parameters all_get_params in
      let uri = ri.Ocsigen_extensions.ri_full_path_string in
      ((if gps = "" then None else Some gps), 
       Ocsigen_lib.add_to_string uri "?" gps)
    else (ri.Ocsigen_extensions.ri_get_params_string,
          ri.Ocsigen_extensions.ri_url_string)
  in

  let ri', sess =
(*VVV 2011/02/15 TODO: I think we'd better not change ri here.
  Keep ri for original values and use si for Eliom's values?
*)
    {ri with
      Ocsigen_extensions.ri_url_string = url_string;
      Ocsigen_extensions.ri_get_params_string = get_params_string;
      Ocsigen_extensions.ri_method =
        (if
            (ri.Ocsigen_extensions.ri_method =
                Ocsigen_http_frame.Http_header.HEAD) ||
              to_be_considered_as_get
         then Ocsigen_http_frame.Http_header.GET
         else ri.Ocsigen_extensions.ri_method);
       (* Here we modify ri, instead of putting service parameters in si.
          Thus it works better after actions: 
          the request can be taken by other extensions, with new parameters.
          Initial parameters are kept in si.
       *)
      Ocsigen_extensions.ri_get_params = lazy get_params;
      Ocsigen_extensions.ri_post_params = 
        if no_post_param 
        then None
        else Some (fun _ -> Lwt.return post_params)},
    {si_service_session_cookies= service_cookies;
     si_data_session_cookies= data_cookies;
     si_persistent_session_cookies= persistent_cookies;
     si_secure_cookie_info= secure_cookie_info;

     si_service_session_cookies_tab= service_cookies_tab;
     si_data_session_cookies_tab= data_cookies_tab;
     si_persistent_session_cookies_tab= persistent_cookies_tab;
     si_secure_cookie_info_tab= secure_cookie_info_tab;

     si_tab_cookies= tab_cookies;

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
     si_internal_form= internal_form;
    }
  in
  Lwt.return
    ({ req_whole with Ocsigen_extensions.request_info = ri' }, sess, 
     previous_tab_cookies_info)




type ('a, 'b) foundornot = Found of 'a | Notfound of 'b



(*****************************************************************************)
type info =
    (Ocsigen_extensions.request * 
       sess_info * 
       tables cookie_info (* current browser cookie info *) * 
       tables cookie_info (* current tab cookie info *) * 
       Ocsigen_cookies.cookieset (* current user tab cookies *))

exception Eliom_retry_with of info

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
    (fullsessionname * float option * timeout * perssessgrp option)
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



(**** Wrapper type shared by client/server side ***)

type 'a wrapper = 'a Ocsigen_wrap.wrapper

let make_wrapper f = Ocsigen_wrap.create_wrapper f
let empty_wrapper () = Ocsigen_wrap.empty_wrapper

type toucher = (unit XHTML5.M.elt) Ocsigen_wrap.toucher

let make_toucher (f : 'a XHTML5.M.elt -> unit) = Ocsigen_wrap.create_toucher (Obj.magic f : unit XHTML5.M.elt -> unit)

type unwrap_id = Ocsigen_wrap.unwrap_id
type unwrapper = Ocsigen_wrap.unwrapper

let make_unwrapper = Ocsigen_wrap.create_unwrapper
let empty_unwrapper = Ocsigen_wrap.empty_unwrapper
let react_up_unwrap_id : unwrap_id = Ocsigen_wrap.id_of_int react_up_unwrap_id_int
let comet_channel_unwrap_id : unwrap_id = Ocsigen_wrap.id_of_int comet_channel_unwrap_id_int
