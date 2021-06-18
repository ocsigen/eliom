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

open Eliom_lib

include Eliom_common_base

exception Eliom_Session_expired

exception Eliom_there_are_unregistered_services of
    (string list * string list list * na_key_serv list)
exception Eliom_error_while_loading_site of string

exception Eliom_do_redirection of string
exception Eliom_do_half_xhr_redirection of string

type 'a tenable_value = < get : 'a ; set : ?override_tenable:bool -> 'a -> unit >

let tenable_value ~name v = object
  val mutable value = v
  val mutable tenable = false
  method get = value
  method set ?(override_tenable=false) v =
    if not tenable || override_tenable then (
      value <- v;
      tenable <- override_tenable
    ) else Lwt_log.ign_warning_f ~section:Lwt_log.eliom "Ignored setting tenable value %S." name
end

(*****************************************************************************)
(*VVV Do not forget to change the version number
  when the internal format change!!! *)
let persistent_cookie_table_version = "_v5"
(* v2 introduces session groups *)
(* v3 introduces tab sessions *)
(* v4 introduces group tables *)
(* v5 removes secure scopes *)

let eliom_persistent_cookie_table =
  "eliom_persist_cookies"^persistent_cookie_table_version

let datacookiename = "eliomdatasession|"
let servicecookiename = "eliomservicesession|"
(* must not be a prefix of the following and vice versa (idem for data) *)
let persistentcookiename = "eliompersistentsession|"

(*****************************************************************************)

let eliom_link_too_old : bool Polytables.key = Polytables.make_key ()
(** The coservice does not exist any more *)

let eliom_service_session_expired :
  (full_state_name list * full_state_name list) Polytables.key =
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

(* 2013-03-01 From now on, cookie expire 10 years after opening the session.
   Before, it was when the browser was closed but we think it has no sense,
   and many people do not understand why their session is closed, even if
   the session duration on server side is long.
   If you want this, you now have to set this manually.
*)
let default_client_cookie_exp () =
  CESome (Unix.time () +. 315532800.)

type timeout =
  | TGlobal (** see global setting *)
  | TNone   (** explicitly set no timeout *)
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
  (string * cookie_level * (string, Ipaddr.t) leftright)
(* The full session group is the triple
   (site_dir_string, scope, session group name).
   The scope is the scope of group members (`Session by default).
   If there is no session group,
   we limit the number of sessions by IP address. *)
type perssessgrp = string (* same triple, marshaled *)

let make_persistent_full_group_name ~cookie_level site_dir_string = function
  | None -> None
  | Some g ->
    Some (Marshal.to_string
            (site_dir_string, cookie_level, Left g) [])

let getperssessgrp a : 'a sessgrp = Marshal.from_string a 0

let string_of_perssessgrp = id

(* cookies information during page generation: *)

type 'a one_service_cookie_info =
  (* service sessions: *)
  {sc_hvalue:string            (* hash of current value *);
   sc_set_value:string option  (* new value to set *);
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
   sc_session_group: cookie_level sessgrp ref
  (* session group *);
   mutable sc_session_group_node:string Ocsigen_cache.Dlist.node;
  }


type one_data_cookie_info =
  (* in memory data sessions: *)
  {dc_hvalue:string                   (* hash of current value *);
   dc_set_value:string option         (* new value to set *);
   dc_timeout:timeout ref             (* user timeout -
                                         ref towards cookie table
                                      *);
   dc_exp:float option ref            (* expiration date ref (server side) -
                                         None = never
                                         ref towards cookie table
                                      *);
   dc_cookie_exp:cookie_exp ref       (* cookie expiration date to set *);
   dc_session_group: cookie_level sessgrp ref (* session group *);
   mutable dc_session_group_node:string Ocsigen_cache.Dlist.node;
  }

type one_persistent_cookie_info =
  {pc_hvalue:string                   (* hash of current value *);
   pc_set_value:string option         (* new value to set *);
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
    Full_state_name_table.t ref (* The key is the full session name *) *

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
    Full_state_name_table.t ref (* The key is the full session name *) *

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
    Full_state_name_table.t ref


type 'a cookie_info =
  'a cookie_info1 (* unsecure *) *
  'a cookie_info1 (* secure *)



(* non persistent cookies for services *)
type 'a servicecookiestablecontent =
  (full_state_name *
   'a                  (* session table *) *
   float option ref    (* expiration date by timeout
                          (server side) *) *
   timeout ref         (* user timeout *) *
   cookie_level sessgrp ref   (* session group *) *
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
  (full_state_name *
   float option ref        (* expiration date by timeout
                              (server side) *) *
   timeout ref             (* user timeout *) *
   cookie_level sessgrp ref   (* session group *) *
   string Ocsigen_cache.Dlist.node (* session group node *))

type datacookiestable = datacookiestablecontent SessionCookies.t




(*****************************************************************************)
let ipv4mask = ref 16
let ipv6mask = ref 56

let get_mask4 m =
  match fst m with
  | Some m -> m
  | None -> !ipv4mask

let get_mask6 m =
  match fst m with
  | Some m -> m
  | None -> !ipv6mask

let network_of_ip k mask4 mask6 = match k with
  | Ipaddr.V4 ip -> Ipaddr.(V4 (V4.Prefix.(network (make mask4 ip))))
  | Ipaddr.V6 ip -> Ipaddr.(V6 (V6.Prefix.(network (make mask6 ip))))

module Net_addr_Hashtbl =
  (* keys are IP address modulo "network equivalence" *)
  (struct
    include Hashtbl.Make(struct
        type t = Ipaddr.t
        let equal a b = Ipaddr.compare a b = 0
        let hash = Hashtbl.hash
      end)

    let add m4 m6 t k v =
      add t (network_of_ip k (get_mask4 m4) (get_mask6 m6)) v

    let remove m4 m6 t k =
      remove t (network_of_ip k (get_mask4 m4) (get_mask6 m6))

    let find m4 m6 t k =
      find t (network_of_ip k (get_mask4 m4) (get_mask6 m6))

  end : sig

     type key = Ipaddr.t
     type 'a t
     val create : int -> 'a t
     val add      : int option * 'bb -> int option * 'bb -> 'a t -> key -> 'a -> unit
     val remove   : int option * 'bb -> int option * 'bb -> 'a t -> key -> unit
     val find     : int option * 'bb -> int option * 'bb -> 'a t -> key -> 'a
   end)

module Serv_Table = Map.Make(struct
    type t = page_table_key
    let compare = compare
  end)

module NAserv_Table = Map.Make(struct
    type t = na_key_serv
    let compare = compare
  end)

type node_info = {
  ni_id : node_ref;
  mutable ni_sent : bool;
}

module Hier_set = String.Set

type server_params =
  {sp_request: Ocsigen_extensions.request;
   sp_si: sess_info;
   sp_sitedata: sitedata (* data for the whole site *);
   sp_cookie_info: tables cookie_info;
   sp_tab_cookie_info: tables cookie_info;
   mutable sp_user_cookies: Ocsigen_cookie_map.t;
   (* cookies (un)set by the user during service *)
   mutable sp_user_tab_cookies: Ocsigen_cookie_map.t;
   mutable sp_client_appl_name: string option; (* The application name,
                                                  as sent by the browser *)
   sp_suffix: Url.path option (* suffix *);
   sp_full_state_name: full_state_name option
  (* the name of the session
     to which belong the service that answered
     (if it is a session service) *);
   sp_client_process_info: client_process_info;
  }

and page_table = page_table_content Serv_Table.t

and page_table_content = [
    `Ptc of
      (page_table ref * page_table_key, na_key_serv) leftright
        Ocsigen_cache.Dlist.node option *
      (server_params, Ocsigen_response.t) service list ]

and naservice_table_content =
  (int (* generation (= number of reloads of sites
          after which that service has been created) *) *
   int ref option (* max_use *) *
   (float * float ref) option (* timeout and expiration date *) *
   (server_params -> Ocsigen_response.t Lwt.t) *
   (page_table ref * page_table_key, na_key_serv) leftright
     Ocsigen_cache.Dlist.node option
     (* for limitation of number of dynamic coservices *)
  )

and naservice_table =
  | AVide
  | ATable of naservice_table_content NAserv_Table.t

and tables =
  {mutable table_services : (int (* generation *) *
                             int (* priority *) *
                             page_table dircontent ref) list;
     table_naservices : naservice_table ref;
     (* ref, and not mutable field because it simpler to use
        recursively with Dir of dircontent ref *)
    (* Information for the GC: *)
     mutable table_contains_services_with_timeout : bool;
     (* true if dircontent contains services with timeout *)
     mutable table_contains_naservices_with_timeout : bool;
     (* true if naservice_table contains services with timeout *)
     mutable csrf_get_or_na_registration_functions :
       (sp:server_params -> string) Int.Table.t;
     mutable csrf_post_registration_functions :
       (sp:server_params -> att_key_serv -> string) Int.Table.t;
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
       (page_table ref * page_table_key, na_key_serv) leftright ->
       (page_table ref * page_table_key, na_key_serv) leftright
         Ocsigen_cache.Dlist.node
         (* We use a dlist for limiting the number of dynamic
            anonymous coservices in each table (and avoid DoS).  There
            is one dlist for each session, and one for each IP in
            global tables.  The dlist parameter is the table and
            coservice number for attached coservices, and the
            coservice number for non-attached ones. *)
    }

and sitedata =
  {site_dir: Url.path;
   site_dir_string: string;
   config_info: Ocsigen_extensions.config_info;
   default_links_xhr : bool tenable_value;

   (* Timeouts:
       - default for site (browser sessions)
       - default for site (tab sessions)
       - then default for each full session name
      The booleans means "has been set from config file"
   *)
   mutable servtimeout:
     (float option * bool) option *
     (float option * bool) option *
     ((full_state_name * (float option * bool)) list);
   mutable datatimeout:
     (float option * bool) option *
     (float option * bool) option *
     ((full_state_name * (float option * bool)) list);
   mutable perstimeout:
     (float option * bool) option *
     (float option * bool) option *
     ((full_state_name * (float option * bool)) list);

   site_value_table : Polytables.t; (* table containing evaluated
                                       lazy site values *)

   mutable registered_scope_hierarchies: Hier_set.t;

   (* All services, and state data are stored in these tables,
      for scopes session and client process.
      The scope is registered in the full session name. *)
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
   mutable exn_handler: exn -> Ocsigen_response.t Lwt.t;
   mutable unregistered_services: Url.path list;
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
   mutable secure_cookies : bool;
   dlist_ip_table : dlist_ip_table;
   mutable ipv4mask : int option * bool;
   mutable ipv6mask : int option * bool;
   mutable application_script : bool (* defer *) * bool; (* async *)
   mutable cache_global_data : (string list * int) option;
   mutable html_content_type : string option;
   mutable ignored_get_params : (string * Re.re) list;
   mutable ignored_post_params : (string * Re.re) list;
  }

and dlist_ip_table = (page_table ref * page_table_key, na_key_serv)
    leftright Ocsigen_cache.Dlist.t Net_addr_Hashtbl.t


let create_dlist_ip_table = Net_addr_Hashtbl.create
let find_dlist_ip_table :
  int option * 'b ->
  int option * 'b ->
  dlist_ip_table -> Ipaddr.t ->
  (page_table ref * page_table_key, na_key_serv)
    leftright Ocsigen_cache.Dlist.t  = Net_addr_Hashtbl.find
(*****************************************************************************)


(*****************************************************************************)

let make_full_cookie_name cookieprefix (cookie_scope, secure, site_dir_string) =
  let scope_hier = scope_hierarchy_of_scope cookie_scope in
  let secure = if secure then "S|" else "|" in
  let hier1, hiername = match scope_hier with
    | User_hier hiername -> "||", hiername
    | Default_ref_hier -> "|ref|", ""
    | Default_comet_hier -> "|comet|", ""
  in
  let s = String.concat ""
    [cookieprefix; secure; site_dir_string; hier1; hiername]
  in
  s

let make_full_state_name2
    site_dir_string secure ~(scope:[< user_scope ]) : full_state_name =
  (* The information in the cookie name, without the kind of session *)
  ((scope :> user_scope),
   secure,
   site_dir_string)

let make_full_state_name ~sp ~secure ~(scope:[< user_scope ]) =
  make_full_state_name2 sp.sp_sitedata.site_dir_string secure scope

let get_cookie_info sp = function
  | `Session -> sp.sp_cookie_info
  | `Client_process -> sp.sp_tab_cookie_info



(*****************************************************************************)
(** Create server parameters record *)
let make_server_params
    sitedata
    (ri, si, all_cookie_info, all_tab_cookie_info, user_tab_cookies)
    suffix
    full_state_name =
  let appl_name =
    try
      Some
        (Ocsigen_cookie_map.Map_inner.find
           appl_name_cookie_name si.si_tab_cookies)
    (* It is an XHR from the client application, or an internal form *)
    with Not_found -> None
  in
  let cpi =
    match si.si_client_process_info with
    | Some cpi -> cpi
    | None ->
      let request_info = ri.Ocsigen_extensions.request_info in
      { cpi_ssl = Ocsigen_request.ssl request_info;
        cpi_hostname = Ocsigen_extensions.get_hostname ri;
        cpi_server_port = Ocsigen_extensions.get_port ri;
        cpi_original_full_path =
          Ocsigen_request.original_full_path request_info;
      }
  in
  { sp_request = ri;
    sp_si = si;
    sp_sitedata = sitedata;
    sp_cookie_info = all_cookie_info;
    sp_tab_cookie_info = all_tab_cookie_info;
    sp_user_cookies = Ocsigen_cookie_map.empty;
    sp_user_tab_cookies = user_tab_cookies;
    sp_client_appl_name = appl_name;
    sp_suffix = suffix;
    sp_full_state_name = full_state_name;
    sp_client_process_info = cpi;
  }

let sp_key = Lwt.new_key ()

let get_sp_option () = Lwt.get sp_key

let get_sp () =
  match Lwt.get sp_key with
  | Some sp -> sp
  | None ->
      let msg = "This function cannot be called here because it needs information about the request or the site." in
      failwith @@ String.concat "\n" @@ msg :: Eliom_common_base.backtrace_lwt 2

let sp_of_option sp =
  match sp with
    | None -> get_sp ()
    | Some sp -> sp

(*****************************************************************************)
(* Scope registration                                                        *)
(*****************************************************************************)

let global_scope : [> global_scope ] = `Global
let site_scope : [> site_scope ] = `Site

let default_group_scope : [> session_group_scope ] = `Session_group Default_ref_hier
let default_session_scope : [> session_scope ] = `Session Default_ref_hier
let default_process_scope : [> client_process_scope ] = `Client_process Default_ref_hier

let comet_client_process_scope : [> client_process_scope ] = `Client_process Default_comet_hier
let request_scope : [> request_scope ] = `Request

let registered_scope_hierarchies = ref Hier_set.empty

let register_scope_hierarchy (name:string) =
  match get_sp_option () with
    | None ->
      if Hier_set.mem name !registered_scope_hierarchies
      then failwith (Printf.sprintf "the scope hierarchy %s has already been registered" name)
      else registered_scope_hierarchies :=
        Hier_set.add name !registered_scope_hierarchies
    | Some sp ->
      if Hier_set.mem name !registered_scope_hierarchies ||
         Hier_set.mem name sp.sp_sitedata.registered_scope_hierarchies
      then failwith (Printf.sprintf "the scope hierarchy %s has already been registered" name)
      else sp.sp_sitedata.registered_scope_hierarchies <-
        Hier_set.add name sp.sp_sitedata.registered_scope_hierarchies

let create_scope_hierarchy name : scope_hierarchy =
  register_scope_hierarchy name;
  User_hier name

let list_scope_hierarchies () =
  let sp = get_sp () in
  Default_comet_hier::Default_ref_hier::
    (List.map (fun s -> User_hier s)
       (Hier_set.elements !registered_scope_hierarchies)
     @ List.map (fun s -> User_hier s)
       (Hier_set.elements sp.sp_sitedata.registered_scope_hierarchies) )

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
    List.remove_first_if_any a sitedata.unregistered_services

let remove_unregistered_na sitedata a =
  sitedata.unregistered_na_services <-
    List.remove_first_if_any a sitedata.unregistered_na_services

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

let get_site_data () =
  match get_sp_option () with
    | Some sp ->
        sp.sp_sitedata
    | None ->
        if during_eliom_module_loading () then
          get_current_sitedata ()
        else
          failwith "get_site_data"

(*****************************************************************************)
(* Lazy site value: each site have a different value *)
(* Evaluated values are never collected by the GC, the table always
   keeps a reference on it. *)
(* there is no test for cycles *)

type 'a lazy_site_value =
    { lazy_sv_fun : unit -> 'a;
      lazy_sv_key : 'a Polytables.key }

let force_lazy_site_value v =
  let sitedata =
    match get_sp_option () with
      | Some sp -> sp.sp_sitedata
      | None ->
          match global_register_allowed () with
            | Some f -> f ()
            | None ->
                raise (Eliom_site_information_not_available
                         "force_lazy_site_value")
  in
  try Polytables.get
    ~table:sitedata.site_value_table
    ~key:v.lazy_sv_key
  with
    | Not_found ->
      let value = v.lazy_sv_fun () in
      Polytables.set
        ~table:sitedata.site_value_table
        ~key:v.lazy_sv_key
        ~value;
      value

let lazy_site_value_from_fun f =
  { lazy_sv_key = Polytables.make_key ();
    lazy_sv_fun = f }


(*****************************************************************************)
(*****************************************************************************)
(* The table of dynamic pages for each virtual server, and naservices        *)
(* Each node contains either a list of nodes (case directory)
    or a table of "answers" (functions that will generate the page) *)


let empty_page_table () = Serv_Table.empty
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
    | Left (page_table_ref, page_table_key) ->
        page_table_ref := Serv_Table.remove page_table_key !page_table_ref
    | Right na_key_serv ->
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
   csrf_get_or_na_registration_functions = Int.Table.empty;
   csrf_post_registration_functions = Int.Table.empty;
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
                  Ipaddr.(V6 V6.localhost), max,
                  (match global_register_allowed () with
                     | None ->
                         failwith "global tables created outside initialisation"
                     | Some get -> get ())
              | Some sp ->
                  ((Ocsigen_request.remote_ip_parsed
                      sp.sp_request.Ocsigen_extensions.request_info),
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

(* The cookie name is

sessionkind|S?|sitedirstring|"ref" ou "comet" ou ""|hiername

*)

let full_state_name_of_cookie_name cookie_level cookiename =
  let pref, cookiename = Ocsigen_lib.String.sep '|' cookiename in
  let secure, cookiename = Ocsigen_lib.String.sep '|' cookiename in
  let sitedirstring, cookiename = Ocsigen_lib.String.sep '|' cookiename in
  let hier1, hiername = Ocsigen_lib.String.sep '|' cookiename in
  let secure = secure = "S" in
  let sc_hier = match hier1 with
    | "" -> Eliom_common_base.User_hier hiername
    | "ref" -> Eliom_common_base.Default_ref_hier
    | "comet" -> Eliom_common_base.Default_comet_hier
    | _ -> raise Not_found
  in
  match cookie_level with
    | `Session -> (`Session sc_hier, secure, sitedirstring)
    | `Client_process -> (`Client_process sc_hier, secure, sitedirstring)

let hash_cookie c =
  (* To preserve compatibility, we only hash cookies that ends with an
     'H'.  This is the case for all new cookies (see Eliommod_cookies). *)
  if c <> "" &&  c.[String.length c - 1] = 'H' then
    let to_b64 = Cryptokit.Base64.encode_compact () in
    Cryptokit.transform_string to_b64
      (Cryptokit.(hash_string (Hash.sha256 ()) c))
  else
    c

let getcookies secure cookie_level cookienamepref cookies =
  let length = String.length cookienamepref in
  let last = length - 1 in
  Ocsigen_cookie_map.Map_inner.fold
    (fun name value beg ->
      if String.first_diff cookienamepref name 0 last = length
      then
        try
          let (_, sec, _) as expcn =
            full_state_name_of_cookie_name cookie_level name in
          if sec = secure
          then Full_state_name_table.add expcn value beg
          else beg
        with Not_found -> beg
      else beg
    )
    cookies
    Full_state_name_table.empty

(* After an action, we do not take into account actual get params,
   but these ones: *)
let eliom_params_after_action = Polytables.make_key ()

(* After an action, we get tab_cookies info from rc: *)
let tab_cookie_action_info_key = Polytables.make_key ()

type cpi = client_process_info =  {
  cpi_ssl : bool;
  cpi_hostname : string;
  cpi_server_port : int;
  cpi_original_full_path : string list;
} [@@deriving json]

let matches_regexp name (_, re) =
  try
    let _ = Re.exec re name in
    true
  with Not_found -> false

let matches_regexps regexps (name, _) =
  List.exists (matches_regexp name) regexps

let get_session_info ~sitedata ~req previous_extension_err =
  let req_whole = req
  and ri = req.Ocsigen_extensions.request_info
  and ci = req.Ocsigen_extensions.request_config in

  let rc = Ocsigen_request.request_cache ri in
  let no_post_param, p =
    match
      Ocsigen_request.post_params ri
        ci.Ocsigen_extensions.uploaddir
        ci.Ocsigen_extensions.maxuploadfilesize
    with
    | None ->
      true, Lwt.return []
    | Some v ->
      false, v
  in
  let no_file_param, file_params =
    match
      Ocsigen_request.files ri
        ci.Ocsigen_extensions.uploaddir
        ci.Ocsigen_extensions.maxuploadfilesize
    with
    | None ->
      true, Lwt.return []
    | Some v ->
      false, v
  in

  let%lwt post_params = p in

  let (previous_tab_cookies_info, tab_cookies, post_params) =
    try
      let (tci, utc, tc) =
        Polytables.get ~table:rc ~key:tab_cookie_action_info_key
      in
      Polytables.remove ~table:rc ~key:tab_cookie_action_info_key;
      (Some (tci, utc), tc, post_params)
    with Not_found -> begin
      let tab_cookies, post_params =
        try
(* Tab cookies are found in HTTP headers,
   but also sometimes in POST params (when we do not want to do an XHR
   because we want to stop the client side process).
   It should never be both.
*)
          let (tc, pp) =
            List.assoc_remove tab_cookies_param_name post_params
          in
          let tc = [%of_json: (string * string) list] tc in
          (List.fold_left
             (fun t (k,v) -> Ocsigen_cookie_map.Map_inner.add k v t)
             Ocsigen_cookie_map.Map_inner.empty tc, pp)
          (*Marshal.from_string (Ocsigen_lib.decode tc) 0, pp*)
        with Not_found ->
          (match
             Ocsigen_request.header ri
               (Ocsigen_header.Name.of_string tab_cookies_header_name)
           with
           | Some tc ->
             let tc = [%of_json: (string * string) list] tc in
             List.fold_left
               (fun t (k,v) -> Ocsigen_cookie_map.Map_inner.add k v t)
               Ocsigen_cookie_map.Map_inner.empty
               tc,
             post_params
           | None ->
             Ocsigen_cookie_map.Map_inner.empty, post_params)
      in
      (None, tab_cookies, post_params)
      end
  in

  let cpi =
    match
      Ocsigen_request.header ri
        (Ocsigen_header.Name.of_string tab_cpi_header_name)
    with
    | Some cpi ->
      Some ([%of_json: cpi] cpi)
    | None ->
      None
  in

  let epd = lazy (
    match
      Ocsigen_request.header ri
        (Ocsigen_header.Name.of_string expecting_process_page_name)
    with
    | Some epd ->
      [%of_json: bool] epd
    | None ->
      false
  ) in

  let post_params, get_params, to_be_considered_as_get =
    let g = Ocsigen_request.get_params_flat ri in
    try
      [],
      g @ snd (List.assoc_remove
                 to_be_considered_as_get_param_name post_params),
      true
    (* It was a POST request to be considered as GET *)
    with Not_found ->
      post_params, g, false
  in


(*204FORMS* old implementation of forms with 204 and change_page_event

  let get_params, internal_form =
    try
      (snd (List.assoc_remove internal_form_full_name get_params),
       true)
    with Not_found -> (get_params, false)
  in
*)

  let get_params0 = get_params in
  let post_params0 = post_params in
  let%lwt file_params0 = file_params in
  let get_params, post_params, file_params,
      (all_get_params, all_post_params, all_file_params,
       nl_get_params, nl_post_params, nl_file_params,
       all_get_but_nl (*204FORMS*, internal_form *),
       ignored_get, ignored_post) =
    try
      (get_params,
       post_params,
       file_params0,
       Polytables.get
         ~table:(Ocsigen_request.request_cache ri)
         ~key:eliom_params_after_action)
    with Not_found ->
      let nl_get_params, get_params = split_nl_prefix_param get_params0 in
      let nl_post_params, post_params = split_nl_prefix_param post_params0 in
      let nl_file_params, file_params = split_nl_prefix_param file_params0 in
      let ignored_get, get_params =
        List.partition (matches_regexps sitedata.ignored_get_params) get_params
      in
      let ignored_post, post_params =
        List.partition (matches_regexps sitedata.ignored_post_params) post_params
      in
      let all_get_but_nl = get_params in
      get_params, post_params, file_params,
      (get_params0,
       (if no_post_param then None else Some post_params0),
       (if no_file_param then None else Some file_params0),
       nl_get_params, nl_post_params, nl_file_params,
       all_get_but_nl (*204FORMS*, internal_form *),
      ignored_get, ignored_post)
  in

  let browser_cookies =
    match
      Ocsigen_request.header ri
        (Ocsigen_header.Name.of_string cookie_substitutes_header_name)
    with
    | Some tc ->
      List.fold_left (fun t (k,v) -> Ocsigen_cookie_map.Map_inner.add k v t)
        Ocsigen_cookie_map.Map_inner.empty
        ([%of_json: (string * string) list] tc)
    | None ->
      Ocsigen_request.cookies ri
  in

  let data_cookies = getcookies false `Session datacookiename browser_cookies in
  let service_cookies = getcookies false `Session servicecookiename browser_cookies in
  let persistent_cookies = getcookies false `Session persistentcookiename browser_cookies in

  let secure_cookie_info =
    let sdata_cookies = getcookies true `Session datacookiename browser_cookies
    in
    let sservice_cookies =
      getcookies true `Session servicecookiename browser_cookies
    in
    let spersistent_cookies =
      getcookies true `Session persistentcookiename browser_cookies
    in
    (sservice_cookies, sdata_cookies, spersistent_cookies)
  in

  let naservice_info,
    (get_state, post_state),
    (get_params, other_get_params),
    na_get_params,
    post_params =
    let post_naservice_name, na_post_params =
      try
        let n, pp =
          List.assoc_remove naservice_num post_params
        in (RNa_post' n, pp)
      with Not_found ->
        try
          let n, pp =
            List.assoc_remove naservice_name post_params
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
                List.assoc_remove naservice_num get_params
              in (RNa_get' n,
                  [(naservice_num, n)],
                  (split_prefix_param na_co_param_prefix gp))
            with Not_found ->
              try
                let n, gp =
                  List.assoc_remove naservice_name get_params
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
                      List.assoc_remove
                        post_numstate_param_name post_params
                    in (RAtt_anon s, pp)
                  with
                      Not_found ->
                        try
                          let s, pp =
                            List.assoc_remove
                              post_state_param_name post_params
                          in (RAtt_named s, pp)
                        with
                            Not_found -> (RAtt_no, post_params)
                in
                let get_state, (get_params, other_get_params) =
                  try
                    let s, gp =
                      List.assoc_remove
                        get_numstate_param_name get_params
                    in ((RAtt_anon s),
                        (split_prefix_param co_param_prefix gp))
                  with Not_found ->
                    try
                      let s, gp =
                        List.assoc_remove
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
      (String.Table.fold
         (fun k a t -> if nl_is_persistent k
          then String.Table.add k a t
          else t)
         nl_get_params String.Table.empty)
  in

  let data_cookies_tab = getcookies false `Client_process datacookiename tab_cookies in
  let service_cookies_tab = getcookies false `Client_process servicecookiename tab_cookies in
  let persistent_cookies_tab = getcookies false `Client_process persistentcookiename tab_cookies in

  let secure_cookie_info_tab =
    let sdata_cookies =
      getcookies true `Client_process datacookiename tab_cookies
    in
    let sservice_cookies =
      getcookies true `Client_process servicecookiename tab_cookies
    in
    let spersistent_cookies =
      getcookies true `Client_process persistentcookiename tab_cookies
    in
    (sservice_cookies, sdata_cookies, spersistent_cookies)
  in

  let ri, sess =
(*VVV 2011/02/15 TODO: I think we'd better not change ri here.
  Keep ri for original values and use si for Eliom's values?
*)
    Ocsigen_request.update ri
      ?meth:
        (if Ocsigen_request.meth ri = `HEAD || to_be_considered_as_get then
           Some `GET
         else
           None)
      (* Here we modify ri, instead of putting service parameters in
         si.  Thus it works better after actions: the request can be
         taken by other extensions, with new parameters.  Initial
         parameters are kept in si.  *)
      ~get_params_flat:get_params
      ~post_data:(
        if no_post_param then
          None
        else
          Some (post_params, file_params)
      ),
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
     si_all_file_params= all_file_params;
     si_previous_extension_error= previous_extension_err;
     si_na_get_params= na_get_params;
     si_nl_get_params= nl_get_params;
     si_nl_post_params= nl_post_params;
     si_nl_file_params= nl_file_params;
     si_persistent_nl_get_params= persistent_nl_get_params;
     si_all_get_but_nl= all_get_but_nl;
     si_all_get_but_na_nl= lazy (remove_na_prefix_params all_get_but_nl);
     si_ignored_get_params= ignored_get;
     si_ignored_post_params= ignored_post;
     si_client_process_info= cpi;
     si_expect_process_data= epd;
(*204FORMS*     si_internal_form= internal_form; *)
    }
  in
  Lwt.return
    ({ req_whole with Ocsigen_extensions.request_info = ri }, sess,
     previous_tab_cookies_info)

type info =
    (Ocsigen_extensions.request *
       sess_info *
       tables cookie_info (* current browser cookie info *) *
       tables cookie_info (* current tab cookie info *) *
       Ocsigen_cookie_map.t (* current user tab cookies *))

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

module Persistent_cookies = struct
  (* Another table, containing the session info for each cookie *)
  (* the table contains:
     - the expiration date (by timeout), changed at each access to the table
       (float option) None -> no expiration
     - the timeout for the user (float option option) None -> see global config
       Some None -> no timeout
   *)
  (* It is lazy, because we must delay the creation of the table until
     the initialization of eliom in case we use static linking with
     sqlite backend ... *)

  type date = float
  type cookie = full_state_name * date option * timeout * perssessgrp option

  module Cookies =
    Ocsipersist.Table
      (struct let name = eliom_persistent_cookie_table end)
      (Ocsipersist.Column.String)
      (Ocsipersist.Column.Marshal (struct type t = cookie end))

  (* maps expiry dates to cookie IDs; may have superfluous entries, i.e cookies
     that will not actually expire on the given date. *)
  module Expiry_dates = struct
    include Ocsipersist.Table
              (struct let name = "eliom_persist_cookies_expiry_dates" end)
              (Ocsipersist.Column.Float)
              (Ocsipersist.Column.String)

    let add_cookie exp cookie =
      modify_opt exp @@
        function
          | None -> Some cookie
          | Some cookies_str ->
              let cookies = String.split_on_char ',' cookies_str in
              if List.mem cookie cookies
              then Some cookies_str
              else Some (cookies_str ^ "," ^ cookie)
  end

  let add cookie ((_, exp, _, _) as content) =
    Eliom_lib.Option.Lwt.iter (fun t -> Expiry_dates.add_cookie t cookie) exp
    >>= fun _ ->
    Cookies.add cookie content

  let replace_if_exists cookie ((_, exp, _, _) as content) =
    Eliom_lib.Option.Lwt.iter (fun t -> Expiry_dates.add_cookie t cookie) exp
    >>= fun _ ->
    Cookies.replace_if_exists cookie content

  let garbage_collect ~section gc_cookie =
    let now = Unix.time () in
    Expiry_dates.iter ~lt:now @@ fun date cookies ->
      Lwt_log.ign_notice_f ~section "potentially expired cookies %.0f: %s"
                                    date cookies;
      Lwt_list.iter_s gc_cookie (String.split_on_char ',' cookies) >>= fun _ ->
      Expiry_dates.remove date
end


(** removes the entry from all opened tables *)
let remove_from_all_persistent_tables key =
  (* doesn't remove entry from Persistent_cookies_expiry_dates; not a problem *)
  Persistent_cookies.Cookies.remove key >>= fun () ->
  Perstables.fold (* could be replaced by a parallel map *)
    (fun thr t -> thr >>= fun () ->
      Ocsipersist.open_table t >>= fun table ->
      Ocsipersist.remove table key >>= Lwt_unix.yield)
    return_unit
    !perstables


(**** Wrapper type shared by client/server side ***)

type 'a wrapper = 'a Eliom_wrap.wrapper

let make_wrapper f = Eliom_wrap.create_wrapper f
let empty_wrapper () = Eliom_wrap.empty_wrapper

type unwrap_id = Eliom_wrap.unwrap_id
type unwrapper = Eliom_wrap.unwrapper

let make_unwrapper = Eliom_wrap.create_unwrapper
let empty_unwrapper = Eliom_wrap.empty_unwrapper
let react_up_unwrap_id : unwrap_id = Eliom_wrap.id_of_int react_up_unwrap_id_int
let react_down_unwrap_id : unwrap_id = Eliom_wrap.id_of_int react_down_unwrap_id_int
let signal_down_unwrap_id : unwrap_id = Eliom_wrap.id_of_int signal_down_unwrap_id_int
let comet_channel_unwrap_id : unwrap_id = Eliom_wrap.id_of_int comet_channel_unwrap_id_int
let bus_unwrap_id : unwrap_id = Eliom_wrap.id_of_int bus_unwrap_id_int


(* HACK: Remove the 'nl_get_appl_parameter' used to avoid confusion
   between XHR and classical request in App. *)
let patch_request_info ({Ocsigen_extensions.request_info} as r) =
  let u = Ocsigen_request.uri request_info in
  match Uri.get_query_param u nl_get_appl_parameter with
  | Some _ ->
    { r with
      Ocsigen_extensions.request_info =
        let get_params_flat =
          List.remove_assoc nl_get_appl_parameter
            (Ocsigen_request.get_params_flat request_info)
        in
        Ocsigen_request.update ~get_params_flat request_info
    }
  | None ->
    r


let get_site_dir sitedata = sitedata.site_dir
let get_site_dir_string sitedata = sitedata.site_dir_string

(* Returns if we want secure cookie *)
let get_secure ~secure_o ~sitedata () =
  match secure_o with
  | None -> sitedata.secure_cookies
  | Some s -> s

module To_and_of_shared = struct

  (* FIXME : work-around for weak polymorphism in create :( *)
  type wrapper

  type 'a t = {
    server  : 'a to_and_of ;
    client  : 'a to_and_of Eliom_client_value.t option;
    wrapper : wrapper
  }

  let wrapper : wrapper = Obj.magic @@
    Eliom_wrap.create_wrapper @@ function
    | {client = Some tao} ->
      tao
    | {client = None} ->
      failwith
        "Cannot wrap user type parameter.\n\
         Use the ?client_to_and_of parameter of Eliom_parameter.user_type\n\
         or (Eliom_parameter.all_suffix_user)"

  let to_string {server = {to_string}} = to_string

  let of_string {server = {of_string}} = of_string

  let to_and_of {server} = server

  let create ?client_to_and_of server = {
    server ;
    client = client_to_and_of ;
    wrapper
  }

end

let client_html_file () =
  failwith "client_html_file is only defined on client"
