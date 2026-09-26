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

open Lwt.Syntax
open Lib
include Common_base

exception Eliom_Session_expired

exception
  Eliom_there_are_unregistered_services of
    (string list * string list list * na_key_serv list)

exception Cannot_call_this_function_before_app_is_linked_to_a_site
exception Eliom_error_while_loading_site of string
exception Do_redirection of string

(* Backwards-compatible alias; new code should use Do_redirection. *)
exception Eliom_do_redirection = Do_redirection
exception Do_half_xhr_redirection of string

(* Backwards-compatible alias; new code should use Do_half_xhr_redirection. *)
exception Eliom_do_half_xhr_redirection = Do_half_xhr_redirection

type 'a tenable_value =
  < get : 'a ; set : ?override_tenable:bool -> 'a -> unit >

let tenable_value ~name v =
  object
    val mutable value = v
    val mutable tenable = false
    method get = value

    method set ?(override_tenable = false) v =
      if (not tenable) || override_tenable
      then (
        value <- v;
        tenable <- override_tenable)
      else
        Logs.warn ~src:eliom_logs_src (fun fmt ->
          fmt "Ignored setting tenable value %S." name)
  end

(*****************************************************************************)

let datacookiename = "eliomdatasession|"
let servicecookiename = "eliomservicesession|"

(* must not be a prefix of the following and vice versa (idem for data) *)
let persistentcookiename = "eliompersistentsession|"

(*****************************************************************************)

(** The coservice does not exist any more *)
let eliom_link_too_old : bool Polytables.key = Polytables.make_key ()

(** If present in request data,  means that
    the service session cookies does not exist any more.
    The string lists are the list of names of expired sessions
*)
let eliom_service_session_expired :
  (full_state_name list * full_state_name list) Polytables.key
  =
  Polytables.make_key ()

let found_stop_key = Polytables.make_key ()

(*****************************************************************************)

type 'a session_cookie = SCNo_data | SCData_session_expired | SC of 'a

type cookie_exp =
  | CENothing  (** nothing to set (keep current value) *)
  | CEBrowser  (** expires at browser close *)
  | CESome of float  (** expiration date *)

(* 2013-03-01 From now on, cookie expire 10 years after opening the session.
   Before, it was when the browser was closed but we think it has no sense,
   and many people do not understand why their session is closed, even if
   the session duration on server side is long.
   If you want this, you now have to set this manually.
*)
let default_client_cookie_exp () = CESome (Unix.time () +. 315532800.)

[@@@warning "-39"]

type timeout =
  | TGlobal  (** see global setting *)
  | TNone  (** explicitly set no timeout *)
  | TSome of float  (** timeout duration in seconds *)
[@@deriving json]

[@@@warning "+39"]

let timeout_of_option = function None -> TNone | Some t -> TSome t

(* A table of state data, with the scope and security of its states *)
type 'table state_table =
  {table_scope : user_scope; table_secure : bool; table : 'table}

(* The table of tables for each session. Keys are hashes of cookies or group names *)
module SessionCookies = Hashtbl.Make (struct
    type t = string

    let equal = ( = )
    let hash = Hashtbl.hash
  end)

(* keys in tables are hashes of cookie values *)
module Hashed_cookies : sig
  type t

  val sha256 : string -> string
  val hash : string -> t
  val to_string : t -> string
end = struct
  type t = string

  let sha256 c =
    let to_b64 = Cryptokit.Base64.encode_compact () in
    Cryptokit.transform_string to_b64
    @@ Cryptokit.(hash_string (Hash.sha256 ()) c)

  let hash c =
    (* To preserve compatibility, we only hash cookies that ends with an
       'H'.  This is the case for all new cookies (see Mod_cookies). *)
    if c <> "" && c.[String.length c - 1] = 'H' then sha256 c else c

  let to_string x = x
end

(* The group of a session: a named group or, for sessions that are not in a
   group, the subnet of the client (to limit the number of sessions by IP
   address) *)
type session_group = Group_name of string | Subnet of Ipaddr.t

(* A full session group: the site, the cookie level of the group members
   (`Session by default) and the group *)
type full_session_group =
  {sg_site_dir : string; sg_level : cookie_level; sg_group : session_group}

(* The parameter only documents the level of the group *)
type 'a sessgrp = full_session_group

[@@@warning "-39"]

type perssessgrp = string
(* the same information, JSON-encoded *) [@@deriving json]

(* Persistent representation of a session group. Stored on disk through
   {!perssessgrp}: a JSON-encoded value of this record. The triple form is
   {!sessgrp} but always with a [Group_name] for persistent groups, hence the
   simpler representation here. *)
type perssessgrp_payload =
  {p_site_dir_str : string; p_cookie_level : cookie_level; p_group : string}
[@@deriving json]

[@@@warning "+39"]

let make_persistent_full_group_name ~cookie_level site_dir_string = function
  | None -> None
  | Some g ->
      Some
        (Deriving_Json.to_string [%json: perssessgrp_payload]
           { p_site_dir_str = site_dir_string
           ; p_cookie_level = cookie_level
           ; p_group = g })

let getperssessgrp a : 'a sessgrp =
  match Deriving_Json.from_string [%json: perssessgrp_payload] a with
  | {p_site_dir_str; p_cookie_level; p_group} ->
      { sg_site_dir = p_site_dir_str
      ; sg_level = p_cookie_level
      ; sg_group = Group_name p_group }
  | exception Failure msg ->
      (* Old (pre-Eliom-13, Marshal-encoded) or corrupt persistent
         session-group cookie: treat it as an expired session instead of
         letting the Deriving_Json failure escape and 500 every request. *)
      Logs.info ~src:eliom_logs_src (fun fmt ->
        fmt
          "Unreadable persistent session group (pre-13 or corrupt), session expired: %s"
          msg);
      raise Eliom_Session_expired

(* Client-supplied JSON (tab cookies, client process info): a malformed value
   falls back to the default, with a debug trace to keep the request alive
   without hiding the event. *)
let of_json_or_default ~what ~default of_json s =
  try of_json s
  with Failure msg ->
    Logs.debug ~src:eliom_logs_src (fun fmt ->
      fmt "Ignored malformed %s sent by the client: %s" what msg);
    default

let string_of_perssessgrp = Fun.id

(* cookies information during page generation: *)

type 'a one_service_cookie_info =
  { (* service sessions: *)
    sc_hvalue : Hashed_cookies.t (* hash of current value *)
  ; sc_set_value : string option (* new value to set *)
  ; sc_table : 'a ref
    (* service session table
                                  ref towards cookie table
    *)
  ; sc_timeout : timeout ref
    (* user timeout -
                                  ref towards cookie table
    *)
  ; sc_exp : float option ref
    (* expiration date ref
                                  (server side) -
                                  None = never
                                  ref towards cookie table
    *)
  ; sc_cookie_exp : cookie_exp ref (* cookie expiration date to set *)
  ; sc_session_group : cookie_level sessgrp ref (* session group *)
  ; mutable sc_session_group_node : string Ocsigen_base.Cache.Dlist.node }

type one_data_cookie_info =
  { (* in memory data sessions: *)
    dc_hvalue : Hashed_cookies.t (* hash of current value *)
  ; dc_set_value : string option (* new value to set *)
  ; dc_timeout : timeout ref
    (* user timeout -
                                         ref towards cookie table
    *)
  ; dc_exp : float option ref
    (* expiration date ref (server side) -
                                         None = never
                                         ref towards cookie table
    *)
  ; dc_cookie_exp : cookie_exp ref (* cookie expiration date to set *)
  ; dc_session_group : cookie_level sessgrp ref (* session group *)
  ; mutable dc_session_group_node : string Ocsigen_base.Cache.Dlist.node }

type one_persistent_cookie_info =
  { pc_hvalue : Hashed_cookies.t (* hash of current value *)
  ; pc_set_value : string option (* new value to set *)
  ; pc_timeout : timeout ref (* user timeout *)
  ; pc_cookie_exp : cookie_exp ref (* cookie expiration date to set *)
  ; pc_session_group : perssessgrp option ref (* session group *) }

(* A persistent cookie sent by the browser, with the state of its session at
   the beginning of the request *)
type persistent_cookie_sent =
  { ps_value : string
  ; ps_timeout : timeout (* user timeout *)
  ; ps_expiry : float option (* server side expiration date, if any *)
  ; ps_group : perssessgrp option (* session group *) }

(* The state cookies of a request, for one security level. In each table,
   the key is the full state name, and the value is:
   - what the browser sent: None for a new cookie (not sent by the browser),
     the value of the cookie otherwise, with, for persistent states, the
     timeout, the (server side) expiration date and the session group at the
     beginning of the request;
   - the new state: SCNo_data means that the state has been closed,
     SCData_session_expired that the cookie has not been found in the table;
     for both, the browser is asked to remove the cookie. *)
type 'a cookie_info1 =
  { ci_service :
      (string option * 'a one_service_cookie_info session_cookie ref)
        Full_state_name_table.t
        ref
    (* Not lazy because we must check all service states at each request to
       find the services *)
  ; ci_data :
      (string option * one_data_cookie_info session_cookie ref) Lazy.t
        Full_state_name_table.t
        ref
    (* Lazy because we do not want to ask the browser to unset the cookie if
       the cookie has not been used, otherwise it is impossible to write a
       message "Your session has expired" *)
  ; ci_persistent :
      (persistent_cookie_sent option
      * one_persistent_cookie_info session_cookie ref)
        Lwt.t
        Lazy.t
        Full_state_name_table.t
        ref }

type 'a cookie_info =
  {ci_unsecure : 'a cookie_info1; ci_secure : 'a cookie_info1}

module Service_cookie = struct
  (* non persistent cookies for services *)
  type 'a t =
    { full_state_name : full_state_name
    ; session_table : 'a
    ; expiry : float option ref
    ; timeout : timeout ref
    ; session_group : cookie_level sessgrp ref
    ; session_group_node : string Ocsigen_base.Cache.Dlist.node }

  type 'a table = 'a t SessionCookies.t
  (* the table contains:
     - the table of services
     - the expiration date (by timeout), changed at each access to the table
       (float option) None -> no expiration
     - the timeout for the user (float option option) None -> see global config
       Some None -> no timeout
     - the group to which belongs the session
  *)
end

module Data_cookie = struct
  (* non persistent cookies for in-memory data *)
  type t =
    { full_state_name : full_state_name
    ; expiry : float option ref
    ; timeout : timeout ref
    ; session_group : cookie_level sessgrp ref
    ; session_group_node : string Ocsigen_base.Cache.Dlist.node }

  type table = t SessionCookies.t
end

(*****************************************************************************)
let ipv4mask = ref 16
let ipv6mask = ref 56

let network_of_ip k mask4 mask6 =
  match k with
  | Ipaddr.V4 ip -> Ipaddr.(V4 V4.Prefix.(network (make mask4 ip)))
  | Ipaddr.V6 ip -> Ipaddr.(V6 V6.Prefix.(network (make mask6 ip)))

let network_of_request r ~mask4 ~mask6 =
  match Ocsigen.Request.client_conn r with
  | `Inet (ip, _) -> network_of_ip ip mask4 mask6
  | _ -> Ipaddr.(V6 V6.localhost)

module Net_addr_Hashtbl : sig
  type key = Ipaddr.t
  type 'a t

  val create : int -> 'a t
  val add : mask4:int -> mask6:int -> 'a t -> key -> 'a -> unit
  val remove : mask4:int -> mask6:int -> 'a t -> key -> unit
  val find : mask4:int -> mask6:int -> 'a t -> key -> 'a
end =
(* keys are IP address modulo "network equivalence" *)
struct
  include Hashtbl.Make (struct
      type t = Ipaddr.t

      let equal a b = Ipaddr.compare a b = 0
      let hash = Hashtbl.hash
    end)

  let add ~mask4 ~mask6 t k v = add t (network_of_ip k mask4 mask6) v
  let remove ~mask4 ~mask6 t k = remove t (network_of_ip k mask4 mask6)
  let find ~mask4 ~mask6 t k = find t (network_of_ip k mask4 mask6)
end

module Serv_Table = Map.Make (struct
    type t = page_table_key

    let compare = compare
  end)

module NAserv_Table = Map.Make (struct
    type t = na_key_serv

    let compare = compare
  end)

type node_info = {ni_id : node_ref; mutable ni_sent : bool}

module Hier_set = String.Set

type omitpersistentstorage_rule =
  | HeaderRule of Ocsigen_http.Header.Name.t * Re.re

(* A setting, with whether it was set by the configuration file (the program
   only overrides such a setting when asked to) *)
type 'a configured = {cf_value : 'a; cf_from_config : bool}

(* [c] set to [v], unless [c] was set by the configuration file and
   [override] is false *)
let set_configured ~override c v =
  if override || not c.cf_from_config then {c with cf_value = v} else c

let configured_of_pair (cf_value, cf_from_config) = {cf_value; cf_from_config}

(* The global timeouts of a site for one kind of state: the defaults for
   browser sessions and for tabs, and the timeouts of given states *)
type site_timeouts =
  { browser_default : float option configured option
  ; tab_default : float option configured option
  ; per_state : (full_state_name * float option configured) list }

let no_site_timeouts =
  {browser_default = None; tab_default = None; per_state = []}

(* The attributes of the script tag loading the client program *)
type application_script = {defer : bool; async : bool}

type server_params =
  { sp_request : Ocsigen.Extensions.request
  ; sp_si : sess_info
  ; sp_sitedata : sitedata (* data for the whole site *)
  ; sp_cookie_info : tables cookie_info
  ; sp_tab_cookie_info : tables cookie_info
  ; mutable sp_user_cookies : Ocsigen_cookie_map.t
  ; (* cookies (un)set by the user during service *)
    mutable sp_user_tab_cookies : Ocsigen_cookie_map.t
  ; mutable sp_client_appl_name : string option
  ; (* The application name,
                                                  as sent by the browser *)
    sp_suffix : Url.path option (* suffix *)
  ; sp_full_state_name : full_state_name option
    (* the name of the session
     to which belong the service that answered
     (if it is a session service) *)
  ; sp_client_process_info : client_process_info }

and page_table = page_table_content Serv_Table.t

and page_table_content =
  [ `Ptc of
      (page_table ref * page_table_key, na_key_serv) Either.t
        Ocsigen_base.Cache.Dlist.node
        option
      * (server_params, Ocsigen.Response.t) service list ]

and naservice_table_content =
  { na_generation : int
    (** Number of reloads of sites after which the service was created *)
  ; na_max_use : int ref option
  ; na_expiry : (float * float ref) option  (** Timeout and expiration date *)
  ; na_handler : server_params -> Ocsigen.Response.t Lwt.t
  ; na_node :
      (page_table ref * page_table_key, na_key_serv) Either.t
        Ocsigen_base.Cache.Dlist.node
        option
    (** For the limitation of the number of dynamic coservices *) }

and naservice_table =
  | AEmpty
  | ATable of naservice_table_content NAserv_Table.t

and tables =
  { mutable table_services : page_table service_table list
  ; table_naservices : naservice_table ref
  ; (* ref, and not mutable field because it simpler to use
        recursively with Dir of dircontent ref *)
    (* Information for the GC: *)
    mutable table_contains_services_with_timeout : bool
  ; (* true if dircontent contains services with timeout *)
    mutable table_contains_naservices_with_timeout : bool
  ; (* true if naservice_table contains services with timeout *)
    mutable csrf_get_or_na_registration_functions :
      (sp:server_params -> string) Int.Table.t
  ; mutable csrf_post_registration_functions :
      (sp:server_params -> att_key_serv -> string) Int.Table.t
  ; (* These two table are used for CSRF safe services:
         We associate to each service unique id the function that will
         register a new anonymous coservice each time we create a link or form.
         Attached POST coservices may have both a GET and POST
         registration function. That's why there are two tables.
         The functions associated to each service may be different for
         each session. That's why we use these table, and not a field in
         the service record.
    *)
    service_dlist_add :
      ?sp:server_params
      -> (page_table ref * page_table_key, na_key_serv) Either.t
      -> (page_table ref * page_table_key, na_key_serv) Either.t
           Ocsigen_base.Cache.Dlist.node
    (* We use a dlist for limiting the number of dynamic
            anonymous coservices in each table (and avoid DoS).  There
            is one dlist for each session, and one for each IP in
            global tables.  The dlist parameter is the table and
            coservice number for attached coservices, and the
            coservice number for non-attached ones. *)
  }

and sitedata =
  { mutable site_dir : Url.path option
    (* None when statically linked 
                                           before module init*)
  ; mutable site_dir_string : string option (* idem *)
  ; mutable config_info : Ocsigen.Extensions.config_info option (* idem *)
  ; default_links_xhr : bool tenable_value
  ; (* Timeouts:
       - default for site (browser sessions)
       - default for site (tab sessions)
       - then default for each full session name
      The booleans means "has been set from config file"
    *)
    mutable servtimeout : site_timeouts
  ; mutable datatimeout : site_timeouts
  ; mutable perstimeout : site_timeouts
  ; site_value_table : Polytables.t
  ; (* table containing evaluated
                                       lazy site values *)
    mutable registered_scope_hierarchies : Hier_set.t
  ; (* All services, and state data are stored in these tables,
      for scopes session and client process.
      The scope is registered in the full session name. *)
    global_services : tables
  ; (* global service table *)
    session_services : tables Service_cookie.table
  ; (* cookie table for services (tab and browser sessions) *)
    session_data : Data_cookie.table
  ; (* cookie table for in memory session data
                                      (tab and browser sessions)
                                      contains the information about the cookie
                                      (expiration, group ...). *)
    group_of_groups : [`Session_group] sessgrp Ocsigen_base.Cache.Dlist.t
  ; (* Limitation of the number of groups per site *)
    mutable remove_session_data : string -> unit
  ; mutable not_bound_in_data_tables : string -> bool
  ; mutable exn_handler : exn -> Ocsigen.Response.t Lwt.t
  ; mutable unregistered_services : Url.path list
  ; mutable unregistered_na_services : na_key_serv list
  ; mutable max_volatile_data_sessions_per_group : int configured
  ; mutable max_volatile_data_sessions_per_subnet : int configured
  ; mutable max_volatile_data_tab_sessions_per_group : int configured
  ; mutable max_service_sessions_per_group : int configured
  ; mutable max_service_sessions_per_subnet : int configured
  ; mutable max_service_tab_sessions_per_group : int configured
  ; mutable max_persistent_data_sessions_per_group : int option configured
  ; mutable max_persistent_data_tab_sessions_per_group : int option configured
  ; mutable max_anonymous_services_per_session : int configured
  ; mutable max_anonymous_services_per_subnet : int configured
  ; mutable secure_cookies : bool
  ; dlist_ip_table : dlist_ip_table
  ; mutable ipv4mask : int option configured
  ; mutable ipv6mask : int option configured
  ; mutable application_script : application_script
  ; (* async *)
    mutable enable_wasm : bool
  ; mutable cache_global_data : (string list * int) option
  ; mutable html_content_type : string option
  ; mutable ignored_get_params : (string * Re.re) list
  ; mutable ignored_post_params : (string * Re.re) list
  ; mutable omitpersistentstorage : omitpersistentstorage_rule list option }

and dlist_ip_table =
  (page_table ref * page_table_key, na_key_serv) Either.t
    Ocsigen_base.Cache.Dlist.t
    Net_addr_Hashtbl.t

let check_initialised field =
  match field with
  | None -> raise Cannot_call_this_function_before_app_is_linked_to_a_site
  | Some a -> a

let get_site_dir sitedata = check_initialised sitedata.site_dir
let get_site_dir_string sitedata = check_initialised sitedata.site_dir_string
let get_config_info sitedata = check_initialised sitedata.config_info

let get_mask4 sitedata =
  Option.value sitedata.ipv4mask.cf_value ~default:!ipv4mask

let get_mask6 sitedata =
  Option.value sitedata.ipv6mask.cf_value ~default:!ipv6mask

let create_dlist_ip_table = Net_addr_Hashtbl.create

let find_dlist_ip_table :
   mask4:int
  -> mask6:int
  -> dlist_ip_table
  -> Ipaddr.t
  -> (page_table ref * page_table_key, na_key_serv) Either.t
       Ocsigen_base.Cache.Dlist.t
  =
  Net_addr_Hashtbl.find
(*****************************************************************************)

(*****************************************************************************)

let make_full_cookie_name cookieprefix {user_scope; secure; site_dir_str} =
  let scope_hier = scope_hierarchy_of_user_scope user_scope in
  let secure = if secure then "S|" else "|" in
  let hier1, hiername =
    match scope_hier with
    | User_hier hiername -> "||", hiername
    | Default_ref_hier -> "|ref|", ""
    | Default_comet_hier -> "|comet|", ""
  in
  String.concat "" [cookieprefix; secure; site_dir_str; hier1; hiername]

let make_full_state_name_of_sitedata ~sitedata ~secure ~(scope : [< user_scope])
  : full_state_name
  =
  (* The information in the cookie name, without the kind of session *)
  { user_scope = (scope :> user_scope)
  ; secure
  ; site_dir_str = get_site_dir_string sitedata }

let make_full_state_name ~sp ~secure ~(scope : [< user_scope]) =
  make_full_state_name_of_sitedata ~sitedata:sp.sp_sitedata ~secure ~scope

let get_cookie_info sp = function
  | `Session -> sp.sp_cookie_info
  | `Client_process -> sp.sp_tab_cookie_info

type info =
  { request : Ocsigen.Extensions.request
  ; session_info : sess_info
  ; all_cookie_info : tables cookie_info
  ; tab_cookie_info : tables cookie_info
  ; user_tab_cookies : Ocsigen_cookie_map.t }

(*****************************************************************************)

(** Create server parameters record *)
let make_server_params
      sitedata
      ({request = ri; session_info = si; _} as info)
      suffix
      full_state_name
  =
  let appl_name =
    try
      Some
        (Ocsigen_cookie_map.Map_inner.find appl_name_cookie_name
           si.si_tab_cookies)
      (* It is an XHR from the client application, or an internal form *)
    with Not_found -> None
  in
  let cpi =
    match si.si_client_process_info with
    | Some cpi -> cpi
    | None ->
        let request_info = ri.Ocsigen.Extensions.request_info in
        { cpi_ssl = Ocsigen.Request.ssl request_info
        ; cpi_hostname = Ocsigen.Extensions.get_hostname ri
        ; cpi_server_port = Ocsigen.Extensions.get_port ri
        ; cpi_original_full_path =
            Ocsigen.Request.original_full_path request_info }
  in
  { sp_request = ri
  ; sp_si = si
  ; sp_sitedata = sitedata
  ; sp_cookie_info = info.all_cookie_info
  ; sp_tab_cookie_info = info.tab_cookie_info
  ; sp_user_cookies = Ocsigen_cookie_map.empty
  ; sp_user_tab_cookies = info.user_tab_cookies
  ; sp_client_appl_name = appl_name
  ; sp_suffix = suffix
  ; sp_full_state_name = full_state_name
  ; sp_client_process_info = cpi }

let sp_key = Lwt.new_key ()
let get_sp_option () = Lwt.get sp_key

let get_sp () =
  match Lwt.get sp_key with
  | Some sp -> sp
  | None ->
      let msg =
        "This function cannot be called here because it needs information about the request or the site."
      in
      failwith @@ String.concat "\n" @@ (msg :: Common_base.backtrace_lwt 2)

let sp_of_option sp = match sp with None -> get_sp () | Some sp -> sp

(*****************************************************************************)
(* Scope registration                                                        *)
(*****************************************************************************)

let global_scope : [> global_scope] = `Global
let site_scope : [> site_scope] = `Site

let default_group_scope : [> session_group_scope] =
  `Session_group Default_ref_hier

let default_session_scope : [> session_scope] = `Session Default_ref_hier

let default_process_scope : [> client_process_scope] =
  `Client_process Default_ref_hier

let comet_client_process_scope : [> client_process_scope] =
  `Client_process Default_comet_hier

let request_scope : [> request_scope] = `Request
let registered_scope_hierarchies = ref Hier_set.empty

let register_scope_hierarchy (name : string) =
  match get_sp_option () with
  | None ->
      if Hier_set.mem name !registered_scope_hierarchies
      then
        failwith
          (Printf.sprintf "the scope hierarchy %s has already been registered"
             name)
      else
        registered_scope_hierarchies :=
          Hier_set.add name !registered_scope_hierarchies
  | Some sp ->
      if
        Hier_set.mem name !registered_scope_hierarchies
        || Hier_set.mem name sp.sp_sitedata.registered_scope_hierarchies
      then
        failwith
          (Printf.sprintf "the scope hierarchy %s has already been registered"
             name)
      else
        sp.sp_sitedata.registered_scope_hierarchies <-
          Hier_set.add name sp.sp_sitedata.registered_scope_hierarchies

let create_scope_hierarchy name : scope_hierarchy =
  register_scope_hierarchy name;
  User_hier name

let list_scope_hierarchies () =
  let sp = get_sp () in
  Default_comet_hier :: Default_ref_hier
  :: (List.map
        (fun s -> User_hier s)
        (Hier_set.elements !registered_scope_hierarchies)
     @ List.map
         (fun s -> User_hier s)
         (Hier_set.elements sp.sp_sitedata.registered_scope_hierarchies))

(*****************************************************************************)
(* The current registration directory *)
let sitedata_stack : sitedata list ref = ref []

let absolute_change_sitedata sitedata =
  sitedata_stack := sitedata :: !sitedata_stack

let get_current_sitedata () =
  match !sitedata_stack with
  | [] -> raise (Site_information_not_available "get_current_sitedata")
  | sd :: _ -> sd

let end_current_sitedata () =
  match !sitedata_stack with _ :: t -> sitedata_stack := t | [] -> ()

let has_current_sitedata () = !sitedata_stack <> []
(* Warning: these functions are used only during the initialisation
   phase, which is not threaded ... That's why it works, but ...
   it is not really clean ... public registration relies on this
   directory (defined for each site in the config file)
*)

(*****************************************************************************)
let add_unregistered sitedata a =
  sitedata.unregistered_services <- a :: sitedata.unregistered_services

let add_unregistered_na sitedata a =
  sitedata.unregistered_na_services <- a :: sitedata.unregistered_na_services

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
      raise
        (Eliom_there_are_unregistered_services (get_site_dir sitedata, l1, l2))

let global_register_allowed () =
  if Ocsigen.Extensions.during_initialisation ()
  then Some get_current_sitedata
  else None

let get_site_data () =
  match get_sp_option () with
  | Some sp -> sp.sp_sitedata
  | None ->
      if Ocsigen.Extensions.during_initialisation ()
      then get_current_sitedata ()
      else failwith "get_site_data"

(*****************************************************************************)
(* Lazy site value: each site have a different value *)
(* Evaluated values are never collected by the GC, the table always
   keeps a reference on it. *)
(* there is no test for cycles *)

type 'a lazy_site_value =
  {lazy_sv_fun : unit -> 'a; lazy_sv_key : 'a Polytables.key}

let force_lazy_site_value v =
  let sitedata =
    match get_sp_option () with
    | Some sp -> sp.sp_sitedata
    | None -> (
      match global_register_allowed () with
      | Some f -> f ()
      | None -> raise (Site_information_not_available "force_lazy_site_value"))
  in
  try Polytables.get ~table:sitedata.site_value_table ~key:v.lazy_sv_key
  with Not_found ->
    let value = v.lazy_sv_fun () in
    Polytables.set ~table:sitedata.site_value_table ~key:v.lazy_sv_key ~value;
    value

let lazy_site_value_from_fun f =
  {lazy_sv_key = Polytables.make_key (); lazy_sv_fun = f}

(*****************************************************************************)
(*****************************************************************************)
(* The table of dynamic pages for each virtual server, and naservices        *)
(* Each node contains either a list of nodes (case directory)
    or a table of "answers" (functions that will generate the page) *)

let empty_page_table () = Serv_Table.empty
let empty_naservice_table () = AEmpty

let service_tables_are_empty t =
  !(t.table_naservices) = AEmpty
  &&
  (* !(t.table_services) = [] <---- probably enough? *)
  List.for_all (fun {st_content; _} -> !st_content = Empty) t.table_services

let remove_naservice_table at k =
  match at with
  | AEmpty -> AEmpty
  | ATable t -> ATable (NAserv_Table.remove k t)

let dlist_finaliser na_table_ref node =
  (* If the node disappears from the dlist,
     we remove the service from the service table *)
  match Ocsigen_base.Cache.Dlist.value node with
  | Either.Left (page_table_ref, page_table_key) ->
      page_table_ref := Serv_Table.remove page_table_key !page_table_ref
  | Either.Right na_key_serv ->
      na_table_ref := remove_naservice_table !na_table_ref na_key_serv

let dlist_finaliser_ip sitedata ip na_table_ref node =
  dlist_finaliser na_table_ref node;
  match Ocsigen_base.Cache.Dlist.list_of node with
  | Some cl -> (
      if Ocsigen_base.Cache.Dlist.size cl = 1
      then
        try
          Net_addr_Hashtbl.remove ~mask4:(get_mask4 sitedata)
            ~mask6:(get_mask6 sitedata) sitedata.dlist_ip_table ip
        with Not_found -> ())
  | None -> ()

let add_dlist_ dlist v =
  ignore (Ocsigen_base.Cache.Dlist.add v dlist);
  match Ocsigen_base.Cache.Dlist.newest dlist with
  | Some a -> a
  | None -> assert false

let default_ip_table_key = Ipaddr.(V6 V6.localhost)

let empty_tables max forsession =
  let t1 = [] in
  let t2 = ref (empty_naservice_table ()) in
  { table_services = t1
  ; table_naservices = t2
  ; table_contains_services_with_timeout = false
  ; table_contains_naservices_with_timeout = false
  ; csrf_get_or_na_registration_functions = Int.Table.empty
  ; csrf_post_registration_functions = Int.Table.empty
  ; service_dlist_add =
      (if forsession
       then (
         let dlist = Ocsigen_base.Cache.Dlist.create max in
         Ocsigen_base.Cache.Dlist.set_finaliser_before (dlist_finaliser t2)
           dlist;
         fun ?sp:_ v -> add_dlist_ dlist v)
       else
         fun ?sp v ->
           let ip, max, sitedata =
             match sp with
             | None -> (
                 ( default_ip_table_key
                 , max
                 , match global_register_allowed () with
                   | None ->
                       failwith "global tables created outside initialisation"
                   | Some get -> get () ))
             | Some sp ->
                 let ip =
                   match
                     Ocsigen.Request.client_conn
                       sp.sp_request.Ocsigen.Extensions.request_info
                   with
                   | `Inet (ip, _) -> ip
                   | _ -> default_ip_table_key
                 in
                 ( ip
                 , sp.sp_sitedata.max_anonymous_services_per_subnet.cf_value
                 , sp.sp_sitedata )
           in
           let dlist =
             try
               Net_addr_Hashtbl.find ~mask4:(get_mask4 sitedata)
                 ~mask6:(get_mask6 sitedata) sitedata.dlist_ip_table ip
             with Not_found ->
               let dlist = Ocsigen_base.Cache.Dlist.create max in
               Net_addr_Hashtbl.add ~mask4:(get_mask4 sitedata)
                 ~mask6:(get_mask6 sitedata) sitedata.dlist_ip_table ip dlist;
               Ocsigen_base.Cache.Dlist.set_finaliser_before
                 (dlist_finaliser_ip sitedata ip t2)
                 dlist;
               dlist
           in
           add_dlist_ dlist v) }

let new_service_session_tables sitedata =
  empty_tables sitedata.max_anonymous_services_per_session.cf_value true

(*****************************************************************************)

(* The cookie name is

sessionkind|S?|sitedirstring|"ref" ou "comet" ou ""|hiername
*)

let full_state_name_of_cookie_name cookie_level cookiename =
  let _pref, cookiename = Ocsigen_base.Lib.String.sep '|' cookiename in
  let secure, cookiename = Ocsigen_base.Lib.String.sep '|' cookiename in
  let site_dir_str, cookiename = Ocsigen_base.Lib.String.sep '|' cookiename in
  let hier1, hiername = Ocsigen_base.Lib.String.sep '|' cookiename in
  let secure = secure = "S" in
  let sc_hier =
    match hier1 with
    | "" -> Common_base.User_hier hiername
    | "ref" -> Common_base.Default_ref_hier
    | "comet" -> Common_base.Default_comet_hier
    | _ -> raise Not_found
  in
  let user_scope =
    match cookie_level with
    | `Session -> `Session sc_hier
    | `Client_process -> `Client_process sc_hier
  in
  {user_scope; secure; site_dir_str}

let getcookies secure cookie_level cookienamepref cookies =
  let length = String.length cookienamepref in
  let last = length - 1 in
  Ocsigen_cookie_map.Map_inner.fold
    (fun name value beg ->
       if String.first_diff cookienamepref name 0 last = length
       then
         try
           let expcn = full_state_name_of_cookie_name cookie_level name in
           if expcn.secure = secure
           then Full_state_name_table.add expcn value beg
           else beg
         with Not_found -> beg
       else beg)
    cookies Full_state_name_table.empty

(* The state cookies of the given security and cookie level in [cookies] *)
let get_state_cookies secure cookie_level cookies =
  { service_cookies = getcookies secure cookie_level servicecookiename cookies
  ; data_cookies = getcookies secure cookie_level datacookiename cookies
  ; persistent_cookies =
      getcookies secure cookie_level persistentcookiename cookies }

(* After an action, we do not take into account actual get params,
   but these ones: *)
type params_after_action =
  { pa_all_get_params : (string * string) list
  ; pa_all_post_params : (string * string) list option
  ; pa_all_file_params : (string * file_info) list option
  ; pa_nl_get_params : (string * string) list String.Table.t
  ; pa_nl_post_params : (string * string) list String.Table.t
  ; pa_nl_file_params : (string * file_info) list String.Table.t
  ; pa_all_get_but_nl : (string * string) list
  ; pa_ignored_get_params : (string * string) list
  ; pa_ignored_post_params : (string * string) list }

let eliom_params_after_action : params_after_action Polytables.key =
  Polytables.make_key ()

(* After an action, we get tab_cookies info from rc: *)
let tab_cookie_action_info_key = Polytables.make_key ()

[@@@warning "-39"]

type cpi = client_process_info =
  { cpi_ssl : bool
  ; cpi_hostname : string
  ; cpi_server_port : int
  ; cpi_original_full_path : string list }
[@@deriving json]

[@@@warning "+39"]

let matches_regexp name (_, re) = Re.execp re name

let matches_regexps regexps (name, _) =
  List.exists (matches_regexp name) regexps

(* Decode a JSON list of (name, value) pairs into a cookie map *)
let cookie_map_of_json ~what s =
  of_json_or_default ~what ~default:[] [%of_json: (string * string) list] s
  |> List.fold_left
       (fun t (k, v) -> Ocsigen_cookie_map.Map_inner.add k v t)
       Ocsigen_cookie_map.Map_inner.empty

let get_session_info ~sitedata ~req previous_extension_err =
  let req_whole = req
  and ri = req.Ocsigen.Extensions.request_info
  and ci = req.Ocsigen.Extensions.request_config in
  let rc = Ocsigen.Request.request_cache ri in
  let no_post_param, p =
    match
      Ocsigen.Request.post_params ri ci.Ocsigen.Extensions.uploaddir
        ci.Ocsigen.Extensions.maxuploadfilesize
    with
    | None -> true, Lwt.return []
    | Some v -> false, v
  in
  let no_file_param, file_params =
    match
      Ocsigen.Request.files ri ci.Ocsigen.Extensions.uploaddir
        ci.Ocsigen.Extensions.maxuploadfilesize
    with
    | None -> true, Lwt.return []
    | Some v -> false, v
  in
  let* post_params = p in
  let previous_tab_cookies_info, tab_cookies, post_params =
    try
      let tci, utc, tc =
        Polytables.get ~table:rc ~key:tab_cookie_action_info_key
      in
      Polytables.remove ~table:rc ~key:tab_cookie_action_info_key;
      Some (tci, utc), tc, post_params
    with Not_found ->
      let tab_cookies, post_params =
        try
          (* Tab cookies are found in HTTP headers,
   but also sometimes in POST params (when we do not want to do an XHR
   because we want to stop the client side process).
   It should never be both.
          *)
          let tc, pp = List.assoc_remove tab_cookies_param_name post_params in
          cookie_map_of_json ~what:"tab cookies" tc, pp
        with Not_found -> (
          match
            Ocsigen.Request.header ri
              (Ocsigen_http.Header.Name.of_string tab_cookies_header_name)
          with
          | Some tc -> cookie_map_of_json ~what:"tab cookies" tc, post_params
          | None -> Ocsigen_cookie_map.Map_inner.empty, post_params)
      in
      None, tab_cookies, post_params
  in
  let cpi =
    match
      Ocsigen.Request.header ri
        (Ocsigen_http.Header.Name.of_string tab_cpi_header_name)
    with
    | Some cpi ->
        of_json_or_default ~what:"client process info" ~default:None
          (fun s -> Some ([%of_json: cpi] s))
          cpi
    | None -> None
  in
  let epd =
    lazy
      (match
         Ocsigen.Request.header ri
           (Ocsigen_http.Header.Name.of_string expecting_process_page_name)
       with
      | Some epd ->
          of_json_or_default ~what:"expecting-process-page flag" ~default:false
            [%of_json: bool] epd
      | None -> false)
  in
  let post_params, get_params, to_be_considered_as_get =
    let g = Ocsigen.Request.get_params_flat ri in
    try
      ( []
      , g
        @ snd (List.assoc_remove to_be_considered_as_get_param_name post_params)
      , true )
      (* It was a POST request to be considered as GET *)
    with Not_found -> post_params, g, false
  in
  let get_params0 = get_params in
  let post_params0 = post_params in
  let* file_params0 = file_params in
  let ( get_params
      , post_params
      , file_params
      , { pa_all_get_params = all_get_params
        ; pa_all_post_params = all_post_params
        ; pa_all_file_params = all_file_params
        ; pa_nl_get_params = nl_get_params
        ; pa_nl_post_params = nl_post_params
        ; pa_nl_file_params = nl_file_params
        ; pa_all_get_but_nl = all_get_but_nl
        ; pa_ignored_get_params = ignored_get
        ; pa_ignored_post_params = ignored_post } )
    =
    try
      ( get_params
      , post_params
      , file_params0
      , Polytables.get
          ~table:(Ocsigen.Request.request_cache ri)
          ~key:eliom_params_after_action )
    with Not_found ->
      let nl_get_params, get_params = split_nl_prefix_param get_params0 in
      let nl_post_params, post_params = split_nl_prefix_param post_params0 in
      let nl_file_params, file_params = split_nl_prefix_param file_params0 in
      let ignored_get, get_params =
        List.partition (matches_regexps sitedata.ignored_get_params) get_params
      in
      let ignored_post, post_params =
        List.partition
          (matches_regexps sitedata.ignored_post_params)
          post_params
      in
      let all_get_but_nl = get_params in
      ( get_params
      , post_params
      , file_params
      , { pa_all_get_params = get_params0
        ; pa_all_post_params =
            (if no_post_param then None else Some post_params0)
        ; pa_all_file_params =
            (if no_file_param then None else Some file_params0)
        ; pa_nl_get_params = nl_get_params
        ; pa_nl_post_params = nl_post_params
        ; pa_nl_file_params = nl_file_params
        ; pa_all_get_but_nl = all_get_but_nl
        ; pa_ignored_get_params = ignored_get
        ; pa_ignored_post_params = ignored_post } )
  in
  let browser_cookies =
    match
      Ocsigen.Request.header ri
        (Ocsigen_http.Header.Name.of_string cookie_substitutes_header_name)
    with
    | Some tc -> cookie_map_of_json ~what:"cookie substitutes" tc
    | None -> Ocsigen.Request.cookies ri
  in
  let state_cookies = get_state_cookies false `Session browser_cookies in
  let secure_state_cookies = get_state_cookies true `Session browser_cookies in
  let ( naservice_info
      , (get_state, post_state)
      , (get_params, other_get_params)
      , na_get_params
      , post_params )
    =
    let post_naservice_name, na_post_params =
      try
        let n, pp = List.assoc_remove naservice_num post_params in
        RNa_post' n, pp
      with Not_found -> (
        try
          let n, pp = List.assoc_remove naservice_name post_params in
          RNa_post_ n, pp
        with Not_found -> RNa_no, [])
    in
    match post_naservice_name with
    | RNa_post_ _ | RNa_post' _ ->
        (* POST non attached coservice *)
        ( post_naservice_name
        , (RAtt_no, RAtt_no)
        , ([], get_params)
        , lazy
            (try
               (try naservice_name, List.assoc naservice_name get_params
                with Not_found ->
                  naservice_num, List.assoc naservice_num get_params)
               :: fst (split_prefix_param na_co_param_prefix get_params)
             with Not_found -> [])
        , na_post_params )
    | _ -> (
        let get_naservice_name, na_name_num, (na_get_params, other_get_params) =
          try
            let n, gp = List.assoc_remove naservice_num get_params in
            ( RNa_get' n
            , [naservice_num, n]
            , split_prefix_param na_co_param_prefix gp )
          with Not_found -> (
            try
              let n, gp = List.assoc_remove naservice_name get_params in
              ( RNa_get_ n
              , [naservice_name, n]
              , split_prefix_param na_co_param_prefix gp )
            with Not_found -> RNa_no, [], ([], get_params))
        in
        match get_naservice_name with
        | RNa_get_ _ | RNa_get' _ ->
            (* GET non attached coservice *)
            ( get_naservice_name
            , (RAtt_no, RAtt_no)
            , (na_get_params, other_get_params)
            , lazy (na_name_num @ na_get_params)
            , [] )
            (* Not possible to have POST parameters
                     without naservice_num
                     if there is a GET naservice_num
            *)
        | _ ->
            let post_state, post_params =
              try
                let s, pp =
                  List.assoc_remove post_numstate_param_name post_params
                in
                RAtt_anon s, pp
              with Not_found -> (
                try
                  let s, pp =
                    List.assoc_remove post_state_param_name post_params
                  in
                  RAtt_named s, pp
                with Not_found -> RAtt_no, post_params)
            in
            let get_state, (get_params, other_get_params) =
              try
                let s, gp =
                  List.assoc_remove get_numstate_param_name get_params
                in
                RAtt_anon s, split_prefix_param co_param_prefix gp
              with Not_found -> (
                try
                  let s, gp =
                    List.assoc_remove get_state_param_name get_params
                  in
                  RAtt_named s, split_prefix_param co_param_prefix gp
                with Not_found -> RAtt_no, (get_params, []))
            in
            ( RNa_no
            , (get_state, post_state)
            , (get_params, other_get_params)
            , lazy (na_name_num @ na_get_params)
            , post_params ))
  in
  let persistent_nl_get_params =
    lazy
      (String.Table.fold
         (fun k a t -> if nl_is_persistent k then String.Table.add k a t else t)
         nl_get_params String.Table.empty)
  in
  let state_cookies_tab = get_state_cookies false `Client_process tab_cookies in
  let secure_state_cookies_tab =
    get_state_cookies true `Client_process tab_cookies
  in
  let ri, sess =
    (*VVV 2011/02/15 TODO: I think we'd better not change ri here.
  Keep ri for original values and use si for Eliom's values?
    *)
    ( Ocsigen.Request.update ri
        ?meth:
          (if Ocsigen.Request.meth ri = `HEAD || to_be_considered_as_get
           then Some `GET
           else
             None
             (* Here we modify ri, instead of putting service parameters in
         si.  Thus it works better after actions: the request can be
         taken by other extensions, with new parameters.  Initial
         parameters are kept in si.  *))
        ~get_params_flat:get_params
        ?post_data:
          (if no_post_param
           then None
           else Some (Some (post_params, file_params)))
    , { si_state_cookies = state_cookies
      ; si_secure_state_cookies = secure_state_cookies
      ; si_state_cookies_tab = state_cookies_tab
      ; si_secure_state_cookies_tab = secure_state_cookies_tab
      ; si_tab_cookies = tab_cookies
      ; si_nonatt_info = naservice_info
      ; si_state_info = get_state, post_state
      ; si_other_get_params = other_get_params
      ; si_all_get_params = all_get_params
      ; si_all_post_params = all_post_params
      ; si_all_file_params = all_file_params
      ; si_previous_extension_error = previous_extension_err
      ; si_na_get_params = na_get_params
      ; si_nl_get_params = nl_get_params
      ; si_nl_post_params = nl_post_params
      ; si_nl_file_params = nl_file_params
      ; si_persistent_nl_get_params = persistent_nl_get_params
      ; si_all_get_but_nl = all_get_but_nl
      ; si_all_get_but_na_nl = lazy (remove_na_prefix_params all_get_but_nl)
      ; si_ignored_get_params = ignored_get
      ; si_ignored_post_params = ignored_post
      ; si_client_process_info = cpi
      ; si_expect_process_data = epd } )
  in
  Lwt.return
    ( {req_whole with Ocsigen.Extensions.request_info = ri}
    , sess
    , previous_tab_cookies_info )

exception Eliom_retry_with of info

(*****************************************************************************)

module Omit_persistent_storage = struct
  let check_if_omitting_storage () =
    match get_sp_option () with
    | Some {sp_request; sp_sitedata = {omitpersistentstorage = Some rules; _}; _}
      ->
        let apply_rule = function
          | HeaderRule (header_name, regexp) -> (
            match
              Ocsigen.Request.header sp_request.Ocsigen.Extensions.request_info
                header_name
            with
            | None -> false (* no User-Agent header *)
            | Some header_value -> Re.execp regexp header_value)
        in
        List.for_all apply_rule rules
    | _ -> false

  let not_if_omitting_storage f =
    if check_if_omitting_storage () then Lwt.return_unit else f ()
end

module Ocsipersist = struct
  include Ocsipersist

  module Store_json = struct
    include Ocsipersist.Store_json

    let set pv value =
      Omit_persistent_storage.not_if_omitting_storage (fun () -> set pv value)
  end

  module Ref_json = struct
    include Ocsipersist.Ref_json

    let set r v =
      Omit_persistent_storage.not_if_omitting_storage (fun () -> set r v)
  end

  module Functorial = struct
    include Ocsipersist.Functorial

    module Table
        (T : sig
           val name : string
         end)
        (Key : COLUMN)
        (Value : COLUMN) =
    struct
      include Table (T) (Key) (Value)

      let add key value =
        Omit_persistent_storage.not_if_omitting_storage (fun () ->
          add key value)

      let remove key =
        Omit_persistent_storage.not_if_omitting_storage (fun () -> remove key)

      let replace_if_exists key value =
        Omit_persistent_storage.not_if_omitting_storage (fun () ->
          replace_if_exists key value)

      let modify_opt key f =
        Omit_persistent_storage.not_if_omitting_storage (fun () ->
          modify_opt key f)
    end
  end
end

(* keeping track of all the persistent tables *)
module Persistent_tables = struct
  let functorial_tables = ref []
  let add_functorial_table t = functorial_tables := t :: !functorial_tables

  (* Prefix of the JSON-encoded persistent tables (Eliom 13). Before Eliom
     13 these tables held Stdlib.Marshal data under their bare [name]; the
     JSON tables now use a distinct name so that the stale Marshal tables of
     the same logical name are left orphaned (never read) instead of being
     decoded as JSON. "_json_" begins with '_', hence a valid unquoted
     PostgreSQL identifier. *)
  let json_table_prefix = "_json_"

  let create_json (type a) ~name (json : a Deriving_Json.t) :
    (module Ocsipersist.TABLE with type key = string and type value = a)
    =
    let name = json_table_prefix ^ name in
    let module T =
      Ocsipersist.Functorial.Table
        (struct
          let name = name
        end)
        (Ocsipersist.Functorial.Column.String)
        (Ocsipersist.Functorial.Column.Json (struct
             type t = a

             let t = json
           end))
    in
    add_functorial_table (module T : Ocsipersist.TABLE with type key = string);
    (module T : Ocsipersist.TABLE with type key = string and type value = a)

  (** removes the entry from all opened tables *)
  let remove_key_from_all_tables key =
    (* doesn't remove entry from Persistent_cookies_expiry_dates; not a problem *)
    Lwt_list.iter_s
      (fun (module T : Ocsipersist.TABLE with type key = string) ->
         T.remove key)
      !functorial_tables

  let number_of_tables () = List.length !functorial_tables

  let number_of_table_elements () =
    Lwt_list.map_s
      (fun (module T : Ocsipersist.TABLE with type key = string) ->
         let* n = T.length () in
         Lwt.return (T.name, n))
      !functorial_tables
end

(**** Wrapper type shared by client/server side ***)

type 'a wrapper = 'a Wrap.wrapper

let make_wrapper f = Wrap.create_wrapper f
let empty_wrapper () = Wrap.empty_wrapper

type unwrap_id = Wrap.unwrap_id
type unwrapper = Wrap.unwrapper

let make_unwrapper = Wrap.create_unwrapper
let empty_unwrapper = Wrap.empty_unwrapper
let react_up_unwrap_id : unwrap_id = Wrap.id_of_int react_up_unwrap_id_int
let react_down_unwrap_id : unwrap_id = Wrap.id_of_int react_down_unwrap_id_int
let signal_down_unwrap_id : unwrap_id = Wrap.id_of_int signal_down_unwrap_id_int

let comet_channel_unwrap_id : unwrap_id =
  Wrap.id_of_int comet_channel_unwrap_id_int

let bus_unwrap_id : unwrap_id = Wrap.id_of_int bus_unwrap_id_int

(* HACK: Remove the 'nl_get_appl_parameter' used to avoid confusion
   between XHR and classical request in App. *)
let patch_request_info ({Ocsigen.Extensions.request_info; _} as r) =
  let u = Ocsigen.Request.uri request_info in
  match Uri.get_query_param u nl_get_appl_parameter with
  | Some _ ->
      { r with
        Ocsigen.Extensions.request_info =
          (let get_params_flat =
             List.remove_assoc nl_get_appl_parameter
               (Ocsigen.Request.get_params_flat request_info)
           in
           Ocsigen.Request.update ~get_params_flat request_info) }
  | None -> r

(* Returns if we want secure cookie *)
let get_secure ~secure_o ~sitedata =
  Option.value secure_o ~default:sitedata.secure_cookies

module To_and_of_shared = struct
  (* FIXME : work-around for weak polymorphism in create :( *)
  type wrapper

  type 'a t =
    { server : 'a to_and_of
    ; client : 'a to_and_of Client_value.t option
    ; wrapper : wrapper }
  [@@warning "-69"]

  let wrapper : wrapper =
    Obj.magic @@ Wrap.create_wrapper
    @@ function
    | {client = Some tao; _} -> tao
    | {client = None; _} ->
        failwith
          "Cannot wrap user type parameter.\nUse the ?client_to_and_of parameter of Parameter.user_type\nor (Parameter.all_suffix_user)"

  let to_string {server = {to_string; _}; _} = to_string
  let of_string {server = {of_string; _}; _} = of_string
  let to_and_of {server; _} = server

  let create ?client_to_and_of server =
    {server; client = client_to_and_of; wrapper}
end

let client_html_file () = failwith "client_html_file is only defined on client"
let default_app_name = "__eliom_default_app__"
let current_app_name = ref default_app_name
let get_app_name () = !current_app_name

let defer get f =
  let r = ref None in
  (match get () with
  | Some v -> r := Some (f v)
  | None ->
      Ocsigen_base.Loader.add_module_init_function (get_app_name ()) (fun () ->
        match get () with
        | Some v -> r := Some (f v)
        | None -> raise (Site_information_not_available "defer")));
  r
