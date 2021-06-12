(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_common.mli
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

(** Low level functions for Eliom, exceptions and types. *)

open Eliom_lib

(** {2 Scopes} *)
(* those types are not available to the user, a scope must be created using
   create_..._scope functions *)
type scope_hierarchy = Eliom_common_base.scope_hierarchy

type cookie_scope = [ `Session of scope_hierarchy
                    | `Client_process of scope_hierarchy ]

type user_scope = [ `Session_group of scope_hierarchy
                  | cookie_scope ]

type scope = [ `Site
             | user_scope ]

type all_scope = [ scope
                 | `Global
                 | `Request ]

type cookie_level = [ `Session | `Client_process ]

type user_level = [ `Session_group | cookie_level ]

val cookie_scope_of_user_scope : [< user_scope ] -> [> cookie_scope ]
val cookie_level_of_user_scope : [< user_scope ] -> [> cookie_level ]
val level_of_user_scope : [< user_scope ] -> [> user_level ]

(** Eliom is using regular (browser) cookies but can also use its own
    browser tab cookies (only if you are using a client side Eliom application).

    It is possible to define Eliom references or services for one
    (browser) session, for one tab, or for one group of sessions.

    Using [`Global] scope means you want the data or service to
    be available to any client. [`Site] is limited to current sub-site
    (if you have several sites on the same server).

    If you want to restrict the visibility of an Eliom reference or
    a service:
    * to a browser session, use [~scope:Eliom_common.default_session_scope],
    * to a group of sessions, use [~scope:Eliom_common.default_group_scope],
    * to a client process, use [~scope:Eliom_common.default_process_scope].
    If you have a client side Eliom program running, and you want to restrict
    the visibility of the service to this instance of the program,
    use [~scope:Eliom_common.default_process_scope].

    You can create new scope
    hierarchies with {!Eliom_common.create_scope_hierarchy}.
    Thus it is possible to have for example several sessions that can
    be opened or closed independently. They use different cookies.

    Secure scopes are associated to secure cookies (that is, cookies sent
    by browsers only if the protocol is https).
*)

type global_scope = [`Global]
type site_scope = [`Site]
type session_group_scope = [`Session_group of scope_hierarchy]
type session_scope = [`Session of scope_hierarchy]
type client_process_scope = [`Client_process of scope_hierarchy]
type request_scope = [`Request]

val global_scope : [> global_scope ]
val site_scope : [> site_scope ]
val default_group_scope : [> session_group_scope ]
val default_session_scope : [> session_scope ]
val default_process_scope : [> client_process_scope ]
val comet_client_process_scope : [> client_process_scope ]
val request_scope : [> request_scope ]

val create_scope_hierarchy : string -> scope_hierarchy

val list_scope_hierarchies : unit -> scope_hierarchy list

(** {2 Exception and error handling} *)

(** Page not found *)
exception Eliom_404

(** Service called with wrong parameter names *)
exception Eliom_Wrong_parameter

exception Eliom_Session_expired

(** The service (GET or POST) parameters do not match expected type *)
exception Eliom_Typing_Error of (string * exn) list


(** That function cannot be used when the site information is not available,
    that is, outside a request or the initialisation phase of your Eliom module
    (while reading the configuration file).

    In particular, you cannot use the function before the configuration file
    is read for example when you are using {e static linking}.
    In that case you must
    delay the function call using {!Eliom_service.register_eliom_module}.
*)
exception Eliom_site_information_not_available of string

type full_state_name =
    user_scope * bool (* secure *) * string (* site_dir_string *)
module Full_state_name_table : Map.S with type key = full_state_name

(** If present and true in request data, it means that
    the previous coservice does not exist any more *)
val eliom_link_too_old : bool Polytables.key

(** If present in request data, means that
    the service session cookies does not exist any more.
    The string lists are the list of names of expired sessions
*)
val eliom_service_session_expired :
  (full_state_name list * full_state_name list) Polytables.key

(**/**)

(*VVV Warning: raising these exceptions will NOT send cookies!
  Do not use them inside services! *)
exception Eliom_do_redirection of string

(* Used to redirect to the suffix version of the service *)
exception Eliom_do_half_xhr_redirection of string

(** A [(v:tenable_value)] captures a value, which is available through [v#get].
    The value can be be set to by [v#set]. However, once set by [v#set
    ~override_tenable:true] it can only be overridden by further calls to [v#set
    ~override_tenable:true]. Other attempts will be ignored. *)
type 'a tenable_value = < get : 'a ; set : ?override_tenable:bool -> 'a -> unit >

(** Create a named {!type:Eliom_common.tenable_value} with the given initial
    value. The name will only be used for warnings when setting a strong value
    isn't possible. *)
val tenable_value : name:string -> 'a -> 'a tenable_value

(* Service kinds: *)
type att_key_serv =
  | SAtt_no (* regular service *)
  | SAtt_named of string (* named coservice *)
  | SAtt_anon of string (* anonymous coservice *)
  | SAtt_csrf_safe of (int * user_scope * bool option)
      (* CSRF safe anonymous coservice *)
      (* CSRF safe service registration delayed until form/link creation *)
      (* the int is an unique id,
         the user_scope is used for delayed registration
         (if the service is registered in the global table),
         the bool option is the ?secure parameter for delayed registration
         (if the service is registered in the global table) *)
  (* The following three are for non-attached coservices
     that have been attached on a service afterwards *)
  | SAtt_na_named of string
  | SAtt_na_anon of string
  | SAtt_na_csrf_safe of (int * user_scope * bool option)

type na_key_serv =
  | SNa_no (* no na information *)
  | SNa_void_keep (* void coservice that keeps GET na parameters *)
  | SNa_void_dontkeep (* void coservice that does not keep GET na parameters *)
  | SNa_get_ of string (* named *)
  | SNa_post_ of string (* named *)
  | SNa_get' of string (* anonymous *)
  | SNa_post' of string (* anonymous *)
  | SNa_get_csrf_safe of (int * user_scope * bool option)
      (* CSRF safe anonymous coservice *)
  | SNa_post_csrf_safe of (int * user_scope * bool option)
      (* CSRF safe anonymous coservice *)

(* the same, for incoming requests: *)
type att_key_req =
  | RAtt_no (* no coservice information *)
  | RAtt_named of string (* named coservice *)
  | RAtt_anon of string (* anonymous coservice *)

type na_key_req =
  | RNa_no (* no na information *)
  | RNa_get_ of string (* named *)
  | RNa_post_ of string (* named *)
  | RNa_get' of string (* anonymous *)
  | RNa_post' of string (* anonymous *)





exception Eliom_duplicate_registration of string
exception Eliom_there_are_unregistered_services of
            (string list * string list list * na_key_serv list)
exception Eliom_page_erasing of string
exception Eliom_error_while_loading_site of string

val defaultpagename : string
val eliom_suffix_name : string
val eliom_suffix_internal_name : string
val eliom_nosuffix_page : string
val naservice_num : string
val naservice_name : string
val get_state_param_name : string
val post_state_param_name : string
val get_numstate_param_name : string
val post_numstate_param_name : string
val co_param_prefix : string
val na_co_param_prefix : string
val nl_param_prefix : string
val eliom_internal_nlp_prefix : string
val pnl_param_prefix : string
val npnl_param_prefix : string
(*204FORMS* old implementation of forms with 204 and change_page_event
val internal_form_name : string
val internal_form_bool_name : string
*)

val datacookiename : string
val servicecookiename : string
val persistentcookiename : string

val appl_name_cookie_name : string
val tab_cookies_param_name : string
val to_be_considered_as_get_param_name : string
val full_xhr_redir_header : string
val half_xhr_redir_header : string

type client_process_info =  {
  cpi_ssl : bool;
  cpi_hostname : string;
  cpi_server_port : int;
  cpi_original_full_path : Url.path;
}

type sess_info = {
  si_other_get_params : (string * string) list;
  si_all_get_params : (string * string) list;
  si_all_post_params : (string * string) list option;
  si_all_file_params: (string * file_info) list option;

  si_service_session_cookies : string Full_state_name_table.t;
  si_data_session_cookies : string Full_state_name_table.t;
  si_persistent_session_cookies : string Full_state_name_table.t;
  si_secure_cookie_info:
    (string Full_state_name_table.t *
     string Full_state_name_table.t *
     string Full_state_name_table.t);

  si_service_session_cookies_tab: string Full_state_name_table.t;
  si_data_session_cookies_tab: string Full_state_name_table.t;
  si_persistent_session_cookies_tab: string Full_state_name_table.t;
  si_secure_cookie_info_tab:
    (string Full_state_name_table.t *
     string Full_state_name_table.t *
     string Full_state_name_table.t);

  si_tab_cookies: string Ocsigen_cookie_map.Map_inner.t;

  si_nonatt_info : na_key_req;
  si_state_info: (att_key_req * att_key_req);
  si_previous_extension_error : int;

  si_na_get_params: (string * string) list Lazy.t;
  si_nl_get_params: (string * string) list String.Table.t;
  si_nl_post_params: (string * string) list String.Table.t;
  si_nl_file_params: (string * file_info) list String.Table.t;
  si_persistent_nl_get_params: (string * string) list String.Table.t Lazy.t;

  si_all_get_but_na_nl: (string * string) list Lazy.t;
  si_all_get_but_nl: (string * string) list;

  si_ignored_get_params: (string * string) list;
  si_ignored_post_params: (string * string) list;

  si_client_process_info: client_process_info option;
  si_expect_process_data : bool Lazy.t;

(*204FORMS*  si_internal_form: bool; *)
}

module SessionCookies : Hashtbl.S with type key = string

(* session groups *)
type 'a sessgrp =
    (string * cookie_level
     * (string, Ipaddr.t) leftright)
    (* The full session group is the triple
       (site_dir_string, scope, session group name).
       The scope is the scope of group members (`Session by default).
       If there is no session group,
       we limit the number of sessions by IP address. *)
type perssessgrp (* the same triple, marshaled *)

val make_persistent_full_group_name :
  cookie_level:cookie_level -> string -> string option -> perssessgrp option

val getperssessgrp : perssessgrp -> 'a sessgrp

val string_of_perssessgrp : perssessgrp -> string


type 'a session_cookie = SCNo_data | SCData_session_expired | SC of 'a

type cookie_exp =
  | CENothing (* keep current browser value *)
  | CEBrowser (* ask to remove the cookie when the browser is closed *)
  | CESome of float (* date (not duration!) *)

val default_client_cookie_exp : unit -> cookie_exp

type timeout = TGlobal | TNone | TSome of float
type 'a one_service_cookie_info = {
  sc_hvalue : string;
  sc_set_value : string option;
  sc_table : 'a ref;
  sc_timeout : timeout ref;
  sc_exp : float option ref;
  sc_cookie_exp : cookie_exp ref;
  sc_session_group: cookie_level sessgrp ref (* session group *);
  mutable sc_session_group_node:string Ocsigen_cache.Dlist.node;
}
type one_data_cookie_info = {
  dc_hvalue : string;
  dc_set_value : string option;
  dc_timeout : timeout ref;
  dc_exp : float option ref;
  dc_cookie_exp : cookie_exp ref;
  dc_session_group: cookie_level sessgrp ref (* session group *);
  mutable dc_session_group_node:string Ocsigen_cache.Dlist.node;
}
type one_persistent_cookie_info = {
  pc_hvalue : string;
  pc_set_value : string option;
  pc_timeout : timeout ref;
  pc_cookie_exp : cookie_exp ref;
  pc_session_group : perssessgrp option ref;
}

type 'a cookie_info1 =
    (bool * 'a one_service_cookie_info session_cookie ref)
    Full_state_name_table.t ref *
    (bool * one_data_cookie_info session_cookie ref) Lazy.t
    Full_state_name_table.t ref *
      ((timeout * float option * perssessgrp option) option *
         one_persistent_cookie_info session_cookie ref)
    Lwt.t Lazy.t Full_state_name_table.t ref

type 'a cookie_info =
    'a cookie_info1 (* unsecure *) *
      'a cookie_info1  (* secure *)

type 'a servicecookiestablecontent =
    full_state_name * 'a * float option ref * timeout ref *
      cookie_level sessgrp ref *
      string Ocsigen_cache.Dlist.node
type 'a servicecookiestable =
    'a servicecookiestablecontent SessionCookies.t
type datacookiestablecontent =
    full_state_name * float option ref * timeout ref *
      cookie_level sessgrp ref *
      string Ocsigen_cache.Dlist.node
type datacookiestable =
    datacookiestablecontent SessionCookies.t

type meth = [`Get | `Post | `Put | `Delete | `Other]

type page_table_key = {
  key_state : att_key_serv * att_key_serv;
  key_meth  : meth
}

module NAserv_Table : Map.S with type key = na_key_serv
module Serv_Table : Map.S with type key = page_table_key

type dlist_ip_table

type anon_params_type = int

type node_ref = string

type node_info = {
  ni_id : node_ref;
  mutable ni_sent : bool;
}

module Hier_set : Set.S

type 'a dircontent = Vide | Table of 'a direlt ref String.Table.t
and 'a direlt = Dir of 'a dircontent ref | File of 'a ref

type ('params, 'result) service = {
  s_id              : anon_params_type * anon_params_type;
  mutable s_max_use : int option;
  s_expire          : (float * float ref) option;
  s_f               : bool -> 'params -> 'result Lwt.t
}

type server_params = {
  sp_request : Ocsigen_extensions.request;
  sp_si : sess_info;
  sp_sitedata : sitedata;
  sp_cookie_info : tables cookie_info;
  sp_tab_cookie_info : tables cookie_info;
  mutable sp_user_cookies: Ocsigen_cookie_map.t;
  (* cookies (un)set by the user during service *)
  mutable sp_user_tab_cookies: Ocsigen_cookie_map.t;
  mutable sp_client_appl_name: string option; (* The application name,
                                                 as sent by the browser *)
  sp_suffix : Url.path option;
  sp_full_state_name : full_state_name option;
  sp_client_process_info: client_process_info;
  (* Contains the base URL information from which the client process
     has been launched (if any). All relative links and forms will be
     created with respect to this information (if present - from
     current URL otherwise). It is taken form a client process state
     if the application has been launched before (and not timeouted on
     server side).  Otherwise, it is created and registered in a
     server side state the first time we need it.  *)
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
and sitedata = {
  site_dir : Url.path;
  site_dir_string : string;
  config_info: Ocsigen_extensions.config_info;
  default_links_xhr : bool tenable_value;

   (* Timeouts:
       - default for site (browser sessions)
       - default for site (tab sessions)
       - then default for each full state name
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

  global_services : tables;
  session_services : tables servicecookiestable;
  session_data : datacookiestable;
  group_of_groups: [ `Session_group ] sessgrp Ocsigen_cache.Dlist.t;
  (* Limitation of the number of groups per site *)
  mutable remove_session_data : string -> unit;
  mutable not_bound_in_data_tables : string -> bool;
  mutable exn_handler : exn -> Ocsigen_response.t Lwt.t;
  mutable unregistered_services : Url.path list;
  mutable unregistered_na_services : na_key_serv list;
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
  mutable secure_cookies : bool; (* Use secure cookies (default is false). *)
  dlist_ip_table : dlist_ip_table;
  mutable ipv4mask : int option * bool;
  mutable ipv6mask : int option * bool;
  mutable application_script : bool (* defer *) * bool; (* async *)
  mutable cache_global_data : (string list * int) option;
  mutable html_content_type : string option;
  mutable ignored_get_params : (string * Re.re) list;
  mutable ignored_post_params : (string * Re.re) list;
}

type 'a lazy_site_value (** lazy site values, are lazy values with
                            content available only in the context of a
                            site: the closure one time for each site (
                            requesting it ) *)

val force_lazy_site_value : 'a lazy_site_value -> 'a
val lazy_site_value_from_fun : ( unit -> 'a ) -> 'a lazy_site_value


type info =
    (Ocsigen_extensions.request * sess_info *
     tables cookie_info * tables cookie_info * Ocsigen_cookie_map.t)

exception Eliom_retry_with of info

val make_server_params :
  sitedata ->
  info ->
  Url.path option ->
  full_state_name option -> server_params
val empty_page_table : unit -> page_table
val empty_dircontent : unit -> 'a dircontent
val empty_naservice_table : unit -> naservice_table
val service_tables_are_empty : tables -> bool
val empty_tables : int -> bool -> tables
val new_service_session_tables : sitedata -> tables
val split_prefix_param :
  string -> (string * 'a) list -> (string * 'a) list * (string * 'a) list
val get_session_info :
  sitedata:sitedata ->
  req:Ocsigen_extensions.request ->
  int -> (Ocsigen_extensions.request * sess_info *
          (tables cookie_info * Ocsigen_cookie_map.t) option) Lwt.t
type ('a, 'b) foundornot = Found of 'a | Notfound of 'b

val make_full_cookie_name : string -> full_state_name -> string
val make_full_state_name :
  sp:server_params -> secure:bool -> scope:[< user_scope ] -> full_state_name
val make_full_state_name2 :
  string -> bool -> scope:[< user_scope ] -> full_state_name



module Perstables :
  sig
    val empty : 'a list
    val add : 'a -> 'a list -> 'a list
    val fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  end
val perstables : string list ref
val create_persistent_table : string -> 'a Ocsipersist.table Lwt.t

module Persistent_cookies : sig
  type date = float
  type cookie = full_state_name * date option * timeout * perssessgrp option
  module Cookies : Ocsipersist.TABLE
    with type key = string and type value = cookie
  module Expiry_dates : sig
    include Ocsipersist.TABLE with type key = date and type value = string
    val add_cookie : date -> string -> unit Lwt.t
  end
  val add : string -> cookie -> unit Lwt.t
  val replace_if_exists : string -> cookie -> unit Lwt.t
  val garbage_collect :
    section:Lwt_log_core.Section.t -> (Cookies.key -> unit Lwt.t) -> unit Lwt.t
end

val remove_from_all_persistent_tables : string -> unit Lwt.t
val absolute_change_sitedata : sitedata -> unit
val get_current_sitedata : unit -> sitedata
val end_current_sitedata : unit -> unit
val add_unregistered : sitedata -> Url.path -> unit
val add_unregistered_na : sitedata -> na_key_serv -> unit
val remove_unregistered : sitedata -> Url.path -> unit
val remove_unregistered_na : sitedata -> na_key_serv -> unit
val verify_all_registered : sitedata -> unit
val during_eliom_module_loading : unit -> bool
val begin_load_eliom_module : unit -> unit
val end_load_eliom_module : unit -> unit
val global_register_allowed : unit -> (unit -> sitedata) option

(** Get the site data, which is only available {e during the loading of eliom
    modules, and during a request.} *)
val get_site_data : unit -> sitedata


val eliom_params_after_action :
  ((string * string) list *
   (string * string) list option *
   (string * file_info) list option *
     (string * string) list String.Table.t *
     (string * string) list String.Table.t *
   (string * file_info) list String.Table.t *
     (string * string) list (*204FORMS* * bool *) *
     (string * string) list *
     (string * string) list)
  Polytables.key

val att_key_serv_of_req : att_key_req -> att_key_serv
val na_key_serv_of_req : na_key_req -> na_key_serv

val remove_naservice_table :
  naservice_table -> NAserv_Table.key -> naservice_table

val get_mask4 : sitedata -> int
val get_mask6 : sitedata -> int
val network_of_ip : Ipaddr.t -> int -> int -> Ipaddr.t
val ipv4mask : int ref
val ipv6mask : int ref

val create_dlist_ip_table : int -> dlist_ip_table
val find_dlist_ip_table :
  int option * 'a ->
  int option * 'a ->
  dlist_ip_table -> Ipaddr.t ->
  (page_table ref * page_table_key, na_key_serv)
    leftright Ocsigen_cache.Dlist.t

val get_cookie_info : server_params -> [< cookie_level ] -> tables cookie_info

val tab_cookie_action_info_key :
  (tables cookie_info *
   Ocsigen_cookie_map.t *
   string Ocsigen_cookie_map.Map_inner.t) Polytables.key

val sp_key : server_params Lwt.key
val get_sp_option : unit -> server_params option
val get_sp : unit -> server_params
val sp_of_option : server_params option -> server_params

val found_stop_key : unit Polytables.key


(**** Wrapper type shared by client/server side ***)

type 'a wrapper = 'a Eliom_wrap.wrapper

val make_wrapper : ('a -> 'b) -> 'a wrapper
val empty_wrapper : unit -> 'a wrapper

type unwrapper = Eliom_wrap.unwrapper
type unwrap_id = Eliom_wrap.unwrap_id
val make_unwrapper : unwrap_id -> unwrapper
val empty_unwrapper : unwrapper

val react_up_unwrap_id : unwrap_id
val react_down_unwrap_id : unwrap_id
val signal_down_unwrap_id : unwrap_id
val comet_channel_unwrap_id : unwrap_id
val bus_unwrap_id : unwrap_id


val nl_get_appl_parameter: string
val patch_request_info: Ocsigen_extensions.request -> Ocsigen_extensions.request

type eliom_js_page_data = {
  ejs_global_data:
    (Eliom_runtime.global_data * Eliom_wrap.unwrapper) option;
  ejs_request_data: Eliom_runtime.request_data;
  (* Event handlers *)
  ejs_event_handler_table: Eliom_runtime.RawXML.event_handler_table;
  (* Client attrib *)
  ejs_client_attrib_table: Eliom_runtime.RawXML.client_attrib_table;
  (* Session info *)
  ejs_sess_info: sess_info;
}

val get_site_dir : sitedata -> Url.path
val get_site_dir_string : sitedata -> string
val get_secure : secure_o:bool option -> sitedata:sitedata -> unit -> bool

val is_client_app : bool ref

val make_actual_path : string list -> string list

type 'a to_and_of = {
  of_string : string -> 'a;
  to_string : 'a -> string
}

module To_and_of_shared : sig

  type 'a t

  val create :
    ?client_to_and_of : 'a to_and_of Eliom_client_value.t ->
    'a to_and_of ->
    'a t

  val to_string : 'a t -> 'a -> string

  val of_string : 'a t -> string -> 'a

  val to_and_of : 'a t -> 'a to_and_of

end

(*/*)

(** Raises exception on server, only relevant for client apps *)
val client_html_file : unit -> string

val hash_cookie : string -> string
