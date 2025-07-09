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
(** Internal functions used by Eliom:                                        *)

(** Tables of services (global and session tables,                           *)

(** persistent and volatile data tables)                                     *)

(** Store and load services                                                  *)

(*****************************************************************************)
(*****************************************************************************)

open Eliom_lib
open Ocsigen_extensions

(****************************************************************************)
let default_max_persistent_data_sessions_per_group = ref 50
let default_max_service_sessions_per_group = ref 50
let default_max_service_sessions_per_subnet = ref 1000000
let default_max_volatile_data_sessions_per_group = ref 50
let default_max_volatile_data_sessions_per_subnet = ref 1000000
let default_max_persistent_data_tab_sessions_per_group = ref 50
let default_max_service_tab_sessions_per_group = ref 50
let default_max_volatile_data_tab_sessions_per_group = ref 50
let default_secure_cookies = ref false
let default_application_script = ref (false, false)
let default_cache_global_data = ref None
let default_html_content_type = ref None
let default_ignored_get_params = ref []
let default_ignored_post_params = ref []
let default_omitpersistentstorage = ref None

(* Subnet defaults be large enough, because it must work behind a reverse proxy.

   If 1 session takes 1000 bytes (data + tables etc),
   1 million sessions take 1 GB.

   If somebody opens 1000 sessions per second,
   then it will take 1000 s (16 minutes) to reach 1000000.

   It means that regular users will have their sessions closed
   after 16 minutes of inactivity if they share their sub network with
   someone doing an attack (or if the server is behind a proxy).

   In any case, it is better to use session groups when possible.

   For persistent session, there is a limitation per session group,
   efficient only for small values.
   But there is no limitation by subnet.
   1 billion sessions take 1 TB.
   If somebody opens 1000 sessions per second,
   then it will take 1 million s (16000 minutes = 266 h = 11 days)
   to reach 1TB.
*)

let default_max_anonymous_services_per_subnet = ref 500000
let default_max_anonymous_services_per_session = ref 1000
let default_max_volatile_groups_per_site = ref 1000000
(*VVV value ??? *)

module S = Hashtbl.Make (struct
    type t = Ocsigen_extensions.virtual_hosts * Eliom_lib.Url.path

    let equal ((vh1, u1) : t) ((vh2, u2) : t) =
      Ocsigen_extensions.equal_virtual_hosts vh1 vh2 && u1 = u2

    let hash ((vh, u) : t) =
      Hashtbl.hash (Ocsigen_extensions.hash_virtual_hosts vh, u)
  end)

let create_sitedata_aux site_dir config_info =
  let dlist_table = Eliom_common.create_dlist_ip_table 100
  and group_of_groups =
    Ocsigen_cache.Dlist.create !default_max_volatile_groups_per_site
  in
  let sitedata =
    { (* One dlist for each site? *)
      Eliom_common.servtimeout = None, None, []
    ; datatimeout = None, None, []
    ; perstimeout = None, None, []
    ; site_value_table = Polytables.create ()
    ; site_dir
    ; (*VVV encode=false??? *)
      site_dir_string =
        Option.map (Eliom_lib.Url.string_of_url_path ~encode:false) site_dir
    ; config_info
    ; default_links_xhr =
        Eliom_common.tenable_value ~name:"default_links_xhr" true
    ; global_services =
        Eliom_common.empty_tables
          !default_max_anonymous_services_per_subnet
          false
    ; registered_scope_hierarchies = Eliom_common.Hier_set.empty
    ; session_services = Eliommod_cookies.new_service_cookie_table ()
    ; session_data = Eliommod_cookies.new_data_cookie_table ()
    ; group_of_groups
    ; remove_session_data = (fun _cookie -> ())
    ; not_bound_in_data_tables = (fun _cookie -> true)
    ; exn_handler = Eliommod_pagegen.def_handler
    ; unregistered_services = []
    ; unregistered_na_services = []
    ; max_service_sessions_per_group =
        !default_max_service_sessions_per_group, false
    ; max_volatile_data_sessions_per_group =
        !default_max_volatile_data_sessions_per_group, false
    ; max_persistent_data_sessions_per_group =
        Some !default_max_persistent_data_sessions_per_group, false
    ; max_service_tab_sessions_per_group =
        !default_max_service_tab_sessions_per_group, false
    ; max_volatile_data_tab_sessions_per_group =
        !default_max_volatile_data_tab_sessions_per_group, false
    ; max_persistent_data_tab_sessions_per_group =
        Some !default_max_persistent_data_tab_sessions_per_group, false
    ; max_service_sessions_per_subnet =
        !default_max_service_sessions_per_subnet, false
    ; max_volatile_data_sessions_per_subnet =
        !default_max_volatile_data_sessions_per_subnet, false
    ; max_anonymous_services_per_session =
        !default_max_anonymous_services_per_session, false
    ; max_anonymous_services_per_subnet =
        !default_max_anonymous_services_per_subnet, false
    ; secure_cookies = !default_secure_cookies
    ; dlist_ip_table = dlist_table
    ; ipv4mask = None, false
    ; ipv6mask = None, false
    ; application_script = !default_application_script
    ; cache_global_data = !default_cache_global_data
    ; html_content_type = !default_html_content_type
    ; ignored_get_params = !default_ignored_get_params
    ; ignored_post_params = !default_ignored_post_params
    ; omitpersistentstorage = !default_omitpersistentstorage }
  in
  Ocsigen_cache.Dlist.set_finaliser_after
    (fun node ->
       (* Finaliser for the session groups *)
       (* See in eliommod_sessiongroups for the finaliser of sessions *)
       let fullbrowsersessgrp = Ocsigen_cache.Dlist.value node in
       (* When removing a group from the dlist, we must close it.
          Actually, it must be the only way to close a group. *)
       (* This finaliser is almost identical to the finaliser for
          other groups, defined in Eliommod_sessiongroups. *)
       (* First we close all browser sessions in the group, by
          removing the group from its dlist: *)
       Eliommod_sessiongroups.Data.remove_group fullbrowsersessgrp;
       (* Then we remove data from group tables: *)
       match Tuple3.thd fullbrowsersessgrp with
       | Left key ->
           (* iterate on all session data tables: *)
           sitedata.Eliom_common.remove_session_data key
       | _ ->
           (* No group has been set. No group table.
                   Data associated to default (automatic) groups
                   is removed when closing associated sessions.
           *)
           ())
    group_of_groups;
  Eliommod_gc.service_session_gc sitedata;
  Eliommod_gc.data_session_gc sitedata;
  Eliommod_gc.persistent_session_gc sitedata;
  sitedata

(** We associate to each service a function server_params -> page *)
let create_sitedata, update_sitedata =
  (* We want to keep the old site data even if we reload the server.
     To do that, we keep the site data in a table *)
  let t = S.create 5 in
  ( (fun host site_dir config_info ->
      let key = host, site_dir in
      try S.find t key
      with Not_found ->
        let sitedata = create_sitedata_aux (Some site_dir) (Some config_info) in
        S.add t key sitedata; sitedata)
  , fun host site_dir sitedata ->
      let key = host, site_dir in
      S.replace t key sitedata )

(*****************************************************************************)
(* Session service table *)

(****************************************************************************)
(****************************************************************************)
(****************************************************************************)

(* The following is common to global config and site config *)
let parse_eliom_option
      ( set_volatile_timeout
      , set_data_timeout
      , set_service_timeout
      , set_persistent_timeout
      , set_max_service_sessions_per_group
      , set_max_service_sessions_per_subnet
      , set_max_data_sessions_per_group
      , set_max_data_sessions_per_subnet
      , set_max_persistent_sessions_per_group
      , set_max_service_tab_sessions_per_group
      , set_max_data_tab_sessions_per_group
      , set_max_persistent_tab_sessions_per_group
      , set_max_services_per_session
      , set_max_services_per_subnet
      , set_max_volatile_groups_per_site
      , set_secure_cookies
      , set_ipv4mask
      , set_ipv6mask
      , set_application_script
      , set_global_data_caching
      , set_html_content_type
      , set_ignored_get_params
      , set_ignored_post_params
      , set_omitpersistentstorage )
  =
  let parse_timeout_attrs tn attrs =
    let rec aux ((v, sn, ct) as res) = function
      | [] -> res
      | ("value", s) :: l -> aux (Some s, sn, ct) l
      | ("hierarchyname", sn) :: l -> aux (v, Some sn, ct) l
      | ("level", "session") :: l | ("level", "browser") :: l ->
          aux (v, sn, `Session) l
      | ("level", "clientprocess") :: l
      | ("level", "process") :: l
      | ("level", "tab") :: l ->
          aux (v, sn, `Client_process) l
      | ("level", _) :: _ ->
          raise
            (Error_in_config_file
               ("Eliom: Wrong attribute value for level in " ^ tn ^ " tag"))
      | _ ->
          raise
            (Error_in_config_file
               ("Eliom: Wrong attribute name for " ^ tn ^ " tag"))
    in
    let a, sn, ct = aux (None, None, `Session) attrs in
    let a =
      match a with
      | None ->
          raise
            (Error_in_config_file ("Eliom: Missing value for " ^ tn ^ " tag"))
      | Some "infinity" -> None
      | Some a -> (
        try Some (float_of_string a)
        with Failure _ ->
          raise
            (Error_in_config_file
               ("Eliom: Wrong attribute value for " ^ tn ^ " tag")))
    in
    let parse_scope_hierarchy = function
      | "" -> Eliom_common_base.Default_ref_hier
      | s when String.lowercase_ascii s = "default" ->
          Eliom_common_base.Default_ref_hier
      | s when String.lowercase_ascii s = "comet" ->
          Eliom_common_base.Default_comet_hier
      | s -> Eliom_common_base.User_hier s
    in
    a, Eliom_lib.Option.map parse_scope_hierarchy sn, ct
  in
  let convert_attr tag f v =
    try f v
    with Invalid_argument _ ->
      raise
        (Error_in_config_file
           (Printf.sprintf
              "Eliom: Wrong attribute value for tag %s in element cacheglobaldata"
              tag))
  in
  let parse_application_script_attrs attrs =
    let rec aux defer async attrs =
      match attrs with
      | [] -> defer, async
      | ("defer", v) :: rem ->
          aux (convert_attr "defer" bool_of_string v) async rem
      | ("async", v) :: rem ->
          aux defer (convert_attr "async" bool_of_string v) rem
      | (tag, _) :: _ ->
          raise
            (Error_in_config_file
               (Printf.sprintf
                  "Eliom: attribute %s not allowed in element applicationscript"
                  tag))
    in
    aux false false attrs
  in
  let parse_global_data_caching_attrs attrs =
    let rec aux path max_age attrs =
      match attrs with
      | [] -> Some (path, max_age)
      | ("path", p) :: rem -> aux (Eliom_lib.Url.split_path p) max_age rem
      | ("cache", v) :: rem ->
          aux path (convert_attr "cache" int_of_string v) rem
      | (tag, _) :: _ ->
          raise
            (Error_in_config_file
               (Printf.sprintf
                  "Eliom: attribute %s not allowed in element cacheglobaldata"
                  tag))
    in
    aux [] 0 attrs
  in
  function
  | Xml.Element ("volatiletimeout", attrs, []) ->
      let t, snoo, ct = parse_timeout_attrs "volatiletimeout" attrs in
      set_volatile_timeout ct snoo (t : float option)
  | Xml.Element ("datatimeout", attrs, []) ->
      let t, snoo, ct = parse_timeout_attrs "datatimeout" attrs in
      set_data_timeout ct snoo t
  | Xml.Element ("servicetimeout", attrs, []) ->
      let t, snoo, ct = parse_timeout_attrs "servicetimeout" attrs in
      set_service_timeout ct snoo t
  | Xml.Element ("persistenttimeout", attrs, []) ->
      let t, snoo, ct = parse_timeout_attrs "persistenttimeout" attrs in
      set_persistent_timeout ct snoo t
  | Xml.Element ("maxvolatilesessionspergroup", [("value", v)], []) -> (
    try
      let i = int_of_string v in
      set_max_service_sessions_per_group i;
      set_max_data_sessions_per_group i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for maxvolatilesessionspergroup tag"))
  | Xml.Element ("maxservicesessionspergroup", [("value", v)], []) -> (
    try
      let i = int_of_string v in
      set_max_service_sessions_per_group i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for maxservicesessionspergroup tag"))
  | Xml.Element ("maxdatasessionspergroup", [("value", v)], []) -> (
    try
      let i = int_of_string v in
      set_max_data_sessions_per_group i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for maxdatasessionspergroup tag"))
  | Xml.Element ("maxvolatilesessionspersubnet", [("value", v)], []) -> (
    try
      let i = int_of_string v in
      set_max_service_sessions_per_subnet i;
      set_max_data_sessions_per_subnet i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for maxvolatilesessionspersubnet tag"))
  | Xml.Element ("maxservicesessionspersubnet", [("value", v)], []) -> (
    try
      let i = int_of_string v in
      set_max_service_sessions_per_subnet i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for maxservicesessionspersubnet tag"))
  | Xml.Element ("maxdatasessionspersubnet", [("value", v)], []) -> (
    try
      let i = int_of_string v in
      set_max_data_sessions_per_subnet i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for maxdatasessionspersubnet tag"))
  | Xml.Element ("maxpersistentsessionspergroup", [("value", v)], []) -> (
    try
      let i = int_of_string v in
      set_max_persistent_sessions_per_group i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for maxpersistentsessionspergroup tag")
    )
  | Xml.Element ("maxvolatiletabsessionspergroup", [("value", v)], []) -> (
    try
      let i = int_of_string v in
      set_max_service_tab_sessions_per_group i;
      set_max_data_tab_sessions_per_group i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for maxvolatiletabsessionspergroup tag")
    )
  | Xml.Element ("maxservicetabsessionspergroup", [("value", v)], []) -> (
    try
      let i = int_of_string v in
      set_max_service_tab_sessions_per_group i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for maxservicetabsessionspergroup tag")
    )
  | Xml.Element ("maxdatatabsessionspergroup", [("value", v)], []) -> (
    try
      let i = int_of_string v in
      set_max_data_tab_sessions_per_group i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for maxdatatabsessionspergroup tag"))
  | Xml.Element ("maxpersistenttabsessionspergroup", [("value", v)], []) -> (
    try
      let i = int_of_string v in
      set_max_persistent_tab_sessions_per_group i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for maxpersistenttabsessionspergroup tag")
    )
  | Xml.Element ("maxanonymouscoservicespersession", [("value", v)], []) -> (
    try
      let i = int_of_string v in
      set_max_services_per_session i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for maxanonymouscoservicespersession tag")
    )
  | Xml.Element ("maxanonymouscoservicespersubnet", [("value", v)], []) -> (
    try
      let i = int_of_string v in
      set_max_services_per_subnet i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for maxanonymouscoservicespersubnet tag")
    )
  | Xml.Element ("maxvolatilegroupspersite", [("value", v)], []) -> (
    try
      let i = int_of_string v in
      set_max_volatile_groups_per_site i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for maxvolatilegroupspersite tag"))
  | Xml.Element ("securecookies", [("value", v)], []) -> (
    try
      let i =
        match v with "true" -> true | "false" -> false | _ -> failwith ""
      in
      set_secure_cookies i
    with Failure _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for securecookies tag"))
  | Xml.Element ("ipv4subnetmask", [("value", v)], []) -> (
    try
      let mask = int_of_string v in
      set_ipv4mask mask
    with _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for ipv4subnetmask tag"))
  | Xml.Element ("ipv6subnetmask", [("value", v)], []) -> (
    try
      let mask = int_of_string v in
      set_ipv6mask mask
    with _ ->
      raise
        (Error_in_config_file
           "Eliom: Wrong attribute value for ipv6subnetmask tag"))
  | Xml.Element ("applicationscript", attrs, []) ->
      set_application_script (parse_application_script_attrs attrs)
  | Xml.Element ("cacheglobaldata", attrs, []) ->
      set_global_data_caching (parse_global_data_caching_attrs attrs)
  | Xml.Element ("htmlcontenttype", [("value", v)], []) ->
      set_html_content_type v
  | Xml.Element ("ignoredgetparams", [("regexp", v)], []) ->
      let re = Re.seq [Re.start; Re.Pcre.re v; Re.stop] |> Re.compile in
      set_ignored_get_params (v, re)
  | Xml.Element ("ignoredpostparams", [("regexp", v)], []) ->
      let re = Re.seq [Re.start; Re.Pcre.re v; Re.stop] |> Re.compile in
      set_ignored_post_params (v, re)
  | Xml.Element ("omitpersistentstorage", attrs, tags) ->
      assert (attrs = []);
      let parse_rule = function
        | Xml.Element ("header", attrs, tags) ->
            assert (tags = []);
            let attr_name, attr_value =
              match attrs with [a] -> a | _ -> assert false
            in
            let header_name = Ocsigen_header.Name.of_string attr_name in
            let header_regexp = Re.compile @@ Re.Pcre.re attr_value in
            Eliom_common.HeaderRule (header_name, header_regexp)
        | _ -> assert false
      in
      let rules = List.map parse_rule tags in
      set_omitpersistentstorage (Some rules)
  | Xml.Element (s, _, _) ->
      raise
        (Error_in_config_file
           ("Unexpected content <" ^ s ^ "> inside eliom config"))
  | _ -> raise (Error_in_config_file "Unexpected content inside eliom config")

let parse_eliom_options f l =
  let rec aux rest = function
    | [] -> rest
    | e :: l -> (
      try parse_eliom_option f e; aux rest l
      with Error_in_config_file _ -> aux (e :: rest) l)
  in
  List.rev (aux [] l)

(*****************************************************************************)
(** Parsing global configuration for Eliommod: *)

let rec parse_global_config = function
  | [] -> ()
  | Xml.Element ("sessiongcfrequency", [("value", s)], _) :: ll ->
      (try
         let t = float_of_string s in
         Eliommod_gc.set_servicesessiongcfrequency (Some t);
         Eliommod_gc.set_datasessiongcfrequency (Some t)
       with Failure _ ->
         if s = "infinity"
         then (
           Eliommod_gc.set_servicesessiongcfrequency None;
           Eliommod_gc.set_datasessiongcfrequency None)
         else
           raise
             (Error_in_config_file "Eliom: Wrong value for <sessiongcfrequency>"));
      parse_global_config ll
  | Xml.Element ("servicesessiongcfrequency", [("value", s)], _) :: ll ->
      (try Eliommod_gc.set_servicesessiongcfrequency (Some (float_of_string s))
       with Failure _ ->
         if s = "infinity"
         then Eliommod_gc.set_servicesessiongcfrequency None
         else
           raise
             (Error_in_config_file
                "Eliom: Wrong value for <servicesessiongcfrequency>"));
      parse_global_config ll
  | Xml.Element ("datasessiongcfrequency", [("value", s)], _) :: ll ->
      (try Eliommod_gc.set_datasessiongcfrequency (Some (float_of_string s))
       with Failure _ ->
         if s = "infinity"
         then Eliommod_gc.set_datasessiongcfrequency None
         else
           raise
             (Error_in_config_file
                "Eliom: Wrong value for <datasessiongcfrequency>"));
      parse_global_config ll
  | Xml.Element ("persistentsessiongcfrequency", [("value", s)], _) :: ll ->
      (try
         Eliommod_gc.set_persistentsessiongcfrequency (Some (float_of_string s))
       with Failure _ ->
         if s = "infinity"
         then Eliommod_gc.set_persistentsessiongcfrequency None
         else
           raise
             (Error_in_config_file
                "Eliom: Wrong value for <persistentsessiongcfrequency>"));
      parse_global_config ll
  | e :: ll ->
      parse_eliom_option
        ( (fun ct sh m ->
            Eliommod_timeouts.set_default ?scope_hierarchy:sh `Data ct m;
            Eliommod_timeouts.set_default ?scope_hierarchy:sh `Service ct m)
        , (fun ct sh ->
            Eliommod_timeouts.set_default ?scope_hierarchy:sh `Data ct)
        , (fun ct sh ->
            Eliommod_timeouts.set_default ?scope_hierarchy:sh `Service ct)
        , (fun ct sh ->
            Eliommod_timeouts.set_default ?scope_hierarchy:sh `Persistent ct)
        , (fun v -> default_max_service_sessions_per_group := v)
        , (fun v -> default_max_service_sessions_per_subnet := v)
        , (fun v -> default_max_volatile_data_sessions_per_group := v)
        , (fun v -> default_max_volatile_data_sessions_per_subnet := v)
        , (fun v -> default_max_persistent_data_sessions_per_group := v)
        , (fun v -> default_max_service_tab_sessions_per_group := v)
        , (fun v -> default_max_volatile_data_tab_sessions_per_group := v)
        , (fun v -> default_max_persistent_data_tab_sessions_per_group := v)
        , (fun v -> default_max_anonymous_services_per_session := v)
        , (fun v -> default_max_anonymous_services_per_subnet := v)
        , (fun v -> default_max_volatile_groups_per_site := v)
        , (fun v -> default_secure_cookies := v)
        , (fun v -> Eliom_common.ipv4mask := v)
        , (fun v -> Eliom_common.ipv6mask := v)
        , (fun v -> default_application_script := v)
        , (fun v -> default_cache_global_data := v)
        , (fun v -> default_html_content_type := Some v)
        , (fun regexp ->
            default_ignored_get_params := regexp :: !default_ignored_get_params)
        , (fun regexp ->
            default_ignored_post_params :=
              regexp :: !default_ignored_post_params)
        , fun v -> default_omitpersistentstorage := v )
        e;
      parse_global_config ll

(*****************************************************************************)

let exception_during_eliommodule_loading = ref false

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  if !exception_during_eliommodule_loading
  then
    (* An eliom module failed with an exception. We do not check
            for the missing services, so that the exception can be correctly
            propagated by Ocsigen_extensions *)
    ()
  else
    try
      Eliom_common.verify_all_registered (Eliom_common.get_current_sitedata ());
      Eliom_common.end_current_sitedata ()
    with Eliom_common.Eliom_site_information_not_available _ -> ()
(*VVV The "try with" looks like a hack:
            end_init is called even for user config files ... but in that case,
            current_sitedata is not set ...
            It would be better to avoid calling end_init for user config files. *)

(** Function that will handle exceptions during the initialisation phase *)
let handle_init_exn = function
  | Eliom_common.Eliom_error_while_loading_site s -> s
  | Eliom_common.Eliom_duplicate_registration s ->
      "Eliom: Duplicate registration of service \"" ^ s
      ^ "\". Please correct the module."
  | Eliom_common.Eliom_there_are_unregistered_services (s, l1, l2) ->
      "Eliom: in site /"
      ^ Url.string_of_url_path ~encode:false s
      ^ " - "
      ^ (match l1 with
        | [] -> ""
        | [a] ->
            "One service or coservice has not been registered on URL /"
            ^ Url.string_of_url_path ~encode:false a
            ^ ". "
        | a :: ll ->
            let string_of = Url.string_of_url_path ~encode:false in
            "Some services or coservices have not been registered on URLs: "
            ^ List.fold_left
                (fun beg v -> beg ^ ", /" ^ string_of v)
                ("/" ^ string_of a)
                ll
            ^ ". ")
      ^ (match l2 with
        | [] -> ""
        | [Eliom_common.SNa_get' _] ->
            "One non-attached GET coservice has not been registered."
        | [Eliom_common.SNa_post' _] ->
            "One non-attached POST coservice has not been registered."
        | [Eliom_common.SNa_get_ a] ->
            "The non-attached GET service \"" ^ a
            ^ "\" has not been registered."
        | [Eliom_common.SNa_post_ a] ->
            "The non-attached POST service \"" ^ a
            ^ "\" has not been registered."
        | a :: ll ->
            let string_of = function
              | Eliom_common.SNa_void_keep | Eliom_common.SNa_void_dontkeep
              | Eliom_common.SNa_no ->
                  assert false
              | Eliom_common.SNa_get' _ -> "<GET coservice>"
              | Eliom_common.SNa_get_ n -> n ^ " (GET)"
              | Eliom_common.SNa_post' _ -> "<POST coservice>"
              | Eliom_common.SNa_post_ n -> n ^ " (POST)"
              | Eliom_common.SNa_get_csrf_safe _ -> " <GET CSRF-safe coservice>"
              | Eliom_common.SNa_post_csrf_safe _ ->
                  "<POST CSRF-safe coservice>"
            in
            "Some non-attached services or coservices have not been registered: "
            ^ List.fold_left
                (fun beg v -> beg ^ ", " ^ string_of v)
                (string_of a) ll
            ^ ".")
      ^ "\nPlease correct your modules and make sure you have linked in all the modules..."
  | Eliom_common.Eliom_site_information_not_available f ->
      "Eliom: Bad use of function \"" ^ f
      ^ "\". Must be used only during site initialisation phase (or, sometimes, also during request)."
  | Eliom_common.Eliom_page_erasing s ->
      "Eliom: You cannot create a page or directory here. " ^ s
      ^ " already exists. Please correct your modules."
  | e -> raise e

(*****************************************************************************)
(** Module loading *)

let get_sitedata =
  let r = ref String_map.empty in
  fun name ->
    try String_map.find name !r
    with Not_found ->
      let sitedata = create_sitedata_aux None None in
      r := String_map.add name sitedata !r;
      sitedata

let update_sitedata app vh site_dir conf_info =
  let sitedata = get_sitedata app in
  sitedata.Eliom_common.site_dir <- Some site_dir;
  sitedata.Eliom_common.site_dir_string <-
    Some (Eliom_lib.Url.string_of_url_path ~encode:false site_dir);
  sitedata.Eliom_common.config_info <- Some conf_info;
  update_sitedata vh site_dir sitedata;
  sitedata

let _ =
  Eliom_common.absolute_change_sitedata
    (get_sitedata (Eliom_common.get_app_name ()))

let set_app_name s =
  Eliom_common.current_app_name := s;
  Eliom_common.absolute_change_sitedata
    (get_sitedata (Eliom_common.get_app_name ()))

let site_init_ref = ref []

(** Register function for evaluation at site initialisation *)
let register_site_init e = site_init_ref := e :: !site_init_ref

let config = ref None (* None means no config file (static linking) *)
let config_in_tag = ref "" (* the parent tag of the currently handled tag *)

type module_to_load = Files of string list | Name of string

let site_init firstmodule =
  if !firstmodule
  then (
    (* I want to be able to define global client values during that phase: *)
    Eliom_syntax.set_global true;
    List.iter (fun f -> f ()) !site_init_ref;
    Eliom_syntax.set_global false;
    firstmodule := false)

let load_eliom_module _sitedata cmo_or_name parent_tag content =
  let preload () =
    config := Some content;
    config_in_tag := parent_tag
  in
  let postload () = config := Some [] in
  try
    match cmo_or_name with
    | Files cmo -> Ocsigen_loader.loadfiles preload postload true cmo
    | Name name -> Ocsigen_loader.init_module preload postload true name
  with Ocsigen_loader.Dynlink_error (n, e) ->
    raise
      (Eliom_common.Eliom_error_while_loading_site
         (Printf.sprintf "Eliom: while loading %s: %s" n
            (try handle_init_exn e with
            | Dynlink.Error err -> Dynlink.error_message err
            | e -> Printexc.to_string e)))

(*****************************************************************************)
(* If page has already been generated becauise there are several <eliom>
   tags in the same site:
*)
let gen_nothing () _ = Ocsigen_extensions.Ext_do_nothing

(*****************************************************************************)
let default_module_action _ = failwith "default_module_action"

let set_timeout
      (f :
        ?full_st_name:Eliom_common.full_state_name
        -> ?cookie_level:[< Eliom_common.cookie_level]
        -> recompute_expdates:bool
        -> bool (* override configfile *)
        -> bool (* from config file *)
        -> Eliom_common.sitedata
        -> float option
        -> unit)
      sitedata
      cookie_type
      state_hier
      v
  =
  let make_full_st_name secure state_hier =
    let scope =
      match cookie_type with
      | `Session -> `Session state_hier
      | `Client_process -> `Client_process state_hier
    in
    Eliom_common.make_full_state_name2
      (Eliom_common.get_site_dir_string sitedata)
      secure ~scope
  in
  (*VVV We set timeout for both secure and unsecure states.
Make possible to customize this? *)
  f
    ?full_st_name:(Option.map (make_full_st_name false) state_hier)
    ?cookie_level:(Some cookie_type) ~recompute_expdates:false true true
    sitedata v;
  f
    ?full_st_name:(Option.map (make_full_st_name true) state_hier)
    ?cookie_level:(Some cookie_type) ~recompute_expdates:false true true
    sitedata v

(** Parsing of config file for each site: *)
let parse_config _ hostpattern conf_info site_dir =
  (*--- if we put the following line here: *)
  let sitedata = create_sitedata hostpattern site_dir conf_info in
  (*--- then there is one service tree for each <site> *)
  (*--- (mutatis mutandis for the following line:) *)
  Eliom_common.absolute_change_sitedata sitedata;
  let firsteliomtag = ref true in
  let firstmodule = ref true in
  let eliommodulewarningdisplayed = ref false in
  let rec parse_default_links_xhr atts default_links_xhr = function
    | [] -> default_links_xhr, List.rev atts
    | ("xhr-links", str_value) :: suite ->
        let default_links_xhr =
          match str_value with
          | "yes" -> true
          | "no" -> false
          | _ ->
              raise
                (Error_in_config_file
                   ("Invalid value for attribute xhr-links: " ^ str_value))
        in
        parse_default_links_xhr atts (Some default_links_xhr) suite
    | att :: suite ->
        parse_default_links_xhr (att :: atts) default_links_xhr suite
  in
  let rec parse_module_attrs file = function
    | [] -> file
    | ("name", s) :: suite -> (
      match file with
      | None -> parse_module_attrs (Some (Name s)) suite
      | _ ->
          raise (Error_in_config_file "Duplicate attribute module in <eliom>"))
    | ("module", s) :: suite -> (
      match file with
      | None -> parse_module_attrs (Some (Files [s])) suite
      | _ ->
          raise (Error_in_config_file "Duplicate attribute module in <eliom>"))
    | ("findlib-package", s) :: suite -> (
      match file with
      | None -> (
        try parse_module_attrs (Some (Files (Ocsigen_loader.findfiles s))) suite
        with Ocsigen_loader.Findlib_error _ as e ->
          raise
            (Error_in_config_file
               (Printf.sprintf "Findlib error: %s" (Printexc.to_string e))))
      | _ ->
          raise (Error_in_config_file "Duplicate attribute module in <eliom>"))
    | (s, _) :: _ ->
        raise (Error_in_config_file ("Wrong attribute for <eliom>: " ^ s))
  in
  fun _ _parse_site -> function
    | Xml.Element ("eliommodule", atts, content) ->
        Eliom_extension.register_eliom_extension default_module_action;
        (match parse_module_attrs None atts with
        | Some file_or_name ->
            exception_during_eliommodule_loading := true;
            site_init firstmodule;
            load_eliom_module sitedata file_or_name "eliommodule" content;
            exception_during_eliommodule_loading := false
        | _ -> ());
        if Eliom_extension.get_eliom_extension () != default_module_action
        then
          Eliommod_pagegen.gen
            (Some (Eliom_extension.get_eliom_extension ()))
            sitedata
        else gen_nothing ()
    | Xml.Element ("eliom", atts, content) ->
        (*--- if we put the line "new_sitedata" here, then there is
  one service table for each <eliom> tag ...
  I think the other one is the best,
  because it corresponds to the way
  browsers manage cookies (one cookie for one site).
  Thus we can have one site in several cmo (with one session).
        *)
        let oldipv6mask = sitedata.Eliom_common.ipv6mask in
        let content =
          parse_eliom_options
            ( (fun ct snoo v ->
                set_timeout
                  (Eliommod_timeouts.set_global_ ~kind:`Data)
                  sitedata ct snoo v;
                set_timeout
                  (Eliommod_timeouts.set_global_ ~kind:`Service)
                  sitedata ct snoo v)
            , set_timeout (Eliommod_timeouts.set_global_ ~kind:`Data) sitedata
            , set_timeout
                (Eliommod_timeouts.set_global_ ~kind:`Service)
                sitedata
            , set_timeout
                (Eliommod_timeouts.set_global_ ~kind:`Persistent)
                sitedata
            , (fun v ->
                sitedata.Eliom_common.max_service_sessions_per_group <- v, true)
            , (fun v ->
                sitedata.Eliom_common.max_service_sessions_per_subnet <- v, true)
            , (fun v ->
                sitedata.Eliom_common.max_volatile_data_sessions_per_group <-
                  v, true)
            , (fun v ->
                sitedata.Eliom_common.max_volatile_data_sessions_per_subnet <-
                  v, true)
            , (fun v ->
                sitedata.Eliom_common.max_persistent_data_sessions_per_group <-
                  Some v, true)
            , (fun v ->
                sitedata.Eliom_common.max_service_tab_sessions_per_group <-
                  v, true)
            , (fun v ->
                sitedata.Eliom_common.max_volatile_data_tab_sessions_per_group <-
                  v, true)
            , (fun v ->
                sitedata.Eliom_common.max_persistent_data_tab_sessions_per_group <-
                  Some v, true)
            , (fun v ->
                sitedata.Eliom_common.max_anonymous_services_per_session <-
                  v, true)
            , (fun v ->
                sitedata.Eliom_common.max_anonymous_services_per_subnet <-
                  v, true;
                (* The global table has already been created, with old max
                   and old ipv6mask.
                   I update it, otherwise the setting has no effect
                   for this table: *)
                try
                  let dlist =
                    Eliom_common.find_dlist_ip_table
                      sitedata.Eliom_common.ipv4mask (* unused *) oldipv6mask
                      sitedata.Eliom_common.dlist_ip_table
                      Ipaddr.(V6 V6.localhost)
                  in
                  ignore (Ocsigen_cache.Dlist.set_maxsize dlist v)
                with Not_found -> ()
                (* should not occur *))
            , (fun v ->
                ignore
                  (Ocsigen_cache.Dlist.set_maxsize
                     sitedata.Eliom_common.group_of_groups v))
            , (fun v -> sitedata.Eliom_common.secure_cookies <- v)
            , (fun v -> sitedata.Eliom_common.ipv4mask <- Some v, true)
            , (fun v -> sitedata.Eliom_common.ipv6mask <- Some v, true)
            , (fun v -> sitedata.Eliom_common.application_script <- v)
            , (fun v -> sitedata.Eliom_common.cache_global_data <- v)
            , (fun v -> sitedata.Eliom_common.html_content_type <- Some v)
            , (fun regexp ->
                sitedata.Eliom_common.ignored_get_params <-
                  regexp :: sitedata.Eliom_common.ignored_get_params)
            , (fun regexp ->
                sitedata.Eliom_common.ignored_post_params <-
                  regexp :: sitedata.Eliom_common.ignored_post_params)
            , fun v -> sitedata.Eliom_common.omitpersistentstorage <- v )
            content
        in
        let default_links_xhr, atts = parse_default_links_xhr [] None atts in
        (match default_links_xhr with
        | Some default_links_xhr ->
            sitedata.Eliom_common.default_links_xhr#set ~override_tenable:true
              default_links_xhr
        | None -> ());
        Eliom_extension.register_eliom_extension default_module_action;
        (match parse_module_attrs None atts with
        | Some file_or_name ->
            exception_during_eliommodule_loading := true;
            site_init firstmodule;
            load_eliom_module sitedata file_or_name "eliom" content;
            exception_during_eliommodule_loading := false
        | _ -> ());
        (*VVV 2012/08
        It is not possible to load an eliom extension using <eliom>. Why?
        Is there a reason for this? For now I fail in that case. *)
        if Eliom_extension.get_eliom_extension () != default_module_action
        then
          raise
            (Error_in_config_file
               "Eliom extensions cannot be loaded using <eliom>. Use <eliommodule> instead.");
        (* We must generate the page only if it is the first <eliom> tag
           for that site: *)
        if !firsteliomtag
        then (
          firsteliomtag := false;
          Eliommod_pagegen.gen None sitedata)
        else (
          if not !eliommodulewarningdisplayed
          then
            Logs.warn ~src:eliom_logs_src (fun fmt ->
              fmt
                "Tag <eliom> used several times in the same site: will run Eliom only the first time. Prefer <eliommodule> to load a module, and <eliom/> without attribute only once at the position you want to generate your Eliom pages for this site.");
          eliommodulewarningdisplayed := true;
          gen_nothing ())
    | Xml.Element (t, _, _) ->
        raise (Ocsigen_extensions.Bad_config_tag_for_extension t)
    | _ -> raise (Error_in_config_file "(Eliommod extension)")

(*****************************************************************************)

(** extension registration *)
let () =
  Ocsigen_extensions.register ~name:"eliom" ~fun_site:parse_config ~end_init
    ~exn_handler:handle_init_exn ~init_fun:parse_global_config ()
