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

(** Tables of services (global and session tables, persistent and
    volatile data tables); store and load services *)

let default_max_persistent_data_sessions_per_group = ref 5
let default_max_service_sessions_per_group = ref 5
let default_max_service_sessions_per_subnet = ref 1000000
let default_max_volatile_data_sessions_per_group = ref 5
let default_max_volatile_data_sessions_per_subnet = ref 1000000
let default_max_persistent_data_tab_sessions_per_group = ref 50
let default_max_service_tab_sessions_per_group = ref 50
let default_max_volatile_data_tab_sessions_per_group = ref 50
let default_secure_cookies = ref false
let default_application_script = ref (false, false)

(* Subnet defaults must be large enough, because it must work behind a
   reverse proxy.

   If 1 session takes 1000 bytes (data + tables etc), 1 million
   sessions take 1 GB.

   If somebody opens 1000 sessions per second, then it will take 1000
   seconds (16 minutes) to reach 1000000.

   It means that regular users will have their sessions closed after
   16 minutes of inactivity if they share their sub network with
   someone doing an attack (or if the server is behind a proxy).

   In any case, it is better to use session groups when possible.

   For persistent session, there is a limitation per session group,
   efficient only for small values. But there is no limitation by
   subnet. 1 billion sessions take 1 TB. If somebody opens 1000
   sessions per second, then it will take 1 million s (16000 minutes =
   266 h = 11 days) to reach 1TB. *)

let default_max_anonymous_services_per_subnet = ref 500000
let default_max_anonymous_services_per_session = ref 1000

let default_max_volatile_groups_per_site  = ref 1000000

module S =
  Hashtbl.Make(struct
    type t = Ocsigen_extensions.virtual_hosts * Eliom_lib.Url.path
    let equal (vh1, u1 : t) (vh2, u2 : t) =
      Ocsigen_extensions.equal_virtual_hosts vh1 vh2 &&
      u1 = u2
    let hash (vh, u : t) =
      Hashtbl.hash (Ocsigen_extensions.hash_virtual_hosts vh, u)
  end)

let create_sitedata site_dir config_info =
  let dlist_table = Eliom_common.create_dlist_ip_table 100
  and group_of_groups =
    Ocsigen_cache.Dlist.create !default_max_volatile_groups_per_site
  in
  let sitedata = {
    (* One dlist for each site? *)
    Eliom_common.servtimeout = None, None, [];
    datatimeout =  None, None, [];
    perstimeout =  None, None, [];
    site_value_table = Polytables.create ();
    site_dir;
    (*VVV encode=false??? *)
    site_dir_string = Eliom_lib.Url.string_of_url_path ~encode:false site_dir;
    config_info;
    default_links_xhr =
      Eliom_common.tenable_value ~name:"default_links_xhr" true;
    global_services =
      Eliom_common.empty_tables
        !default_max_anonymous_services_per_subnet
        false;
    registered_scope_hierarchies = Eliom_common.Hier_set.empty;
    session_services = Eliommod_cookies.new_service_cookie_table ();
    session_data = Eliommod_cookies.new_data_cookie_table ();
    group_of_groups;
    remove_session_data = (fun cookie -> ());
    not_bound_in_data_tables = (fun cookie -> true);
    exn_handler = Eliommod_pagegen.def_handler;
    unregistered_services = [];
    unregistered_na_services = [];
    max_service_sessions_per_group =
      !default_max_service_sessions_per_group, false;
    max_volatile_data_sessions_per_group =
      !default_max_volatile_data_sessions_per_group, false;
    max_persistent_data_sessions_per_group =
      Some !default_max_persistent_data_sessions_per_group, false;
    max_service_tab_sessions_per_group =
      !default_max_service_tab_sessions_per_group, false;
    max_volatile_data_tab_sessions_per_group =
      !default_max_volatile_data_tab_sessions_per_group, false;
    max_persistent_data_tab_sessions_per_group =
      Some !default_max_persistent_data_tab_sessions_per_group, false;
    max_service_sessions_per_subnet =
      !default_max_service_sessions_per_subnet, false;
    max_volatile_data_sessions_per_subnet =
      !default_max_volatile_data_sessions_per_subnet, false;
    max_anonymous_services_per_session =
      !default_max_anonymous_services_per_session, false;
    max_anonymous_services_per_subnet =
      !default_max_anonymous_services_per_subnet, false;
    secure_cookies = !default_secure_cookies;
    dlist_ip_table = dlist_table;
    ipv4mask = None, false;
    ipv6mask = None, false;
    application_script = !default_application_script;
  } in
  Ocsigen_cache.Dlist.set_finaliser_after
    (fun node ->
       let fullbrowsersessgrp = Ocsigen_cache.Dlist.value node in
       (* When removing a group from the dlist, we must close it.
          Actually, it must be the only way to close a group. *)
       (* This finaliser is almost identical to the finaliser for
          other groups, defined in Eliommod_sessiongroups. *)
       (* First we close all browser sessions in the group, by
          removing the group from its dlist: *)
       Eliommod_sessiongroups.Data.remove_group fullbrowsersessgrp;
       (* Then we close all group tables: *)
       sitedata.Eliom_common.remove_session_data
         (match fullbrowsersessgrp with
          | _, _, Left a -> a
          | _, _, _ -> Eliom_common.default_group_name))
    group_of_groups;
  Eliommod_gc.service_session_gc sitedata;
  Eliommod_gc.data_session_gc sitedata;
  Eliommod_gc.persistent_session_gc sitedata;
  sitedata

let create_sitedata =
  (* We want to keep the old site data even if we reload the server.
     To do that, we keep the site data in a table *)
  let t = S.create 5 in
  fun host site_dir config_info ->
    let key = host, site_dir in
    try
      S.find t key
    with Not_found ->
      let sitedata = create_sitedata site_dir config_info in
      S.add t key sitedata;
      sitedata

(* Session service table; We associate to each service a function
   server_params -> page *)

let catch_ tag f =
  (try
     f ()
   with Failure _ ->
     raise (Ocsigen_extensions.Error_in_config_file
              ("Eliom: Wrong attribute value for " ^ tag ^ " tag")))

let parse_int_tag
    (_, _, _, _,
     set_max_service_sessions_per_group,
     set_max_service_sessions_per_subnet,
     set_max_data_sessions_per_group,
     set_max_data_sessions_per_subnet,
     set_max_persistent_sessions_per_group,
     set_max_service_tab_sessions_per_group,
     set_max_data_tab_sessions_per_group,
     set_max_persistent_tab_sessions_per_group,
     set_max_services_per_session,
     set_max_services_per_subnet,
     set_max_volatile_groups_per_site,
     _, set_ipv4mask, set_ipv6mask, set_application_script)
    tag v =
  let v = catch_ tag (fun () -> int_of_string v) in
  match tag with
  | "maxvolatilesessionspergroup" ->
    set_max_service_sessions_per_group v;
    set_max_data_sessions_per_group v
  | "maxservicesessionspergroup" ->
    set_max_service_sessions_per_group v
  | "maxdatasessionspergroup" ->
    set_max_data_sessions_per_group v
  | "maxvolatilesessionspersubnet" ->
    set_max_service_sessions_per_subnet v;
    set_max_data_sessions_per_subnet v
  | "maxservicesessionspersubnet" ->
    set_max_service_sessions_per_subnet v
  | "maxdatasessionspersubnet" ->
    set_max_data_sessions_per_subnet v
  | "maxpersistentsessionspergroup" ->
    set_max_persistent_sessions_per_group v
  | "maxvolatiletabsessionspergroup" ->
    set_max_service_tab_sessions_per_group v;
    set_max_data_tab_sessions_per_group v
  | "maxservicetabsessionspergroup" ->
    set_max_service_tab_sessions_per_group v
  | "maxdatatabsessionspergroup" ->
    set_max_data_tab_sessions_per_group v
  | "maxpersistenttabsessionspergroup" ->
    set_max_persistent_tab_sessions_per_group v
  | "maxanonymouscoservicespersession" ->
    set_max_services_per_session v
  | "maxanonymouscoservicespersubnet" ->
    set_max_services_per_subnet v
  | "maxvolatilegroupspersite" ->
    set_max_volatile_groups_per_site v
  | "ipv4subnetmask" ->
    set_ipv4mask v
  | "ipv6subnetmask" ->
    set_ipv6mask v
  | tag ->
    raise (Ocsigen_extensions.Error_in_config_file
             ("Unexpected content <" ^ tag ^ "> inside eliom config"))

let parse_timeout_attrs ~global tn attrs =
  let rec aux ((v, sn, ct) as res) = function
    | [] ->
      res
    | ("value", s) :: l ->
      aux (Some s, sn, ct) l
    | ("hierarchyname", sn) :: l ->
      aux (v, Some sn, ct) l
    | ("level", ("session" | "browser")) :: l ->
      aux (v, sn, `Session) l
    | ("level", ("clientprocess" | "process" | "tab")) :: l ->
      aux (v, sn, `Client_process) l
    | ("level", _) :: l ->
      raise (Ocsigen_extensions.Error_in_config_file
               ("Eliom: Wrong attribute value for level in "^tn^" tag"))
    | _ ->
      raise (Ocsigen_extensions.Error_in_config_file
               ("Eliom: Wrong attribute name for "^tn^" tag"))
  in
  let (a, sn, ct) = aux (None, None, `Session) attrs in
  let a =
    match a with
    | None ->
      raise (Ocsigen_extensions.Error_in_config_file
               ("Eliom: Missing value for "^tn^" tag"))
    | Some "infinity" ->
      None
    | Some a ->
      catch_ tn @@ fun () -> Some (float_of_string a)
  in
  if not global || sn = None then
    a,
    (match sn with
     | None -> None (* all hierarchies *)
     | Some "" -> Some None (* default hierarchy *)
     | c -> Some c),
    ct
  else
    raise (Ocsigen_extensions.Error_in_config_file
             ("Eliom: hierarchyname attribute not allowed for "^ tn ^
              " tag in global configuration"))


let convert_attr tag f v =
  try
    f v
  with Invalid_argument _ ->
    raise
      (Ocsigen_extensions.Error_in_config_file
         (Printf.sprintf
            "Eliom: Wrong attribute value for tag %s \
             in element cacheglobaldata"
            tag))

let parse_application_script_attrs attrs =
  let rec aux defer async = function
    | [] -> (defer, async)
    | ("defer", v) :: rem ->
      aux (convert_attr "defer" bool_of_string v) async rem
    | ("async", v) :: rem ->
      aux defer (convert_attr "async" bool_of_string v) rem
    | (tag, _) :: _ ->
      raise
        (Ocsigen_extensions.Error_in_config_file
           (Printf.sprintf
              "Eliom: attribute %s not allowed in element applicationscript"
              tag))
  in
  aux false false attrs

(* The following is common to global config and site config *)
let parse_eliom_option ~global
    ((set_volatile_timeout,
      set_data_timeout,
      set_service_timeout,
      set_persistent_timeout,
      _, _, _, _, _, _, _, _, _, _, _,
      set_secure_cookies,
      _, _, set_application_script) as setters) =
  function
  | Xml.Element ("volatiletimeout" as s, attrs, []) ->
    let t, snoo, ct = parse_timeout_attrs ~global s attrs in
    set_volatile_timeout ct snoo (t : float option)
  | Xml.Element ("datatimeout" as s, attrs, []) ->
    let t, snoo, ct = parse_timeout_attrs ~global s attrs in
    set_data_timeout ct snoo t
  | Xml.Element ("servicetimeout" as s, attrs, []) ->
    let t, snoo, ct = parse_timeout_attrs ~global s attrs in
    set_service_timeout ct snoo t
  | Xml.Element ("persistenttimeout" as s, attrs, []) ->
    let t, snoo, ct = parse_timeout_attrs ~global s attrs in
    set_persistent_timeout ct snoo t
  | Xml.Element ("securecookies" as s, [("value", v)], []) ->
    catch_ s @@ fun () ->
    set_secure_cookies @@ (match v with
      | "true" -> true
      | "false" -> false
      | _ -> failwith "")
  | Xml.Element (s, [("value", v)], []) ->
    parse_int_tag setters s v
  | Xml.Element ("applicationscript", attrs, []) ->
    set_application_script (parse_application_script_attrs attrs)
  | Xml.Element (s, _, _) ->
    raise (Ocsigen_extensions.Error_in_config_file
             ("Unexpected content <"^s^"> inside eliom config"))
  | _ ->
    raise (Ocsigen_extensions.Error_in_config_file
             ("Unexpected content inside eliom config"))

let parse_eliom_options f l =
  let rec aux rest = function
    | [] ->
      rest
    | e :: l ->
      try
        parse_eliom_option ~global:false f e;
        aux rest l
      with Ocsigen_extensions.Error_in_config_file _ ->
        aux (e :: rest) l
  in
  List.rev (aux [] l)

let rec parse_global_config = function
  | [] -> ()
  | Xml.Element ("sessiongcfrequency", [("value", s)], p) :: ll ->
    (try
       let t = float_of_string s in
       Eliommod_gc.set_servicesessiongcfrequency (Some t);
       Eliommod_gc.set_datasessiongcfrequency (Some t)
     with Failure _ ->
       if s = "infinity"
       then begin
         Eliommod_gc.set_servicesessiongcfrequency None;
         Eliommod_gc.set_datasessiongcfrequency None
       end
       else
         raise (Ocsigen_extensions.Error_in_config_file
                  "Eliom: Wrong value for <sessiongcfrequency>"));
    parse_global_config ll
  | Xml.Element ("servicesessiongcfrequency", [("value", s)], p) :: ll ->
    (try
       Eliommod_gc.set_servicesessiongcfrequency (Some (float_of_string s))
     with Failure _ ->
       if s = "infinity"
       then Eliommod_gc.set_servicesessiongcfrequency None
       else
         raise (Ocsigen_extensions.Error_in_config_file
                  "Eliom: Wrong value for <servicesessiongcfrequency>"));
    parse_global_config ll
  | Xml.Element ("datasessiongcfrequency", [("value", s)], p) :: ll ->
    (try
       Eliommod_gc.set_datasessiongcfrequency (Some (float_of_string s))
     with Failure _ ->
       if s = "infinity"
       then Eliommod_gc.set_datasessiongcfrequency None
       else
         raise (Ocsigen_extensions.Error_in_config_file
                  "Eliom: Wrong value for <datasessiongcfrequency>"));
    parse_global_config ll
  | Xml.Element ("persistentsessiongcfrequency", [("value", s)], p) :: ll ->
    (try
       Eliommod_gc.set_persistentsessiongcfrequency
         (Some (float_of_string s))
     with Failure _ ->
       if s = "infinity"
       then Eliommod_gc.set_persistentsessiongcfrequency None
       else
         raise (Ocsigen_extensions.Error_in_config_file
                  "Eliom: Wrong value for <persistentsessiongcfrequency>"));
    parse_global_config ll
  | e :: ll ->
    parse_eliom_option
      ~global:true
      ((fun ct _ m ->
         Eliommod_timeouts.set_default `Data ct m;
         Eliommod_timeouts.set_default `Service ct m),
       (fun ct _ -> Eliommod_timeouts.set_default `Data ct),
       (fun ct _ -> Eliommod_timeouts.set_default `Service ct),
       (fun ct _ -> Eliommod_timeouts.set_default `Persistent ct),
       (:=) default_max_service_sessions_per_group,
       (:=) default_max_service_sessions_per_subnet,
       (:=) default_max_volatile_data_sessions_per_group,
       (:=) default_max_volatile_data_sessions_per_subnet,
       (:=) default_max_persistent_data_sessions_per_group,
       (:=) default_max_service_tab_sessions_per_group,
       (:=) default_max_volatile_data_tab_sessions_per_group,
       (:=) default_max_persistent_data_tab_sessions_per_group,
       (:=) default_max_anonymous_services_per_session,
       (:=) default_max_anonymous_services_per_subnet,
       (:=) default_max_volatile_groups_per_site,
       (:=) default_secure_cookies,
       (:=) Eliom_common.ipv4mask,
       (:=) Eliom_common.ipv6mask,
       (:=) default_application_script)
      e;
    parse_global_config ll

let exception_during_eliommodule_loading = ref false

let end_init () =
  if !exception_during_eliommodule_loading then
    (* An eliom module failed with an exception. We do not check for
       the missing services, so that the exception can be correctly
       propagated by Ocsigen_extensions *)
    ()
  else
    try
      Eliom_common.verify_all_registered
        (Eliom_common.get_current_sitedata ());
      Eliom_common.end_current_sitedata ()
    with Eliom_common.Eliom_site_information_not_available _ -> ()
(*VVV The "try with" looks like a hack: end_init is called even for
  user config files... but in that case, current_sitedata is not
  set...  It would be better to avoid calling end_init for user config
  files. *)

let handle_init_exn = function
  | Eliom_common.Eliom_error_while_loading_site s -> s
  | Eliom_common.Eliom_duplicate_registration s ->
    ("Eliom: Duplicate registration of service \""^s^
     "\". Please correct the module.")
  | Eliom_common.Eliom_there_are_unregistered_services (s, l1, l2) ->
    ("Eliom: in site /"^
     (Eliom_lib.Url.string_of_url_path ~encode:false s)^" - "^
     (match l1 with
      | [] -> ""
      | [a] -> "A service or coservice has not been registered on URL /"
               ^ Eliom_lib.Url.string_of_url_path ~encode:false a ^ ". "
      | a :: ll ->
        let string_of = Eliom_lib.Url.string_of_url_path ~encode:false in
        "Some services or coservices have not been registered \
         on URLs: "^
        (List.fold_left
           (fun beg v -> beg^", /"^(string_of v))
           ("/"^(string_of a))
           ll
        )^". ")^
     (match l2 with
      | [] -> ""
      | [Eliom_common.SNa_get' _] ->
        "A non-attached GET coservice has not been registered."
      | [Eliom_common.SNa_post' _] ->
        "A non-attached POST coservice has not been registered."
      | [Eliom_common.SNa_get_ a] ->
        "The non-attached GET service \"" ^ a ^
        "\" has not been registered."
      | [Eliom_common.SNa_post_ a] ->
        "The non-attached POST service \"" ^ a ^
        "\" has not been registered."
      | a :: ll ->
        let string_of = function
          | Eliom_common.SNa_void_keep
          | Eliom_common.SNa_void_dontkeep
          | Eliom_common.SNa_no -> assert false
          | Eliom_common.SNa_get' _ -> "<GET coservice>"
          | Eliom_common.SNa_get_ n -> n^" (GET)"
          | Eliom_common.SNa_post' _ -> "<POST coservice>"
          | Eliom_common.SNa_post_ n -> n^" (POST)"
          | Eliom_common.SNa_get_csrf_safe _ -> " <GET CSRF-safe coservice>"
          | Eliom_common.SNa_post_csrf_safe _ -> "<POST CSRF-safe coservice>"
        in
        "Some non-attached services or coservices have not been registered: "^
        (List.fold_left
           (fun beg v -> beg^", "^(string_of v))
           (string_of a)
           ll
        )^".")^
     "\nPlease correct your modules and make sure you have linked in \
      all the modules...")
  | Eliom_common.Eliom_site_information_not_available f ->
    ("Eliom: Bad use of function \""^f^
     "\". Must be used only during site intialisation phase \
      (or, sometimes, also during request).")
  | Eliom_common.Eliom_page_erasing s ->
    ("Eliom: You cannot create a page or directory here. "^s^
     " already exists. Please correct your modules.")
  | e -> raise e

let site_init_ref = ref []

let register_site_init e = site_init_ref := e:: !site_init_ref

let config = ref []
let config_in_tag = ref "" (* the parent tag of the currently handled tag *)

type module_to_load = Files of string list | Name of string

let preload () =
  Eliom_common.begin_load_eliom_module ();
  (* I want to be able to define global client values during that
     phase *)
  Eliom_syntax.set_global true;
  List.iter (fun f -> f ()) !site_init_ref;
  Eliom_syntax.set_global false

let load_eliom_module sitedata cmo_or_name parent_tag content =
  let preload () =
    config := content;
    config_in_tag := parent_tag;
    preload ()
  and postload () =
    Eliom_common.end_load_eliom_module ();
    config := []
  in
  try
    match cmo_or_name with
    | Files cmo -> Ocsigen_loader.loadfiles preload postload true cmo
    | Name name -> Ocsigen_loader.init_module preload postload true name
  with
  | Ocsigen_loader.Dynlink_error (n, e) ->
    raise
      (Eliom_common.Eliom_error_while_loading_site
         (Printf.sprintf "Eliom: while loading %s: %s" n
            (try handle_init_exn e with
             | Dynlink.Error err -> Dynlink.error_message err
             | e -> Printexc.to_string e)))


(* If page has already been generated becauise there are several
   <eliom> tags in the same site: *)
let gen_nothing () _ = Lwt.return Ocsigen_extensions.Ext_do_nothing

let default_module_action _ = failwith "default_module_action"

let rec parse_module_attrs file = function
  | [] -> file
  | ("name", s) :: rest ->
    (match file with
     | None -> parse_module_attrs (Some (Name s)) rest
     | _ ->
       raise (Ocsigen_extensions.Error_in_config_file
                ("Duplicate attribute module in <eliom>")))
  | ("module", s) :: rest ->
    (match file with
     | None -> parse_module_attrs (Some (Files [s])) rest
     | _ ->
       raise (Ocsigen_extensions.Error_in_config_file
                ("Duplicate attribute module in <eliom>")))
  | ("findlib-package", s) :: rest ->
    begin match file with
      | None ->
        (try
           parse_module_attrs
             (Some (Files (Ocsigen_loader.findfiles s))) rest
         with Ocsigen_loader.Findlib_error _ as e ->
           raise (Ocsigen_extensions.Error_in_config_file
                    (Printf.sprintf "Findlib error: %s"
                       (Printexc.to_string e))))
      | _ ->
        raise (Ocsigen_extensions.Error_in_config_file
                 ("Duplicate attribute module in <eliom>"))
    end
  | (s, _) :: _ ->
    raise (Ocsigen_extensions.Error_in_config_file
             ("Wrong attribute for <eliom>: "^s))

let rec parse_default_links_xhr atts default_links_xhr = function
  | [] ->
    default_links_xhr, List.rev atts
  | ("xhr-links", str_value) :: rest ->
    let default_links_xhr =
      match str_value with
      | "yes" -> true
      | "no" -> false
      | _ ->
        raise (Ocsigen_extensions.Error_in_config_file
                 ("Invalid value for attribute xhr-links: "^str_value))
    in
    parse_default_links_xhr atts (Some default_links_xhr) rest
  | att :: rest ->
    parse_default_links_xhr (att :: atts) default_links_xhr rest

let set_timeout
    (f : ?full_st_name:Eliom_common.full_state_name ->
     ?cookie_level:[< Eliom_common.cookie_level ] ->
     recompute_expdates:bool ->
     bool (* override configfile *) ->
     bool (* from config file *) ->
     Eliom_common.sitedata ->
     float option -> unit)
    sitedata cookie_type state_hier_oo v =
  let make_full_st_name secure state_hier =
    let state_hier : Eliom_common.scope_hierarchy =
      match state_hier with
      | None -> Eliom_common_base.Default_ref_hier
      | Some s when String.lowercase s = "default" ->
        Eliom_common_base.Default_ref_hier
      | Some s when String.lowercase s = "comet" ->
        Eliom_common_base.Default_comet_hier
      | Some s -> Eliom_common_base.User_hier s
    in
    let scope =
      match cookie_type with
      | `Session -> `Session state_hier
      | `Client_process -> `Client_process state_hier
    in
    Eliom_common.make_full_state_name2
      sitedata.Eliom_common.site_dir_string
      secure
      ~scope
  in
  (*VVV We set timeout for both secure and insecure states.
    Make possible to customize this? *)
  f
    ?full_st_name:
      (Eliom_lib.Option.map (make_full_st_name false) state_hier_oo)
    ?cookie_level:(Some cookie_type)
    ~recompute_expdates:false
    true
    true
    sitedata
    v;
  f
    ?full_st_name:
      (Eliom_lib.Option.map (make_full_st_name true) state_hier_oo)
    ?cookie_level:(Some cookie_type)
    ~recompute_expdates:false
    true
    true
    sitedata
    v

let sitedata_setters sitedata =
  (fun ct snoo v ->
     set_timeout
       (Eliommod_timeouts.set_global_ ~kind:`Data)
       sitedata ct snoo v;
     set_timeout
       (Eliommod_timeouts.set_global_ ~kind:`Service)
       sitedata ct snoo v),

  set_timeout
    (Eliommod_timeouts.set_global_ ~kind:`Data)
    sitedata,

  set_timeout
    (Eliommod_timeouts.set_global_ ~kind:`Service)
    sitedata,

  set_timeout
    (Eliommod_timeouts.set_global_ ~kind:`Persistent)
    sitedata,

  (fun v ->
     sitedata.Eliom_common.max_service_sessions_per_group <- v, true),

  (fun v ->
     sitedata.Eliom_common.max_service_sessions_per_subnet <- v, true),

  (fun v ->
     sitedata.Eliom_common.max_volatile_data_sessions_per_group <- v, true),

  (fun v ->
     sitedata.Eliom_common.max_volatile_data_sessions_per_subnet <- v, true),

  (fun v ->
     sitedata.Eliom_common.max_persistent_data_sessions_per_group <-
       Some v, true),

  (fun v ->
     sitedata.Eliom_common.max_service_tab_sessions_per_group <- v, true),

  (fun v ->
     sitedata.Eliom_common.max_volatile_data_tab_sessions_per_group <-
       v, true),

  (fun v ->
     sitedata.Eliom_common.max_persistent_data_tab_sessions_per_group <-
       Some v, true),

  (fun v ->
     sitedata.Eliom_common.max_anonymous_services_per_session <- v, true),

  (fun v ->
     sitedata.Eliom_common.max_anonymous_services_per_subnet <- v, true;
     (* The global table has already been created, with old max
        and old ipv6mask. I update it, otherwise the setting has
        no effect for this table: *)
     try
       let dlist =
         Eliom_common.find_dlist_ip_table
           sitedata.Eliom_common.ipv4mask (* unused *)
           sitedata.Eliom_common.ipv6mask
           sitedata.Eliom_common.dlist_ip_table
           Ipaddr.(V6 V6.localhost)
       in
       ignore (Ocsigen_cache.Dlist.set_maxsize dlist v)
     with Not_found -> () (* should not occure *)
  ),

  (fun v ->
     ignore (Ocsigen_cache.Dlist.set_maxsize
               sitedata.Eliom_common.group_of_groups v)),

  (fun v -> sitedata.Eliom_common.secure_cookies <- v),

  (fun v -> sitedata.Eliom_common.ipv4mask <- Some v, true),

  (fun v -> sitedata.Eliom_common.ipv6mask <- Some v, true),

  (fun v -> sitedata.Eliom_common.application_script <- v)

let parse_init_eliommodule ~tag sitedata atts content =
  Eliom_extension.register_eliom_extension default_module_action;
  match parse_module_attrs None atts with
  | Some file_or_name ->
    exception_during_eliommodule_loading := true;
    load_eliom_module sitedata file_or_name tag content;
    exception_during_eliommodule_loading := false
  | _ ->
    ()

let parse_init_eliom sitedata atts content =
  (*--- if we put the line "new_sitedata" here, then there is one
    service table for each <eliom> tag ...  I think the other one is
    the best, because it corresponds to the way browsers manage
    cookies (one cookie for one site).  Thus we can have one site in
    several cmo (with one session).  *)
  let content = parse_eliom_options (sitedata_setters sitedata) content
  and default_links_xhr, atts = parse_default_links_xhr [] None atts in
  (match default_links_xhr with
   | Some default_links_xhr ->
     sitedata.Eliom_common.default_links_xhr#set
       ~override_tenable:true default_links_xhr
   | None ->
     ());
  parse_init_eliommodule ~tag:"eliom" sitedata atts content

let fun_site _ hostpattern conf_info site_dir =
  (*--- if we put the following line here: *)
  let sitedata = create_sitedata hostpattern site_dir conf_info in
  (*--- then there is one service tree for each <site> *)
  (*--- (mutatis mutandis for the following line:) *)
  Eliom_common.absolute_change_sitedata sitedata;
  let first = ref true and warning_displayed = ref false in
  fun _ _ -> function
    | Xml.Element ("eliommodule" as tag, atts, content) ->
      parse_init_eliommodule ~tag sitedata atts content;
      if Eliom_extension.get_eliom_extension () != default_module_action then
        Eliommod_pagegen.gen
          (Some (Eliom_extension.get_eliom_extension ()))
          sitedata
      else
        gen_nothing ()
    | Xml.Element ("eliom", atts, content) ->
      parse_init_eliom sitedata atts content;
      (*VVV 2012/08 It is not possible to load an eliom extension
        using <eliom>. Why? Is there a reason for this? For now I fail
        in that case. *)
      if Eliom_extension.get_eliom_extension () != default_module_action then
        raise (Ocsigen_extensions.Error_in_config_file
                 ("Eliom extensions cannot be loaded using <eliom>. \
                   Use <eliommodule> instead."));
      if !first then (
        first := false;
        Eliommod_pagegen.gen None sitedata
      ) else (
        if not !warning_displayed then
          Lwt_log.ign_warning ~section:Eliom_lib.Lwt_log.eliom
            "Tag <eliom> used several times in the same site: will run \
             Eliom only the first time. Prefer <eliommodule> to load a \
             module, and <eliom/> without attibute only once at the \
             position you want to generate your Eliom pages for this site.";
        warning_displayed := true;
        gen_nothing ()
      )
    | Xml.Element (t, _, _) ->
      raise (Ocsigen_extensions.Bad_config_tag_for_extension t)
    | _ -> raise (Ocsigen_extensions.Error_in_config_file
                    "(Eliommod extension)")

let () =
  Ocsigen_extensions.register
    ~name:"eliom"
    ~fun_site
    ~end_init
    ~exn_handler:handle_init_exn
    ~init_fun:parse_global_config
    ()
