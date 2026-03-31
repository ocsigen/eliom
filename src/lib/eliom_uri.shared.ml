(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_uri
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

(* Building href *)

let rec string_of_url_path' = function
  | [] -> ""
  | [a] when a = Common.eliom_suffix_internal_name -> ""
  | [a] -> Lib.Url.encode ~plus:false a
  | a :: b :: l when b = Common.eliom_suffix_internal_name ->
      string_of_url_path' (a :: l)
  | a :: l when a = Common.eliom_suffix_internal_name ->
      string_of_url_path' l
  | a :: l -> Lib.Url.encode ~plus:false a ^ "/" ^ string_of_url_path' l

let string_of_url_path_suff u = function
  | None -> string_of_url_path' u
  | Some suff ->
      let pref = string_of_url_path' u in
      let suf = string_of_url_path' suff in
      if String.length pref = 0 then suf else String.concat "/" [pref; suf]

let reconstruct_absolute_url_path = string_of_url_path_suff

let reconstruct_relative_url_path current_url u =
  let rec drop cururl desturl =
    match cururl, desturl with
    | _ :: l, [_] -> l, desturl
    | [_], m -> [], m
    | a :: l, b :: m when a = b -> drop l m
    | _ :: l, m -> l, m
    | [], m -> [], m
  in
  let rec makedotdot = function
    | [] -> []
    (*    | [a] -> "" *)
    | _ :: l -> ".." :: makedotdot l
  in
  let aremonter, aaller = drop current_url u in
  makedotdot aremonter @ aaller

let reconstruct_relative_url_path_string current_url u suff =
  let relurl = reconstruct_relative_url_path current_url u in
  let s = string_of_url_path_suff relurl suff in
  if String.length s = 0
  then Common.defaultpagename
  else if s.[0] = '/'
  then (* possible with optional parameters *) "./" ^ s
  else s

let make_actual_path = Common.make_actual_path

(*****************************************************************************)

let make_proto_prefix ?hostname ?port https : string =
  let sp = Common.get_sp_option () in
  let ssl =
    match sp with Some sp -> Request_info.get_csp_ssl_sp sp | None -> false
  in
  let host =
    match hostname, sp with
    | None, Some sp -> Request_info.get_csp_hostname_sp sp
    | None, None -> Config.get_default_hostname ()
    | Some h, _ -> h
  in
  let port =
    match port, sp with
    | Some p, _ -> p
    | None, Some sp ->
        if https = ssl
        then Request_info.get_csp_server_port_sp sp
        else if https
        then Config.get_default_sslport ()
        else Config.get_default_port ()
    | None, None ->
        if https
        then Config.get_default_sslport ()
        else Config.get_default_port ()
  in
  Lib.Url.make_absolute_url ~https ~host ~port "/"

let is_https https ssl service =
  https = Some true
  || (https = None && Config.default_protocol_is_https ())
  || Service.https service
  || (https = None && ssl)

let make_uri_components_
      ?(* does not take into account getparams *) absolute
      ?((* absolute is used to force absolute link.
       The default is false for regular application.
       But for client side apps (mobile apps), it is true, because
       relative URLs are used for local assets. *)
        absolute_path = false)
      ?(* used to force absolute link without protocol/server/port *)
       https
      (type a)
      ~(service : (_, _, _, a, _, _, _, _, _, _, _) Service.t)
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?(nl_params = Parameter.empty_nl_params_set)
      ()
  =
  let absolute =
    match absolute with
    | Some a -> a
    | None ->
        !Common.is_client_app && not (Service.has_client_fun service)
  in
  let ssl =
    match Common.get_sp_option () with
    | Some sp -> Request_info.get_csp_ssl_sp sp
    | None -> false
  in
  let https = is_https https ssl service in
  let absolute =
    if absolute || https <> ssl
    then Some (make_proto_prefix ?hostname ?port https)
    else if absolute_path
    then Some "/"
    else None
  in
  (*VVV We trust current protocol? *)
  let nl_params = Parameter.table_of_nl_params_set nl_params in
  let keep_nl_params =
    match keep_nl_params with
    | None -> Service.keep_nl_params service
    | Some b -> b
  in
  (* for preapplied non localized and not non localized: *)
  let preappnlp, preapplied_params = Service.pre_applied_parameters service in
  let nlp =
    match Common.get_sp_option () with
    | None -> preappnlp
    | Some sp -> (
      match keep_nl_params with
      | `All ->
          (* We replace current nl params by preapplied ones *)
          Lib.String.Table.fold
            (fun key v b -> Lib.String.Table.add key v b)
            preappnlp
            (Mod_parameters.inject_param_table
               (Request_info.get_nl_get_params_sp sp))
      | `Persistent ->
          (* We replace current nl params by preapplied ones *)
          Lib.String.Table.fold Lib.String.Table.add preappnlp
            (Mod_parameters.inject_param_table
               (Request_info.get_persistent_nl_get_params_sp sp))
      | `None -> preappnlp)
  in
  let nlp =
    (* We replace current nl params by nl_params *)
    Lib.String.Table.fold Lib.String.Table.add nl_params nlp
  in
  (* remove in nlp the one present in the service parameters *)
  let getparamstype = Service.get_params_type service in
  let nlp = Parameter.remove_from_nlp nlp getparamstype in
  let hiddenparams =
    Lib.String.Table.fold (fun _ l beg -> l @ beg) nlp preapplied_params
  in
  match Service.info service with
  | Service.Attached attser -> (
      let uri =
        let suff = None in
        if Service.is_external service
        then
          Service.prefix attser ^ "/"
          ^
          (* we add the "/" even if there is no prefix, because
                    we should do absolute links in that case *)
          reconstruct_absolute_url_path (Service.full_path attser) suff
        else
          match absolute with
          | Some proto_prefix ->
              proto_prefix
              ^ reconstruct_absolute_url_path (Service.full_path attser) suff
          | None ->
              let sp = Common.get_sp () in
              reconstruct_relative_url_path_string
                (Request_info.get_csp_original_full_path_sp sp)
                (Service.full_path attser) suff
      in
      match Service.get_name attser with
      | Common.SAtt_no -> uri, hiddenparams, fragment
      | Common.SAtt_anon s ->
          ( uri
          , ( Common.get_numstate_param_name
            , Mod_parameters.insert_string s )
            :: hiddenparams
          , fragment )
      | Common.SAtt_named s ->
          ( uri
          , ( Common.get_state_param_name
            , Mod_parameters.insert_string s )
            :: hiddenparams
          , fragment )
      | Common.SAtt_csrf_safe csrf_info ->
          let sp = Common.get_sp () in
          let s = Service.register_delayed_get_or_na_coservice ~sp csrf_info in
          ( uri
          , ( Common.get_numstate_param_name
            , Mod_parameters.insert_string s )
            :: hiddenparams
          , fragment )
      | Common.SAtt_na_anon s ->
          ( uri
          , (Common.naservice_num, Mod_parameters.insert_string s)
            :: hiddenparams
          , fragment )
      | Common.SAtt_na_named s ->
          ( uri
          , (Common.naservice_name, Mod_parameters.insert_string s)
            :: hiddenparams
          , fragment )
      | Common.SAtt_na_csrf_safe csrf_info ->
          let sp = Common.get_sp () in
          let s = Service.register_delayed_get_or_na_coservice ~sp csrf_info in
          ( uri
          , (Common.naservice_num, Mod_parameters.insert_string s)
            :: hiddenparams
          , fragment ))
  | Service.Nonattached naser ->
      let sp = Common.get_sp () in
      let na_name = Service.na_name naser in
      let params' =
        let current_get_params =
          if na_name = Common.SNa_void_keep
          then (Request_info.get_si sp).Common.si_all_get_but_nl
          else
            Lazy.force
              (Request_info.get_si sp).Common.si_all_get_but_na_nl
        in
        match na_name with
        | Common.SNa_void_keep | Common.SNa_void_dontkeep ->
            current_get_params
        | Common.SNa_get' n ->
            (Common.naservice_num, n) :: current_get_params
        | Common.SNa_get_ n ->
            (Common.naservice_name, n) :: current_get_params
        | Common.SNa_get_csrf_safe csrf_info ->
            let sp = Common.get_sp () in
            let n =
              Service.register_delayed_get_or_na_coservice ~sp csrf_info
            in
            (Common.naservice_num, n) :: current_get_params
        | _ -> assert false
      in
      let params =
        Mod_parameters.inject_param_list params' @ hiddenparams
      in
      let beg =
        match absolute with
        | None ->
            reconstruct_relative_url_path_string
              (Request_info.get_csp_original_full_path_sp sp)
              (Request_info.get_original_full_path_sp sp)
              None
        | Some proto_prefix ->
            proto_prefix ^ Request_info.get_original_full_path_string_sp sp
      in
      beg, params, fragment

let make_uri_components
      ?absolute
      ?absolute_path
      ?https
      (type a)
      ~(service : (_, _, _, a, _, _, _, _, _, _, _) Service.t)
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?nl_params
      getparams
  =
  let uri, pregetparams, fragment =
    make_uri_components_ ?absolute ?absolute_path ?https ~service ?hostname
      ?port ?fragment ?keep_nl_params ?nl_params ()
  in
  (* for getparams and non localized params: *)
  let suff, params =
    Parameter.construct_params_list Lib.String.Table.empty
      (Service.get_params_type service)
      getparams
    (* if nl params were already present, they will be replaced
         by new values *)
  in
  let uri =
    match suff with
    | None -> uri
    | Some suff ->
        let suff = string_of_url_path' suff in
        if uri.[String.length uri - 1] = '/'
        then uri ^ suff
        else String.concat "/" [uri; suff]
  in
  let fragment = Lib.Option.map Lib.Url.encode fragment in
  uri, params @ pregetparams, fragment

let make_string_uri_from_components (uri, params, fragment) =
  let s =
    Lib.String.may_concat uri ~sep:"?"
      (Parameter.construct_params_string params)
  in
  match fragment with None -> s | Some f -> Lib.String.concat "#" [s; f]

let make_string_uri
      ?absolute
      ?absolute_path
      ?https
      ~service
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?nl_params
      getparams : string
  =
  make_string_uri_from_components
    (make_uri_components ?absolute ?absolute_path ?https ~service ?hostname
       ?port ?fragment ?keep_nl_params ?nl_params getparams)

let make_string_uri_ = make_string_uri

let make_post_uri_components_
      ?((* do not take into account postparams *)
        absolute = !Common.is_client_app)
      ?(absolute_path = false)
      ?https
      (type a)
      ~(service : (_, _, _, a, _, _, _, _, _, _, _) Service.t)
      ?hostname
      ?port
      ?fragment
      ?(keep_nl_params : [`All | `Persistent | `None] option)
      ?(nl_params = Parameter.empty_nl_params_set)
      ?keep_get_na_params
      getparams
      ()
  =
  match Service.info service with
  | Service.Attached attser ->
      let (uri, getparams, fragment), getname =
        let getname = Service.get_name attser in
        match getname with
        | Common.SAtt_csrf_safe csrf_info ->
            (* special case for post-coservices on get csrf safe services:
           we must register the get service first *)
            let sp = Common.get_sp () in
            let s =
              Common.SAtt_anon
                (Service.register_delayed_get_or_na_coservice ~sp csrf_info)
            in
            ( make_uri_components ~absolute ~absolute_path ?https
                ~service:(Service.change_get_num service attser s)
                ?hostname ?port ?fragment ?keep_nl_params ~nl_params getparams
            , s )
        | _ ->
            ( make_uri_components ~absolute ~absolute_path ?https ~service
                ?hostname ?port ?fragment ?keep_nl_params ~nl_params getparams
            , getname )
      in
      let postparams =
        match Service.post_name attser with
        | Common.SAtt_no -> []
        | Common.SAtt_anon s -> [Common.post_numstate_param_name, s]
        | Common.SAtt_named s -> [Common.post_state_param_name, s]
        | Common.SAtt_csrf_safe csrf_info ->
            let sp = Common.get_sp () in
            let s =
              Service.register_delayed_post_coservice ~sp csrf_info getname
            in
            [Common.post_numstate_param_name, s]
        | Common.SAtt_na_anon s -> [Common.naservice_num, s]
        | Common.SAtt_na_named s -> [Common.naservice_name, s]
        | Common.SAtt_na_csrf_safe csrf_info ->
            let sp = Common.get_sp () in
            let s =
              Service.register_delayed_post_coservice ~sp csrf_info getname
            in
            [Common.naservice_num, s]
      in
      uri, getparams, fragment, Mod_parameters.inject_param_list postparams
  | Service.Nonattached naser ->
      let sp = Common.get_sp () in
      let nl_params = Parameter.table_of_nl_params_set nl_params in
      let keep_nl_params =
        match keep_nl_params with
        | None -> Service.keep_nl_params service
        | Some b -> b
      in
      let preappnlp, preapp = Service.pre_applied_parameters service in
      let nlp =
        match keep_nl_params with
        | `All ->
            (* We replace current nl params by preapplied ones *)
            Lib.String.Table.fold Lib.String.Table.add preappnlp
              (Mod_parameters.inject_param_table
                 (Request_info.get_nl_get_params ()))
        | `Persistent ->
            (* We replace current nl params by preapplied ones *)
            Lib.String.Table.fold Lib.String.Table.add preappnlp
              (Mod_parameters.inject_param_table
                 (Request_info.get_persistent_nl_get_params_sp sp))
        | `None -> preappnlp
      in
      let nlp =
        (* We replace current nl params by nl_params *)
        Lib.String.Table.fold Lib.String.Table.add nl_params nlp
      in
      (* for getparams and non localized params: *)
      let _suff, params =
        Parameter.construct_params_list nlp
          (Service.get_params_type service)
          getparams
        (* if nl params were already present, they will be replaced
           by new values *)
        (* getparams can be something else than []
           if we have added nl params to the service (?) *)
      in
      let params = params @ preapp in
      let keep_get_na_params =
        match keep_get_na_params with
        | Some b -> b
        | None -> Service.na_keep_get_na_params naser
      in
      let params =
        params
        @ Mod_parameters.inject_param_list
            (if keep_get_na_params
             then (Request_info.get_si sp).Common.si_all_get_but_nl
             else
               Lazy.force
                 (Request_info.get_si sp).Common.si_all_get_but_na_nl)
      in
      let ssl = Request_info.get_csp_ssl_sp sp in
      let https = is_https https ssl service in
      let absolute =
        if absolute || https <> ssl
        then Some (make_proto_prefix ?hostname ?port https)
        else if absolute_path
        then Some "/"
        else None
      in
      (* absolute URL does not work behind a reverse proxy! *)
      let uri =
        match absolute with
        | Some proto_prefix ->
            if
              !Common.is_client_app
              &&
              let s = Request_info.get_original_full_path_string_sp sp
              and s' = Common.client_html_file () in
              let n = String.length s and n' = String.length s' in
              n >= n' && String.(sub s (n - n') n') = s'
            then
              (* Workaround for GitHub issue #465.

             Given an app under a certain path and a server function,
             we would perform requests on

             http://${SERVER}/${LOCAL_PATH},

             where ${LOCAL_PATH} refers to the file system on the
             mobile device. This is both wrong (because it doesn't
             take care of the application path) and a security issue.

             To fix the issue, if the URL contains
             [Common.client_html_file ()] (default:
             "eliom.html"), we disregard it and use the site dir as
             the path. *)
              let sd = Request_info.get_site_dir () in
              proto_prefix ^ String.concat "/" sd ^ "/"
            else proto_prefix ^ Request_info.get_original_full_path_string_sp sp
        | None ->
            reconstruct_relative_url_path_string
              (Request_info.get_csp_original_full_path_sp sp)
              (Request_info.get_original_full_path_sp sp)
              None
      in
      let naservice_line =
        match Service.na_name naser with
        | Common.SNa_post' n -> Common.naservice_num, n
        | Common.SNa_post_ n -> Common.naservice_name, n
        | Common.SNa_post_csrf_safe csrf_info ->
            let n =
              Service.register_delayed_get_or_na_coservice ~sp csrf_info
            in
            Common.naservice_num, n
        | _ -> assert false
      in
      let fragment =
        None
        (* fragment is not sent to the server *)
      in
      let postparams = [naservice_line] in
      uri, params, fragment, Mod_parameters.inject_param_list postparams

let make_post_uri_components
      ?absolute
      ?absolute_path
      ?https
      ~service
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?nl_params
      ?keep_get_na_params
      getparams
      postparams
  =
  let uri, getparams, fragment, prepostparams =
    make_post_uri_components_ ?absolute ?absolute_path ?https ~service ?hostname
      ?port ?fragment ?keep_nl_params ?nl_params ?keep_get_na_params getparams
      ()
  in
  let _, postparams =
    Parameter.construct_params_list Lib.String.Table.empty
      (Service.post_params_type service)
      postparams
  in
  uri, getparams, fragment, postparams @ prepostparams

let make_post_uri_components__ = make_post_uri_components

(**** Tab cookies: *)

(*VVV

  WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
  We do not take into account the suffix for computing process cookies
  of GET forms (because the suffix is taken from the form).
  This corresponds to what the browser is doing with session cookies.
  For links and POST forms, the url already contains the suffix.
  It is taken into account for computing process cookies.
  Again, it is what the browser is doing for session cookies.

  This is not completely satisfactory,
  but should always do what we want,
  but for very non-standard uses of cookies ...
*)
let make_cookies_info (https, service) =
  (* https is what the user asked while creating the link/form *)
  let get_path_
        (type a)
        ~(* simplified version of make_uri_components.
                            Returns only the absolute path without
                            protocol/server/port AND WITHOUT SUFFIX *)
        (service : (_, _, _, a, _, _, _, _, _, _, _) Service.t)
    =
    match Service.info service with
    | Service.Attached attser ->
        if Service.is_external service
        then None
        else Some (Service.full_path attser)
    | Service.Nonattached _naser ->
        Some (Request_info.get_csp_original_full_path ())
  in
  match get_path_ ~service with
  | None -> None
  | Some path ->
      let ssl = Request_info.get_csp_ssl () in
      let https =
        https = Some true || Service.https service || (https = None && ssl)
      in
      Some (https, path)
