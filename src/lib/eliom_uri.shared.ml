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
  | [a] when a = Eliom_common.eliom_suffix_internal_name -> ""
  | [a] -> Eliom_lib.Url.encode ~plus:false a
  | a :: b :: l when b = Eliom_common.eliom_suffix_internal_name ->
      string_of_url_path' (a :: l)
  | a :: l when a = Eliom_common.eliom_suffix_internal_name ->
      string_of_url_path' l
  | a :: l -> Eliom_lib.Url.encode ~plus:false a ^ "/" ^ string_of_url_path' l

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
  then Eliom_common.defaultpagename
  else if s.[0] = '/'
  then (* possible with optional parameters *) "./" ^ s
  else s

let make_actual_path = Eliom_common.make_actual_path

(*****************************************************************************)

let make_proto_prefix ?hostname ?port https : string =
  let sp = Eliom_common.get_sp_option () in
  let ssl =
    match sp with
    | Some sp -> Eliom_request_info.get_csp_ssl_sp sp
    | None -> false
  in
  let host =
    match hostname, sp with
    | None, Some sp -> Eliom_request_info.get_csp_hostname_sp sp
    | None, None -> Eliom_config.get_default_hostname ()
    | Some h, _ -> h
  in
  let port =
    match port, sp with
    | Some p, _ -> p
    | None, Some sp ->
        if https = ssl
        then Eliom_request_info.get_csp_server_port_sp sp
        else if https
        then Eliom_config.get_default_sslport ()
        else Eliom_config.get_default_port ()
    | None, None ->
        if https
        then Eliom_config.get_default_sslport ()
        else Eliom_config.get_default_port ()
  in
  Eliom_lib.Url.make_absolute_url ~https ~host ~port "/"

let is_https https ssl service =
  https = Some true
  || (https = None && Eliom_config.default_protocol_is_https ())
  || Eliom_service.https service
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
      ~(service : (_, _, _, a, _, _, _, _, _, _, _) Eliom_service.t)
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?(nl_params = Eliom_parameter.empty_nl_params_set)
      ()
  =
  let absolute =
    match absolute with
    | Some a -> a
    | None ->
        !Eliom_common.is_client_app
        && not (Eliom_service.has_client_fun service)
  in
  let ssl =
    match Eliom_common.get_sp_option () with
    | Some sp -> Eliom_request_info.get_csp_ssl_sp sp
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
  let nl_params = Eliom_parameter.table_of_nl_params_set nl_params in
  let keep_nl_params =
    match keep_nl_params with
    | None -> Eliom_service.keep_nl_params service
    | Some b -> b
  in
  (* for preapplied non localized and not non localized: *)
  let preappnlp, preapplied_params =
    Eliom_service.pre_applied_parameters service
  in
  let nlp =
    match Eliom_common.get_sp_option () with
    | None -> preappnlp
    | Some sp -> (
      match keep_nl_params with
      | `All ->
          (* We replace current nl params by preapplied ones *)
          Eliom_lib.String.Table.fold
            (fun key v b -> Eliom_lib.String.Table.add key v b)
            preappnlp
            (Eliommod_parameters.inject_param_table
               (Eliom_request_info.get_nl_get_params_sp sp))
      | `Persistent ->
          (* We replace current nl params by preapplied ones *)
          Eliom_lib.String.Table.fold Eliom_lib.String.Table.add preappnlp
            (Eliommod_parameters.inject_param_table
               (Eliom_request_info.get_persistent_nl_get_params_sp sp))
      | `None -> preappnlp)
  in
  let nlp =
    (* We replace current nl params by nl_params *)
    Eliom_lib.String.Table.fold Eliom_lib.String.Table.add nl_params nlp
  in
  (* remove in nlp the one present in the service parameters *)
  let getparamstype = Eliom_service.get_params_type service in
  let nlp = Eliom_parameter.remove_from_nlp nlp getparamstype in
  let hiddenparams =
    Eliom_lib.String.Table.fold (fun _ l beg -> l @ beg) nlp preapplied_params
  in
  match Eliom_service.info service with
  | Eliom_service.Attached attser -> (
      let uri =
        let suff = None in
        if Eliom_service.is_external service
        then
          Eliom_service.prefix attser
          ^ "/"
          ^
          (* we add the "/" even if there is no prefix, because
                    we should do absolute links in that case *)
          reconstruct_absolute_url_path (Eliom_service.full_path attser) suff
        else
          match absolute with
          | Some proto_prefix ->
              proto_prefix
              ^ reconstruct_absolute_url_path
                  (Eliom_service.full_path attser)
                  suff
          | None ->
              let sp = Eliom_common.get_sp () in
              reconstruct_relative_url_path_string
                (Eliom_request_info.get_csp_original_full_path_sp sp)
                (Eliom_service.full_path attser)
                suff
      in
      match Eliom_service.get_name attser with
      | Eliom_common.SAtt_no -> uri, hiddenparams, fragment
      | Eliom_common.SAtt_anon s ->
          ( uri
          , ( Eliom_common.get_numstate_param_name
            , Eliommod_parameters.insert_string s )
            :: hiddenparams
          , fragment )
      | Eliom_common.SAtt_named s ->
          ( uri
          , ( Eliom_common.get_state_param_name
            , Eliommod_parameters.insert_string s )
            :: hiddenparams
          , fragment )
      | Eliom_common.SAtt_csrf_safe csrf_info ->
          let sp = Eliom_common.get_sp () in
          let s =
            Eliom_service.register_delayed_get_or_na_coservice ~sp csrf_info
          in
          ( uri
          , ( Eliom_common.get_numstate_param_name
            , Eliommod_parameters.insert_string s )
            :: hiddenparams
          , fragment )
      | Eliom_common.SAtt_na_anon s ->
          ( uri
          , (Eliom_common.naservice_num, Eliommod_parameters.insert_string s)
            :: hiddenparams
          , fragment )
      | Eliom_common.SAtt_na_named s ->
          ( uri
          , (Eliom_common.naservice_name, Eliommod_parameters.insert_string s)
            :: hiddenparams
          , fragment )
      | Eliom_common.SAtt_na_csrf_safe csrf_info ->
          let sp = Eliom_common.get_sp () in
          let s =
            Eliom_service.register_delayed_get_or_na_coservice ~sp csrf_info
          in
          ( uri
          , (Eliom_common.naservice_num, Eliommod_parameters.insert_string s)
            :: hiddenparams
          , fragment ))
  | Eliom_service.Nonattached naser ->
      let sp = Eliom_common.get_sp () in
      let na_name = Eliom_service.na_name naser in
      let params' =
        let current_get_params =
          if na_name = Eliom_common.SNa_void_keep
          then (Eliom_request_info.get_si sp).Eliom_common.si_all_get_but_nl
          else
            Lazy.force
              (Eliom_request_info.get_si sp).Eliom_common.si_all_get_but_na_nl
        in
        match na_name with
        | Eliom_common.SNa_void_keep | Eliom_common.SNa_void_dontkeep ->
            current_get_params
        | Eliom_common.SNa_get' n ->
            (Eliom_common.naservice_num, n) :: current_get_params
        | Eliom_common.SNa_get_ n ->
            (Eliom_common.naservice_name, n) :: current_get_params
        | Eliom_common.SNa_get_csrf_safe csrf_info ->
            let sp = Eliom_common.get_sp () in
            let n =
              Eliom_service.register_delayed_get_or_na_coservice ~sp csrf_info
            in
            (Eliom_common.naservice_num, n) :: current_get_params
        | _ -> assert false
      in
      let params =
        Eliommod_parameters.inject_param_list params' @ hiddenparams
      in
      let beg =
        match absolute with
        | None ->
            reconstruct_relative_url_path_string
              (Eliom_request_info.get_csp_original_full_path_sp sp)
              (Eliom_request_info.get_original_full_path_sp sp)
              None
        | Some proto_prefix ->
            proto_prefix
            ^ Eliom_request_info.get_original_full_path_string_sp sp
      in
      beg, params, fragment

let make_uri_components
      ?absolute
      ?absolute_path
      ?https
      (type a)
      ~(service : (_, _, _, a, _, _, _, _, _, _, _) Eliom_service.t)
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
    Eliom_parameter.construct_params_list Eliom_lib.String.Table.empty
      (Eliom_service.get_params_type service)
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
  let fragment = Eliom_lib.Option.map Eliom_lib.Url.encode fragment in
  uri, params @ pregetparams, fragment

let make_string_uri_from_components (uri, params, fragment) =
  let s =
    Eliom_lib.String.may_concat uri ~sep:"?"
      (Eliom_parameter.construct_params_string params)
  in
  match fragment with None -> s | Some f -> Eliom_lib.String.concat "#" [s; f]

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
        absolute = !Eliom_common.is_client_app)
      ?(absolute_path = false)
      ?https
      (type a)
      ~(service : (_, _, _, a, _, _, _, _, _, _, _) Eliom_service.t)
      ?hostname
      ?port
      ?fragment
      ?(keep_nl_params : [`All | `Persistent | `None] option)
      ?(nl_params = Eliom_parameter.empty_nl_params_set)
      ?keep_get_na_params
      getparams
      ()
  =
  match Eliom_service.info service with
  | Eliom_service.Attached attser ->
      let (uri, getparams, fragment), getname =
        let getname = Eliom_service.get_name attser in
        match getname with
        | Eliom_common.SAtt_csrf_safe csrf_info ->
            (* special case for post-coservices on get csrf safe services:
           we must register the get service first *)
            let sp = Eliom_common.get_sp () in
            let s =
              Eliom_common.SAtt_anon
                (Eliom_service.register_delayed_get_or_na_coservice ~sp
                   csrf_info)
            in
            ( make_uri_components ~absolute ~absolute_path ?https
                ~service:(Eliom_service.change_get_num service attser s)
                ?hostname ?port ?fragment ?keep_nl_params ~nl_params getparams
            , s )
        | _ ->
            ( make_uri_components ~absolute ~absolute_path ?https ~service
                ?hostname ?port ?fragment ?keep_nl_params ~nl_params getparams
            , getname )
      in
      let postparams =
        match Eliom_service.post_name attser with
        | Eliom_common.SAtt_no -> []
        | Eliom_common.SAtt_anon s -> [Eliom_common.post_numstate_param_name, s]
        | Eliom_common.SAtt_named s -> [Eliom_common.post_state_param_name, s]
        | Eliom_common.SAtt_csrf_safe csrf_info ->
            let sp = Eliom_common.get_sp () in
            let s =
              Eliom_service.register_delayed_post_coservice ~sp csrf_info
                getname
            in
            [Eliom_common.post_numstate_param_name, s]
        | Eliom_common.SAtt_na_anon s -> [Eliom_common.naservice_num, s]
        | Eliom_common.SAtt_na_named s -> [Eliom_common.naservice_name, s]
        | Eliom_common.SAtt_na_csrf_safe csrf_info ->
            let sp = Eliom_common.get_sp () in
            let s =
              Eliom_service.register_delayed_post_coservice ~sp csrf_info
                getname
            in
            [Eliom_common.naservice_num, s]
      in
      uri, getparams, fragment, Eliommod_parameters.inject_param_list postparams
  | Eliom_service.Nonattached naser ->
      let sp = Eliom_common.get_sp () in
      let nl_params = Eliom_parameter.table_of_nl_params_set nl_params in
      let keep_nl_params =
        match keep_nl_params with
        | None -> Eliom_service.keep_nl_params service
        | Some b -> b
      in
      let preappnlp, preapp = Eliom_service.pre_applied_parameters service in
      let nlp =
        match keep_nl_params with
        | `All ->
            (* We replace current nl params by preapplied ones *)
            Eliom_lib.String.Table.fold Eliom_lib.String.Table.add preappnlp
              (Eliommod_parameters.inject_param_table
                 (Eliom_request_info.get_nl_get_params ()))
        | `Persistent ->
            (* We replace current nl params by preapplied ones *)
            Eliom_lib.String.Table.fold Eliom_lib.String.Table.add preappnlp
              (Eliommod_parameters.inject_param_table
                 (Eliom_request_info.get_persistent_nl_get_params_sp sp))
        | `None -> preappnlp
      in
      let nlp =
        (* We replace current nl params by nl_params *)
        Eliom_lib.String.Table.fold Eliom_lib.String.Table.add nl_params nlp
      in
      (* for getparams and non localized params: *)
      let _suff, params =
        Eliom_parameter.construct_params_list nlp
          (Eliom_service.get_params_type service)
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
        | None -> Eliom_service.na_keep_get_na_params naser
      in
      let params =
        params
        @ Eliommod_parameters.inject_param_list
            (if keep_get_na_params
             then (Eliom_request_info.get_si sp).Eliom_common.si_all_get_but_nl
             else
               Lazy.force
                 (Eliom_request_info.get_si sp)
                   .Eliom_common.si_all_get_but_na_nl)
      in
      let ssl = Eliom_request_info.get_csp_ssl_sp sp in
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
              !Eliom_common.is_client_app
              &&
              let s = Eliom_request_info.get_original_full_path_string_sp sp
              and s' = Eliom_common.client_html_file () in
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
             [Eliom_common.client_html_file ()] (default:
             "eliom.html"), we disregard it and use the site dir as
             the path. *)
              let sd = Eliom_request_info.get_site_dir () in
              proto_prefix ^ String.concat "/" sd ^ "/"
            else
              proto_prefix
              ^ Eliom_request_info.get_original_full_path_string_sp sp
        | None ->
            reconstruct_relative_url_path_string
              (Eliom_request_info.get_csp_original_full_path_sp sp)
              (Eliom_request_info.get_original_full_path_sp sp)
              None
      in
      let naservice_line =
        match Eliom_service.na_name naser with
        | Eliom_common.SNa_post' n -> Eliom_common.naservice_num, n
        | Eliom_common.SNa_post_ n -> Eliom_common.naservice_name, n
        | Eliom_common.SNa_post_csrf_safe csrf_info ->
            let n =
              Eliom_service.register_delayed_get_or_na_coservice ~sp csrf_info
            in
            Eliom_common.naservice_num, n
        | _ -> assert false
      in
      let fragment =
        None
        (* fragment is not sent to the server *)
      in
      let postparams = [naservice_line] in
      uri, params, fragment, Eliommod_parameters.inject_param_list postparams

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
    Eliom_parameter.construct_params_list Eliom_lib.String.Table.empty
      (Eliom_service.post_params_type service)
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
        (service : (_, _, _, a, _, _, _, _, _, _, _) Eliom_service.t)
    =
    match Eliom_service.info service with
    | Eliom_service.Attached attser ->
        if Eliom_service.is_external service
        then None
        else Some (Eliom_service.full_path attser)
    | Eliom_service.Nonattached _naser ->
        Some (Eliom_request_info.get_csp_original_full_path ())
  in
  match get_path_ ~service with
  | None -> None
  | Some path ->
      let ssl = Eliom_request_info.get_csp_ssl () in
      let https =
        https = Some true || Eliom_service.https service || (https = None && ssl)
      in
      Some (https, path)
