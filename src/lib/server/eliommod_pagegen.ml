(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_pagegen.ml
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

open Lwt.Infix

let headers_with_content_type headers =
  Cohttp.Header.add_opt
    headers
    Ocsigen_header.Name.(to_string content_type)
    (Printf.sprintf "%s; charset=utf-8"
       Eliom_content_core.Html.D.Info.content_type)

let out =
  let encode x = fst (Xml_print.Utf8.normalize_html x) in
  Eliom_content_core.Html.Printer.pp ~encode ()

let make_response ?headers ~status body =
  let body = Cohttp_lwt.Body.of_string (Format.asprintf "%a" out body)
  and response =
    let headers = headers_with_content_type headers in
    Cohttp.Response.make ~status ~headers ()
  in
  Lwt.return (Ocsigen_response.make ~body response)

(* module Html_content = Ocsigen_senders.Make_XML_Content(Xml)(Html.F) *)

(* Exception handler for the site *)

let def_handler e = Lwt.fail e

let handle_site_exn exn info sitedata =
  let sp = Eliom_common.make_server_params sitedata info None None in
  Lwt.with_value Eliom_common.sp_key (Some sp)
    (fun () -> sitedata.Eliom_common.exn_handler exn)

(* Update cookie tables *)
let update_cookie_table ?now sitedata (ci, sci) =
  let now = match now with
    | Some n -> n
    | None -> Unix.gettimeofday ()
  in

  let update_exp
      (service_cookies_info, data_cookies_info, pers_cookies_info) =
  (* Update service expiration date and value *)
    Eliom_common.Full_state_name_table.iter

      (fun name (oldvalue, newr) ->
      (* catch fun () -> *)
        match !newr with
          | Eliom_common.SCData_session_expired
          | Eliom_common.SCNo_data -> () (* The cookie has been removed *)
          | Eliom_common.SC newc ->
            newc.Eliom_common.sc_exp :=
              match !(newc.Eliom_common.sc_timeout) with
                | Eliom_common.TGlobal ->
                  let globaltimeout =
                    Eliommod_timeouts.find_global `Service name sitedata
                  in
                  (match globaltimeout with
                    | None -> None
                    | Some t -> Some (t +. now))
                | Eliom_common.TNone -> None
                | Eliom_common.TSome t -> Some (t +. now)
      )

      !service_cookies_info;

    (* Update "in memory data" expiration date and value *)
    Eliom_common.Full_state_name_table.iter

      (fun name v ->
        (* 2018-07-17 We do this for all volatile sessions,
           even if it has not been used,
           otherwise, sessions could have different duration.
           (Before: we were doing this only if (Lazy.is_val v))
           Keeping same duration is important for example for comet
           (which is using both service and volatile data sessions).
        *)
         let (oldvalue, newr) = Lazy.force v in
         match !newr with
         | Eliom_common.SCData_session_expired
         | Eliom_common.SCNo_data -> () (* The cookie has been removed *)
         | Eliom_common.SC newc ->
           newc.Eliom_common.dc_exp :=
             match !(newc.Eliom_common.dc_timeout) with
             | Eliom_common.TGlobal ->
               let globaltimeout =
                 Eliommod_timeouts.find_global `Data name sitedata
               in
               (match globaltimeout with
                | None -> None
                | Some t -> Some (t +. now))
             | Eliom_common.TNone -> None
             | Eliom_common.TSome t -> Some (t +. now)
      )

      !data_cookies_info;


    (* Update persistent expiration date, user timeout and value *)
    (* 2018-07-17 We do this for all persistent sessions
       only if one persistent session has been used:
       - all persistent sessions will have same duration
       - will not do too many database requests
    *)
    if
      Eliom_common.Full_state_name_table.exists
        (fun _ v -> Lazy.is_val v)
        !pers_cookies_info
    then
      Eliom_common.Full_state_name_table.fold

        (fun name v thr ->
           let thr2 =
            Lazy.force v >>= fun (oldvalue, newr) ->
            match !newr with
              | Eliom_common.SCData_session_expired
              | Eliom_common.SCNo_data -> (* The cookie has been removed *)
                Lwt.return ()
              | Eliom_common.SC newc ->
                let newexp =
                  match !(newc.Eliom_common.pc_timeout) with
                    | Eliom_common.TGlobal ->
                      let globaltimeout =
                        Eliommod_timeouts.find_global `Persistent
                          name sitedata
                      in
                      (match globaltimeout with
                        | None -> None
                        | Some t -> Some (t +. now))
                    | Eliom_common.TNone -> None
                    | Eliom_common.TSome t -> Some (t +. now)
                in
                match oldvalue with
                  | Some (_, oldti, oldexp, oldgrp) when
                      (oldexp = newexp &&
                          oldti = !(newc.Eliom_common.pc_timeout) &&
                          oldgrp = !(newc.Eliom_common.pc_session_group) &&
                       newc.Eliom_common.pc_set_value = `None) -> Lwt.return ()
                (* nothing to do *)
                  | Some (_, oldti, oldexp, oldgrp) when
                      newc.Eliom_common.pc_set_value = `None ->
                    Lwt.catch
                      (fun () ->
                        Eliom_common.Persistent_cookies.replace_if_exists
                          Eliom_common.(Hashed_cookies.to_string newc.pc_hvalue)
                          (name,
                           newexp,
                           !(newc.Eliom_common.pc_timeout),
                           !(newc.Eliom_common.pc_session_group)))
                      (function
                        | Not_found -> Lwt.return ()
                        (* someone else closed the session *)
                        | e -> Lwt.fail e)
                  | _ ->
                    Eliom_common.Persistent_cookies.add
                      Eliom_common.(Hashed_cookies.to_string newc.pc_hvalue)
                      (name,
                       newexp,
                       !(newc.Eliom_common.pc_timeout),
                       !(newc.Eliom_common.pc_session_group))

        (*VVV Do not forget to change persistent_cookie_table_version
          if you change the type of persistent table data,
          otherwise the server will crash!!!
        *)
        in thr >>= fun () -> thr2
      )

      !pers_cookies_info

      Lwt.return_unit
    else Lwt.return_unit
  in
  update_exp ci >>= fun () ->

  (* the same, for secure cookies: *)
  update_exp sci


(*****************************************************************************)
(* Generation of the page or naservice
   + update the cookie tables (value, expiration date and timeout)        *)

let execute
    now
    generate_page
    ((ri,
      si,
      ((service_cookies_info, data_cookies_info, pers_cookies_info),
       secure_ci),
      ((service_tab_cookies_info, data_tab_cookies_info, pers_tab_cookies_info),
       secure_ci_tab),
      user_tab_cookies) as info)
    sitedata =

  Lwt.catch
    (fun () -> generate_page now info sitedata)
    (fun e -> handle_site_exn e info sitedata)
  >>= fun result ->
  update_cookie_table ~now sitedata
    ((service_cookies_info, data_cookies_info, pers_cookies_info), secure_ci)
  >>= fun () ->

  update_cookie_table ~now sitedata
    ((service_tab_cookies_info, data_tab_cookies_info, pers_tab_cookies_info), secure_ci_tab)
  >>= fun () ->
  Lwt.return result



(** Set expired sessions in request data *)
let set_expired_sessions ri closedservsessions =
  if closedservsessions = ([], [])
  then ()
  else
    Polytables.set
      (Ocsigen_request.request_cache
        ri.Ocsigen_extensions.request_info)
      Eliom_common.eliom_service_session_expired
      closedservsessions


open Ocsigen_extensions

let handled_method = function
  | `GET
  | `HEAD
  | `POST
  | `PUT
  | `DELETE -> true
  | _ -> false

let do_redirection header_id status uri =
  Ocsigen_extensions.Ext_found
    (fun () ->
      let response =
        let headers =
          Cohttp.Header.init_with
            Ocsigen_header.Name.(to_string header_id)
            uri
        in
        Cohttp.Response.make ~status ~headers ()
      in
      Lwt.return (Ocsigen_response.make response))

let gen is_eliom_extension sitedata = function
| Ocsigen_extensions.Req_found _ ->
  Lwt.return Ocsigen_extensions.Ext_do_nothing
| Ocsigen_extensions.Req_not_found (`Not_found as previous_extension_err, req)
  when handled_method (Ocsigen_request.meth
                         req.Ocsigen_extensions.request_info) ->
  let req = Eliom_common.patch_request_info req in
  let now = Unix.gettimeofday () in
  Eliom_common.get_session_info sitedata req 404
  >>= fun (ri, si, previous_tab_cookies_info) ->
  let (all_cookie_info, closedsessions) =
    Eliommod_cookies.get_cookie_info now
      sitedata
      si.Eliom_common.si_service_session_cookies
      si.Eliom_common.si_data_session_cookies
      si.Eliom_common.si_persistent_session_cookies
      si.Eliom_common.si_secure_cookie_info
  in
  let ((all_tab_cookie_info, closedsessions_tab), user_tab_cookies) =
    (* If tab cookie info exists in rc (because an action put them here),
       we get it from here.
       Otherwise we get it from tab cookies in parameters.
    *)
    match previous_tab_cookies_info with
      | Some (atci, utc) -> ((atci, []), utc)
      | None ->
        ((Eliommod_cookies.get_cookie_info now
            sitedata
            si.Eliom_common.si_service_session_cookies_tab
            si.Eliom_common.si_data_session_cookies_tab
            si.Eliom_common.si_persistent_session_cookies_tab
            si.Eliom_common.si_secure_cookie_info_tab),
         Ocsigen_cookie_map.empty
        )
  in
  set_expired_sessions ri (closedsessions, closedsessions_tab);
  let rec gen_aux ((ri, si,
                    all_cookie_info,
                    all_tab_cookie_info,
                    user_tab_cookies) as info) =
    match is_eliom_extension with
      | Some ext ->
          Eliom_extension.run_eliom_extension ext now info sitedata
      | None ->
          let genfun =
            match si.Eliom_common.si_nonatt_info with
              | Eliom_common.RNa_no ->
                (* page generation *)
                Eliom_route.get_page
              | _ ->
                (* anonymous service *)
                Eliom_route.make_naservice
          in
          Lwt.catch
            (fun () ->
               execute
                 now
                 genfun
                 info
                 sitedata >>= fun res ->

               let response, _ = Ocsigen_response.to_cohttp res
               and all_user_cookies = Ocsigen_response.cookies res in
               Eliommod_cookies.compute_cookies_to_send ~final:true
                 sitedata
                 all_cookie_info
                 all_user_cookies
               >>= fun cookies ->
               let res =
                 match
                   Ocsigen_request.header
                     ri.Ocsigen_extensions.request_info
                     (Ocsigen_header.Name.of_string
                        Eliom_common_base.cookie_substitutes_header_name)
                 with
                 | Some _ ->
                   let response =
                     let headers =
                       Cohttp.Header.add
                         (Cohttp.Response.headers response)
                         Eliom_common_base.set_cookie_substitutes_header_name
                         (Eliommod_cookies.cookieset_to_json cookies)
                     in
                     { response with Cohttp.Response.headers }
                   in
                   Ocsigen_response.update ~response ~cookies res
                 | None ->
                   Ocsigen_response.update ~cookies res
               in
               try
                 Polytables.get
                   ~table:
                     (Ocsigen_request.request_cache
                        ri.Ocsigen_extensions.request_info)
                   ~key:Eliom_common.found_stop_key;
                 (* if we find this information in request cache,
                    the request has already been completed.
                    (used after an action).
                    Do not try the following extensions.
                 *)
                 Lwt.return
                   (Ocsigen_extensions.Ext_found_stop
                      (fun () -> Lwt.return res))
               with Not_found ->
                 Lwt.return
                   (Ocsigen_extensions.Ext_found (fun () -> Lwt.return res))

            )
            (function
              (* FIXME COHTTP transition ; restore all that *)
               | Eliom_common.Eliom_Typing_Error l ->
                 Lwt.return
                   (Ocsigen_extensions.Ext_found
                      (fun () ->
                         make_response ~status:`Bad_request
                           (Eliom_error_pages.page_error_param_type l)))
               | Eliom_common.Eliom_Wrong_parameter ->
                 let ripp =
                   match
                     Ocsigen_request.post_params
                       req.request_info
                       ri.request_config.Ocsigen_extensions.uploaddir
                       ri.request_config.Ocsigen_extensions.maxuploadfilesize
                   with
                   | None ->
                     Lwt.return []
                   | Some l ->
                     l
                 in
                 ripp >>= fun ripp ->
                 let response =
                   Eliom_error_pages.page_bad_param
                     (try
                        ignore @@ Polytables.get
                          ~table:
                            (Ocsigen_request.request_cache
                               ri.request_info)
                          ~key:Eliom_common.eliom_params_after_action;
                        true
                      with Not_found ->
                        false)
                     (Ocsigen_request.get_params_flat ri.request_info)
                     (List.map fst ripp)
                 in
                 Lwt.return @@
                 Ocsigen_extensions.Ext_found
                   (fun () ->
                      make_response ~status:`Bad_request response)
               | Eliom_common.Eliom_404 ->
                 Lwt.return
                   (Ocsigen_extensions.Ext_next previous_extension_err)
               | Eliom_common.Eliom_retry_with a -> gen_aux a
               | Eliom_common.Eliom_do_redirection uri ->
                 Lwt.return @@
                 do_redirection
                   Ocsigen_header.Name.location
                   `Temporary_redirect
                   uri
               | Eliom_common.Eliom_do_half_xhr_redirection uri ->
                 Lwt.return @@
                 do_redirection
                   (Ocsigen_header.Name.of_string
                      Eliom_common.half_xhr_redir_header)
                   `No_content
                   uri
               | e -> Lwt.fail e)
  in
  gen_aux (ri, si, all_cookie_info, all_tab_cookie_info, user_tab_cookies)
  | Ocsigen_extensions.Req_not_found (_, ri) ->
      Lwt.return Ocsigen_extensions.Ext_do_nothing
