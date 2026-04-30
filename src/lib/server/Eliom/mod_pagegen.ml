open Lwt.Syntax

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
  Cohttp.Header.add_opt headers
    Ocsigen_http.Header.Name.(to_string content_type)
    (Printf.sprintf "%s; charset=utf-8" Content_core.Html.D.Info.content_type)

let out =
  let encode x = fst (Xml_print.Utf8.normalize_html x) in
  Content_core.Html.Printer.pp ~encode ()

let make_response ?headers ~status body =
  let body = Ocsigen.Response.Body.of_string (Format.asprintf "%a" out body)
  and response =
    let headers = headers_with_content_type headers in
    Cohttp.Response.make ~status ~headers ()
  in
  Lwt.return (Ocsigen.Response.make ~body response)

(* module Html_content = Ocsigen_senders.Make_XML_Content(Xml)(Html.F) *)

(* Exception handler for the site *)

let def_handler e = Lwt.fail e

(* Update cookie tables *)
let update_cookie_table ?now sitedata (ci, sci) =
  let now = match now with Some n -> n | None -> Unix.gettimeofday () in
  let update_exp (service_cookies_info, data_cookies_info, pers_cookies_info) =
    (* Update service expiration date and value *)
    Common.Full_state_name_table.iter
      (fun name (_oldvalue, newr) ->
         (* catch fun () -> *)
         match !newr with
         | Common.SCData_session_expired | Common.SCNo_data ->
             () (* The cookie has been removed *)
         | Common.SC newc -> (
             newc.Common.sc_exp :=
               match !(newc.Common.sc_timeout) with
               | Common.TGlobal -> (
                   let globaltimeout =
                     Mod_timeouts.find_global `Service name sitedata
                   in
                   match globaltimeout with
                   | None -> None
                   | Some t -> Some (t +. now))
               | Common.TNone -> None
               | Common.TSome t -> Some (t +. now)))
      !service_cookies_info;
    (* Update "in memory data" expiration date and value *)
    Common.Full_state_name_table.iter
      (fun name v ->
         (* 2018-07-17 We do this for all volatile sessions,
           even if it has not been used,
           otherwise, sessions could have different duration.
           (Before: we were doing this only if (Lazy.is_val v))
           Keeping same duration is important for example for comet
           (which is using both service and volatile data sessions).
         *)
         let _oldvalue, newr = Lazy.force v in
         match !newr with
         | Common.SCData_session_expired | Common.SCNo_data ->
             () (* The cookie has been removed *)
         | Common.SC newc -> (
             newc.Common.dc_exp :=
               match !(newc.Common.dc_timeout) with
               | Common.TGlobal -> (
                   let globaltimeout =
                     Mod_timeouts.find_global `Data name sitedata
                   in
                   match globaltimeout with
                   | None -> None
                   | Some t -> Some (t +. now))
               | Common.TNone -> None
               | Common.TSome t -> Some (t +. now)))
      !data_cookies_info;
    let module Expiry_tolerance = struct
      (* Avoid cookie updates that only change the cookie
         expiry date by a negligible amount of time. *)
      let timeout_tolerance_factor = 0.01

      let within_tolerance x y =
        let diff = Float.abs (x -. y) in
        diff < timeout_tolerance_factor *. Float.abs (x -. now)

      let within_tolerance_opt x y =
        match x, y with Some x, Some y -> within_tolerance x y | _ -> x = y
    end
    in
    (* Update persistent expiration date, user timeout and value *)
    (* 2018-07-17 We do this for all persistent sessions
       only if one persistent session has been used:
       - all persistent sessions will have same duration
       - will not do too many database requests
    *)
    if
      Common.Full_state_name_table.exists
        (fun _ v -> Lazy.is_val v)
        !pers_cookies_info
    then
      Common.Full_state_name_table.fold
        (fun name v thr ->
           let thr2 =
             Lazy.force v >>= fun (oldvalue, newr) ->
             match !newr with
             | Common.SCData_session_expired | Common.SCNo_data ->
                 (* The cookie has been removed *)
                 Lwt.return ()
             | Common.SC newc -> (
                 let newexp =
                   match !(newc.Common.pc_timeout) with
                   | Common.TGlobal -> (
                       let globaltimeout =
                         Mod_timeouts.find_global `Persistent name sitedata
                       in
                       match globaltimeout with
                       | None -> None
                       | Some t -> Some (t +. now))
                   | Common.TNone -> None
                   | Common.TSome t -> Some (t +. now)
                 in
                 match oldvalue with
                 | Some (_, oldti, oldexp, oldgrp)
                   when Expiry_tolerance.within_tolerance_opt oldexp newexp
                        && oldti = !(newc.Common.pc_timeout)
                        && oldgrp = !(newc.Common.pc_session_group)
                        && newc.Common.pc_set_value = None ->
                     Lwt.return ()
                 (* nothing to do *)
                 | Some (_, _oldti, oldexp, _oldgrp)
                   when newc.Common.pc_set_value = None ->
                     Lwt.catch
                       (fun () ->
                          let cookieid =
                            Common.(Hashed_cookies.to_string newc.pc_hvalue)
                          in
                          Mod_cookies.Persistent_cookies.replace_if_exists
                            cookieid
                            { Mod_cookies.full_state_name = name
                            ; expiry = newexp
                            ; timeout = !(newc.Common.pc_timeout)
                            ; session_group = !(newc.Common.pc_session_group) }
                          >>= fun () ->
                          Mod_cookies.Persistent_cookies.Expiry_dates
                          .remove_cookie oldexp cookieid)
                       (function
                         | Not_found -> Lwt.return ()
                         (* someone else closed the session *)
                         | e -> Lwt.fail e)
                 | _ ->
                     Mod_cookies.Persistent_cookies.add
                       Common.(Hashed_cookies.to_string newc.pc_hvalue)
                       { Mod_cookies.full_state_name = name
                       ; expiry = newexp
                       ; timeout = !(newc.Common.pc_timeout)
                       ; session_group = !(newc.Common.pc_session_group) })
             (*VVV Do not forget to change persistent_cookie_table_version
          if you change the type of persistent table data,
          otherwise the server will crash!!!
             *)
           in
           thr >>= fun () -> thr2)
        !pers_cookies_info Lwt.return_unit
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
      ({Common.all_cookie_info; tab_cookie_info; _} as info)
      sitedata
  =
  let* result =
    Lwt.catch
      (fun () -> generate_page now info sitedata)
      (fun e -> sitedata.Common.exn_handler e)
  in
  let* () = update_cookie_table ~now sitedata all_cookie_info in
  let* () = update_cookie_table ~now sitedata tab_cookie_info in
  Lwt.return result

(** Set expired sessions in request data *)
let set_expired_sessions ri closedservsessions =
  if closedservsessions = ([], [])
  then ()
  else
    Polytables.set
      ~table:(Ocsigen.Request.request_cache ri.Ocsigen.Extensions.request_info)
      ~key:Common.eliom_service_session_expired ~value:closedservsessions

open Ocsigen.Extensions

let handled_method = function
  | `GET | `HEAD | `POST | `PUT | `DELETE -> true
  | _ -> false

let do_redirection header_id status uri =
  Ocsigen.Extensions.Ext_found
    (fun () ->
      let response =
        let headers =
          Cohttp.Header.init_with Ocsigen_http.Header.Name.(to_string header_id) uri
        in
        Cohttp.Response.make ~status ~headers ()
      in
      Lwt.return (Ocsigen.Response.make response))

let gen_req_not_found ~is_eliom_extension ~sitedata ~previous_extension_err ~req
  =
  let req = Common.patch_request_info req in
  let now = Unix.gettimeofday () in
  let* ri, si, previous_tab_cookies_info =
    Common.get_session_info ~sitedata ~req 404
  in
  let all_cookie_info, closedsessions =
    Mod_cookies.get_cookie_info now sitedata
      si.Common.si_service_session_cookies si.Common.si_data_session_cookies
      si.Common.si_persistent_session_cookies si.Common.si_secure_cookie_info
  in
  let (tab_cookie_info, closedsessions_tab), user_tab_cookies =
    (* If tab cookie info exists in rc (because an action put them here),
       we get it from here.
       Otherwise we get it from tab cookies in parameters.
    *)
    match previous_tab_cookies_info with
    | Some (atci, utc) -> (atci, []), utc
    | None ->
        ( Mod_cookies.get_cookie_info now sitedata
            si.Common.si_service_session_cookies_tab
            si.Common.si_data_session_cookies_tab
            si.Common.si_persistent_session_cookies_tab
            si.Common.si_secure_cookie_info_tab
        , Ocsigen_cookie_map.empty )
  in
  set_expired_sessions ri (closedsessions, closedsessions_tab);
  let rec gen_aux
            ({Common.request = ri; session_info = si; all_cookie_info; _} as
             info)
    =
    let sp = Common.make_server_params sitedata info None None in
    (* The last two arguments are not yet available, so for now we use None.
       This value will later be overwritten once this information is available. *)
    Lwt.with_value Common.sp_key (Some sp) @@ fun () ->
    let genfun =
      match si.Common.si_nonatt_info with
      | Common.RNa_no ->
          (* page generation *)
          Route.get_page
      | _ ->
          (* anonymous service *)
          Route.make_naservice
    in
    Lwt.catch
      (fun () ->
         let* res = execute now genfun info sitedata in
         let response = Ocsigen.Response.response res
         and all_user_cookies = Ocsigen.Response.cookies res in
         let* cookies =
           Mod_cookies.compute_cookies_to_send sitedata all_cookie_info
             all_user_cookies
         in
         let res =
           match
             Ocsigen.Request.header ri.Ocsigen.Extensions.request_info
               (Ocsigen_http.Header.Name.of_string
                  Common_base.cookie_substitutes_header_name)
           with
           | Some _ ->
               let response =
                 let headers =
                   Cohttp.Header.add
                     (Cohttp.Response.headers response)
                     Common_base.set_cookie_substitutes_header_name
                     (Mod_cookies.cookieset_to_json cookies)
                 in
                 {response with Cohttp.Response.headers}
               in
               Ocsigen.Response.update ~response ~cookies res
           | None -> Ocsigen.Response.update ~cookies res
         in
         try
           Polytables.get
             ~table:
               (Ocsigen.Request.request_cache ri.Ocsigen.Extensions.request_info)
             ~key:Common.found_stop_key;
           (* if we find this information in request cache,
              the request has already been completed.
              (used after an action).
              Do not try the following extensions.
           *)
           Lwt.return
             (Ocsigen.Extensions.Ext_found_stop (fun () -> Lwt.return res))
         with Not_found ->
           Lwt.return (Ocsigen.Extensions.Ext_found (fun () -> Lwt.return res)))
      (function
        (* FIXME COHTTP transition ; restore all that *)
        | Common.Eliom_Typing_Error l ->
            Lwt.return
              (Ocsigen.Extensions.Ext_found
                 (fun () ->
                   make_response ~status:`Bad_request
                     (Error_pages.page_error_param_type l)))
        | Common.Eliom_Wrong_parameter ->
            let* ripp =
              match
                Ocsigen.Request.post_params req.request_info
                  ri.request_config.Ocsigen.Extensions.uploaddir
                  ri.request_config.Ocsigen.Extensions.maxuploadfilesize
              with
              | None -> Lwt.return []
              | Some l -> l
            in
            let response =
              Error_pages.page_bad_param
                (try
                   ignore
                   @@ Polytables.get
                        ~table:(Ocsigen.Request.request_cache ri.request_info)
                        ~key:Common.eliom_params_after_action;
                   true
                 with Not_found -> false)
                (Ocsigen.Request.get_params_flat ri.request_info)
                (List.map fst ripp)
            in
            Lwt.return
            @@ Ocsigen.Extensions.Ext_found
                 (fun () -> make_response ~status:`Bad_request response)
        | Common.Eliom_404 ->
            Lwt.return (Ocsigen.Extensions.Ext_next previous_extension_err)
        | Common.Eliom_retry_with a -> gen_aux a
        | Common.Do_redirection uri ->
            Lwt.return
            @@ do_redirection Ocsigen_http.Header.Name.location `Temporary_redirect
                 uri
        | Common.Do_half_xhr_redirection uri ->
            Lwt.return
            @@ do_redirection
                 (Ocsigen_http.Header.Name.of_string Common.half_xhr_redir_header)
                 `No_content uri
        | e -> Lwt.fail e)
  in
  let info =
    { Common.request = ri
    ; session_info = si
    ; all_cookie_info
    ; tab_cookie_info
    ; user_tab_cookies }
  in
  match is_eliom_extension with
  | Some ext -> Extension.run_eliom_extension ext now info sitedata
  | None -> gen_aux info

let gen is_eliom_extension sitedata =
  let open Ocsigen.Extensions in
  function
  | Req_found _ -> Lwt.return Ext_do_nothing
  | Req_not_found ((`Not_found as previous_extension_err), req)
    when handled_method (Ocsigen.Request.meth req.request_info) ->
      gen_req_not_found ~is_eliom_extension ~sitedata ~previous_extension_err
        ~req
  | Req_not_found (_, _ri) -> Lwt.return Ext_do_nothing
