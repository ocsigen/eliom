(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_pagegen.ml
 * Copyright (C) 2007 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(** Page generation *)

open Lwt

(*****************************************************************************)
(* Exception handler for the site                                            *)

let def_handler sp e = fail e

let handle_site_exn exn (ri, si, _, aci) sitedata =
  sitedata.Eliom_common.exn_handler
    (Eliom_common.make_server_params sitedata aci ri [] si None) exn 
  >>= fun r -> 
  return r


(*****************************************************************************)
(* Generation of the page or naservice
   + update the cookie tables (value, expiration date and timeout)        *)

let execute
    now
    generate_page
    ((ri,
      si,
      old_cookies_to_set,
      ((service_cookies_info, data_cookies_info, pers_cookies_info), 
       secure_ci)) as info)
    sitedata =

  let update_exp (service_cookies_info, data_cookies_info, pers_cookies_info) =

    (* Update service expiration date and value *)
    Ocsigen_http_frame.Cookievalues.iter

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
                    Eliommod_timeouts.find_global_service_timeout name sitedata
                  in
                  (match globaltimeout with
                  | None -> None
                  | Some t -> Some (t +. now))
              | Eliom_common.TNone -> None
              | Eliom_common.TSome t -> Some (t +. now)
      )

      !service_cookies_info;

    (* Update "in memory data" expiration date and value *)
    Ocsigen_http_frame.Cookievalues.iter

      (fun name v ->
        if Lazy.lazy_is_val v (* Only sessions that have been used *)
        then
          let (oldvalue, newr) = Lazy.force v
          in
          match !newr with
          | Eliom_common.SCData_session_expired
          | Eliom_common.SCNo_data -> () (* The cookie has been removed *)
          | Eliom_common.SC newc ->
              newc.Eliom_common.dc_exp :=
                match !(newc.Eliom_common.dc_timeout) with
                | Eliom_common.TGlobal ->
                    let globaltimeout =
                      Eliommod_timeouts.find_global_data_timeout name sitedata
                    in
                    (match globaltimeout with
                    | None -> None
                    | Some t -> Some (t +. now))
                | Eliom_common.TNone -> None
                | Eliom_common.TSome t -> Some (t +. now)
      )

      !data_cookies_info;


    (* Update persistent expiration date, user timeout and value *)
    (* Lwt_util.iter *)
    Ocsigen_http_frame.Cookievalues.fold

      (fun name v thr ->
        let thr2 =
          if Lazy.lazy_is_val v
          then begin
            Lazy.force v >>= fun (oldvalue, newr) ->
              match !newr with
              | Eliom_common.SCData_session_expired
              | Eliom_common.SCNo_data -> (* The cookie has been removed *)
                  return ()
              | Eliom_common.SC newc ->
                  let newexp =
                    match !(newc.Eliom_common.pc_timeout) with
                    | Eliom_common.TGlobal ->
                        let globaltimeout =
                          Eliommod_timeouts.find_global_persistent_timeout
                            name sitedata
                        in
                        (match globaltimeout with
                        | None -> None
                        | Some t -> Some (t +. now))
                    | Eliom_common.TNone -> None
                    | Eliom_common.TSome t -> Some (t +. now)
                  in
                  match oldvalue with
                  | Some (oldv, oldti, oldexp, oldgrp) when
                      (oldexp = newexp &&
                       oldti = !(newc.Eliom_common.pc_timeout) &&
                       oldgrp = !(newc.Eliom_common.pc_session_group) &&
                       oldv = newc.Eliom_common.pc_value) -> return ()
                                                           (* nothing to do *)
                  | Some (oldv, oldti, oldexp, oldgrp) when
                      oldv = newc.Eliom_common.pc_value ->
                      catch
                        (fun () ->
                          Ocsipersist.replace_if_exists
                            Eliommod_persess.persistent_cookies_table
                            newc.Eliom_common.pc_value
                            (name,
                             newexp,
                             !(newc.Eliom_common.pc_timeout),
                             !(newc.Eliom_common.pc_session_group)))
                        (function
                          | Not_found -> return ()
                                (* someone else closed the session *)
                          | e -> fail e)
                  | _ ->
                      Ocsipersist.add
                        Eliommod_persess.persistent_cookies_table
                        newc.Eliom_common.pc_value
                        (name,
                         newexp,
                         !(newc.Eliom_common.pc_timeout),
                         !(newc.Eliom_common.pc_session_group))

(*VVV Do not forget to change persistent_cookie_table_version
   if you change the type of persistent table data,
   otherwise the server will crash!!!
 *)
          end
          else return ()
        in thr >>= fun () -> thr2
      )

      !pers_cookies_info

      (return ())

  in
  catch
    (fun () -> generate_page now info sitedata)
    (fun e -> handle_site_exn e info sitedata)
  >>= fun result ->

  update_exp (service_cookies_info, data_cookies_info, pers_cookies_info)
  >>= fun () ->

  (* the same, for secure cookies: *)
  (match secure_ci with
    | Some info -> update_exp info
    | None -> Lwt.return ())
  >>= fun () ->

  Lwt.return result



(** Compute the exceptions from expired sessions *)
let compute_exn closedservsessions =
  (if closedservsessions = []
  then []
  else [Eliom_common.Eliom_Service_session_expired closedservsessions])


open Ocsigen_extensions

let gen is_eliom_extension sitedata = function
| Ocsigen_extensions.Req_found _ -> 
    Lwt.return Ocsigen_extensions.Ext_do_nothing
| Ocsigen_extensions.Req_not_found (404 as previous_extension_err, ri) ->
  let now = Unix.time () in
  Eliom_common.change_request_info ri previous_extension_err
  >>= fun (ri, si) ->
  let (all_cookie_info, closedsessions) =
    Eliommod_cookies.get_cookie_info now
      sitedata
      si.Eliom_common.si_service_session_cookies
      si.Eliom_common.si_data_session_cookies
      si.Eliom_common.si_persistent_session_cookies
      si.Eliom_common.si_secure_cookie_info
  in
  let exn = compute_exn closedsessions in
  let rec gen_aux ((ri, si, old_cookies_to_set, all_cookie_info) as info) =
    match is_eliom_extension with
      | Some ext -> 
          Eliommod_extensions.run_eliom_extension ext now info sitedata
      | None ->
          let genfun =
            match si.Eliom_common.si_nonatt_info with
              | Eliom_common.Na_no ->
                  (* page generation *)
                  Eliommod_services.get_page
              | _ ->
                  (* anonymous service *)
                  Eliommod_naservices.make_naservice
          in
      catch
        (fun () ->
           execute
             now
             genfun
             info
             sitedata >>= fun result_to_send ->

          match result_to_send with
          | Eliom_common.EliomExn (exnlist, cookies_set_by_page) ->
                     (* It is an action, we reload the page.
                        To do that, we retry without POST params.
                        If no post param at all, we retry
                        without GET non_att info.
                        If no GET non_att info, we retry without
                        GET state.
                        If no GET state,
                        we do not reload, otherwise it will loop.
                      *)
(* be very carefull while re-reading this *)
              let all_user_cookies =
                Eliommod_cookies.add_cookie_list_to_send
                  sitedata
                  cookies_set_by_page
                  old_cookies_to_set
              in

              (match si.Eliom_common.si_nonatt_info,
                si.Eliom_common.si_state_info,
                ri.request_info.ri_method with
              | Eliom_common.Na_no,
                (Eliom_common.Att_no, Eliom_common.Att_no), 
                    Ocsigen_http_frame.Http_header.GET ->
                  Eliommod_cookies.compute_cookies_to_send
                    sitedata
                    all_cookie_info
                    all_user_cookies
                  >>= fun all_new_cookies ->
                  let empty_result = Ocsigen_http_frame.empty_result () in
                  return
                    (Ocsigen_extensions.Ext_found
                       (fun () ->
                         Lwt.return
                           {empty_result with
                            Ocsigen_http_frame.res_cookies= all_new_cookies}))

              | _ ->

                  Eliommod_cookies.compute_new_ri_cookies
                    now
                    ri.request_info.ri_sub_path
                    (Lazy.force ri.request_info.ri_cookies)
                    all_cookie_info
                    cookies_set_by_page
(*VVV old_cookies_to_set already are in ri_cookies, right? *)
                  >>= fun ric ->

                  Eliommod_cookies.compute_cookies_to_send
                    sitedata
                    all_cookie_info
                    all_user_cookies
                  >>= fun all_new_cookies ->

                  (match
                    si.Eliom_common.si_nonatt_info,
                    si.Eliom_common.si_state_info,
                    ri.request_info.ri_method
                  with
                  | Eliom_common.Na_get_ _,
                    (_, Eliom_common.Att_no), Ocsigen_http_frame.Http_header.GET
                  | Eliom_common.Na_get' _,
                    (_, Eliom_common.Att_no), Ocsigen_http_frame.Http_header.GET
                      (* no post params, GET na coservice *)

                  | Eliom_common.Na_no,
                    (_, Eliom_common.Att_no), Ocsigen_http_frame.Http_header.GET
                      (* no post params, GET attached coservice *)
                      ->
                      return
                        (* Ext_retry_with, not Eliom_retry_with *)
                        (Ocsigen_extensions.Ext_retry_with
                           ({ri with request_info = {
                               ri.request_info with
                                 ri_get_params =
                                   lazy si.Eliom_common.si_other_get_params;
                                 ri_cookies= lazy ric;
                                 ri_extension_info= exnlist
(* @ri.ri_extension_info *)
(*VVV I do not keep the old exceptions any more,
  otherwise no way to remove them. *)
                             }},
                            all_new_cookies
                           ))


                  | Eliom_common.Na_post_ _, (_, _), _
                  | Eliom_common.Na_post' _, (_, _), _ ->
                      (* POST na coservice *)
                      (* retry without POST params *)

                      return
                        (* Ext_retry_with, not Eliom_retry_with *)
                        (Ocsigen_extensions.Ext_retry_with
                           ({ri with request_info = {
                               ri.request_info with
                                 ri_post_params = lazy (return []);
                                 ri_get_params =
                                   lazy si.Eliom_common.si_other_get_params;
                                 ri_method = Ocsigen_http_frame.Http_header.GET;
                                 ri_cookies= lazy ric;
                                 ri_extension_info= exnlist
                             }},
                            all_new_cookies
                           ))


                  | _ ->
                      (* retry without POST params *)
(*VVV 
Warning: is it possible to have POST method but no POST parameter?
--> may loop...
*)
                      return
                        (* Ext_retry_with, not Eliom_retry_with *)
                        (Ocsigen_extensions.Ext_retry_with
                           ({ri with request_info = {
                               ri.request_info with
                                 ri_post_params = lazy (return []);
                                 ri_method = Ocsigen_http_frame.Http_header.GET;
                                 ri_cookies= lazy ric;
                                 ri_extension_info= exnlist
                             }},
                            all_new_cookies
                           ))
                  )
              )

          | Eliom_common.EliomResult res ->

              let all_user_cookies =
                Ocsigen_http_frame.add_cookies
                  res.Ocsigen_http_frame.res_cookies
                  old_cookies_to_set
              in

              Eliommod_cookies.compute_cookies_to_send
                sitedata
                all_cookie_info
                all_user_cookies

              >>= fun all_new_cookies ->

              return
                (Ocsigen_extensions.Ext_found
                   (fun () ->
                     Lwt.return
                       {res with
                        Ocsigen_http_frame.res_cookies= all_new_cookies}))
      )
      (function
        | Eliom_common.Eliom_Typing_Error l ->
            Ocsigen_senders.Xhtml_content.result_of_content
              (Error_pages.page_error_param_type l) >>= fun r ->
            return (Ocsigen_extensions.Ext_found
                      (fun () ->
                        Lwt.return
                          {r with
                           Ocsigen_http_frame.res_cookies = old_cookies_to_set;
                           Ocsigen_http_frame.res_code= 500;
                         }))
        | Eliom_common.Eliom_Wrong_parameter ->
            Lazy.force ri.request_info.ri_post_params >>= fun ripp ->
            Ocsigen_senders.Xhtml_content.result_of_content
                (Error_pages.page_bad_param (List.map fst ripp)) >>= fun r ->
            return (Ocsigen_extensions.Ext_found
                      (fun () ->
                        Lwt.return
                          {r with
                           Ocsigen_http_frame.res_cookies= old_cookies_to_set;
                           Ocsigen_http_frame.res_code= 500;
                         }))
        | Eliom_common.Eliom_404 ->
            return (Ocsigen_extensions.Ext_next previous_extension_err)
        | Eliom_common.Eliom_retry_with a -> gen_aux a
        | e -> fail e)
  in
  gen_aux
    ({ri with request_info = {
        ri.request_info with
          ri_extension_info=
            exn@ri.request_info.ri_extension_info}},
     si,
     Ocsigen_http_frame.Cookies.empty,
     all_cookie_info)
  | Ocsigen_extensions.Req_not_found (_, ri) ->
      Lwt.return Ocsigen_extensions.Ext_do_nothing
