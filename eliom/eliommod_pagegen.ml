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

let handle_site_exn exn (ri, si, aci) sitedata =
  sitedata.Eliom_common.exn_handler
    (Eliom_common.make_server_params sitedata aci ri None si None) exn 
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
      ((service_cookies_info, data_cookies_info, pers_cookies_info), 
       secure_ci)) as info)
    sitedata =

  let update_exp (service_cookies_info, data_cookies_info, pers_cookies_info) =

    (* Update service expiration date and value *)
    Ocsigen_lib.String_Table.iter

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
    Ocsigen_lib.String_Table.iter

      (fun name v ->
        if Lazy.lazy_is_val v (* Only sessions that have been used *)
        then
          let (oldvalue, newr) = Lazy.force v in
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
    Ocsigen_lib.String_Table.fold

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
                            (Lazy.force 
                               Eliommod_persess.persistent_cookies_table)
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
                        (Lazy.force Eliommod_persess.persistent_cookies_table)
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



(** Set expired sessions in request data *)
let set_expired_sessions ri closedservsessions =
  if closedservsessions = []
  then ()
  else
    Polytables.set
      ri.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache
      Eliom_common.eliom_service_session_expired
      closedservsessions


open Ocsigen_extensions

let gen is_eliom_extension sitedata = function
| Ocsigen_extensions.Req_found _ -> 
    Lwt.return Ocsigen_extensions.Ext_do_nothing
| Ocsigen_extensions.Req_not_found (404 as previous_extension_err, ri) ->
  let now = Unix.time () in
  Eliom_common.get_session_info ri previous_extension_err
  >>= fun (ri, si) ->
  let (all_cookie_info, closedsessions) =
    Eliommod_cookies.get_cookie_info now
      sitedata
      si.Eliom_common.si_service_session_cookies
      si.Eliom_common.si_data_session_cookies
      si.Eliom_common.si_persistent_session_cookies
      si.Eliom_common.si_secure_cookie_info
  in
  set_expired_sessions ri closedsessions;
  let rec gen_aux ((ri, si, all_cookie_info) as info) =
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
                 sitedata >>= fun res ->

               let all_user_cookies = 
                 res.Ocsigen_http_frame.res_cookies
               in
           
               Eliommod_cookies.compute_cookies_to_send
                 sitedata
                 all_cookie_info
                 all_user_cookies
                 
               >>= fun all_new_cookies ->

               Lwt.return
                 (Ocsigen_extensions.Ext_found
                    (fun () ->
                       Lwt.return
                         {res with
                            Ocsigen_http_frame.res_cookies= 
                             all_new_cookies}))
            )
            (function
               | Eliom_common.Eliom_Typing_Error l ->
                   Ocsigen_senders.Xhtml_content.result_of_content
                     (Error_pages.page_error_param_type l)
                   >>= fun r ->
                   Lwt.return
                     (Ocsigen_extensions.Ext_found
                        (fun () ->
                           Lwt.return
                             {r with
                                Ocsigen_http_frame.res_code= 400;
                             }))
               | Eliom_common.Eliom_Wrong_parameter ->
                   Lazy.force ri.request_info.ri_post_params >>= fun ripp ->
                   Ocsigen_senders.Xhtml_content.result_of_content
                     (Error_pages.page_bad_param 
                        (try 
                           Polytables.get
                             ~table:ri.request_info.Ocsigen_extensions.ri_request_cache
                             ~key:Eliom_common.eliom_params_after_action;
                           true 
                         with Not_found -> false)
                        (Lazy.force ri.request_info.ri_get_params)
                        (List.map fst ripp))
                   >>= fun r ->
                   Lwt.return
                     (Ocsigen_extensions.Ext_found
                        (fun () ->
                           Lwt.return
                             {r with
                                Ocsigen_http_frame.res_code= 500;
                             }))
               | Eliom_common.Eliom_404 ->
                   Lwt.return
                     (Ocsigen_extensions.Ext_next previous_extension_err)
               | Eliom_common.Eliom_retry_with a -> gen_aux a
               | Eliom_common.Eliom_Suffix_redirection uri ->
                   let e = Ocsigen_http_frame.empty_result () in
                   Lwt.return 
                       (Ocsigen_extensions.Ext_found
                          (fun () ->
                             Lwt.return
                               {e with
                                  Ocsigen_http_frame.res_code= 307;
                                  Ocsigen_http_frame.res_location = Some uri}))
               | e -> fail e)
  in
  gen_aux (ri, si, all_cookie_info)
  | Ocsigen_extensions.Req_not_found (_, ri) ->
      Lwt.return Ocsigen_extensions.Ext_do_nothing
