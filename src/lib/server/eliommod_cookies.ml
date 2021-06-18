(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_cookies.ml
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

(** Cookie management                                                       *)
open Eliom_lib
open Lwt

include Eliom_cookies_base

(*****************************************************************************)
let make_new_session_id () =
  Ocsigen_lib.make_cryptographic_safe_string () ^ "H"




(*****************************************************************************)
(* cookie manipulation *)

(** look in table to find if the session cookies sent by the browser
    correspond to existing (and not closed) sessions *)
let get_cookie_info
    now
    sitedata
    service_cookies
    data_cookies
    persistent_cookies
    secure_cookies
    : 'a Eliom_common.cookie_info * 'b list =

  (* get info about service session cookies: *)
  let f_serv service_cookies =
    Eliom_common.Full_state_name_table.fold
      (fun name value (oktable, failedlist) ->
        try
          let hvalue = Eliom_common.hash_cookie value in
          let full_state_name, ta, expref, timeout_ref, sessgrpref, sessgrpnode =
            Eliom_common.SessionCookies.find
              sitedata.Eliom_common.session_services hvalue
          in
          Eliommod_sessiongroups.Serv.up sessgrpnode;
          match !expref with
          | Some t when t < now ->
              (* session expired by timeout *)
              Eliommod_sessiongroups.Serv.remove sessgrpnode;
              ((Eliom_common.Full_state_name_table.add
                  name
                  (Some value          (* value sent by the browser *),
                   ref Eliom_common.SCData_session_expired
                                       (* ask the browser
                                          to remove the cookie *))
                  oktable),
               name::failedlist)
          | _ -> ((Eliom_common.Full_state_name_table.add
                     name
                     (Some value        (* value sent by the browser *),
                      ref
                        (Eliom_common.SC
                           {Eliom_common.sc_hvalue= hvalue  (* value *);
                            Eliom_common.sc_set_value= `None;
                            Eliom_common.sc_table= ref ta (* the table of session services *);
                            Eliom_common.sc_timeout= timeout_ref (* user timeout ref *);
                            Eliom_common.sc_exp= expref  (* expiration date (server side) *);
                            Eliom_common.sc_cookie_exp=
                            ref Eliom_common.CENothing
                              (* cookie expiration date to send
                                 to the browser.
                                 We don't change it *);
                            Eliom_common.sc_session_group= sessgrpref;
                            Eliom_common.sc_session_group_node= sessgrpnode;
                           }))
                     oktable),
                  failedlist)
        with Not_found ->
          ((Eliom_common.Full_state_name_table.add
              name
              (Some value              (* value sent by the browser *),
               ref Eliom_common.SCData_session_expired
                                          (* ask the browser
                                             to remove the cookie *))
              oktable),
           name::failedlist)
      )
      service_cookies
      (Eliom_common.Full_state_name_table.empty, [])
  in

  (* get info about "in memory" data session cookies: *)
  let f_data data_cookies =
    Eliom_common.Full_state_name_table.map
      (fun value ->
        lazy
          (try
            let hvalue = Eliom_common.hash_cookie value in
            let full_state_name, expref, timeout_ref, sessgrpref, sessgrpnode =
              Eliom_common.SessionCookies.find
                sitedata.Eliom_common.session_data hvalue
            in
            Eliommod_sessiongroups.Serv.up sessgrpnode;
            match !expref with
              | Some t when t < now ->
                  (* session expired by timeout *)
                  Eliommod_sessiongroups.Data.remove sessgrpnode;
                  (Some value                 (* value sent by the browser *),
                   ref Eliom_common.SCData_session_expired
                                              (* ask the browser
                                                 to remove the cookie *))
              | _ ->
                  (Some value        (* value sent by the browser *),
                   ref
                     (Eliom_common.SC
                        {Eliom_common.dc_hvalue= hvalue       (* value *);
                         Eliom_common.dc_set_value= `None;
                         Eliom_common.dc_timeout= timeout_ref
                          (* user timeout ref *);
                         Eliom_common.dc_exp= expref (* expiration date
                                                        (server side) *);
                         Eliom_common.dc_cookie_exp=
                            ref Eliom_common.CENothing
                              (* cookie expiration date to send
                                 to the browser.
                                 We don't change it *);
                         Eliom_common.dc_session_group= sessgrpref;
                         Eliom_common.dc_session_group_node= sessgrpnode}
                     )
                  )
           with Not_found ->
             (Some value                  (* value sent by the browser *),
              ref Eliom_common.SCData_session_expired
                (* ask the browser
                   to remove the cookie *))))
      data_cookies
  in

  (* *** get info about persistent session cookies: *)
  let f_pers persistent_cookies =
    Eliom_common.Full_state_name_table.map
      (fun value ->
        lazy
          (catch
             (fun () ->
               let hvalue = Eliom_common.hash_cookie value in
               Eliom_common.Persistent_cookies.Cookies.find hvalue >>=
               fun (full_state_name, persexp, perstimeout, sessgrp) ->

                 Eliommod_sessiongroups.Pers.up hvalue sessgrp >>= fun () ->
                 match persexp with
                 | Some t when t < now ->
                     (* session expired by timeout *)
                     Eliom_common.remove_from_all_persistent_tables
                       hvalue >>= fun () ->
                       return
                         (Some (value         (* value at the beginning
                                                 of the request *),
                                perstimeout   (* user persistent timeout
                                                 at the beginning
                                                 of the request *),
                                persexp       (* expiration date (server)
                                                 at the beginning
                                                 of the request *),
                                sessgrp       (* session group at beginning *)),
                          ref Eliom_common.SCData_session_expired
                                              (* ask the browser to
                                                 remove the cookie *))
                 | _ ->
                     return
                       (Some (value        (* value at the beginning
                                              of the request *),
                              perstimeout  (* user persistent timeout
                                              at the beginning
                                              of the request *),
                              persexp      (* expiration date (server)
                                              at the beginning
                                              of the request *),
                              sessgrp      (* session group at beginning *)),
                        (ref
                           (Eliom_common.SC
                              {Eliom_common.pc_hvalue= hvalue
                                 (* value *);
                               Eliom_common.pc_set_value= `None;
                               Eliom_common.pc_timeout= ref perstimeout
                                 (* user persistent timeout ref *);
                               Eliom_common.pc_cookie_exp=
                               ref Eliom_common.CENothing
                                 (* persistent cookie expiration
                                    date ref to send to the browser:
                                    We don't change it *);
                               Eliom_common.pc_session_group= ref sessgrp
                             })))
             )
             (function
               | Not_found ->
                   return
                     (Some (value         (* value at the beginning
                                             of the request *),
                            Eliom_common.TGlobal
                                          (* user persistent timeout
                                             at the beginning
                                             of the request *),
                            Some 0.       (* expiration date (server)
                                             at the beginning
                                             of the request *),
                            None          (* session group at beginning *)),
                      ref Eliom_common.SCData_session_expired
                                          (* ask the browser
                                             to remove the cookie *))
               | e -> fail e)
          )
      )
      persistent_cookies (* the persistent cookies sent by the request *)
  in


  let (servoktable, servfailedlist) = f_serv service_cookies in
  let dataoktable = f_data data_cookies in
  let persoktable = f_pers persistent_cookies in

  let sec, sservfailedlist =
    let (sc, dc, pc) = secure_cookies in
    let (servoktable, servfailedlist) = f_serv sc in
    let dataoktable = f_data dc in
    let persoktable = f_pers pc in
    ((ref servoktable, ref dataoktable, ref persoktable), servfailedlist)
  in

  (((ref servoktable, ref dataoktable, ref persoktable), sec),
   sservfailedlist@servfailedlist)


(*****************************************************************************)

(* table cookie -> session table *)
let new_service_cookie_table () :
    Eliom_common.tables Eliom_common.servicecookiestable =
  Eliom_common.SessionCookies.create 100

let new_data_cookie_table () : Eliom_common.datacookiestable =
  Eliom_common.SessionCookies.create 100




(*****************************************************************************)
(* Create the table of cookies to send to the browser or to unset            *)
(* (from cookie_info)                                                        *)

let compute_session_cookies_to_send ~final
    sitedata
    ((service_cookie_info,
      data_cookie_info,
      pers_cookies_info), secure_ci) (endlist: Ocsigen_cookie_map.t) =
  let getservvexp (old, newi) =
    return
      (let newinfo =
        match !newi with
        | Eliom_common.SCNo_data
        | Eliom_common.SCData_session_expired -> None
        | Eliom_common.SC c ->
            let v = c.Eliom_common.sc_set_value in
            if final then c.Eliom_common.sc_set_value <- `Used;
            Some (c.Eliom_common.sc_hvalue,
                  v,
                  !(c.Eliom_common.sc_cookie_exp))
      in (old, newinfo))
  in
  let getdatavexp v =
    if Lazy.is_val v
    then
      return
        (let (old, newi) = Lazy.force v in
        let newinfo =
          match !newi with
          | Eliom_common.SCNo_data
          | Eliom_common.SCData_session_expired -> None
          | Eliom_common.SC c ->
              let v = c.Eliom_common.dc_set_value in
              if final then c.Eliom_common.dc_set_value <- `Used;
              Some (c.Eliom_common.dc_hvalue,
                    v,
                    !(c.Eliom_common.dc_cookie_exp))
        in (old, newinfo))
    else fail Not_found
  in
  let getpersvexp v =
    if Lazy.is_val v
    then
      Lazy.force v >>= fun (old, newi) ->
      return
        (let oldinfo =
          match old with
            | None -> None
            | Some (v, _, _, _) -> Some v
         in
         let newinfo =
           match !newi with
             | Eliom_common.SCNo_data
             | Eliom_common.SCData_session_expired -> None
             | Eliom_common.SC c ->
                 let v = c.Eliom_common.pc_set_value in
                 if final then c.Eliom_common.pc_set_value <- `Used;
                 Some (c.Eliom_common.pc_hvalue,
                       v,
                       !(c.Eliom_common.pc_cookie_exp))
         in (oldinfo, newinfo))
    else fail Not_found
  in
  let ch_exp = function
    | Eliom_common.CENothing
    | Eliom_common.CEBrowser -> None
    | Eliom_common.CESome a -> Some a
  in
  let aux f cookiekind secure tab2 cooktab =
    cooktab >>= fun cooktab ->
    Eliom_common.Full_state_name_table.fold
      (fun full_st_name value beg ->
        beg >>= fun beg ->
        catch
          (fun () ->
            f value >>= fun (old, newc) ->
            return
              (match old, newc with
                | None, None -> beg
                | Some _, None ->
                  Ocsigen_cookie_map.add
                    sitedata.Eliom_common.site_dir
                    (Eliom_common.make_full_cookie_name
                       cookiekind full_st_name)
                    OUnset
                    beg
              (* the path is always site_dir because the cookie cannot
                 have been unset by a service outside
                 this site directory *)
                | _, Some (_, `Set v, exp) -> (* New value *)
                  Ocsigen_cookie_map.add
                    sitedata.Eliom_common.site_dir
                    (Eliom_common.make_full_cookie_name
                       cookiekind full_st_name)
                    (OSet (ch_exp exp, v, secure))
                    beg
                | Some oldv, Some (_, `None, exp) ->
                  if exp = Eliom_common.CENothing
                  then beg
                  else
                    Ocsigen_cookie_map.add
                      sitedata.Eliom_common.site_dir
                      (Eliom_common.make_full_cookie_name
                         cookiekind full_st_name)
                      (OSet (ch_exp exp, oldv, secure))
                      beg
                | None, Some (_, `None, _) -> (* Should not happen *)
                   beg
                | _, Some (_, `Used, _) ->
                   assert false (* Something wrong happened *)
              )
          )
          (function
            | Not_found -> return beg
            | e -> fail e)
      )
      tab2
      (return cooktab)
  in

  aux getpersvexp Eliom_common.persistentcookiename false !pers_cookies_info
    (aux getdatavexp Eliom_common.datacookiename false !data_cookie_info
       (aux getservvexp Eliom_common.servicecookiename false !service_cookie_info
          (let (service_cookie_info, data_cookie_info, pers_cookies_info) =
             secure_ci
           in
           aux getpersvexp Eliom_common.persistentcookiename
             true !pers_cookies_info
             (aux getdatavexp Eliom_common.datacookiename
                true !data_cookie_info
                (aux getservvexp Eliom_common.servicecookiename
                   true !service_cookie_info
                   (return endlist))))))


let compute_cookies_to_send = compute_session_cookies_to_send

let compute_new_ri_cookies'
    now
    ripath
    ricookies
    cookies_set_by_page =

  Ocsigen_cookie_map.Map_path.fold
    (fun cpath t cookies ->
      if Url.is_prefix_skip_end_slash
          (Url.remove_slash_at_beginning cpath)
          (Url.remove_slash_at_beginning ripath)
      then
        Ocsigen_cookie_map.Map_inner.fold
          (fun name v cookies ->
(*VVV We always keep secure cookies, event if the protocol is not secure,
  because this function is for actions only. Is that right? *)
            match v with
              | OSet (Some exp, value, secure)
                  when exp>now ->
                Ocsigen_cookie_map.Map_inner.add name value cookies
              | OSet (None, value, secure) ->
                Ocsigen_cookie_map.Map_inner.add name value cookies
              | OSet (Some exp, value, secure)
                  when exp<=now ->
                Ocsigen_cookie_map.Map_inner.remove name cookies
              | OUnset ->
                Ocsigen_cookie_map.Map_inner.remove name cookies
              | _ -> cookies)
          t
          cookies
      else cookies)
    cookies_set_by_page
    ricookies


(** Compute new ri.ri_cookies value
    from an old ri.ri_cookies and all_cookie_info
    as if it had been sent by the browser *)
let compute_new_ri_cookies
    (now : float)
    (ripath : string list)
    (ricookies : string Ocsigen_cookie_map.Map_inner.t)
    ((ci, secure_ci) : Eliom_common.tables Eliom_common.cookie_info)
    (cookies_set_by_page : Ocsigen_cookie_map.t)
    : string Ocsigen_cookie_map.Map_inner.t Lwt.t =

  (* first we add cookies set by page: *)
  let ric =
    compute_new_ri_cookies' now ripath ricookies cookies_set_by_page
  in
  (* then session cookies: *)
  let f secure (service_cookie_info, data_cookie_info, pers_cookie_info) ric =
    let ric =
      Eliom_common.Full_state_name_table.fold
        (fun ((sc, _, _) as full_st_name) (_, v) beg ->
          let ct = Eliom_common.cookie_level_of_user_scope sc in
          if ct = `Client_process
          then beg
          else
            let n = Eliom_common.make_full_cookie_name
              Eliom_common.servicecookiename full_st_name in
            match !v with
              | Eliom_common.SCData_session_expired
              | Eliom_common.SCNo_data ->
                Ocsigen_cookie_map.Map_inner.remove n beg
              | Eliom_common.SC {Eliom_common.sc_set_value = `Set v} ->
                Ocsigen_cookie_map.Map_inner.add n v beg
              | Eliom_common.SC {Eliom_common.sc_set_value = `None} ->
                beg
              | Eliom_common.SC {Eliom_common.sc_set_value = `Used} ->
                assert false
        )
        !service_cookie_info
        ric
    in
    let ric =
      Eliom_common.Full_state_name_table.fold
        (fun ((sc, _, _) as full_st_name) v beg ->
          let ct = Eliom_common.cookie_level_of_user_scope sc in
          if ct = `Client_process
          then beg
          else
            let n = Eliom_common.make_full_cookie_name
              Eliom_common.datacookiename full_st_name in
            if Lazy.is_val v
            then
              let (_, v) = Lazy.force v in
              match !v with
                | Eliom_common.SCData_session_expired
                | Eliom_common.SCNo_data ->
                  Ocsigen_cookie_map.Map_inner.remove n beg
                | Eliom_common.SC {Eliom_common.dc_set_value = `Set v} ->
                  Ocsigen_cookie_map.Map_inner.add n v beg
                | Eliom_common.SC {Eliom_common.dc_set_value = `None} ->
                  beg
                | Eliom_common.SC {Eliom_common.dc_set_value = `Used} ->
                  assert false
            else beg
        )
        !data_cookie_info
        ric
    in
    let ric =
      Eliom_common.Full_state_name_table.fold
        (fun ((sc, _, _) as full_st_name) v beg ->
          let ct = Eliom_common.cookie_level_of_user_scope sc in
          if ct = `Client_process
          then beg
          else
            let n = Eliom_common.make_full_cookie_name
              Eliom_common.persistentcookiename full_st_name in
            beg >>= fun beg ->
            if Lazy.is_val v
            then
              (Lazy.force v >>= fun (_, v) ->
               match !v with
                 | Eliom_common.SCData_session_expired
                 | Eliom_common.SCNo_data ->
                   Lwt.return (Ocsigen_cookie_map.Map_inner.remove n beg)
                 | Eliom_common.SC {Eliom_common.pc_set_value = `Set v} ->
                   Lwt.return (Ocsigen_cookie_map.Map_inner.add n v beg)
                 | Eliom_common.SC {Eliom_common.pc_set_value = `None} ->
                   Lwt.return beg
                 | Eliom_common.SC {Eliom_common.pc_set_value = `Used} ->
                   assert false)
            else return beg
        )
        !pers_cookie_info
        (Lwt.return ric)
    in
    ric
  in
  f false ci ric
  >>= fun ric ->
  f true secure_ci ric
(*VVV We always keep secure cookies, even if the protocol is not secure,
  because this function is for actions only. Is that right? *)
