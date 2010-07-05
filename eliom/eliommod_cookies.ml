(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_cookies.ml
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

(** Cookie management                                                       *)

open Lwt

(*****************************************************************************)
let make_new_session_id = Ocsigen_lib.make_cryptographic_safe_string




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
    Ocsigen_lib.String_Table.fold
      (fun name value (oktable, failedlist) ->
        try
          let fullsessname, ta, expref, timeout_ref, sessgrpref, sessgrpnode =
            Eliom_common.SessionCookies.find
              sitedata.Eliom_common.session_services value
          in
          Eliommod_sessiongroups.Serv.up sessgrpnode;
          match !expref with
          | Some t when t < now ->
              (* session expired by timeout *)
              Eliom_common.SessionCookies.remove
                sitedata.Eliom_common.session_services value;
              ((Ocsigen_lib.String_Table.add
                  name
                  (Some value          (* value sent by the browser *),
                   ref Eliom_common.SCData_session_expired
                                       (* ask the browser
                                          to remove the cookie *))
                  oktable),
               name::failedlist)
          | _ -> ((Ocsigen_lib.String_Table.add
                     name
                     (Some value        (* value sent by the browser *),
                      ref
                        (Eliom_common.SC
                           {Eliom_common.sc_value= value  (* value *);
                            Eliom_common.sc_table= ref ta (* the table of session services *);
                            Eliom_common.sc_timeout= timeout_ref (* user timeout ref *);
                            Eliom_common.sc_exp= expref  (* expiration date (server side) *);
                            Eliom_common.sc_cookie_exp=
                            ref Eliom_common.CENothing
                              (* cookie expiration date to send
                                 to the browser *);
                            Eliom_common.sc_session_group= sessgrpref;
                            Eliom_common.sc_session_group_node= sessgrpnode;
                           }))
                     oktable),
                  failedlist)
        with Not_found ->
          ((Ocsigen_lib.String_Table.add
              name
              (Some value                 (* value sent by the browser *),
               ref Eliom_common.SCData_session_expired
                                          (* ask the browser
                                             to remove the cookie *))
              oktable),
           name::failedlist)
      )
      service_cookies
      (Ocsigen_lib.String_Table.empty, [])
  in

  (* get info about "in memory" data session cookies: *)
  let f_data data_cookies =
    Ocsigen_lib.String_Table.map
      (fun value ->
        lazy
          (try
            let fullsessname, expref, timeout_ref, sessgrpref, sessgrpnode =
              Eliom_common.SessionCookies.find
                sitedata.Eliom_common.session_data value
            in
            Eliommod_sessiongroups.Serv.up sessgrpnode;
            match !expref with
              | Some t when t < now ->
                  (* session expired by timeout *)
                  sitedata.Eliom_common.remove_session_data value;
                  Eliom_common.SessionCookies.remove
                    sitedata.Eliom_common.session_data value;
                  (Some value                 (* value sent by the browser *),
                   ref Eliom_common.SCData_session_expired
                                              (* ask the browser
                                                 to remove the cookie *))
              | _ ->
                  (Some value        (* value sent by the browser *),
                   ref
                     (Eliom_common.SC
                        {Eliom_common.dc_value= value       (* value *);
                         Eliom_common.dc_timeout= timeout_ref
                          (* user timeout ref *);
                         Eliom_common.dc_exp= expref (* expiration date
                                                        (server side) *);
                         Eliom_common.dc_cookie_exp=
                            ref Eliom_common.CENothing
                              (* cookie expiration date to send
                                 to the browser *);
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
    Ocsigen_lib.String_Table.map
      (fun value ->
        lazy
          (catch
             (fun () ->
               Ocsipersist.find
                 (Lazy.force Eliom_common.persistent_cookies_table) value >>=
               fun (fullsessname, persexp, perstimeout, sessgrp) ->

                 Eliommod_sessiongroups.Pers.up value sessgrp >>= fun () ->
                 match persexp with
                 | Some t when t < now ->
                     (* session expired by timeout *)
                     Eliom_common.remove_from_all_persistent_tables
                       value >>= fun () ->
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
                              {Eliom_common.pc_value= value
                                 (* value *);
                               Eliom_common.pc_timeout= ref perstimeout
                                 (* user persistent timeout ref *);
                               Eliom_common.pc_cookie_exp=
                               ref Eliom_common.CENothing
                                 (* persistent cookie expiration
                                    date ref to send to the
                                    browser *);
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
    match secure_cookies with
      | None (* not https *) -> None, []
      | Some (sc, dc, pc) ->
          let (servoktable, servfailedlist) = f_serv sc in
          let dataoktable = f_data dc in
          let persoktable = f_pers pc in
          (Some (ref servoktable, ref dataoktable, ref persoktable), 
           servfailedlist)
  in

  (((ref servoktable, ref dataoktable, ref persoktable), sec),
   sservfailedlist@servfailedlist)


(*****************************************************************************)

(* table cookie -> session table *)
let new_service_cookie_table () :
    Eliom_common.tables Eliom_common.servicecookiestable =
  Eliom_common.SessionCookies.create 1000

let new_data_cookie_table () : Eliom_common.datacookiestable =
  Eliom_common.SessionCookies.create 1000




(*****************************************************************************)
(* Create the table of cookies to send to the browser or to unset            *)
(* (from cookie_info)                                                        *)

let compute_session_cookies_to_send
    sitedata
    ((service_cookie_info,
      data_cookie_info,
      pers_cookies_info), secure_ci) endlist =
  let getservvexp name (old, newi) =
    return
      (let newinfo =
        match !newi with
        | Eliom_common.SCNo_data
        | Eliom_common.SCData_session_expired -> None
        | Eliom_common.SC c ->
            Some (c.Eliom_common.sc_value,
                  !(c.Eliom_common.sc_cookie_exp))
      in (name, old, newinfo))
  in
  let getdatavexp name v =
    if Lazy.lazy_is_val v
    then
      return
        (let (old, newi) = Lazy.force v in
        let newinfo =
          match !newi with
          | Eliom_common.SCNo_data
          | Eliom_common.SCData_session_expired -> None
          | Eliom_common.SC c ->
              Some (c.Eliom_common.dc_value,
                    !(c.Eliom_common.dc_cookie_exp))
        in (name, old, newinfo))
    else fail Not_found
  in
  let getpersvexp name v =
    if Lazy.lazy_is_val v
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
                 Some (c.Eliom_common.pc_value,
                       !(c.Eliom_common.pc_cookie_exp))
         in (name, oldinfo, newinfo))
    else fail Not_found
  in
  let ch_exp = function
    | Eliom_common.CENothing
    | Eliom_common.CEBrowser -> None
    | Eliom_common.CESome a -> Some a
  in
  let aux f cookiename secure tab2 cooktab =
    cooktab >>= fun cooktab ->
    Ocsigen_lib.String_Table.fold
      (fun name value beg ->
        beg >>= fun beg ->
        catch
          (fun () ->
            f name value >>= fun (name, old, newc) ->
            return
              (match old, newc with
              | None, None -> beg
              | Some _, None ->
                  Ocsigen_http_frame.add_cookie
                    sitedata.Eliom_common.site_dir
                    (Eliom_common.make_full_cookie_name cookiename name)
                    Ocsigen_http_frame.OUnset
                    beg
                  (* the path is always site_dir because the cookie cannot
                     have been unset by a service outside
                     this site directory *)
              | None, Some (v, exp) ->
                  Ocsigen_http_frame.add_cookie
                    sitedata.Eliom_common.site_dir
                    (Eliom_common.make_full_cookie_name cookiename name)
                    (Ocsigen_http_frame.OSet (ch_exp exp, v, secure))
                    beg
              | Some oldv, Some (newv, exp) ->
                  if exp = Eliom_common.CENothing && oldv = newv
                  then beg
                  else Ocsigen_http_frame.add_cookie
                      sitedata.Eliom_common.site_dir
                      (Eliom_common.make_full_cookie_name cookiename name)
                      (Ocsigen_http_frame.OSet (ch_exp exp, newv, secure))
                      beg
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
          (match secure_ci with
             | None -> return endlist
             | Some (service_cookie_info,
                     data_cookie_info,
                     pers_cookies_info) ->
                 aux getpersvexp Eliom_common.spersistentcookiename true !pers_cookies_info
                   (aux getdatavexp Eliom_common.sdatacookiename true !data_cookie_info
                      (aux getservvexp Eliom_common.sservicecookiename true !service_cookie_info
                         (return endlist))))))


let compute_cookies_to_send = compute_session_cookies_to_send


(* add a list of Eliom's cookies to an Ocsigen_http_frame cookie table *)
let add_cookie_list_to_send sitedata l t =
  let change_pathopt = function
    | None -> sitedata.Eliom_common.site_dir
          (* Not possible to set a cookie for another site (?) *)
    | Some p -> sitedata.Eliom_common.site_dir@p
  in
  List.fold_left
    (fun t v ->
      match v with
      | Eliom_common.Set (upo, expo, n, v, secure) ->
          Ocsigen_http_frame.add_cookie (change_pathopt upo) n
            (Ocsigen_http_frame.OSet (expo, v, secure)) t
      | Eliom_common.Unset (upo, n) ->
          Ocsigen_http_frame.add_cookie (change_pathopt upo) n Ocsigen_http_frame.OUnset t
    )
    t
    l


let compute_new_ri_cookies'
    now
    ripath
    ricookies
    cookies_set_by_page =

  let prefix upo p = match upo with
    | None -> true
    | Some path ->
        Ocsigen_lib.list_is_prefix_skip_end_slash
          (Ocsigen_lib.remove_slash_at_beginning path)
          (Ocsigen_lib.remove_slash_at_beginning p)
  in
  List.fold_left
    (fun tab v ->
      match v with
      | Eliom_common.Set (upo, Some t, n, v, _)
        when t>now && prefix upo ripath ->
          Ocsigen_lib.String_Table.add n v tab
      | Eliom_common.Set (upo, None, n, v, _) when prefix upo ripath ->
          Ocsigen_lib.String_Table.add n v tab
      | Eliom_common.Set (upo, _, n, _, _)
      | Eliom_common.Unset (upo, n) when prefix upo ripath ->
          Ocsigen_lib.String_Table.remove n tab
      | _ -> tab
    )
    ricookies
    cookies_set_by_page


(** Compute new ri.ri_cookies value
    from an old ri.ri_cookies and all_cookie_info
    as if it had been sent by the browser *)
let compute_new_ri_cookies
    now
    ripath
    ricookies
    (ci, secure_ci)
    cookies_set_by_page =

  let ric =
    compute_new_ri_cookies' now ripath ricookies cookies_set_by_page
  in
  let f (service_cookie_info, data_cookie_info, pers_cookie_info) ric 
      (scn, dcn, pcn) =
    let ric =
      Ocsigen_lib.String_Table.fold
        (fun n (_, v) beg ->
           let n = Eliom_common.make_full_cookie_name scn n in
           match !v with
             | Eliom_common.SCData_session_expired
             | Eliom_common.SCNo_data -> 
                 Ocsigen_lib.String_Table.remove n beg
             | Eliom_common.SC c ->
                 Ocsigen_lib.String_Table.add n c.Eliom_common.sc_value beg
        )
        !service_cookie_info
        ric
    in
    let ric =
      Ocsigen_lib.String_Table.fold
        (fun n v beg ->
          let n = Eliom_common.make_full_cookie_name dcn n in
          if Lazy.lazy_is_val v
          then
            let (_, v) = Lazy.force v in
            match !v with
            | Eliom_common.SCData_session_expired
            | Eliom_common.SCNo_data -> 
                Ocsigen_lib.String_Table.remove n beg
            | Eliom_common.SC c ->
                Ocsigen_lib.String_Table.add n c.Eliom_common.dc_value beg
          else beg
        )
        !data_cookie_info
        ric
    in
    let ric =
      Ocsigen_lib.String_Table.fold
        (fun n v beg ->
          let n = Eliom_common.make_full_cookie_name pcn n in
          beg >>= fun beg ->
          if Lazy.lazy_is_val v
          then
            Lazy.force v >>= fun (_, v) ->
            match !v with
            | Eliom_common.SCData_session_expired
            | Eliom_common.SCNo_data ->
                return (Ocsigen_lib.String_Table.remove n beg)
            | Eliom_common.SC c ->
                return (Ocsigen_lib.String_Table.add n
                          c.Eliom_common.pc_value beg)
          else return beg
        )
        !pers_cookie_info
        (return ric)
    in
    ric
  in
  f ci ric 
    (Eliom_common.servicecookiename, 
     Eliom_common.datacookiename, 
     Eliom_common.persistentcookiename) 
  >>= fun ric ->
  match secure_ci with
    | None -> Lwt.return ric
    | Some ci -> 
        f ci ric
          (Eliom_common.sservicecookiename, 
           Eliom_common.sdatacookiename, 
           Eliom_common.spersistentcookiename) 
