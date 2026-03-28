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

open Lib
(** Cookie management                                                       *)

open Lwt
include Eliom_cookies_base

(*****************************************************************************)
let make_new_session_id () = Ocsigen_lib.make_cryptographic_safe_string () ^ "H"

type date = float

type cookie =
  { full_state_name : Common.full_state_name
  ; expiry : date option
  ; timeout : Common.timeout
  ; session_group : Common.perssessgrp option }

module Persistent_cookies = struct
  (* Another table, containing the session info for each cookie *)
  (* the table contains:
     - the expiration date (by timeout), changed at each access to the table
       (float option) None -> no expiration
     - the timeout for the user (float option option) None -> see global config
       Some None -> no timeout
  *)
  (* It is lazy, because we must delay the creation of the table until
     the initialization of eliom in case we use static linking with
     sqlite backend ... *)

  module Ocsipersist = Common.Ocsipersist.Functorial

  (* NOTE: Do not forget to change the version number when the internal format changes! *)
  let persistent_cookie_table_version = "_v5"

  (* v2 introduces session groups *)
  (* v3 introduces tab sessions *)
  (* v4 introduces group tables *)
  (* v5 removes secure scopes *)
  module Cookies =
    Ocsipersist.Table
      (struct
        let name = "eliom_persist_cookies" ^ persistent_cookie_table_version
      end)
      (Ocsipersist.Column.String)
      (Ocsipersist.Column.Marshal (struct
           type t = cookie
         end))

  let () = Common.Persistent_tables.add_functorial_table (module Cookies)

  (* maps expiry dates to cookie IDs; may have superfluous entries, i.e cookies
     that will not actually expire on the given date. *)
  module Expiry_dates = struct
    include
      Ocsipersist.Table
        (struct
          let name = "eliom_persist_cookies_expiry_dates"
        end)
        (Ocsipersist.Column.Float)
        (Ocsipersist.Column.String)

    let add_cookie exp cookie =
      modify_opt exp @@ function
      | None -> Some cookie
      | Some cookies_str ->
          let cookies = String.split_on_char ',' cookies_str in
          if List.mem cookie cookies
          then Some cookies_str
          else Some (cookies_str ^ "," ^ cookie)

    let remove_cookie exp_o cookie =
      match exp_o with
      | None -> Lwt.return_unit
      | Some exp -> (
          modify_opt exp @@ function
          | None -> None
          | Some cookies_str ->
              let cookies = String.split_on_char ',' cookies_str in
              let cookies' = List.filter (fun c -> c <> cookie) cookies in
              if cookies' = [] then None else Some (String.concat "," cookies'))
  end

  let add cookie ({expiry; _} as content) =
    (match expiry with
      | Some t -> Expiry_dates.add_cookie t cookie
      | None -> Lwt.return_unit)
    >>= fun _ -> Cookies.add cookie content

  let replace_if_exists cookie ({expiry; _} as content) =
    (match expiry with
      | Some t -> Expiry_dates.add_cookie t cookie
      | None -> Lwt.return_unit)
    >>= fun _ -> Cookies.replace_if_exists cookie content

  let garbage_collect ~section gc_cookie =
    let now = Unix.time () in
    Expiry_dates.iter ~lt:now @@ fun date cookies_str ->
    let cookies = String.split_on_char ',' cookies_str in
    let cookies_log =
      String.concat "," @@ List.map Common.Hashed_cookies.sha256 cookies
    in
    Logs.info ~src:section (fun fmt ->
      fmt "potentially expired cookies %.0f: %s" date cookies_log);
    Lwt_list.iter_s gc_cookie cookies >>= fun _ -> Expiry_dates.remove date
end

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
      secure_cookies : 'a Common.cookie_info * 'b list
  =
  (* get info about service session cookies: *)
  let f_serv service_cookies =
    Common.Full_state_name_table.fold
      (fun name value (oktable, failedlist) ->
         try
           let hvalue = Common.Hashed_cookies.hash value in
           let { Common.Service_cookie.session_table
               ; expiry
               ; timeout
               ; session_group
               ; session_group_node
               ; _ }
             =
             Common.SessionCookies.find
               sitedata.Common.session_services
               (Common.Hashed_cookies.to_string hvalue)
           in
           Eliommod_sessiongroups.Serv.up session_group_node;
           match !expiry with
           | Some t when t < now ->
               (* session expired by timeout *)
               Eliommod_sessiongroups.Serv.remove session_group_node;
               ( Common.Full_state_name_table.add name
                   ( Some value (* value sent by the browser *)
                   , ref Common.SCData_session_expired
                     (* ask the browser
                                          to remove the cookie *)
                   )
                   oktable
               , name :: failedlist )
           | _ ->
               ( Common.Full_state_name_table.add name
                   ( Some value (* value sent by the browser *)
                   , ref
                       (Common.SC
                          { Common.sc_hvalue = hvalue (* value *)
                          ; Common.sc_set_value = None
                          ; Common.sc_table = ref session_table
                          ; Common.sc_timeout = timeout
                          ; Common.sc_exp = expiry
                          ; Common.sc_cookie_exp =
                              ref Common.CENothing
                              (* cookie expiration date to send
                                 to the browser.
                                 We don't change it *)
                          ; Common.sc_session_group = session_group
                          ; Common.sc_session_group_node =
                              session_group_node }) )
                   oktable
               , failedlist )
         with Not_found ->
           ( Common.Full_state_name_table.add name
               ( Some value (* value sent by the browser *)
               , ref Common.SCData_session_expired
                 (* ask the browser
                                             to remove the cookie *)
               )
               oktable
           , name :: failedlist ))
      service_cookies
      (Common.Full_state_name_table.empty, [])
  in
  (* get info about "in memory" data session cookies: *)
  let f_data data_cookies =
    Common.Full_state_name_table.map
      (fun value ->
         lazy
           (try
              let hvalue = Common.Hashed_cookies.hash value in
              let { Common.Data_cookie.expiry
                  ; timeout
                  ; session_group
                  ; session_group_node
                  ; _ }
                =
                Common.SessionCookies.find
                  sitedata.Common.session_data
                  (Common.Hashed_cookies.to_string hvalue)
              in
              Eliommod_sessiongroups.Serv.up session_group_node;
              match !expiry with
              | Some t when t < now ->
                  (* session expired by timeout *)
                  Eliommod_sessiongroups.Data.remove session_group_node;
                  ( Some value (* value sent by the browser *)
                  , ref Common.SCData_session_expired
                    (* ask the browser
                                                 to remove the cookie *)
                  )
              | _ ->
                  ( Some value (* value sent by the browser *)
                  , ref
                      (Common.SC
                         { Common.dc_hvalue = hvalue (* value *)
                         ; Common.dc_set_value = None
                         ; Common.dc_timeout =
                             timeout (* user timeout ref *)
                         ; Common.dc_exp =
                             expiry
                             (* expiration date
                                                        (server side) *)
                         ; Common.dc_cookie_exp =
                             ref Common.CENothing
                             (* cookie expiration date to send
                                 to the browser.
                                 We don't change it *)
                         ; Common.dc_session_group = session_group
                         ; Common.dc_session_group_node =
                             session_group_node }) )
            with Not_found ->
              ( Some value (* value sent by the browser *)
              , ref Common.SCData_session_expired
                (* ask the browser
                   to remove the cookie *) )))
      data_cookies
  in
  (* *** get info about persistent session cookies: *)
  let f_pers persistent_cookies =
    Common.Full_state_name_table.map
      (fun value ->
         lazy
           (catch
              (fun () ->
                 let hvalue = Common.Hashed_cookies.hash value in
                 let hvalue_string =
                   Common.Hashed_cookies.to_string hvalue
                 in
                 Persistent_cookies.Cookies.find
                   (Common.Hashed_cookies.to_string hvalue)
                 >>=
                 fun { expiry = persexp
                     ; timeout = perstimeout
                     ; session_group = sessgrp
                     ; _ } ->
                 Eliommod_sessiongroups.Pers.up hvalue_string sessgrp
                 >>= fun () ->
                 match persexp with
                 | Some t when t < now ->
                     (* session expired by timeout *)
                     Common.Persistent_tables.remove_key_from_all_tables
                       hvalue_string
                     >>= fun () ->
                     return
                       ( Some
                           ( value
                             (* value at the beginning
                                                 of the request *)
                           , perstimeout
                             (* user persistent timeout
                                                 at the beginning
                                                 of the request *)
                           , persexp
                             (* expiration date (server)
                                                 at the beginning
                                                 of the request *)
                           , sessgrp (* session group at beginning *) )
                       , ref Common.SCData_session_expired
                         (* ask the browser to
                                                 remove the cookie *)
                       )
                 | _ ->
                     return
                       ( Some
                           ( value
                             (* value at the beginning
                                              of the request *)
                           , perstimeout
                             (* user persistent timeout
                                              at the beginning
                                              of the request *)
                           , persexp
                             (* expiration date (server)
                                              at the beginning
                                              of the request *)
                           , sessgrp (* session group at beginning *) )
                       , ref
                           (Common.SC
                              { Common.pc_hvalue = hvalue (* value *)
                              ; Common.pc_set_value = None
                              ; Common.pc_timeout =
                                  ref perstimeout
                                  (* user persistent timeout ref *)
                              ; Common.pc_cookie_exp =
                                  ref Common.CENothing
                                  (* persistent cookie expiration
                                    date ref to send to the browser:
                                    We don't change it *)
                              ; Common.pc_session_group = ref sessgrp })
                       ))
              (function
                | Not_found ->
                    return
                      ( Some
                          ( value
                            (* value at the beginning
                                             of the request *)
                          , Common.TGlobal
                            (* user persistent timeout
                                             at the beginning
                                             of the request *)
                          , Some 0.
                            (* expiration date (server)
                                             at the beginning
                                             of the request *)
                          , None (* session group at beginning *) )
                      , ref Common.SCData_session_expired
                        (* ask the browser
                                             to remove the cookie *)
                      )
                | e -> fail e)))
      persistent_cookies
    (* the persistent cookies sent by the request *)
  in
  let servoktable, servfailedlist = f_serv service_cookies in
  let dataoktable = f_data data_cookies in
  let persoktable = f_pers persistent_cookies in
  let sec, sservfailedlist =
    let sc, dc, pc = secure_cookies in
    let servoktable, servfailedlist = f_serv sc in
    let dataoktable = f_data dc in
    let persoktable = f_pers pc in
    (ref servoktable, ref dataoktable, ref persoktable), servfailedlist
  in
  ( ((ref servoktable, ref dataoktable, ref persoktable), sec)
  , sservfailedlist @ servfailedlist )

(*****************************************************************************)

(* table cookie -> session table *)
let new_service_cookie_table () :
  Common.tables Common.Service_cookie.table
  =
  Common.SessionCookies.create 100

let new_data_cookie_table () : Common.Data_cookie.table =
  Common.SessionCookies.create 100

(*****************************************************************************)
(* Create the table of cookies to send to the browser or to unset            *)
(* (from cookie_info)                                                        *)

let compute_session_cookies_to_send
      sitedata
      ((service_cookie_info, data_cookie_info, pers_cookies_info), secure_ci)
      (endlist : Ocsigen_cookie_map.t)
  =
  let getservvexp (old, newi) =
    return
      (let newinfo =
         match !newi with
         | Common.SCNo_data | Common.SCData_session_expired -> None
         | Common.SC c ->
             Some
               ( c.Common.sc_hvalue
               , c.Common.sc_set_value
               , !(c.Common.sc_cookie_exp) )
       in
       old, newinfo)
  in
  let getdatavexp v =
    if Lazy.is_val v
    then
      return
        (let old, newi = Lazy.force v in
         let newinfo =
           match !newi with
           | Common.SCNo_data | Common.SCData_session_expired ->
               None
           | Common.SC c ->
               Some
                 ( c.Common.dc_hvalue
                 , c.Common.dc_set_value
                 , !(c.Common.dc_cookie_exp) )
         in
         old, newinfo)
    else fail Not_found
  in
  let getpersvexp v =
    if Lazy.is_val v
    then
      Lazy.force v >>= fun (old, newi) ->
      return
        (let oldinfo =
           match old with None -> None | Some (v, _, _, _) -> Some v
         in
         let newinfo =
           match !newi with
           | Common.SCNo_data | Common.SCData_session_expired ->
               None
           | Common.SC c ->
               Some
                 ( c.Common.pc_hvalue
                 , c.Common.pc_set_value
                 , !(c.Common.pc_cookie_exp) )
         in
         oldinfo, newinfo)
    else fail Not_found
  in
  let ch_exp = function
    | Common.CENothing | Common.CEBrowser -> None
    | Common.CESome a -> Some a
  in
  let aux f cookiekind secure tab2 cooktab =
    cooktab >>= fun cooktab ->
    Common.Full_state_name_table.fold
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
                      ~path:(Common.get_site_dir sitedata)
                      (Common.make_full_cookie_name cookiekind
                         full_st_name)
                      OUnset beg
                (* the path is always site_dir because the cookie cannot
                 have been unset by a service outside
                 this site directory *)
                | _, Some (_, Some v, exp) ->
                    (* New value *)
                    Ocsigen_cookie_map.add
                      ~path:(Common.get_site_dir sitedata)
                      (Common.make_full_cookie_name cookiekind
                         full_st_name)
                      (OSet (ch_exp exp, v, secure))
                      beg
                | Some oldv, Some (_, None, exp) ->
                    if exp = Common.CENothing
                    then beg
                    else
                      Ocsigen_cookie_map.add
                        ~path:(Common.get_site_dir sitedata)
                        (Common.make_full_cookie_name cookiekind
                           full_st_name)
                        (OSet (ch_exp exp, oldv, secure))
                        beg
                | None, Some (_, None, _) ->
                    (* Should not happen *)
                    beg))
           (function Not_found -> return beg | e -> fail e))
      tab2 (return cooktab)
  in
  aux getpersvexp Common.persistentcookiename false !pers_cookies_info
    (aux getdatavexp Common.datacookiename false !data_cookie_info
       (aux getservvexp Common.servicecookiename false
          !service_cookie_info
          (let service_cookie_info, data_cookie_info, pers_cookies_info =
             secure_ci
           in
           aux getpersvexp Common.persistentcookiename true
             !pers_cookies_info
             (aux getdatavexp Common.datacookiename true !data_cookie_info
                (aux getservvexp Common.servicecookiename true
                   !service_cookie_info (return endlist))))))

let compute_cookies_to_send = compute_session_cookies_to_send

let compute_new_ri_cookies' now ripath ricookies cookies_set_by_page =
  Ocsigen_cookie_map.Map_path.fold
    (fun cpath t cookies ->
       if
         Url.is_prefix_skip_end_slash
           (Url.remove_slash_at_beginning cpath)
           (Url.remove_slash_at_beginning ripath)
       then
         Ocsigen_cookie_map.Map_inner.fold
           (fun name v cookies ->
              (*VVV We always keep secure cookies, event if the protocol is not secure,
  because this function is for actions only. Is that right? *)
              match v with
              | OSet (Some exp, value, _secure) when exp > now ->
                  Ocsigen_cookie_map.Map_inner.add name value cookies
              | OSet (None, value, _secure) ->
                  Ocsigen_cookie_map.Map_inner.add name value cookies
              | OSet (Some exp, _value, _secure) when exp <= now ->
                  Ocsigen_cookie_map.Map_inner.remove name cookies
              | OUnset -> Ocsigen_cookie_map.Map_inner.remove name cookies
              | _ -> cookies)
           t cookies
       else cookies)
    cookies_set_by_page ricookies

(** Compute new ri.ri_cookies value
    from an old ri.ri_cookies and all_cookie_info
    as if it had been sent by the browser *)
let compute_new_ri_cookies
      (now : float)
      (ripath : string list)
      (ricookies : string Ocsigen_cookie_map.Map_inner.t)
      ((ci, secure_ci) : Common.tables Common.cookie_info)
      (cookies_set_by_page : Ocsigen_cookie_map.t) :
  string Ocsigen_cookie_map.Map_inner.t Lwt.t
  =
  (* first we add cookies set by page: *)
  let ric = compute_new_ri_cookies' now ripath ricookies cookies_set_by_page in
  (* then session cookies: *)
  let f _secure (service_cookie_info, data_cookie_info, pers_cookie_info) ric =
    let ric =
      Common.Full_state_name_table.fold
        (fun ({Common.user_scope = sc; _} as full_st_name) (_, v) beg ->
           let ct = Common.cookie_level_of_user_scope sc in
           if ct = `Client_process
           then beg
           else
             let n =
               Common.make_full_cookie_name Common.servicecookiename
                 full_st_name
             in
             match !v with
             | Common.SCData_session_expired | Common.SCNo_data ->
                 Ocsigen_cookie_map.Map_inner.remove n beg
             | Common.SC {Common.sc_set_value = Some v; _} ->
                 Ocsigen_cookie_map.Map_inner.add n v beg
             | Common.SC {Common.sc_set_value = None; _} -> beg)
        !service_cookie_info ric
    in
    let ric =
      Common.Full_state_name_table.fold
        (fun ({Common.user_scope = sc; _} as full_st_name) v beg ->
           let ct = Common.cookie_level_of_user_scope sc in
           if ct = `Client_process
           then beg
           else
             let n =
               Common.make_full_cookie_name Common.datacookiename
                 full_st_name
             in
             if Lazy.is_val v
             then
               let _, v = Lazy.force v in
               match !v with
               | Common.SCData_session_expired | Common.SCNo_data ->
                   Ocsigen_cookie_map.Map_inner.remove n beg
               | Common.SC {Common.dc_set_value = Some v; _} ->
                   Ocsigen_cookie_map.Map_inner.add n v beg
               | Common.SC {Common.dc_set_value = None; _} -> beg
             else beg)
        !data_cookie_info ric
    in
    let ric =
      Common.Full_state_name_table.fold
        (fun ({Common.user_scope = sc; _} as full_st_name) v beg ->
           let ct = Common.cookie_level_of_user_scope sc in
           if ct = `Client_process
           then beg
           else
             let n =
               Common.make_full_cookie_name
                 Common.persistentcookiename full_st_name
             in
             beg >>= fun beg ->
             if Lazy.is_val v
             then
               Lazy.force v >>= fun (_, v) ->
               match !v with
               | Common.SCData_session_expired | Common.SCNo_data ->
                   Lwt.return (Ocsigen_cookie_map.Map_inner.remove n beg)
               | Common.SC {Common.pc_set_value = Some v; _} ->
                   Lwt.return (Ocsigen_cookie_map.Map_inner.add n v beg)
               | Common.SC {Common.pc_set_value = None; _} ->
                   Lwt.return beg
             else return beg)
        !pers_cookie_info (Lwt.return ric)
    in
    ric
  in
  f false ci ric >>= fun ric -> f true secure_ci ric
(*VVV We always keep secure cookies, even if the protocol is not secure,
  because this function is for actions only. Is that right? *)
