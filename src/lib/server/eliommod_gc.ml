(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_gc.ml
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

(** Garbage collection of services and session data *)


let section = Lwt_log.Section.make "eliom:gc"
open Eliom_lib

open Lwt
open Lwt.Syntax

(*****************************************************************************)
let servicesessiongcfrequency = ref (Some 1200.) (* 20 min ? *)
let datasessiongcfrequency = ref (Some 1200.)
let persistentsessiongcfrequency = ref (Some 86400.)
let set_servicesessiongcfrequency i = servicesessiongcfrequency := i
let set_datasessiongcfrequency i = datasessiongcfrequency := i
let get_servicesessiongcfrequency () = !servicesessiongcfrequency
let get_datasessiongcfrequency () = !datasessiongcfrequency
let set_persistentsessiongcfrequency i = persistentsessiongcfrequency := i
let get_persistentsessiongcfrequency () = !persistentsessiongcfrequency


(* garbage collection of timeouted sessions *)
let gc_timeouted_services now tables =
  let rec aux t filename direltr thr =
    thr >>= fun () -> (* we wait for the previous one to be completed *)
    match !direltr with
      | Eliom_common.Dir r ->
          (empty_one r >>= fun () ->
           (match !r with
              | Eliom_common.Vide ->
                  (match !t with
                     | Eliom_common.Vide -> ()
                     | Eliom_common.Table tr ->
                         let newr =
                           String.Table.remove filename tr
                         in
                         if String.Table.is_empty newr
                         then t := Eliom_common.Vide
                         else t := Eliom_common.Table newr
                  )
              | _ -> ());
             Lwt.return_unit)
      | Eliom_common.File ptr ->
          Eliom_common.Serv_Table.fold
(*VVV not tail recursive: may be a problem if lots of coservices *)
            (fun ptk (`Ptc (nodeopt, l)) thr ->
               let* _ = thr in (* we wait for the previous one to be completed *)
               (match nodeopt, l with
                  | Some node, {Eliom_common.s_expire = Some (_, e)} :: _
                    (* it is an anonymous coservice.  The list should
                       have length 1 here *)
                      when !e < now ->
                      Ocsigen_cache.Dlist.remove node
                  | Some node, [] (* should not occur *) ->
                      Ocsigen_cache.Dlist.remove node
                  | _ ->
                      (* We find the data associated to ptk once again,
                         because it may have changed, then we update it
                         (without cooperation)
                         (it's ok because the list is probably not large) *)
                      try
                        let (`Ptc (nodeopt, l)), ll =
                          (Eliom_common.Serv_Table.find ptk !ptr,
                           Eliom_common.Serv_Table.remove ptk !ptr)
                        in
                        if nodeopt = None
                        then
                          match
                            List.fold_right
                              (fun ({Eliom_common.s_expire} as a)
                                foll ->
                                match s_expire with
                                | Some (_, e) when !e < now -> foll
                                | _ -> a::foll
                              )
                              l
                              []
                          with
                          | [] ->
                            ptr := ll
                          | newl ->
                            ptr :=
                              Eliom_common.Serv_Table.add
                                ptk (`Ptc (nodeopt, newl)) ll
                      with Not_found -> ());
                 Lwt.pause ())
            !ptr
            return_unit >>= fun () ->
          if Eliom_common.Serv_Table.is_empty !ptr
          then
            (match !t with
               | Eliom_common.Vide -> ()
               | Eliom_common.Table tr ->
                   let newr =
                     String.Table.remove filename tr
                   in
                   if String.Table.is_empty newr
                   then t := Eliom_common.Vide
                   else t := Eliom_common.Table newr
            );
          Lwt.return_unit
  and empty_one t =
    match !t with
      | Eliom_common.Vide -> Lwt.return_unit
      | Eliom_common.Table r ->
        if String.Table.is_empty r
        then begin t := Eliom_common.Vide; Lwt.return_unit end
        else begin
          String.Table.fold (aux t) r (Lwt.return_unit) >>= fun () ->
          match !t with (* !t has probably changed *)
            | Eliom_common.Vide -> Lwt.return_unit
            | Eliom_common.Table r ->
              if String.Table.is_empty r
              then t := Eliom_common.Vide;
              Lwt.return_unit
        end
  in
  Lwt_list.iter_s
    (fun (_, _prio, t) -> empty_one t) tables.Eliom_common.table_services
  >>= fun () ->
  tables.Eliom_common.table_services <-
    List.filter
    (fun r -> !(Tuple3.thd r) <> Eliom_common.Vide)
    tables.Eliom_common.table_services;
  Lwt.return_unit


let gc_timeouted_naservices now tr =
  match !tr with
    | Eliom_common.AVide -> return_unit
    | Eliom_common.ATable t ->
        if Eliom_common.NAserv_Table.is_empty t
        then begin tr := Eliom_common.AVide; Lwt.return_unit end
        else
          Eliom_common.NAserv_Table.fold
            (fun k (_, _, expdate, _, nodeopt) thr ->
               thr >>= fun () ->
               (match expdate with
                  | Some (_, e) when !e < now ->
                      (match nodeopt with
                         | Some node ->
                             Ocsigen_cache.Dlist.remove node
                               (* will remove from the table automatically *);
                         | _ ->
                             tr := Eliom_common.remove_naservice_table !tr k
                      )
                  | _ -> ());
               Lwt.pause ()
            )
            t
            return_unit



(* This is a thread that will work for example every hour. *)
let service_session_gc sitedata =
  let tables = sitedata.Eliom_common.global_services in
  match get_servicesessiongcfrequency () with
  | None -> () (* No garbage collection *)
  | Some t ->
      let rec f () =
        Lwt_unix.sleep t >>= fun () ->
        let service_cookie_table = sitedata.Eliom_common.session_services in
        let now = Unix.time () in
        Lwt_log.ign_info ~section "GC of service sessions";

        (* public continuation tables: *)
        (if tables.Eliom_common.table_contains_services_with_timeout
         then gc_timeouted_services now tables
         else return_unit) >>= fun () ->
        (if tables.Eliom_common.table_contains_naservices_with_timeout
         then gc_timeouted_naservices now tables.Eliom_common.table_naservices
         else return_unit) >>= fun () ->

        (* private continuation tables: *)
        Eliom_common.SessionCookies.fold
          (fun k {Eliom_common.Service_cookie.session_table = tables; expiry;
                                            session_group; session_group_node} thr ->
            thr >>= fun () ->
            (match !expiry with
            | Some exp when exp < now ->
              Eliommod_sessiongroups.Serv.remove session_group_node;
              Lwt.return_unit
            | _ ->
              (if tables.Eliom_common.table_contains_services_with_timeout
               then gc_timeouted_services now tables
               else return_unit) >>= fun () ->
              (if tables.Eliom_common.table_contains_naservices_with_timeout
               then gc_timeouted_naservices now
                  tables.Eliom_common.table_naservices
               else return_unit) >>= fun () ->
              (match !session_group with
              | (_, _scope, Right _) (* no group *)
(*VVV check this *)
                  when
                    (Eliommod_sessiongroups.Serv.group_size
                       (sitedata.Eliom_common.site_dir_string,
                        `Client_process,
                        Left k)
                     = 0) (* no tab sessions *)
                    && Eliom_common.service_tables_are_empty tables ->
                (* The session is not used in any table
                   and is not in a group
                   (scope must be `Session,
                   as all tab sessions are in a group),
                   and is not associated to any tab session.
                   We can remove it. *)
                Eliommod_sessiongroups.Serv.remove session_group_node
              | _ -> () (*VVV enough? *));
              return_unit
            )
            >>= Lwt.pause
          )
          service_cookie_table
          return_unit
        >>= f
      in Lwt.async f

(* This is a thread that will work for example every hour. *)
let data_session_gc sitedata =
  match get_datasessiongcfrequency () with
  | None -> () (* No garbage collection *)
  | Some t ->
      let rec f () =
        Lwt_unix.sleep t >>= fun () ->
        let data_cookie_table = sitedata.Eliom_common.session_data in
        let not_bound_in_data_tables =
          sitedata.Eliom_common.not_bound_in_data_tables in
        let now = Unix.time () in
        Lwt_log.ign_info ~section "GC of session data";
        (* private continuation tables: *)
        Eliom_common.SessionCookies.fold
          (fun k {Eliom_common.Data_cookie.expiry; session_group; session_group_node} thr ->
            thr >>= fun () ->
            (match !expiry with
               | Some exp when exp < now ->
                   Eliommod_sessiongroups.Data.remove session_group_node;
                   return_unit
               | _ ->
                   match !session_group with
                     | (_, scope, Right _) (* no group *)
                         when
                           (Eliommod_sessiongroups.Data.group_size
                              (sitedata.Eliom_common.site_dir_string,
                               `Client_process,
                               Left k)
                            = 0) (* no tab sessions *)
                           &&
                             not_bound_in_data_tables k ->
                       (* The session is not used in any table
                          and is not in a group
                          (scope must be `Session,
                          as all tab sessions are in a group),
                          and is not associated to any tab session.
                          We can remove it. *)
                       if scope <> `Session
                       then
                         Lwt_log.ign_error ~section
                           "Eliom: Group associated to IP has scope different from `Session. Please report the problem.";
                       Eliommod_sessiongroups.Data.remove session_group_node;
                       (* See also the finalisers in Eliommod_sessiongroups
                          and Eliommod.ml *)
                               Lwt.return_unit
                     | _ -> Lwt.return_unit
            )
            >>= Lwt.pause
          )
          data_cookie_table
          return_unit
          >>=
        f
      in Lwt.async f

(* garbage collection of timeouted persistent sessions *)
(* This is a thread that will work every hour/day *)
let persistent_session_gc sitedata =
  let gc () =
    let now = Unix.time () in
    let log_hash c = Eliom_common.Hashed_cookies.(sha256 c) in
    let do_gc_cookie cookie {Eliommod_cookies.full_state_name; expiry; session_group} =
      let scope = full_state_name.Eliom_common.user_scope in
      match expiry with
      | Some exp when exp <= now ->
        Lwt_log.ign_info_f ~section "remove expired cookie %s" (log_hash cookie);
        Eliommod_persess.close_persistent_state2
          ~scope
          sitedata
          session_group cookie
      (*WAS: remove_from_all_persistent_tables k *)
      | _ ->
          Lwt_log.ign_info_f ~section "cookie not expired: %s" (log_hash cookie);
          return_unit
    in
    let gc_cookie c =
      Lwt.try_bind
        (fun () -> Eliommod_cookies.Persistent_cookies.Cookies.find c)
        (do_gc_cookie c)
        (function
         | Not_found ->
             Lwt_log.ign_info_f ~section "cookie does not exist: %s" (log_hash c);
             Lwt.return_unit
         | exn -> Lwt.fail exn)
    in
    Lwt_log.ign_info ~section "GC of persistent sessions";
    Eliommod_cookies.Persistent_cookies.garbage_collect ~section gc_cookie
  in
  match get_persistentsessiongcfrequency () with
  | None -> () (* No garbage collection *)
  | Some t -> let rec f () = Lwt_unix.sleep t >>= gc >>= f in Lwt.async f
