(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_gc.ml
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

(** Garbage collection of services and session data *)

open Lwt

(*****************************************************************************)
let servicesessiongcfrequency = ref (Some 3600.)
let datasessiongcfrequency = ref (Some 3600.)
let persistentsessiongcfrequency = ref (Some 86400.)
let set_servicesessiongcfrequency i = servicesessiongcfrequency := i
let set_datasessiongcfrequency i = datasessiongcfrequency := i
let get_servicesessiongcfrequency () = !servicesessiongcfrequency
let get_datasessiongcfrequency () = !datasessiongcfrequency
let set_persistentsessiongcfrequency i = persistentsessiongcfrequency := i
let get_persistentsessiongcfrequency () = !persistentsessiongcfrequency


(* garbage collection of timeouted sessions *)
let rec gc_timeouted_services now t =
  let rec aux k direltr thr =
    thr >>=
    (fun table ->
      match !direltr with
      | Eliom_common.Dir r -> gc_timeouted_services now r >>=
          (fun () -> match !r with
          | Eliom_common.Vide ->
              return (Eliom_common.String_Table.remove k table)
          | Eliom_common.Table t -> return table)
      | Eliom_common.File ptr ->
          List.fold_right
            (fun (ptk, l) foll ->
              foll >>=
              (fun foll ->
                let newl =
                  List.fold_right
                    (fun ((i, (_, (_, expdate, _))) as a) foll ->
                      match expdate with
                      | Some (_, e) when !e < now -> foll
                      | _ -> a::foll
                    )
                    l
                    []
                in
                Lwt_unix.yield () >>=
                (fun () ->
                  match newl with
                  | [] -> return foll
                  | _ -> return ((ptk, newl)::foll))
              )
            )
            !ptr
            (return []) >>=
          (function
            | [] -> return (Eliom_common.String_Table.remove k table)
            | r -> ptr := r; return table)
    )
  in
  match !t with
  | Eliom_common.Vide -> return ()
  | Eliom_common.Table r ->
      (Eliom_common.String_Table.fold aux r (return r)) >>=
      (fun table ->
        if Eliom_common.String_Table.is_empty table
        then begin t := Eliom_common.Vide; return () end
        else begin t := Eliom_common.Table table; return () end)

let gc_timeouted_naservices now tr =
  match !tr with
  | Eliom_common.AVide -> return ()
  | Eliom_common.ATable t ->
      Eliom_common.NAserv_Table.fold
        (fun k (_, _, expdate, _) thr ->
          thr >>=
          (fun table ->
            Lwt_unix.yield () >>=
            (fun () ->
              match expdate with
              | Some (_, e) when !e < now ->
                  return (Eliom_common.NAserv_Table.remove k table)
              | _ -> return table)
          ))
        t
        (return t) >>=
      (fun t ->
        if Eliom_common.NAserv_Table.is_empty t
        then tr := Eliom_common.AVide
        else tr := Eliom_common.ATable t;
        return ())



(* This is a thread that will work for example every hour. *)
let service_session_gc sitedata =
  let (servicetable,
       naservicetable,
       contains_services_with_timeout,
       contains_naservices_with_timeout) =
    sitedata.Eliom_common.global_services
  in
  match get_servicesessiongcfrequency () with
  | None -> () (* No garbage collection *)
  | Some t ->
      let rec f () =
        Lwt_unix.sleep t >>=
        (fun () ->
          let service_cookie_table =
            sitedata.Eliom_common.session_services
          in
          let now = Unix.time () in
          Ocsigen_messages.debug2 "--Eliom: GC of service sessions";
          (* public continuation tables: *)
          (if !contains_services_with_timeout
          then gc_timeouted_services now servicetable
          else return ()) >>=
          (fun () -> if !contains_naservices_with_timeout
          then gc_timeouted_naservices now naservicetable
          else return ()) >>=
          (* private continuation tables: *)
          (fun () ->
            (* private continuation tables: *)
            Eliom_common.SessionCookies.fold
              (fun k (sessname,
                      ((servicetable,
                        naservicetable,
                        contains_services_with_timeout,
                        contains_naservices_with_timeout) as tables),
                      exp,
                      _,
                      session_group_ref) thr ->
                        thr >>= fun () ->
                          (match !exp with
                          | Some exp when exp < now ->
                              Eliom_common.close_service_session2
                                sitedata !session_group_ref k;
                              return ()
                          | _ ->
                              (if !contains_services_with_timeout
                              then gc_timeouted_services now servicetable
                              else return ()) >>=
                              (fun () -> if !contains_naservices_with_timeout
                              then gc_timeouted_naservices now naservicetable
                              else return ()) >>=
                              (fun () ->
                                if Eliom_common.service_tables_are_empty
                                    tables
                                then
                                  Eliom_common.close_service_session2
                                    sitedata !session_group_ref k;
                                return ()
                              )
                          )
                            >>= Lwt_unix.yield
              )
              service_cookie_table
              (return ()))
        )
          >>=
        f
      in ignore (f ())

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
        Ocsigen_messages.debug2 "--Eliom: GC of session data";
        (* private continuation tables: *)
        Eliom_common.SessionCookies.fold
          (fun k (sessname, exp, _, session_group_ref) thr ->
            thr >>= fun () ->
              (match !exp with
              | Some exp when exp < now ->
                  Eliommod_datasess.close_data_session2
                    sitedata !session_group_ref k;
                  return ()
              | _ ->
                  if !session_group_ref = None && not_bound_in_data_tables k
                  then
                    Eliom_common.SessionCookies.remove data_cookie_table k;
                  return ()
              )
                >>= Lwt_unix.yield
          )
          data_cookie_table
          (return ())
          >>=
        f
      in ignore (f ())

(* garbage collection of timeouted persistent sessions *)
(* This is a thread that will work every hour/day *)
let persistent_session_gc () =
  match get_persistentsessiongcfrequency () with
  | None -> () (* No garbage collection *)
  | Some t ->
      let rec f () =
        Lwt_unix.sleep t >>=
        (fun () ->
          let now = Unix.time () in
          Ocsigen_messages.debug2 "--Eliom: GC of persistent sessions";
          (Ocsipersist.iter_table
             (fun k (_, exp, _, session_group) ->
               (match exp with
               | Some exp when exp < now ->
(*VVV ? *)
                   Eliommod_persess.close_persistent_session2 session_group k
                     (*WAS: remove_from_all_persistent_tables k *)
               | _ -> return ())
             )
             (Lazy.force Eliommod_persess.persistent_cookies_table)))
          >>=
        f
      in ignore (f ())

