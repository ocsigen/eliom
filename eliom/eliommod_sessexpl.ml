(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_sessexpl.ml
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
(*****************************************************************************)
(*****************************************************************************)
(** Internal functions used by Eliom:                                        *)
(** Exploration of sessions                                                  *)
(*****************************************************************************)
(*****************************************************************************)


open Lwt

(*****************************************************************************)
(* Iterators on sessions *)

  (** Iterator on service sessions *)
let iter_service_sessions sitedata f =
  Eliom_common.SessionCookies.fold
    (fun k v thr ->
      thr >>= fun () ->
        f (k, v, sitedata) >>=
        Lwt_unix.yield
    )
    sitedata.Eliom_common.session_services
    (return ())


    (** Iterator on data sessions *)
let iter_data_sessions sitedata f =
  Eliom_common.SessionCookies.fold
    (fun k v thr ->
      thr >>= fun () ->
        f (k, v, sitedata) >>=
        Lwt_unix.yield
    )
    sitedata.Eliom_common.session_data
    (return ())

    (** Iterator on persistent sessions *)
let iter_persistent_sessions f =
  Ocsipersist.iter_table
    (fun k v ->
      f (k, v) >>=
      Lwt_unix.yield
    )
    (Lazy.force Eliommod_persess.persistent_cookies_table)


    (** Iterator on service sessions *)
let fold_service_sessions sitedata f beg =
  Eliom_common.SessionCookies.fold
    (fun k v thr ->
      thr >>= fun res1 ->
        f (k, v, sitedata) res1 >>= fun res ->
          Lwt_unix.yield () >>= fun () ->
            return res
    )
    sitedata.Eliom_common.session_services
    (return beg)


    (** Iterator on data sessions *)
let fold_data_sessions sitedata f beg =
  Eliom_common.SessionCookies.fold
    (fun k v thr ->
      thr >>= fun res1 ->
        f (k, v, sitedata) res1 >>= fun res ->
          Lwt_unix.yield () >>= fun () ->
            return res
    )
    sitedata.Eliom_common.session_data
    (return beg)

    (** Iterator on persistent sessions *)
let fold_persistent_sessions f beg =
  Ocsipersist.fold_table
    (fun k v beg ->
      f (k, v) beg >>= fun res ->
        Lwt_unix.yield () >>= fun () ->
          return res
    )
    (Lazy.force Eliommod_persess.persistent_cookies_table)
    beg

(*****************************************************************************)
(* Exploration *)

let number_of_service_sessions ~sp =
  Eliom_common.SessionCookies.length
    sp.Eliom_common.sp_sitedata.Eliom_common.session_services

let number_of_data_sessions ~sp =
  Eliom_common.SessionCookies.length
    sp.Eliom_common.sp_sitedata.Eliom_common.session_data

let number_of_tables () =
  List.length !Eliommod_datasess.counttableelements

let number_of_table_elements () =
  List.map (fun f -> f ()) !Eliommod_datasess.counttableelements

let number_of_persistent_sessions () =
  Ocsipersist.length (Lazy.force Eliommod_persess.persistent_cookies_table)


