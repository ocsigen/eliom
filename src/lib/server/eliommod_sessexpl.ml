open Eio.Std

(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_sessexpl.ml
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
(*****************************************************************************)
(*****************************************************************************)
(** Internal functions used by Eliom:                                        *)

(** Exploration of cookies                                                  *)

(*****************************************************************************)
(*****************************************************************************)

(*****************************************************************************)
(* Iterators on cookies *)

(** Iterator on service cookies *)
let iter_service_cookies f =
  let sitedata =
    Eliom_request_info.find_sitedata "Eliommod_sessexpl.iter_service_cookies"
  in
  Eliom_common.SessionCookies.fold
    (fun k v thr ->
       thr;
       Fiber.yield (f (k, v)))
    sitedata.Eliom_common.session_services ()

(** Iterator on data cookies *)
let iter_data_cookies f =
  let sitedata =
    Eliom_request_info.find_sitedata "Eliommod_sessexpl.iter_data_cookies"
  in
  Eliom_common.SessionCookies.fold
    (fun k v thr ->
       thr;
       Fiber.yield (f (k, v)))
    sitedata.Eliom_common.session_data ()

(** Iterator on persistent cookies *)
let iter_persistent_cookies f =
  Eliommod_cookies.Persistent_cookies.Cookies.iter (fun k v ->
    Fiber.yield (f (k, v)))

(** Iterator on service cookies *)
let fold_service_cookies f beg =
  let sitedata =
    Eliom_request_info.find_sitedata "Eliommod_sessexpl.fold_service_cookies"
  in
  Eliom_common.SessionCookies.fold
    (fun k v thr ->
       let res1 = thr in
       let res = f (k, v) res1 in
       Fiber.yield (); res)
    sitedata.Eliom_common.session_services beg

(** Iterator on data cookies *)
let fold_data_cookies f beg =
  let sitedata =
    Eliom_request_info.find_sitedata "Eliommod_sessexpl.fold_data_cookies"
  in
  Eliom_common.SessionCookies.fold
    (fun k v thr ->
       let res1 = thr in
       let res = f (k, v) res1 in
       Fiber.yield (); res)
    sitedata.Eliom_common.session_data beg

(** Iterator on persistent cookies *)
let fold_persistent_cookies f beg =
  Eliommod_cookies.Persistent_cookies.Cookies.fold
    (fun k v beg ->
       let res = f (k, v) beg in
       Fiber.yield (); res)
    beg

(*****************************************************************************)
(* Exploration *)

let number_of_service_cookies () =
  Eliom_common.SessionCookies.length
    (Eliom_request_info.get_sitedata ()).Eliom_common.session_services

let number_of_data_cookies () =
  Eliom_common.SessionCookies.length
    (Eliom_request_info.get_sitedata ()).Eliom_common.session_data

let number_of_tables () = List.length !Eliommod_datasess.counttableelements

let number_of_table_elements () =
  List.map (fun f -> f ()) !Eliommod_datasess.counttableelements

let number_of_persistent_cookies () =
  Eliommod_cookies.Persistent_cookies.Cookies.length ()
