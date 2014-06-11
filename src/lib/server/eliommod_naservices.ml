(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_naservices.ml
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

(** Non-attached services                                                   *)

open Eliom_lib

open Lwt
open Ocsigen_extensions


let add_naservice_table at (key, elt) =
  match at with
    | Eliom_common.AVide ->
        Eliom_common.ATable
          (Eliom_common.NAserv_Table.add
             key elt Eliom_common.NAserv_Table.empty)
    | Eliom_common.ATable t ->
        Eliom_common.ATable (Eliom_common.NAserv_Table.add key elt t)

let find_naservice_table at k =
  match at with
    | Eliom_common.AVide -> raise Not_found
    | Eliom_common.ATable t -> Eliom_common.NAserv_Table.find k t

let add_naservice
    tables
    name
    (max_use, expdate, naservice) =
  let sp = Eliom_common.get_sp_option () in
  let generation = Ocsigen_extensions.get_numberofreloads () in
  (if (sp = None) (* not duringsession *)
  then
    try
      let (g, _, _, _, _) =
        find_naservice_table !(tables.Eliom_common.table_naservices) name
      in
      if g = generation then
        match name with
        | Eliom_common.SNa_no
        | Eliom_common.SNa_get' _
        | Eliom_common.SNa_post' _ ->
            raise (Eliom_common.Eliom_duplicate_registration
                     "<non-attached coservice>")
        | Eliom_common.SNa_get_ n ->
            raise (Eliom_common.Eliom_duplicate_registration
                     ("GET non-attached service "^n))
        | Eliom_common.SNa_post_ n ->
            raise (Eliom_common.Eliom_duplicate_registration
                     ("POST non-attached service "^n))
        | Eliom_common.SNa_void_dontkeep
        | Eliom_common.SNa_void_keep ->
            raise (Eliom_common.Eliom_duplicate_registration
                     "<void coservice>")
        | Eliom_common.SNa_get_csrf_safe _
        | Eliom_common.SNa_post_csrf_safe _
          -> assert false
    with Not_found -> ());

  (match expdate with
  | Some _ -> tables.Eliom_common.table_contains_naservices_with_timeout
      <- true
  | _ -> ());

  let node = match name with
    | Eliom_common.SNa_get' _ | Eliom_common.SNa_post' _ ->
        Some
          (tables.Eliom_common.service_dlist_add ?sp (Right name))
    | _ -> None
  in

  tables.Eliom_common.table_naservices :=
    add_naservice_table !(tables.Eliom_common.table_naservices)
      (name, (generation, max_use, expdate, naservice, node))

let remove_naservice_ tables name nodeopt =
  match nodeopt with
    | None ->
        tables.Eliom_common.table_naservices :=
          Eliom_common.remove_naservice_table
            !(tables.Eliom_common.table_naservices) name
    | Some node ->
        Ocsigen_cache.Dlist.remove node

let find_naservice now tables name =
  let ((_, _, expdate, _, nodeopt) as p) =
    find_naservice_table !(tables.Eliom_common.table_naservices) name
  in
  match expdate with
    | Some (_, e) when !e < now ->
        (* Service expired. Removing it. *)
        Ocsigen_messages.debug2
          "--Eliom: Non attached service expired. I'm removing it";
        remove_naservice_ tables name nodeopt;
        raise Not_found
    | _ ->
        (match nodeopt with
           | Some node -> Ocsigen_cache.Dlist.up node
           | None -> ());
        p

let remove_naservice tables name =
  let (_, _, _, _, nodeopt) =
    find_naservice_table !(tables.Eliom_common.table_naservices) name
  in
  remove_naservice_ tables name nodeopt


(******************************************************************)
(* non attached services                                          *)
let make_naservice
    now
    ((ri,
      si,
      (((service_cookies_info, _, _), secure_ci) as all_cookie_info),
      (((service_tab_cookies_info, _, _), secure_ci_tab)
          as all_tab_cookie_info),
      user_tab_cookies
     ) as info)
    sitedata
    =

  let rec find_aux sci =
    match
      Eliom_common.Full_state_name_table.fold
        (fun fullsessname (_, r) beg ->
          match beg with
          | Eliom_common.Found _ -> beg
          | Eliom_common.Notfound _ ->
              match !r with
              | Eliom_common.SCNo_data
              | Eliom_common.SCData_session_expired -> beg
              | Eliom_common.SC c ->
                  try
                    Eliom_common.Found
                      ((find_naservice
                          now !(c.Eliom_common.sc_table)
                          (Eliom_common.na_key_serv_of_req
                             si.Eliom_common.si_nonatt_info)),
                       !(c.Eliom_common.sc_table),
                        Some fullsessname)
                  with Not_found -> beg
        )
        sci
        (Eliom_common.Notfound ())
    with
    | Eliom_common.Found v -> v
    | Eliom_common.Notfound _ -> raise Not_found
  in


  let tables = [] in
  let tables = (!service_cookies_info, "session table")::tables in
  let tables =
    match secure_ci with
      | Some (service_cookies_info, _, _) ->
        (!service_cookies_info, "secure session table")::tables
      | _ -> tables
  in
  let tables = (!service_tab_cookies_info, "tab session table")::tables in
  let tables =
    match secure_ci_tab with
      | Some (service_cookies_info, _, _) ->
        (!service_cookies_info, "secure tab session table")::tables
      | _ -> tables
  in

  (try
     try
       let rec f = function
         | [] -> raise Not_found
         | (table, table_name)::l ->
           Ocsigen_messages.debug
             (fun () -> String.concat ""
               ["--Eliom: I'm looking for a non attached service in the ";
                table_name; ":"]);
           try return (find_aux table)
           with Not_found -> f l
       in f tables
     with Not_found ->
       begin
         Ocsigen_messages.debug2
           "--Eliom: Looking for a non attached service in the global table";
         return
           (find_naservice now sitedata.Eliom_common.global_services
              (Eliom_common.na_key_serv_of_req si.Eliom_common.si_nonatt_info),
            sitedata.Eliom_common.global_services,
            None)
       end
   with Not_found ->
    (* The non-attached service has not been found.
       We call the same URL without non-attached parameters.
    *)
     match si.Eliom_common.si_nonatt_info with
       | Eliom_common.RNa_no -> assert false
       | Eliom_common.RNa_post_ _
       | Eliom_common.RNa_post' _ ->
         (*VVV (Some, Some) or (_, Some)? *)
         Ocsigen_messages.debug2
           "--Eliom: Link too old to a non-attached POST coservice. I will try without POST parameters:";
         Polytables.set
           (Ocsigen_request_info.request_cache ri.request_info)
           Eliom_common.eliom_link_too_old
           true;
         Eliom_common.get_session_info
           {ri with Ocsigen_extensions.request_info =
               Ocsigen_request_info.update ri.request_info
                 ~get_params:(lazy si.Eliom_common.si_other_get_params)
                 ~post_params:(match Ocsigen_request_info.post_params ri.request_info with
                               | None -> None
                               | Some _ -> Some (fun _ -> Lwt.return []))
                 ~files:(match Ocsigen_request_info.files ri.request_info with
                         | None -> None
                         | Some _ -> Some (fun _ -> Lwt.return []))
                 ~meth:Ocsigen_http_frame.Http_header.GET
                 ()
               }
           si.Eliom_common.si_previous_extension_error
         >>= fun (ri', si', previous_tab_cookies_info) ->
         Lwt.fail (Eliom_common.Eliom_retry_with (ri',
                                                  si',
                                                  all_cookie_info,
                                                  all_tab_cookie_info,
                                                  user_tab_cookies))

       | Eliom_common.RNa_get_ _
       | Eliom_common.RNa_get' _ ->
         Ocsigen_messages.debug2
           "--Eliom: Link too old. I will try without non-attached parameters:";
         Polytables.set
           (Ocsigen_request_info.request_cache ri.request_info)
           Eliom_common.eliom_link_too_old
           true;
         Eliom_common.get_session_info
           {ri with request_info =
               Ocsigen_request_info.update ri.request_info
                 ~get_params:(lazy si.Eliom_common.si_other_get_params)
                 ~post_params:(match Ocsigen_request_info.post_params ri.request_info with
                               | None -> None
                               | Some _ -> Some (fun _ -> Lwt.return []))
                 ~files:(match Ocsigen_request_info.files ri.request_info with
                         | None -> None
                         | Some _ -> Some (fun _ -> Lwt.return []))
                 ~meth:Ocsigen_http_frame.Http_header.GET
                 ()
           }
           si.Eliom_common.si_previous_extension_error
         >>= fun (ri', si', previous_tab_cookies_info) ->
         Lwt.fail (Eliom_common.Eliom_retry_with (ri', si',
                                                  all_cookie_info,
                                                  all_tab_cookie_info,
                                                  user_tab_cookies)))

  >>= fun ((_, max_use, expdate, naservice, node),
           tablewhereithasbeenfound,
           fullsessname) ->
  let sp =
    Eliom_common.make_server_params sitedata info None fullsessname in
  naservice sp >>= fun r ->
  Ocsigen_messages.debug2
    "--Eliom: Non attached page found and generated successfully";
  (match expdate with
    | Some (timeout, e) -> e := timeout +. now
    | None -> ());
  (match max_use with
    | None -> ()
    | Some r ->
      if !r = 1
      then
        remove_naservice_
          tablewhereithasbeenfound
          (Eliom_common.na_key_serv_of_req si.Eliom_common.si_nonatt_info)
          node
      else r := !r - 1);
  return r
