(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_services.ml
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
(** Tables of services                                                       *)
(** Store and load services                                                  *)
(*****************************************************************************)
(*****************************************************************************)

open Lwt


(*****************************************************************************)

let find_page_table 
    now
    (pagetableref : Eliom_common.page_table ref)
    fullsessname
    sitedata
    all_cookie_info
    ri
    urlsuffix
    k
    si
    = 
  let sp = 
    Eliom_common.make_server_params
      sitedata all_cookie_info ri urlsuffix si fullsessname 
  in
  let rec aux toremove = function
    | [] -> Lwt.return ((Eliom_common.Notfound
                           Eliom_common.Eliom_Wrong_parameter), [])
    | (((_, (_, (max_use, expdate, funct))) as a)::l) ->
        match expdate with
        | Some (_, e) when !e < now ->
            (* Service expired. Removing it. *)
            Ocsigen_messages.debug2 "--Eliom: Service expired. I'm removing it";
            aux toremove l >>= 
            (fun (r, toremove) -> Lwt.return (r, a::toremove))
        | _ ->
            catch 
              (fun () ->
                Ocsigen_messages.debug2 "--Eliom: I'm trying a service";
                funct sp
                  >>=
                (* warning: the list ll may change during funct
                   if funct register something on the same URL!! *)
                (fun p -> 
                  Ocsigen_messages.debug2
                    "--Eliom: Page found and generated successfully";

                  (* We update the expiration date *)
                  (match expdate with
                  | Some (timeout, e) -> e := timeout +. now
                  | None -> ());
                  let newtoremove =
                    (match max_use with
                    | Some r -> 
                        if !r = 1
                        then a::toremove
                        else (r := !r - 1; toremove)
                    | _ -> toremove)
                  in
                  Lwt.return (Eliom_common.Found p,
                               newtoremove)))
              (function
                | Eliom_common.Eliom_Wrong_parameter -> 
                    aux toremove l >>= 
                    (fun (r, toremove) -> Lwt.return (r, toremove))
                | e -> Lwt.return ((Eliom_common.Notfound e), toremove))
  in 
  (catch 
     (fun () -> return (List.assoc k !pagetableref))
     (function Not_found -> 
       fail Eliom_common.Eliom_404 | e -> fail e)) >>=
  aux [] >>=
  (fun (r, toremove) -> 
    let list, newptr = Ocsigen_lib.list_assoc_remove k !pagetableref in
    (* We do it once again because it may have changed! *)
    let newlist = 
      List.fold_left 
        (fun l a -> Ocsigen_lib.list_remove_first_if_any_q a l) list toremove 
    in
    (if newlist = []
    then pagetableref := newptr
    else pagetableref := (k, newlist)::newptr);
    match r with
    | Eliom_common.Found r -> Lwt.return r
    | Eliom_common.Notfound e -> fail e)


let rec insert_as_last_of_generation generation x = function
  | [] -> [x]
  | ((_, (g, _))::l) as ll when g < generation -> x::ll
  | a::l -> a::(insert_as_last_of_generation generation x l)



let add_page_table duringsession url_act t (key, (id, va)) = 
  (* Duplicate registration forbidden in global table with same generation *)
  let generation = Ocsigen_extensions.get_numberofreloads () in
  let v = (id, (generation, va)) in
  try
    let l, newt = Ocsigen_lib.list_assoc_remove key t in
    try
      if key.Eliom_common.key_state = (None, None)
      then begin
(********* Vérifier ici qu'il n'y a pas qqchose similaire déjà enregistré ?! *)
        let (oldgen, n), oldl = Ocsigen_lib.list_assoc_remove id l in
        if not duringsession && (generation = oldgen)
        then
          raise (Eliom_common.Eliom_duplicate_registration
                   (Ocsigen_lib.string_of_url_path url_act))
        else (key, (insert_as_last_of_generation generation v oldl))::newt 
      end
      else (key, (insert_as_last_of_generation generation v l))::newt
(********* et ici on ne vérifie pas s'il y a déjà l'unique_id ? à rev 20070712 *)
    with Not_found -> 
      (key, (insert_as_last_of_generation generation v l))::newt
  with Not_found -> (key, [v])::t

let add_dircontent dc (key, elt) =
  match dc with
  | Eliom_common.Vide -> 
      Eliom_common.Table 
        (Eliom_common.String_Table.add 
           key elt Eliom_common.String_Table.empty)
  | Eliom_common.Table t -> 
      Eliom_common.Table (Eliom_common.String_Table.add key elt t)

let find_dircontent dc k =
  match dc with
  | Eliom_common.Vide -> raise Not_found
  | Eliom_common.Table t -> Eliom_common.String_Table.find k t


(*****************************************************************************)

let add_service 
    (dircontentref, _, containstimeouts, _)
    duringsession
    url_act
    (page_table_key, 
     ((unique_id1, unique_id2), max_use, expdate, action)) =

  let aux search dircontentref a l =
    try 
      let direltref = find_dircontent !dircontentref a in
      match !direltref with
      | Eliom_common.Dir dcr -> search dcr l
      | Eliom_common.File ptr -> 
          raise (Eliom_common.Eliom_page_erasing a)
            (* Ocsigen_messages.warning ("Eliom page registration: Page "^
               a^" has been replaced by a directory");
               let newdcr = ref (Eliom_common.empty_dircontent ()) in
               (direltref := Eliom_common.Dir newdcr;
               search newdcr l) *)
    with
    | Not_found -> 
        let newdcr = ref (Eliom_common.empty_dircontent ()) in
        (dircontentref := 
          add_dircontent !dircontentref (a, ref (Eliom_common.Dir newdcr));
         search newdcr l)
  in 

  let rec search_page_table_ref dircontentref = function
    | [] | [""] -> 
        search_page_table_ref dircontentref [Eliom_common.defaultpagename]
    | [a] -> 
        (try 
          let direltref = find_dircontent !dircontentref a in
          (match !direltref with
          | Eliom_common.Dir _ -> 
              raise (Eliom_common.Eliom_page_erasing a)
                (* Ocsigen_messages.warning ("Eliom page registration: Directory "^
                   a^" has been replaced by a page");
                   let newpagetableref = ref (empty_page_table ()) in
                   (direltref := File newpagetableref;
                   newpagetableref) *)
          | Eliom_common.File ptr -> ptr)
        with
        | Not_found ->
            let newpagetableref = ref (Eliom_common.empty_page_table ()) in
            (dircontentref := 
              add_dircontent !dircontentref 
                (a, ref (Eliom_common.File newpagetableref));
             newpagetableref))
    | ""::l -> search_page_table_ref dircontentref l
    | a::l -> aux search_page_table_ref dircontentref a l
          (* and search_dircontentref dircontentref = function
             [] -> dircontentref
             | ""::l -> search_dircontentref dircontentref l
             | a::l -> aux search_dircontentref a l *)
  in

  (match expdate with
  | Some _ -> containstimeouts := true
  | _ -> ());

  let content = (page_table_key,
                 ((unique_id1, unique_id2), 
                  (max_use, expdate, action))) in

  let page_table_ref = 
    search_page_table_ref (*current_*) dircontentref url_act in
    page_table_ref := 
      add_page_table duringsession url_act !page_table_ref content


      
exception Exn1

let find_service 
    now
    (dircontentref, _, _, _)
    fullsessname
    (sitedata,
     all_cookie_info,
     ri,
     si) =

  let rec search_page_table dircontent =
    let aux a l =
      let aa = match a with
      | None -> Eliom_common.defaultpagename
      | Some aa -> aa
      in
      try
        let dc = 
          try !(find_dircontent dircontent aa) 
          with Not_found -> raise Exn1
        in
        (match dc with
        | Eliom_common.Dir dircontentref2 -> 
            search_page_table !dircontentref2 l
        | Eliom_common.File page_table_ref -> page_table_ref, l)
      with Exn1 -> 
        (match !(find_dircontent dircontent 
                   Eliom_common.eliom_suffix_internal_name) with
        | Eliom_common.Dir _ -> raise Not_found
        | Eliom_common.File page_table_ref -> 
            (page_table_ref, (if a = None then [""] else aa::l)))
    in function
      | [] -> raise Ocsigen_extensions.Ocsigen_Is_a_directory
      | [""] -> aux None []
      | ""::l -> search_page_table dircontent l
      | a::l -> aux (Some a) l
  in
  let page_table_ref, suffix = 
    try 
      search_page_table !dircontentref 
        (Ocsigen_lib.change_empty_list ri.Ocsigen_extensions.ri_sub_path)
    with Not_found -> raise Eliom_common.Eliom_404
  in
  find_page_table 
    now
    page_table_ref
    fullsessname
    sitedata
    all_cookie_info
    ri
    suffix
    {Eliom_common.key_state = si.Eliom_common.si_state_info;
     Eliom_common.key_kind = ri.Ocsigen_extensions.ri_method}
    si

(******************************************************************)
(* attached services                                              *)
let get_page
    now 
    (ri, 
     si,
     cookies_to_set,
     ((service_cookies_info, _, _) as all_cookie_info))
    sitedata
    =

  let find_aux e sci =
    Ocsigen_http_frame.Cookievalues.fold
      (fun fullsessname (_, r) beg -> 
        catch
          (fun () -> beg)
          (function
            | Eliom_common.Eliom_404 
            | Eliom_common.Eliom_Wrong_parameter ->
                (match !r with
                | Eliom_common.SCData_session_expired
                | Eliom_common.SCNo_data (* cookie removed *) -> beg
                | Eliom_common.SC c ->
                    find_service
                      now
                      !(c.Eliom_common.sc_table)
                      (Some fullsessname)
                      (sitedata,
                       all_cookie_info,
                       ri,
                       si))
            | e -> fail e)
      )
      sci
      (fail Eliom_common.Eliom_404)
  in

  (catch
     (fun () -> 
        Ocsigen_messages.debug 
          (fun () ->
            "--Eliom: I'm looking for "^
            (Ocsigen_lib.string_of_url_path ri.Ocsigen_extensions.ri_sub_path)^
            " in the session table:");
        find_aux Eliom_common.Eliom_404 !service_cookies_info
      )
      (function 
        | Eliom_common.Eliom_404
        | Eliom_common.Eliom_Wrong_parameter -> 
            catch (* ensuite dans la table globale *)
              (fun () -> 
                Ocsigen_messages.debug2 "--Eliom: I'm searching in the global table:";
                find_service 
                  now
                  sitedata.Eliom_common.global_services
                  None
                  (sitedata,
                   all_cookie_info,
                   ri,
                   si))
              (function
                | Eliom_common.Eliom_404 
                | Eliom_common.Eliom_Wrong_parameter as exn -> 
                    (* si pas trouvé avec, on essaie sans l'état *)
                    (match si.Eliom_common.si_state_info with
                    | (None, None) -> fail exn
                    | (g, Some _) -> 
                        (* There was a POST state. 
                           We remove it, and remove POST parameters.
                         *)
                        Ocsigen_messages.debug2 
                          "--Eliom: Link to old. I will try without POST parameters:";
                        fail (Eliom_common.Eliom_retry_with 
                                ({ri with 
                                  Ocsigen_extensions.ri_post_params = lazy (return []);
                                  Ocsigen_extensions.ri_method = 
                                  Ocsigen_http_frame.Http_header.GET;
                                  Ocsigen_extensions.ri_extension_info=
                                  Eliom_common.Eliom_Link_too_old::
                                  ri.Ocsigen_extensions.ri_extension_info
                                }, 
                                 {si with
                                  Eliom_common.si_nonatt_info= 
                                  Eliom_common.Na_no;
                                  Eliom_common.si_state_info= (g, None);
                                },
                                 cookies_to_set,
                                 all_cookie_info
                                ))
                    | (Some _, None) -> 
                        (* There was a GET state, but no POST state. 
                           We remove it with its parameters, 
                           and remove POST parameters.
                         *)
                        Ocsigen_messages.debug2 
                          "--Eliom: Link to old. I will try without GET state parameters and POST parameters:";
                        fail (Eliom_common.Eliom_retry_with 
                                ({ri with 
                                  ri_get_params = 
                                  lazy si.Eliom_common.si_other_get_params;
                                  Ocsigen_extensions.ri_post_params = lazy (return []);
                                  Ocsigen_extensions.ri_method = 
                                  Ocsigen_http_frame.Http_header.GET;
                                  Ocsigen_extensions.ri_extension_info= 
                                  Eliom_common.Eliom_Link_too_old::
                                  ri.Ocsigen_extensions.ri_extension_info
                                },
                                 {si with
                                  Eliom_common.si_nonatt_info= 
                                  Eliom_common.Na_no;
                                  Eliom_common.si_state_info=(None, None);
                                  Eliom_common.si_other_get_params=[];
                                },
                                 cookies_to_set,
                                 all_cookie_info)))
                | e -> fail e)
        | e -> fail e)
  )
