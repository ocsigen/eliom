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
open Ocsigen_extensions


(*****************************************************************************)

let find_page_table
    ?(redirectifsuffix=false)
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
                funct redirectifsuffix sp >>= fun p ->
                (* warning: the list ll may change during funct
                   if funct register something on the same URL!! *)
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
                            newtoremove))
              (function
                | Eliom_common.Eliom_Wrong_parameter ->
                    aux toremove l >>=
                    (fun (r, toremove) -> Lwt.return (r, toremove))
                | e -> Lwt.return ((Eliom_common.Notfound e), toremove))
  in
  (catch
     (fun () -> return (List.assoc k !pagetableref))
     (function
        | Not_found -> fail Eliom_common.Eliom_404
        | e -> fail e)) >>=
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
      if key.Eliom_common.key_state = (Eliom_common.Att_no, Eliom_common.Att_no)
      then begin
(********* Vérifier ici qu'il n'y a pas qqchose similaire déjà enregistré ?! *)
        let (oldgen, n), oldl = Ocsigen_lib.list_assoc_remove id l in
        if not duringsession && (generation = oldgen)
        then
          raise (Eliom_common.Eliom_duplicate_registration
                   (Ocsigen_lib.string_of_url_path ~encode:false url_act))
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
        (Ocsigen_lib.String_Table.add
           key elt Ocsigen_lib.String_Table.empty)
  | Eliom_common.Table t ->
      Eliom_common.Table (Ocsigen_lib.String_Table.add key elt t)

let find_dircontent dc k =
  match dc with
  | Eliom_common.Vide -> raise Not_found
  | Eliom_common.Table t -> Ocsigen_lib.String_Table.find k t


(*****************************************************************************)

let add_service
    (dircontentref, _, containstimeouts, _)
    duringsession
    url_act
    (page_table_key,
     ((unique_id1, unique_id2), max_use, expdate, action)) =

  let rec aux dircontentref a l =
    try
      let direltref = find_dircontent !dircontentref a in
      match !direltref with
      | Eliom_common.Dir dcr -> search_page_table_ref dcr l
      | Eliom_common.File ptr ->
          raise (Eliom_common.Eliom_page_erasing a)
            (* Ocsigen_messages.warning ("Eliom page registration: Page "^
               a^" has been replaced by a directory");
               let newdcr = ref (Eliom_common.empty_dircontent ()) in
               (direltref := Eliom_common.Dir newdcr;
               search_page_table_ref newdcr l) *)
    with
    | Not_found ->
        let newdcr = ref (Eliom_common.empty_dircontent ()) in
        (dircontentref :=
          add_dircontent !dircontentref (a, ref (Eliom_common.Dir newdcr));
         search_page_table_ref newdcr l)


  and search_page_table_ref dircontentref = function
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
    | a::l -> aux dircontentref a l
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
    let find ?redirectifsuffix page_table_ref suffix =
      find_page_table
        ?redirectifsuffix
        now
        page_table_ref
        fullsessname
        sitedata
        all_cookie_info
        ri
        suffix
        {Eliom_common.key_state = si.Eliom_common.si_state_info;
         Eliom_common.key_kind = ri.request_info.ri_method}
        si
    in
    let aux a l =
      let aa = match a with
      | None -> Eliom_common.defaultpagename
      | Some aa -> aa
      in
      Lwt.catch
        (fun () ->
           let dc =
             try !(find_dircontent dircontent aa)
             with Not_found -> raise Exn1
           in
           (match dc with
              | Eliom_common.Dir dircontentref2 ->
                  search_page_table !dircontentref2 l
              | Eliom_common.File page_table_ref -> 
                  (match l with
                     | [] -> find page_table_ref None
                     | l -> (* We have a file with suffix *)
                         raise Eliom_common.Eliom_Wrong_parameter)))
        (function
           | Exn1 | Eliom_common.Eliom_Wrong_parameter as e ->
               (* If no service matches, we try a suffix service *)
               (try
                  match !(try
                            find_dircontent dircontent
                              Eliom_common.eliom_suffix_internal_name
                          with Not_found -> raise e)
                  with
                    | Eliom_common.Dir _ -> Lwt.fail Exn1
                    | Eliom_common.File page_table_ref ->
                        find page_table_ref 
                          (if a = None then Some [] else Some (aa::l))
                with e -> Lwt.fail e)
           | e -> Lwt.fail e)
    in function
      | [] -> 
          (* First we test if it is the version without suffix of a
          suffix service (will be redirected) *)
          (try
             match !(try
                       find_dircontent dircontent
                         Eliom_common.eliom_suffix_internal_name
                     with Not_found -> raise Exn1)
             with
               | Eliom_common.Dir _ -> raise Exn1
               | Eliom_common.File page_table_ref ->
                   Lwt.catch
                     (fun () -> find ~redirectifsuffix:true page_table_ref None)
                     (function
                        | Eliom_common.Eliom_Typing_Error _
                        | Eliom_common.Eliom_Wrong_parameter ->
                            Lwt.fail
                              (Ocsigen_extensions.Ocsigen_Is_a_directory ri)
                        | e -> Lwt.fail e
                     )
           with Eliom_common.Eliom_Wrong_parameter | Exn1 ->
             (* otherwise, it is a directory, we redirect *)
             Lwt.fail (Ocsigen_extensions.Ocsigen_Is_a_directory ri))
      | [""] -> aux None []
(*      | ""::l -> search_page_table dircontent l *)
          (* We do not remove "//" any more 
             because of optional suffixes *)
      | a::l -> aux (Some a) l
  in
  Lwt.catch
    (fun () -> search_page_table !dircontentref
       (Ocsigen_lib.change_empty_list ri.request_info.ri_sub_path))
    (function Exn1 -> Lwt.fail Eliom_common.Eliom_404 | e -> Lwt.fail e)


(******************************************************************)
(* attached services                                              *)
let get_page
    now
    (ri,
     si,
     (((service_cookies_info, _, _), secure_ci) as all_cookie_info))
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
        (catch
           (fun () ->
              match secure_ci with
                | Some (service_cookies_info, _, _) ->
                    Ocsigen_messages.debug
                      (fun () ->
                         "--Eliom: I'm looking for "^
                           (Ocsigen_lib.string_of_url_path
                              ~encode:true
                              ri.request_info.ri_sub_path)^
                           " in the secure session table:");
                    find_aux Eliom_common.Eliom_404 !service_cookies_info
                | _ -> Lwt.fail Eliom_common.Eliom_404
           )
           (function
              | Eliom_common.Eliom_404
              | Eliom_common.Eliom_Wrong_parameter ->
                  Ocsigen_messages.debug
                    (fun () ->
                       "--Eliom: I'm looking for "^
                         (Ocsigen_lib.string_of_url_path
                            ~encode:true
                            ri.request_info.ri_sub_path)^
                         " in the session table:");
                  find_aux Eliom_common.Eliom_404 !service_cookies_info
              | e -> fail e)
        )
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
                    | (Eliom_common.Att_no, Eliom_common.Att_no) -> fail exn
                    | (g, Eliom_common.Att_anon _)
                    | (g, Eliom_common.Att_named _) ->
                        (* There was a POST state.
                           We remove it, and remove POST parameters.
                         *)
                        Ocsigen_messages.debug2
                          "--Eliom: Link to old. I will try without POST parameters:";
                        Polytables.set 
                          ri.request_info.ri_request_cache
                          Eliom_common.eliom_link_too_old
                          true;
                        fail (Eliom_common.Eliom_retry_with
                                ({ri with request_info =
                                     { ri.request_info with
                                         ri_post_params = lazy (return []);
                                         ri_method =
                                           Ocsigen_http_frame.Http_header.GET;
                                }},
                                 {si with
                                  Eliom_common.si_nonatt_info=
                                  Eliom_common.Na_no;
                                  Eliom_common.si_state_info= 
                                     (g, Eliom_common.Att_no);
                                },
                                 all_cookie_info
                                ))
                    | (Eliom_common.Att_named _, Eliom_common.Att_no)
                    | (Eliom_common.Att_anon _, Eliom_common.Att_no) ->
                        (* There was a GET state, but no POST state.
                           We remove it with its parameters,
                           and remove POST parameters.
                         *)
                        Ocsigen_messages.debug2
                          "--Eliom: Link to old. I will try without GET state parameters and POST parameters:";
                        Polytables.set 
                          ri.request_info.ri_request_cache
                          Eliom_common.eliom_link_too_old
                          true;
                        fail (Eliom_common.Eliom_retry_with
                                ({ri with request_info =
                                     { ri.request_info with
                                         ri_get_params =
                                           lazy si.Eliom_common.si_other_get_params;
                                         ri_post_params = lazy (return []);
                                         ri_method =
                                           Ocsigen_http_frame.Http_header.GET;
                                     }
                                },
                                 {si with
                                  Eliom_common.si_nonatt_info=
                                  Eliom_common.Na_no;
                                  Eliom_common.si_state_info=
                                     (Eliom_common.Att_no, Eliom_common.Att_no);
                                  Eliom_common.si_other_get_params=[];
                                },
                                 all_cookie_info)))
                | e -> fail e)
        | e -> fail e)
  )
