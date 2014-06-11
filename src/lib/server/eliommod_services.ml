(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_services.ml
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
(** Tables of services                                                       *)
(** Store and load services                                                  *)
(*****************************************************************************)
(*****************************************************************************)

open Eliom_lib

open Lwt
open Ocsigen_extensions


(*****************************************************************************)

let find_page_table
    nosuffixversion
    now
    (pagetableref : Eliom_common.page_table ref)
    fullsessname
    sitedata
    ((ri,
      si,
      all_cookie_info,
      all_tab_cookie_info,
      user_tab_cookies) as info)
    urlsuffix
    k
    =
  let sp =
    Eliom_common.make_server_params sitedata info urlsuffix fullsessname in
  catch
    (fun () -> return (Eliom_common.Serv_Table.find k !pagetableref))
    (function
      | Not_found -> fail Eliom_common.Eliom_404
      | e -> fail e) >>= fun (Eliom_common.Ptc (nodeopt, l)) ->
  let rec aux toremove = function
    | [] -> Lwt.return ((Eliom_common.Notfound
                           Eliom_common.Eliom_Wrong_parameter), [])
    | (((_anontyp, (max_use, expdate, funct)) as a)::l) ->
      match expdate with
        | Some (_, e) when !e < now ->
              (* Service expired. Removing it. *)
          Ocsigen_messages.debug2 "--Eliom: Service expired. I'm removing it";
          aux toremove l >>= fun (r, toremove) ->
          Lwt.return (r, a::toremove)
            | _ ->
              catch
                (fun () ->
                  Ocsigen_messages.debug2 "--Eliom: I'm trying a service";
                  funct nosuffixversion sp >>= fun p ->
                      (* warning: the list ll may change during funct
                         if funct register something on the same URL!! *)
                  Ocsigen_messages.debug2
                    "--Eliom: Page found and generated successfully";

                      (* If this is an anonymous coservice,
                         we place it at the top of the dlist
                         (limitation of number of coservices) *)
                  (match nodeopt with
                    | None -> ()
                    | Some node -> Ocsigen_cache.Dlist.up node);

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
  aux [] l >>= fun (r, toremove) ->

  (match nodeopt, toremove with
    | _, [] -> ()
    | Some node, _ -> (* it is an anonymous coservice that has expired.
                         We remove it form the dlist.
                         This will do the removal from this table
                         automatically.
                         Note that in that case, toremove has length 1
                         (like the initial list l).
                      *)
      Ocsigen_cache.Dlist.remove node;
    | None, _ -> (* removing manually *)
      try
        let (Eliom_common.Ptc (_, list)), newptr =
          (Eliom_common.Serv_Table.find k !pagetableref,
           Eliom_common.Serv_Table.remove k !pagetableref)
        in
            (* We do find once again because it may have changed! *)

        let newlist =
          List.fold_left
            (fun l a -> List.remove_first_if_any_q a l)
                (* physical equality! *)
            list
            toremove
        in
        (if newlist = []
         then pagetableref := newptr
         else pagetableref :=
            Eliom_common.Serv_Table.add
            k
            (Eliom_common.Ptc (None, newlist))
            newptr)
      with Not_found -> ()
  );
  match r with
    | Eliom_common.Found r -> Lwt.return r
    | Eliom_common.Notfound e -> fail e


let add_page_table tables url_act tref
    key (id, ((max_use, expdate, action) as va)) =

  let sp = Eliom_common.get_sp_option () in
  (match expdate with
     | Some _ ->
         tables.Eliom_common.table_contains_services_with_timeout <- true
     | _ -> ());

  (* Duplicate registration forbidden in global table with same generation *)
  let v = (id, va) in
  match key with
    | {Eliom_common.key_state = (Eliom_common.SAtt_anon _, _) ;
       Eliom_common.key_kind = Ocsigen_http_frame.Http_header.GET }
    | {Eliom_common.key_state = (_, Eliom_common.SAtt_anon _) ;
       Eliom_common.key_kind = Ocsigen_http_frame.Http_header.POST }
    | {Eliom_common.key_state = (_, Eliom_common.SAtt_anon _) ;
       Eliom_common.key_kind = Ocsigen_http_frame.Http_header.PUT }
    | {Eliom_common.key_state = (_, Eliom_common.SAtt_anon _) ;
       Eliom_common.key_kind = Ocsigen_http_frame.Http_header.DELETE } ->
        (* Anonymous coservice:
           - only one for each key
           - we add a node in the dlist to limit their number *)
        (try
           let (Eliom_common.Ptc (nodeopt, l)), newt =
             (Eliom_common.Serv_Table.find key !tref,
              Eliom_common.Serv_Table.remove key !tref)
           in
           (match nodeopt with
              | None -> () (* should not occure *)
              | Some node -> Ocsigen_cache.Dlist.up node);
           tref :=
             Eliom_common.Serv_Table.add
               key (Eliom_common.Ptc (nodeopt, [v])) newt
         with Not_found ->
           let node =
             tables.Eliom_common.service_dlist_add
               ?sp
               (Left (tref, key))
           in
           tref :=
             Eliom_common.Serv_Table.add
               key (Eliom_common.Ptc (Some node, [v])) !tref)
    | {Eliom_common.key_state = (Eliom_common.SAtt_no, Eliom_common.SAtt_no) ;
       Eliom_common.key_kind = _ } ->
        (try
           let (Eliom_common.Ptc (nodeopt, l)), newt =
             (Eliom_common.Serv_Table.find key !tref,
              Eliom_common.Serv_Table.remove key !tref)
           in
           (* nodeopt should be None *)
           try
(******** Vérifier ici qu'il n'y a pas qqchose similaire déjà enregistré ?! *)
             let _, oldl = List.assoc_remove id l in
             (* if there was an old version with the same id, we remove it? *)
             if sp = None
             then
               (* but if there was already one with same generation, we fail
                  (if during intialisation) *)
               raise (Eliom_common.Eliom_duplicate_registration
                        (Url.string_of_url_path ~encode:false url_act))
             else
               tref :=
                 Eliom_common.Serv_Table.add
                   key
                   (Eliom_common.Ptc (None,
                                      (* We insert as last element
                                         so that services are tried in
                                         registration order*)
                                      (oldl@[v])))
                   newt
           with Not_found ->
             tref :=
               Eliom_common.Serv_Table.add
                 key
                 (Eliom_common.Ptc (None, (l@[v])))
                 newt
         with Not_found ->
           tref :=
             Eliom_common.Serv_Table.add
               key (Eliom_common.Ptc (None, [v]))
               !tref)
    | _ ->
        try
          let (Eliom_common.Ptc (nodeopt, l)), newt =
             (Eliom_common.Serv_Table.find key !tref,
              Eliom_common.Serv_Table.remove key !tref)
          in
          let oldl = List.remove_assoc id l in
          (* if there was an old version with the same id, we remove it *)
          tref :=
            Eliom_common.Serv_Table.add
              key
              (Eliom_common.Ptc (None, (oldl@[v])))
              newt
        with Not_found ->
          tref :=
            Eliom_common.Serv_Table.add
              key (Eliom_common.Ptc (None, [v]))
              !tref



let remove_page_table _ _ tref key id =
  (* Actually this does not remove empty directories.
     But this will be done by the next service GC *)

    let Eliom_common.Ptc (nodeopt, l) = Eliom_common.Serv_Table.find key !tref
    in
    match nodeopt with
      | Some node ->
          (* In that case, l has size 1, and the id is correct,
             because it is an anonymous coservice *)
          (*VVV the key is searched twice *)
          Ocsigen_cache.Dlist.remove node
      | None ->
          let newt = Eliom_common.Serv_Table.remove key !tref in
          match List.remove_all_assoc id l with
            | [] -> tref := newt
                (* In that case, we must remove it, otherwise we get
                   "Wrong parameters" instead of "404 Not found" *)
            | newl ->
                tref :=
                  Eliom_common.Serv_Table.add
                    key
                    (Eliom_common.Ptc (None, newl))
                    newt

let add_dircontent dc (key, elt) =
  match dc with
  | Eliom_common.Vide ->
      Eliom_common.Table
        (String.Table.add
           key elt String.Table.empty)
  | Eliom_common.Table t ->
      Eliom_common.Table (String.Table.add key elt t)

let find_dircontent dc k =
  match dc with
  | Eliom_common.Vide -> raise Not_found
  | Eliom_common.Table t -> String.Table.find k t


(*****************************************************************************)

let add_or_remove_service
    f
    tables
    table
    url_act
    page_table_key
    va =

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

  let page_table_ref =
    search_page_table_ref table url_act
  in
  f tables url_act page_table_ref page_table_key va

let add_service priority tables url_act page_table_key va =
  let generation = Ocsigen_extensions.get_numberofreloads () in
  let rec find_table = function
    | [] -> let t = ref (Eliom_common.empty_dircontent ()) in
            t, [(generation, priority, t)]
    | ((g, p, t)::_) as l when g = generation && p = priority -> t, l
    | ((g, p, _)::_) as l when g < generation || p < priority ->
      let t = ref (Eliom_common.empty_dircontent ()) in
      t, (generation, priority, t)::l
    | ((g, p, _) as a)::l when g = generation && p > priority ->
      let t, ll = find_table l in
      t, a::ll
    | _ -> assert false
  in
  let table, new_table_services =
    find_table tables.Eliom_common.table_services in
  tables.Eliom_common.table_services <- new_table_services;
  add_or_remove_service add_page_table tables table url_act page_table_key va

let remove_service tables path k unique_id =
  let rec aux = function
    | [] -> ()
    | (_, _, table)::l ->
      try
        add_or_remove_service remove_page_table tables table path k unique_id
      with Not_found -> aux l
  in
  aux tables.Eliom_common.table_services


exception Exn1

let find_service
    now
    tables
    fullsessname
    sitedata
    ((ri,
      si,
      all_cookie_info,
      all_tab_cookie_info,
      user_tab_cookies) as info) =

  let rec search_page_table dircontent =
    let find nosuffixversion page_table_ref suffix =
      find_page_table
        nosuffixversion
        now
        page_table_ref
        fullsessname
        sitedata
        info
        suffix
        {Eliom_common.key_state =
            (Eliom_common.att_key_serv_of_req
               (fst si.Eliom_common.si_state_info),
             Eliom_common.att_key_serv_of_req
               (snd si.Eliom_common.si_state_info));
         Eliom_common.key_kind = Ocsigen_request_info.meth ri.request_info}
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
                     | [] -> find false page_table_ref None
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
                        find false page_table_ref
                          (if a = None then Some [] else Some (aa::l))
                with e -> Lwt.fail e)
           | e -> Lwt.fail e)
    in function
      | [] ->
          (* It is a directory, without / at the end. We do a redirection. *)
          Lwt.fail (Ocsigen_extensions.Ocsigen_Is_a_directory ri)
      | [""] -> aux None []
      | [a] when a = Eliom_common.eliom_nosuffix_page ->
          (* version without suffix of suffix service *)
          (try
             match !(try
                       find_dircontent dircontent
                         Eliom_common.eliom_suffix_internal_name
                     with Not_found -> raise Exn1)
             with
               | Eliom_common.Dir _ -> Lwt.fail Exn1
               | Eliom_common.File page_table_ref ->
                 find true page_table_ref None
           with e -> Lwt.fail e)

(*      | ""::l -> search_page_table dircontent l *)
          (* We do not remove "//" any more
             because of optional suffixes *)
      | a::l -> aux (Some a) l
  in
  let search_by_priority_generation tables path =
      (* New in 1.91: There is now one table for each pair
         (generation, priority) *)
    List.fold_left
      (fun prev (_prio, _gen, table) ->
        Lwt.catch
          (fun () -> prev)
          (function
            | Exn1
            | Eliom_common.Eliom_404
            | Eliom_common.Eliom_Wrong_parameter ->
              search_page_table !table path
            | e -> fail e))
      (fail Exn1)
      tables
  in
  Lwt.catch
    (fun () ->
      search_by_priority_generation
        tables.Eliom_common.table_services
        (Url.change_empty_list (Ocsigen_request_info.sub_path ri.request_info)))
    (function Exn1 -> Lwt.fail Eliom_common.Eliom_404 | e -> Lwt.fail e)


(******************************************************************)
(* attached services                                              *)
let get_page
    now
    ((ri,
      si,
      (((service_cookies_info, _, _), secure_ci) as all_cookie_info),
      (((service_cookies_info_tab, _, _), secure_ci_tab)
          as all_tab_cookie_info),
      user_tab_cookies) as info)
    sitedata
    =

  let find_aux e sci =
    Eliom_common.Full_state_name_table.fold
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
                      sitedata
                      info)
            | e -> fail e)
      )
      sci
      (fail Eliom_common.Eliom_404)
  in

  let tables = [] in
  let tables = (!service_cookies_info, "session table")::tables in
  let tables =
    match secure_ci with
      | Some (service_cookies_info, _, _) ->
        (!service_cookies_info, "secure session table")::tables
      | _ -> tables
  in
  let tables = (!service_cookies_info_tab, "tab session table")::tables in
  let tables =
    match secure_ci_tab with
      | Some (service_cookies_info, _, _) ->
        (!service_cookies_info, "secure tab session table")::tables
      | _ -> tables
  in
  (catch
     (fun () ->
       List.fold_left
         (fun beg (table, table_name) ->
           catch
             (fun () -> beg)
             (function
               | Eliom_common.Eliom_404
               | Eliom_common.Eliom_Wrong_parameter ->
                 Ocsigen_messages.debug
                   (fun () -> String.concat ""
                     ["--Eliom: I'm looking for ";
                      (Url.string_of_url_path
                         ~encode:true (Ocsigen_request_info.sub_path ri.request_info));
                      " in the "; table_name; ":"]);
                 find_aux Eliom_common.Eliom_404 table
               | e -> Lwt.fail e))
         (Lwt.fail Eliom_common.Eliom_404)
         tables
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
               sitedata
               info)
           (function
             | Eliom_common.Eliom_404
             | Eliom_common.Eliom_Wrong_parameter as exn ->
                    (* si pas trouvé avec, on essaie sans l'état *)
               (match si.Eliom_common.si_state_info with
                 | (Eliom_common.RAtt_no, Eliom_common.RAtt_no) -> fail exn
                 | (g, Eliom_common.RAtt_anon _)
                 | (g, Eliom_common.RAtt_named _) ->
                        (* There was a POST state.
                           We remove it, and remove POST parameters.
                        *)
                   Ocsigen_messages.debug2
                     "--Eliom: Link too old. I will try without POST parameters:";
                   Polytables.set
                     (Ocsigen_request_info.request_cache ri.request_info)
                     Eliom_common.eliom_link_too_old
                     true;
                   fail (Eliom_common.Eliom_retry_with
                           ({ri with request_info =
                             Ocsigen_request_info.update ri.request_info
                               ~post_params:
                                 (match Ocsigen_request_info.post_params ri.request_info with
                                  | None -> None
                                  | Some _ -> Some (fun _ -> Lwt.return []))
                               ~files:
                                 (match Ocsigen_request_info.files ri.request_info with
                                  | None -> None
                                  | Some _ -> Some (fun _ -> Lwt.return []))
                               ~meth:Ocsigen_http_frame.Http_header.GET
                             ()},
                            {si with
                              Eliom_common.si_nonatt_info=
                                Eliom_common.RNa_no;
                              Eliom_common.si_state_info=
                                (g, Eliom_common.RAtt_no);
                            },
                            all_cookie_info,
                            all_tab_cookie_info,
                            user_tab_cookies
                           ))
                 | (Eliom_common.RAtt_named _, Eliom_common.RAtt_no)
                 | (Eliom_common.RAtt_anon _, Eliom_common.RAtt_no) ->
                        (* There was a GET state, but no POST state.
                           We remove it with its parameters,
                           and remove POST parameters.
                        *)
                   Ocsigen_messages.debug2
                     "--Eliom: Link to old. I will try without GET state parameters and POST parameters:";
                   Polytables.set
                     (Ocsigen_request_info.request_cache ri.request_info)
                     Eliom_common.eliom_link_too_old
                     true;
                   fail (Eliom_common.Eliom_retry_with
                           ({ri with request_info =
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
                            },
                            {si with
                              Eliom_common.si_nonatt_info=
                                Eliom_common.RNa_no;
                              Eliom_common.si_state_info=
                                (Eliom_common.RAtt_no,
                                 Eliom_common.RAtt_no);
                              Eliom_common.si_other_get_params=[];
                            },
                            all_cookie_info,
                            all_tab_cookie_info,
                            user_tab_cookies))
               )
             | e -> fail e)
       | e -> fail e)
  )
