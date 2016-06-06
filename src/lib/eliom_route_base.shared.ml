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

(*****************************************************************************)

let section = Lwt_log.Section.make "eliom:service"

module type PARAM = sig

  type site_data

  type info

  val sess_info_of_info : info -> Eliom_common.sess_info

  val meth_of_info : info -> Eliom_common.meth

  val subpath_of_info : info -> string list

  val make_server_params :
    site_data ->
    info ->
    string list option ->
    Eliom_common.full_state_name option ->
    Eliom_common.server_params

  type result

  val handle_directory : info -> result Lwt.t

  module Node : sig
    type t
    val up : t -> unit
    val remove : t -> unit
  end

  module Table : sig

    type t

    val empty : unit -> t

    val add  :
      Eliom_common.page_table_key ->
      Node.t option *
      ((Eliom_common.anon_params_type * Eliom_common.anon_params_type) *
       (int ref option * (float * float ref) option *
        (bool -> Eliom_common.server_params -> result Lwt.t))) list ->
      t ->
      t

    val find :
      Eliom_common.page_table_key ->
      t ->
      Node.t option *
      ((Eliom_common.anon_params_type * Eliom_common.anon_params_type) *
       (int ref option * (float * float ref) option *
        (bool -> Eliom_common.server_params -> result Lwt.t))) list

    val remove : Eliom_common.page_table_key -> t -> t

  end

  type tables

  val set_contains_timeout : tables -> bool -> unit

  val service_dlist_add :
    ?sp:Eliom_common.server_params ->
    tables ->
    (Table.t ref * Eliom_common.page_table_key,
     Eliom_common.na_key_serv) Eliom_lib.leftright ->
    Node.t

  val tables_services :
    tables ->
    (int * int * Table.t Eliom_common.dircontent ref) list

  val set_tables_services :
    tables ->
    (int * int * Table.t Eliom_common.dircontent ref) list ->
    unit

  val get_number_of_reloads : unit -> int

end

module Make (P : PARAM) = struct

  let find_page_table
      nosuffixversion
      now
      (pagetableref : P.Table.t ref)
      fullsessname
      (site_data : P.site_data)
      (info : P.info)
      (urlsuffix : _ option)
      k : P.result Lwt.t  =
    let sp = P.make_server_params site_data info urlsuffix fullsessname in
    Lwt.catch
      (fun () -> Lwt.return (P.Table.find k !pagetableref))
      (function
        | Not_found -> fail Eliom_common.Eliom_404
        | e -> fail e) >>= fun (node, l) ->
    let rec aux toremove = function
      | [] -> Lwt.return ((Eliom_common.Notfound
                             Eliom_common.Eliom_Wrong_parameter), [])
      | (((_anontyp, (max_use, expdate, funct)) as a)::l) ->
        match expdate with
        | Some (_, e) when !e < now ->
          (* Service expired. Removing it. *)
          Lwt_log.ign_info ~section "Service expired. Removing it";
          aux toremove l >>= fun (r, toremove) ->
          Lwt.return (r, a::toremove)
        | _ ->
          catch
            (fun () ->
               Lwt_log.ign_info ~section "Trying a service";
               funct nosuffixversion sp >>= fun p ->
               (* warning: the list ll may change during funct
                  if funct register something on the same URL!! *)
               Lwt_log.ign_info ~section "Page found and generated successfully";

               (* If this is an anonymous coservice,
                  we place it at the top of the dlist
                  (limitation of number of coservices) *)
               (match node with
                | None -> ()
                | Some node -> P.Node.up node);

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
               Lwt.return (Eliom_common.Found p, newtoremove))
            (function
              | Eliom_common.Eliom_Wrong_parameter ->
                aux toremove l >>=
                (fun (r, toremove) -> Lwt.return (r, toremove))
              | e -> Lwt.return ((Eliom_common.Notfound e), toremove))
    in
    aux [] l >>= fun (r, toremove) ->

    (match node, toremove with
     | _, [] -> ()
     | Some node, _ -> (* it is an anonymous coservice that has expired.
                          We remove it form the dlist.
                          This will do the removal from this table
                          automatically.
                          Note that in that case, toremove has length 1
                          (like the initial list l).
                       *)
       P.Node.remove node;
     | None, _ -> (* removing manually *)
       try
         let _, l   = P.Table.find k !pagetableref
         and newptr = P.Table.remove k !pagetableref in
         (* We do find once again because it may have changed! *)

         let newlist =
           List.fold_left
             (fun l a -> List.remove_first_if_any_q a l)
             (* physical equality! *)
             l
             toremove
         in
         pagetableref :=
           match newlist with
           | [] ->
             newptr
           | newlist ->
             P.Table.add k (None, newlist) newptr
       with Not_found -> ()
    );
    match r with
    | Eliom_common.Found r -> Lwt.return (r : P.result)
    | Eliom_common.Notfound e -> fail e

  let add_page_table tables url_act tref
      key (id, ((max_use, exp, action) as va)) =

    let sp = Eliom_common.get_sp_option () in

    (match exp with Some _ -> P.set_contains_timeout tables true | _ -> ());

    (* Duplicate registration forbidden in global table with same generation *)
    let v = (id, va) in
    match key with
    | { Eliom_common.key_state = Eliom_common.SAtt_anon _, _;
        key_meth = (`Get | `Post | `Put | `Delete) } ->
      (* Anonymous coservice:
         - only one for each key
         - we add a node in the dlist to limit their number *)
      (try
         let (nodeopt, l), newt =
           P.Table.find key !tref, P.Table.remove key !tref
         in
         (match nodeopt with
          | None -> () (* should not occure *)
          | Some node -> P.Node.up node);
         tref := P.Table.add key (nodeopt, [v]) newt
       with Not_found ->
         let node = P.service_dlist_add ?sp tables (Left (tref, key)) in
         tref := P.Table.add key (Some node, [v]) !tref)
    | { Eliom_common.key_state =
          Eliom_common.SAtt_no, Eliom_common.SAtt_no } ->
      (try
         let nodeopt, l = P.Table.find key !tref
         and newt = P.Table.remove key !tref in
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
             (* We insert as last element so that services are tried in
                registration order *)
             tref := P.Table.add key (None, oldl @ [v]) newt
         with Not_found ->
           tref := P.Table.add key (None, l @ [v]) newt
       with Not_found ->
         tref := P.Table.add key (None, [v]) !tref)
    | _ ->
      try
        let nodeopt, l = P.Table.find key !tref
        and newt = P.Table.remove key !tref in
        let oldl = List.remove_assoc id l in
        (* if there was an old version with the same id, we remove it *)
        tref := P.Table.add key (None, oldl @ [v]) newt
      with Not_found ->
        tref := P.Table.add key (None, [v]) !tref

  let remove_page_table _ _ tref key id =
    (* Actually this does not remove empty directories.
       But this will be done by the next service GC *)

    let nodeopt, l = P.Table.find key !tref in
    match nodeopt with
    | Some node ->
      (* In that case, l has size 1, and the id is correct,
         because it is an anonymous coservice *)
      (*VVV the key is searched twice *)
      P.Node.remove node
    | None ->
      let newt = P.Table.remove key !tref in
      match List.remove_all_assoc id l with
      | [] -> tref := newt
      (* In that case, we must remove it, otherwise we get
         "Wrong parameters" instead of "404 Not found" *)
      | newl ->
        tref := P.Table.add key (None, newl) newt

  let add_dircontent dc (key, (elt : P.Table.t Eliom_common.direlt ref)) =
    match dc with
    | Eliom_common.Vide ->
      Eliom_common.Table (String.Table.add key elt String.Table.empty)
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
            | Eliom_common.File ptr -> ptr)
         with
         | Not_found ->
           let newpagetableref = ref (P.Table.empty ()) in
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
    let generation = P.get_number_of_reloads () in
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
    let table, new_table_services = find_table (P.tables_services tables) in
    P.set_tables_services tables new_table_services;
    add_or_remove_service add_page_table tables table url_act page_table_key va

  let remove_service tables path k unique_id =
    let rec aux = function
      | [] -> ()
      | (_, _, table)::l ->
        try
          add_or_remove_service remove_page_table tables table path k unique_id
        with Not_found -> aux l
    in
    aux (P.tables_services tables)


  exception Exn1

  let find_service
      now
      tables
      fullsessname
      sitedata
      info : P.result Lwt.t =

    let rec search_page_table dircontent : _ -> P.result Lwt.t =
      let find nosuffixversion page_table_ref suffix =
        let si = P.sess_info_of_info info in
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
           Eliom_common.key_meth = P.meth_of_info info}
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
          P.handle_directory info
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
           (P.tables_services tables)
           (Url.change_empty_list
              (P.subpath_of_info info)))
      (function Exn1 -> Lwt.fail Eliom_common.Eliom_404 | e -> Lwt.fail e)

end
