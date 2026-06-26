(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomsessiongroups.ml
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

let make_full_named_group_name_ ~cookie_level sitedata g =
  Common.get_site_dir_string sitedata, cookie_level, Left g

let make_full_group_name ~cookie_level ri site_dir_string ipv4mask ipv6mask
  = function
  (* The scope is the scope of group members (`Session by default). *)
  | None ->
      ( site_dir_string
      , cookie_level
      , Right (Common.network_of_request ri ~mask4:ipv4mask ~mask6:ipv6mask) )
  | Some g -> site_dir_string, cookie_level, Left g

let make_persistent_full_group_name = Common.make_persistent_full_group_name
let getsessgrp a = a
let getperssessgrp = Common.getperssessgrp

module type MEMTAB = sig
  type group_of_group_data

  val add :
     ?set_max:int
    -> Common.sitedata
    -> string
    -> Common.cookie_level Common.sessgrp
    -> string Ocsigen_base.Cache.Dlist.node

  val remove : 'a Ocsigen_base.Cache.Dlist.node -> unit
  val remove_group : Common.cookie_level Common.sessgrp -> unit

  val find :
     [< Common.cookie_level] Common.sessgrp
    -> string Ocsigen_base.Cache.Dlist.t
  (** returns the dlist containing all session group elements *)

  val find_node_in_group_of_groups :
     Common.cookie_level Common.sessgrp
    -> group_of_group_data option
  (** Groups of browser sessions belongs to a group of groups.
        As these groups are not associated to a cookie,
        we put this information here. *)

  val move :
     ?set_max:int
    -> Common.sitedata
    -> string Ocsigen_base.Cache.Dlist.node
    -> Common.cookie_level Common.sessgrp
    -> string Ocsigen_base.Cache.Dlist.node

  val up : string Ocsigen_base.Cache.Dlist.node -> unit
  val nb_of_groups : unit -> int
  val group_size : Common.cookie_level Common.sessgrp -> int
  val set_max : 'a Ocsigen_base.Cache.Dlist.node -> int -> unit
end

module GroupTable = Hashtbl.Make (struct
    type t = Common.cookie_level Common.sessgrp

    let equal = ( = )
    let hash = Hashtbl.hash
  end)

module Make (A : sig
    type group_of_group_data

    val table :
      (group_of_group_data option * string Ocsigen_base.Cache.Dlist.t)
        GroupTable.t

    val close_session : Common.sitedata -> string -> unit
    val max_tab_per_session : Common.sitedata -> int
    val max_session_per_group : Common.sitedata -> int
    val max_session_per_ip : Common.sitedata -> int

    val clean_session :
       Common.sitedata
      -> GroupTable.key
      -> (GroupTable.key -> group_of_group_data option)
      -> (string Ocsigen_base.Cache.Dlist.node -> unit)
      -> (group_of_group_data -> unit)
      -> unit

    val node_of_group_of_group_data :
       group_of_group_data
      -> [`Session] Common.sessgrp Ocsigen_base.Cache.Dlist.node

    val create_group_of_group_data :
       Common.sitedata
      -> [`Session] Common.sessgrp Ocsigen_base.Cache.Dlist.node
      -> group_of_group_data
  end) : MEMTAB with type group_of_group_data = A.group_of_group_data = struct
  type group_of_group_data = A.group_of_group_data

  let grouptable = A.table
  let find g = snd (GroupTable.find grouptable g)

  let find_node_in_group_of_groups g =
    try fst (GroupTable.find grouptable g) with Not_found -> None

  let remove node = Ocsigen_base.Cache.Dlist.remove node

  let remove_group sess_grp =
    try
      let cl = find sess_grp in
      let rec close_all cl =
        match Ocsigen_base.Cache.Dlist.oldest cl with
        | Some node ->
            Ocsigen_base.Cache.Dlist.remove node;
            close_all cl
        | None -> ()
      in
      close_all cl
      (* will remove the group using finaliser *)
    with Not_found -> ()

  let remove_if_empty sitedata sess_grp cl =
    if Ocsigen_base.Cache.Dlist.size cl = 0 (* finaliser after *)
    then (
      A.clean_session sitedata sess_grp find_node_in_group_of_groups remove
        (fun n -> remove (A.node_of_group_of_group_data n));
      GroupTable.remove grouptable sess_grp)

  let get_cl ?set_max sitedata sess_grp =
    try
      let cl = find sess_grp in
      (match set_max with
      | None -> ()
      | Some v -> ignore (Ocsigen_base.Cache.Dlist.set_maxsize cl v));
      cl
    with Not_found ->
      (* We create a group *)
      let size =
        match set_max, sess_grp with
        | None, (_, `Session, Left _) -> A.max_session_per_group sitedata
        | None, (_, `Client_process, Left _) -> A.max_tab_per_session sitedata
        | None, (_, `Session, Right _) -> A.max_session_per_ip sitedata
        | None, _ -> assert false
        | Some v, _ -> v
      in
      let cookie_level = Tuple3.snd sess_grp in
      let cl = Ocsigen_base.Cache.Dlist.create size in
      Ocsigen_base.Cache.Dlist.set_finaliser_after
        (fun node ->
           (* Finaliser of sessions and client processes *)
           let name = Ocsigen_base.Cache.Dlist.value node in
           (* First we close all subsessions
             (that is, all sessions in the group associated to the session) *)
           (match cookie_level with
           (*            | `Session_group -> assert false
              As there is no table of groups of groups
              (only one group of groups for each site),
              the finaliser for these groups is created in eliommod.ml *)
           | `Session (* We are closing a browser session *) ->
               (* First we close all tab sessions in the session (subgrp): *)
               let subgrp =
                 make_full_named_group_name_ ~cookie_level:`Client_process
                   sitedata name
               in
               remove_group subgrp
           | `Client_process (* We are closing a tab session *) -> ());
           (* Then we close all session tables: *)
           A.close_session sitedata name;
           (* If the dlist is empty, we remove it from the group table
             (and possibly close the group itself): *)
           remove_if_empty sitedata sess_grp cl)
        cl;
      let node_in_group_of_group =
        match cookie_level with
        | `Session ->
            ignore
              (Ocsigen_base.Cache.Dlist.add sess_grp
                 sitedata.Common.group_of_groups);
            Ocsigen_base.Cache.Dlist.newest sitedata.Common.group_of_groups
        | _ -> None
      in
      let group_of_group_data =
        Option.map
          (A.create_group_of_group_data sitedata)
          node_in_group_of_group
      in
      GroupTable.add grouptable sess_grp (group_of_group_data, cl);
      cl

  let add ?set_max sitedata sess_id sess_grp =
    let cl = get_cl ?set_max sitedata sess_grp in
    ignore (Ocsigen_base.Cache.Dlist.add sess_id cl);
    match Ocsigen_base.Cache.Dlist.newest cl with
    | Some v -> v
    | None -> assert false

  let up node = Ocsigen_base.Cache.Dlist.up node

  let move ?set_max sitedata node sess_grp =
    (*    if set_max <> None || grp1 <> grp2 then begin *)
    let cl = get_cl ?set_max sitedata sess_grp in
    ignore (Ocsigen_base.Cache.Dlist.move node cl);
    match Ocsigen_base.Cache.Dlist.newest cl with
    | Some v -> v
    | None -> assert false
  (*    end
    else [] *)

  let nb_of_groups () = GroupTable.length grouptable

  let group_size sess_grp =
    try
      let cl = find sess_grp in
      Ocsigen_base.Cache.Dlist.size cl
    with Not_found -> 0

  let set_max node i =
    match Ocsigen_base.Cache.Dlist.list_of node with
    | None -> ()
    | Some cl -> ignore (Ocsigen_base.Cache.Dlist.set_maxsize cl i)
end

module Data = Make (struct
    type group_of_group_data =
      [`Session] Common.sessgrp Ocsigen_base.Cache.Dlist.node

    let table :
      (group_of_group_data option * string Ocsigen_base.Cache.Dlist.t)
        GroupTable.t
      =
      (* The table associates the dlist for a group
         to a full session group name.
         It work both for groups of tab sessions and
         groups of browser sessions.
         For groups of groups, we do not need that table,
         as there is only one group of groups for each site
         (the dlist is found in sitedata).
         The dlist is automatically removed from the table
         when it becomes empty, using the finaliser of nodes.
         In the case of groups of browser sessions,
         the session group is also associated to a node
         which corresponds to the node of that group in the group
         of groups (one group of groups for each site).
      *)
      GroupTable.create 100

    let close_session sitedata sess_id =
      Common.SessionCookies.remove sitedata.Common.session_data sess_id;
      (* iterate on all session data tables: *)
      sitedata.Common.remove_session_data sess_id
    (* see also in eliommod.ml if you modify this *)

    let max_tab_per_session sitedata =
      fst sitedata.Common.max_volatile_data_tab_sessions_per_group

    let max_session_per_group sitedata =
      fst sitedata.Common.max_volatile_data_sessions_per_group

    let max_session_per_ip sitedata =
      fst sitedata.Common.max_volatile_data_sessions_per_subnet

    let clean_session
          sitedata
          sess_grp
          find_node_in_group_of_groups
          remove1
          remove2
      =
      (* We removed the last session from a group.
         Do we want to close the group completely?
         - For volatile browser sessions, yes.
         We do not keep group data when there is no session in the group.
         We remove the group of groups from the site dlist.
-- Vincent 2011/08: This is not coherent with persistent group data!
But as I do not see any correct use of volatile group data for now.
And there is a risk of memory leak if we keep them.
Besides, volatile sessions are (hopefully) going to disappear soon.
         - For tab sessions, yes if the browser cookie is not
         bound in tables and is not in a group (like in Mod_gc)
         (means that we do not use the browser session).
      *)
      (*VVV See also in Mod_gc and
  State.close_volatile_session_if_empty.
  Should we use this function here?
      *)
      (*VVV remove is not polymorphic enough -> remove1 remove2 *)
      match (sess_grp : GroupTable.key) with
      | _, `Client_process, Left sess_id -> (
        try
          let {Common.Data_cookie.session_group; session_group_node; _} =
            Common.SessionCookies.find sitedata.Common.session_data sess_id
          in
          match !session_group with
          | _, `Session, Right _
          (* no group *)
            when sitedata.Common.not_bound_in_data_tables sess_id ->
              remove1 session_group_node
          | _ -> ()
        with Not_found -> ())
      | _, `Session, _ -> (
        match find_node_in_group_of_groups sess_grp with
        | Some node -> remove2 node
        | None -> ())
      | _ -> ()

    let node_of_group_of_group_data x = x
    let create_group_of_group_data _ x = x
  end)

module Serv = Make (struct
    type group_of_group_data =
      Common.tables ref
      * [`Session] Common.sessgrp Ocsigen_base.Cache.Dlist.node

    let table :
      (group_of_group_data option * string Ocsigen_base.Cache.Dlist.t)
        GroupTable.t
      =
      GroupTable.create 100

    let close_session sitedata sess_id =
      Common.SessionCookies.remove sitedata.Common.session_services sess_id

    let max_tab_per_session sitedata =
      fst sitedata.Common.max_service_tab_sessions_per_group

    let max_session_per_group sitedata =
      fst sitedata.Common.max_service_sessions_per_group

    let max_session_per_ip sitedata =
      fst sitedata.Common.max_service_sessions_per_subnet

    let clean_session
          sitedata
          sess_grp
          find_node_in_group_of_groups
          remove1
          remove2
      =
      (* We removed the last session from a group.
         Do we want to close the group completely?
         - For volatile browser sessions, yes.
         We do not keep group data when there is no session in the group.
         We remove the group of groups from the site dlist.
-- Vincent 2011/08: This is not coherent with persistent group data!
But as I do not see any correct use of volatile group data for now.
And there is a risk of memory leak if we keep them.
Besides, volatile sessions are (hopefully) going to disappear soon.
         - For tab sessions, yes if there are no session services
         in the browser service table.
         (means that we do not use the browser session).
      *)
      (*VVV We close even if browser session is in a group.
  It is not coherent with data sessions. *)
      (*VVV See also in Mod_gc and
  State.close_service_session_if_empty.
  Should we use this function here?
      *)
      (*VVV remove is not polymorphic enough -> remove1 remove2 *)
      match (sess_grp : GroupTable.key) with
      | _, `Client_process, Left sess_id -> (
        try
          let { Common.Service_cookie.session_table = tables
              ; session_group_node
              ; _ }
            =
            Common.SessionCookies.find sitedata.Common.session_services sess_id
          in
          if Common.service_tables_are_empty tables
          then remove1 session_group_node
        with Not_found -> ())
      | _, `Session, _ -> (
        match find_node_in_group_of_groups sess_grp with
        | Some node -> remove2 node
        | None -> ())
      | _ -> ()

    let node_of_group_of_group_data = snd

    let create_group_of_group_data sitedata x =
      ref (Common.new_service_session_tables sitedata), x
    (*VVV Check when the table is collected *)
  end)

[@@@warning "-39"]

type nbmax = Val of int | Default | Nolimit [@@deriving json]

[@@@warning "+39"]

let cut n l =
  let rec aux n = function
    | [] -> [], []
    | l when n <= 1 -> [], l
    | a :: l ->
        let l1, l2 = aux (n - 1) l in
        a :: l1, l2
  in
  match n with None -> l, [] (* no limitation *) | Some n -> aux n l

module Pers = struct
  (*VVV Verify this carefully! *)
  (*VVV VERIFY concurrent access *)

  module Ocsipersist = Common.Ocsipersist.Functorial

  (* v2 switches from Marshal to Deriving_Json *)
  module Grouptable =
    Ocsipersist.Table
      (struct
        let name = "__eliom_session_group_table_v2"
      end)
      (Ocsipersist.Column.String)
      (Ocsipersist.Column.Json (struct
           type t = nbmax * string list

           let t = [%json: nbmax * string list]
         end))

  let () = Common.Persistent_tables.add_functorial_table (module Grouptable)

  let find g =
    match g with
    | None -> Lwt.return_nil
    | Some g ->
        Lwt.catch
          (fun () ->
             Grouptable.find (Common.string_of_perssessgrp g) >>= fun (_, a) ->
             Lwt.return a)
          (function Not_found -> Lwt.return_nil | e -> Lwt.fail e)

  let add ?set_max defaultmax sess_id sess_grp =
    match sess_grp with
    | Some sg ->
        let sg = Common.string_of_perssessgrp sg in
        Lwt.catch
          (fun () ->
             Grouptable.find sg >>= fun (max2, cl) ->
             let max, newmax =
               match set_max with
               | None ->
                   ( (match max2 with
                     | Default -> defaultmax
                     | Nolimit -> None
                     | Val m -> Some m)
                   , max2 )
               | Some None -> None, Nolimit
               | Some (Some v) -> Some v, Val v
             in
             let cl, toclose = cut max cl in
             Grouptable.replace_if_exists sg (newmax, sess_id :: cl)
             >>= fun () -> Lwt.return toclose)
          (function
            | Not_found ->
                let max =
                  match set_max with
                  | None -> Default
                  | Some None -> Nolimit
                  | Some (Some v) -> Val v
                in
                Grouptable.add sg (max, [sess_id]) >>= fun () -> Lwt.return_nil
            | e -> Lwt.fail e)
    | None -> Lwt.return_nil

  let rec remove_group ~cookie_level sitedata sess_grp =
    (* cookie_level is the scope of group members *)
    (*VVV NEW 201007 closing all sessions in the group and removing group data *)
    (*VVV VERIFY concurrent access *)
    (*VVV Check this carefully!!!! Verify the order of actions. *)
    Lwt.catch
      (fun () ->
         (* First we close all sessions in the group *)
         find sess_grp >>= fun cl ->
         Lwt_list.iter_p
           (close_persistent_session2
              ~cookie_level:
                (match cookie_level with
                | `Client_process _ -> `Client_process
                | `Session -> `Session)
              sitedata None)
           cl
         (* None because we will close the group *)
         >>= fun () ->
         (* Then, we remove group data: *)
         (match sess_grp with
           | None -> Lwt.return_unit
           | Some sg -> (
             match Common.getperssessgrp sg with
             | _, _, Right _ ->
                 (* No group has been set. No group table.
                 Data associated to default (automatic) groups
                 is removed when closing associated sessions. *)
                 Lwt.return_unit
             | _, _, Left group_name -> (
                 Common.Persistent_tables.remove_key_from_all_tables group_name
                 >>= fun () ->
                 (* If it is associated to a session,
                 we remove the session from its group,
                 and we remove cookie info: *)
                 match cookie_level with
                 | `Client_process grp ->
                     (* We are closing a browser session,
                                           belonging to the group grp *)
                     (* group_name is the cookie value *)
                     remove sitedata group_name grp
                 | _ -> Lwt.return_unit)))
         >>= fun () ->
         (* Then, we remove group from group table: *)
         match sess_grp with
         | Some sg ->
             let sg = Common.string_of_perssessgrp sg in
             Grouptable.remove sg
         | None -> Lwt.return_unit)
      (function Not_found -> Lwt.return_unit | e -> Lwt.fail e)

  (* close a persistent session (tab or browser)
     and the associated group (if browser session) by cookie value *)
  and close_persistent_session2 ~cookie_level sitedata fullsessgrp cookie =
    (*VVV Check this carefully!!!! *)
    (*VVV Optimize the number of marshal/unmarshal (getperssessgrp) *)
    Lwt.catch
      (fun () ->
         match cookie_level with
         | `Client_process ->
             (* We remove the session from its group: *)
             remove sitedata cookie fullsessgrp >>= fun () ->
             (* Then, we remove session data: *)
             Common.Persistent_tables.remove_key_from_all_tables cookie
         | `Session ->
             remove_group ~cookie_level:(`Client_process fullsessgrp) sitedata
               (Common.make_persistent_full_group_name
                  ~cookie_level:`Client_process
                  (Common.get_site_dir_string sitedata)
                  (Some cookie)))
      (function Not_found -> Lwt.return_unit | e -> Lwt.fail e)

  and remove _sitedata sess_id sess_grp =
    match sess_grp with
    | Some sg0 ->
        let sg = Common.string_of_perssessgrp sg0 in
        Lwt.catch
          (fun () ->
             Grouptable.find sg >>= fun (max, cl) ->
             let newcl = List.remove_first_if_any sess_id cl in
             (* Before 2018-10-18, we were closing the session group
             when newcl was empty (no more session in the group).
             But persistent session groups are usually used to store persistent
             information about users. It makes no sense cleaning this
             information when user closes all their sessions.
             I remove this. -- Vincent
             *)
             Grouptable.replace_if_exists sg (max, newcl))
          (function Not_found -> Lwt.return_unit | e -> Lwt.fail e)
    | None -> Lwt.return_unit

  let up sess_id grp =
    match grp with
    | None -> Lwt.return_unit
    | Some sg ->
        let sg = Common.string_of_perssessgrp sg in
        Lwt.catch
          (fun () ->
             Grouptable.find sg >>= fun (max, cl) ->
             let newcl = List.remove_first_if_any sess_id cl in
             Grouptable.replace_if_exists sg (max, sess_id :: newcl))
          (function Not_found -> Lwt.return_unit | e -> Lwt.fail e)

  let move sitedata ?set_max max sess_id grp1 grp2 =
    if set_max <> None || grp1 <> grp2
    then
      remove sitedata sess_id grp1 >>= fun () -> add ?set_max max sess_id grp2
    else Lwt.return_nil

  let nb_of_groups () = Grouptable.length ()
end
