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

open Lwt.Syntax
open Lib

let make_full_named_group_name_ ~cookie_level sitedata g =
  { Common.sg_site_dir = Common.get_site_dir_string sitedata
  ; sg_level = cookie_level
  ; sg_group = Common.Group_name g }

let make_full_group_name ~cookie_level ~sitedata = function
  (* The scope is the scope of group members (`Session by default). *)
  | None ->
      { Common.sg_site_dir = Common.get_site_dir_string sitedata
      ; sg_level = cookie_level
      ; sg_group = Common.No_group }
  | Some g -> make_full_named_group_name_ ~cookie_level sitedata g

let make_persistent_full_group_name = Common.make_persistent_full_group_name
let getperssessgrp = Common.getperssessgrp

module type MEMTAB = sig
  type group_of_group_data

  val add :
     Common.sitedata
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
     Common.sitedata
    -> string Ocsigen_base.Cache.Dlist.node
    -> Common.cookie_level Common.sessgrp
    -> string Ocsigen_base.Cache.Dlist.node

  val up : string Ocsigen_base.Cache.Dlist.node -> unit

  val up_group : Common.cookie_level Common.sessgrp -> unit
  (** [up_group g] makes [g] the group of the site used most recently, if
      it is one. *)

  val nb_of_groups : unit -> int
  val group_size : Common.cookie_level Common.sessgrp -> int
  val set_max : 'a Ocsigen_base.Cache.Dlist.node -> int -> unit

  val set_group_max : Common.cookie_level Common.sessgrp -> int -> unit
  (** [set_group_max g n] sets the maximum size of the group [g], if it
      exists. *)
end

module GroupTable = Hashtbl.Make (struct
    type t = Common.cookie_level Common.sessgrp

    let equal = ( = )
    let hash = Hashtbl.hash
  end)

module Make (A : sig
    type group_of_group_data

    val kind : [`Service | `Data]
    (** The kind of the sessions of the groups *)

    val table :
      (group_of_group_data option * string Ocsigen_base.Cache.Dlist.t)
        GroupTable.t

    val close_session : Common.sitedata -> string -> unit
    val max_tab_per_session : Common.sitedata -> int
    val max_session_per_group : Common.sitedata -> int
    val max_sessions_without_group : Common.sitedata -> int

    val clean_session :
       Common.sitedata
      -> GroupTable.key
      -> (GroupTable.key -> group_of_group_data option)
      -> (string Ocsigen_base.Cache.Dlist.node -> unit)
      -> (group_of_group_data -> unit)
      -> unit

    val node_of_group_of_group_data :
       group_of_group_data
      -> Common.group_of_groups_entry Ocsigen_base.Cache.Dlist.node

    val create_group_of_group_data :
       Common.sitedata
      -> Common.group_of_groups_entry Ocsigen_base.Cache.Dlist.node
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

  let get_cl sitedata sess_grp =
    try find sess_grp
    with Not_found ->
      (* We create a group *)
      let size =
        match sess_grp with
        | {Common.sg_level = `Session; sg_group = Common.Group_name _; _} ->
            A.max_session_per_group sitedata
        | {Common.sg_level = `Client_process; sg_group = Common.Group_name _; _}
          ->
            A.max_tab_per_session sitedata
        | {Common.sg_level = `Session; sg_group = Common.No_group; _} ->
            A.max_sessions_without_group sitedata
        | _ -> assert false
      in
      let cookie_level = sess_grp.Common.sg_level in
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
        match sess_grp with
        | {Common.sg_level = `Session; sg_group = Common.Group_name gg_name; _}
          ->
            ignore
              (Ocsigen_base.Cache.Dlist.add
                 {Common.gg_kind = A.kind; gg_name}
                 sitedata.Common.group_of_groups);
            Ocsigen_base.Cache.Dlist.newest sitedata.Common.group_of_groups
        | _ ->
            (* Groups of tab sessions are not groups of the site, nor is
               the automatic group of the sessions without a group: closing
               it as the oldest group of the site would close all these
               sessions at once, and the session-group scope of such a
               session is the session itself. *)
            None
      in
      let group_of_group_data =
        Option.map
          (A.create_group_of_group_data sitedata)
          node_in_group_of_group
      in
      GroupTable.add grouptable sess_grp (group_of_group_data, cl);
      cl

  let add sitedata sess_id sess_grp =
    let cl = get_cl sitedata sess_grp in
    ignore (Ocsigen_base.Cache.Dlist.add sess_id cl);
    match Ocsigen_base.Cache.Dlist.newest cl with
    | Some v -> v
    | None -> assert false

  let up node = Ocsigen_base.Cache.Dlist.up node

  let up_group sess_grp =
    match find_node_in_group_of_groups sess_grp with
    | Some d -> Ocsigen_base.Cache.Dlist.up (A.node_of_group_of_group_data d)
    | None -> ()

  let move sitedata node sess_grp =
    let cl = get_cl sitedata sess_grp in
    ignore (Ocsigen_base.Cache.Dlist.move node cl);
    match Ocsigen_base.Cache.Dlist.newest cl with
    | Some v -> v
    | None -> assert false

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

  let set_group_max sess_grp i =
    match find sess_grp with
    | cl -> ignore (Ocsigen_base.Cache.Dlist.set_maxsize cl i)
    | exception Not_found -> ()
end

module Data = Make (struct
    let kind = `Data

    type group_of_group_data =
      Common.group_of_groups_entry Ocsigen_base.Cache.Dlist.node

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
      sitedata.Common.max_volatile_data_tab_sessions_per_group.Common.cf_value

    let max_session_per_group sitedata =
      sitedata.Common.max_volatile_data_sessions_per_group.Common.cf_value

    let max_sessions_without_group sitedata =
      sitedata.Common.max_volatile_data_sessions_without_group.Common.cf_value

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
      | { Common.sg_level = `Client_process
        ; sg_group = Common.Group_name sess_id
        ; _ } -> (
        try
          let {Common.Data_cookie.session_group; session_group_node; _} =
            Common.SessionCookies.find sitedata.Common.session_data sess_id
          in
          match !session_group with
          | {Common.sg_level = `Session; sg_group = Common.No_group; _}
          (* no group *)
            when sitedata.Common.not_bound_in_data_tables sess_id ->
              remove1 session_group_node
          | _ -> ()
        with Not_found -> ())
      | {Common.sg_level = `Session; _} -> (
        match find_node_in_group_of_groups sess_grp with
        | Some node -> remove2 node
        | None -> ())
      | _ -> ()

    let node_of_group_of_group_data x = x
    let create_group_of_group_data _ x = x
  end)

module Serv = Make (struct
    let kind = `Service

    type group_of_group_data =
      Common.tables ref
      * Common.group_of_groups_entry Ocsigen_base.Cache.Dlist.node

    let table :
      (group_of_group_data option * string Ocsigen_base.Cache.Dlist.t)
        GroupTable.t
      =
      GroupTable.create 100

    let close_session sitedata sess_id =
      Common.SessionCookies.remove sitedata.Common.session_services sess_id

    let max_tab_per_session sitedata =
      sitedata.Common.max_service_tab_sessions_per_group.Common.cf_value

    let max_session_per_group sitedata =
      sitedata.Common.max_service_sessions_per_group.Common.cf_value

    let max_sessions_without_group sitedata =
      sitedata.Common.max_service_sessions_without_group.Common.cf_value

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
         - For tab sessions, we close the browser session if it has no
         service of its own (we do not use it) and is not in a group,
         as for data sessions: in a group, it may keep the services of
         the group.
      *)
      (*VVV See also Mod_gc. *)
      (*VVV remove is not polymorphic enough -> remove1 remove2 *)
      match (sess_grp : GroupTable.key) with
      | { Common.sg_level = `Client_process
        ; sg_group = Common.Group_name sess_id
        ; _ } -> (
        try
          let { Common.Service_cookie.session_group
              ; session_table = tables
              ; session_group_node
              ; _ }
            =
            Common.SessionCookies.find sitedata.Common.session_services sess_id
          in
          match !session_group with
          | {Common.sg_level = `Session; sg_group = Common.No_group; _}
            when Common.service_tables_are_empty tables ->
              remove1 session_group_node
          | _ -> ()
        with Not_found -> ())
      | {Common.sg_level = `Session; _} -> (
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

  (* The sessions of a group are listed in one entry of the table, that the
     operations below read, change and write back. The operations on a group
     are serialised, otherwise two simultaneous requests (two logins of the
     same user, or two requests of sessions of the group, which move them up
     in the list) lose each other's change, and a session dropped from the
     list escapes the limit of the group and its closing. The lock is held
     by one server process: a database shared by several processes needs the
     same guarantee from its backend. *)
  let locks : (string, Lwt_mutex.t) Hashtbl.t = Hashtbl.create 64

  let with_group_lock sg f =
    let m =
      match Hashtbl.find_opt locks sg with
      | Some m -> m
      | None ->
          let m = Lwt_mutex.create () in
          Hashtbl.add locks sg m; m
    in
    Lwt.finalize
      (fun () -> Lwt_mutex.with_lock m f)
      (fun () ->
         (* Last user of the lock: nobody waits for it, nobody took it when
            it was released, and the entry is still this lock. *)
         (match Hashtbl.find_opt locks sg with
         | Some m'
           when m' == m && Lwt_mutex.is_empty m && not (Lwt_mutex.is_locked m)
           ->
             Hashtbl.remove locks sg
         | _ -> ());
         Lwt.return_unit)

  let find g =
    match g with
    | None -> Lwt.return_nil
    | Some g ->
        Lwt.catch
          (fun () ->
             let* _, a = Grouptable.find (Common.string_of_perssessgrp g) in
             Lwt.return a)
          (function Not_found -> Lwt.return_nil | e -> Lwt.fail e)

  let add ?set_max defaultmax sess_id sess_grp =
    match sess_grp with
    | Some sg ->
        let sg = Common.string_of_perssessgrp sg in
        with_group_lock sg @@ fun () ->
        Lwt.catch
          (fun () ->
             let* max2, cl = Grouptable.find sg in
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
             let* () =
               Grouptable.replace_if_exists sg (newmax, sess_id :: cl)
             in
             Lwt.return toclose)
          (function
            | Not_found ->
                let max =
                  match set_max with
                  | None -> Default
                  | Some None -> Nolimit
                  | Some (Some v) -> Val v
                in
                let* () = Grouptable.add sg (max, [sess_id]) in
                Lwt.return_nil
            | e -> Lwt.fail e)
    | None -> Lwt.return_nil

  (* [members sg] is the list of the sessions of the group [sg]: read with
     the lock, unless a change of the list does not depend on it (the fast
     path of [up], which only reads). [forget sg closed] removes the
     sessions [closed] from the group, and the group when no session is
     left. *)
  let members sg =
    Lwt.catch
      (fun () ->
         let+ _, cl = Grouptable.find sg in
         cl)
      (function Not_found -> Lwt.return_nil | e -> Lwt.fail e)

  let forget sg closed =
    with_group_lock sg @@ fun () ->
    Lwt.catch
      (fun () ->
         let* max, cl = Grouptable.find sg in
         match List.filter (fun s -> not (List.mem s closed)) cl with
         | [] -> Grouptable.remove sg
         | cl -> Grouptable.replace_if_exists sg (max, cl))
      (function Not_found -> Lwt.return_unit | e -> Lwt.fail e)

  let rec remove_group ~cookie_level sitedata sess_grp =
    (* cookie_level is the scope of group members *)
    match sess_grp with
    | None -> Lwt.return_unit
    | Some sg ->
        let key = Common.string_of_perssessgrp sg in
        Lwt.catch
          (fun () ->
             (* First the list of the sessions, and the removal of the group
                data, under the lock of the group: a session that joins the
                group meanwhile waits, then finds a group without data, as a
                new one. *)
             let* cl =
               with_group_lock key @@ fun () ->
               let* cl = members key in
               let+ () =
                 let group_name = Common.name_of_perssessgrp sg in
                 let* () =
                   Common.Persistent_tables.remove_key_from_all_tables
                     group_name
                 in
                 (* If it is associated to a session,
                    we remove the session from its group,
                    and we remove cookie info: *)
                 match cookie_level with
                 | `Client_process grp ->
                     (* We are closing a browser session,
                        belonging to the group grp *)
                     (* group_name is the cookie value *)
                     remove sitedata group_name grp
                 | `Session -> Lwt.return_unit
               in
               cl
             in
             (* Then the sessions are closed, without the lock: closing a
                browser session takes the lock of its group of tab sessions,
                then the one of its session group, while this function may
                be closing that session group. A request of a session of the
                group running meanwhile may write group data again: they
                remain, as the group stays for the sessions that join it. *)
             let close =
               close_persistent_session
                 ~cookie_level:
                   (match cookie_level with
                   | `Client_process _ -> `Client_process
                   | `Session -> `Session)
                 sitedata None
               (* None because the group is closed *)
             in
             let* results =
               Lwt_list.map_p
                 (fun s ->
                    Lwt.catch
                      (fun () ->
                         let+ () = close s in
                         Ok s)
                      (fun e -> Lwt.return (Error e)))
                 cl
             in
             (* Last, the closed sessions leave the list. A session that
                could not be closed stays listed, and a session that joined
                the group meanwhile stays, with the group. *)
             let* () =
               forget key
                 (List.filter_map
                    (function Ok s -> Some s | Error _ -> None)
                    results)
             in
             (* The first failure is raised, the other ones are logged *)
             match
               List.filter_map
                 (function Error e -> Some e | Ok _ -> None)
                 results
             with
             | [] -> Lwt.return_unit
             | e :: others ->
                 List.iter
                   (fun e ->
                      Logs.err ~src:eliom_logs_src (fun fmt ->
                        fmt "Closing a persistent session of a group: %s"
                          (Printexc.to_string e)))
                   others;
                 Lwt.fail e)
          (function Not_found -> Lwt.return_unit | e -> Lwt.fail e)

  (* close a persistent session (tab or browser)
     and the associated group (if browser session) by cookie value *)
  and close_persistent_session ~cookie_level sitedata fullsessgrp cookie =
    (*VVV Check this carefully!!!! *)
    (*VVV Optimize the number of marshal/unmarshal (getperssessgrp) *)
    Lwt.catch
      (fun () ->
         match cookie_level with
         | `Client_process ->
             (* We remove the session from its group: *)
             let* () = remove sitedata cookie fullsessgrp in
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
        with_group_lock sg @@ fun () ->
        Lwt.catch
          (fun () ->
             let* max, cl = Grouptable.find sg in
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
    | Some sg -> (
        let sg = Common.string_of_perssessgrp sg in
        let* cl = members sg in
        match cl with
        | first :: _ when first = sess_id ->
            (* Already the most recent one: the usual case, which needs
               neither the lock nor a write. *)
            Lwt.return_unit
        | _ ->
            with_group_lock sg @@ fun () ->
            Lwt.catch
              (fun () ->
                 let* max, cl = Grouptable.find sg in
                 match cl with
                 | first :: _ when first = sess_id -> Lwt.return_unit
                 | _ when List.mem sess_id cl ->
                     Grouptable.replace_if_exists sg
                       (max, sess_id :: List.remove_first_if_any sess_id cl)
                 | _ ->
                     (* The session has left the group: closed by the limit
                        of the group while its data are being removed by
                        another request. It does not come back. *)
                     Lwt.return_unit)
              (function Not_found -> Lwt.return_unit | e -> Lwt.fail e))

  let move sitedata ?set_max max sess_id grp1 grp2 =
    if set_max <> None || grp1 <> grp2
    then
      let* () = remove sitedata sess_id grp1 in
      add ?set_max max sess_id grp2
    else Lwt.return_nil

  let nb_of_groups () = Grouptable.length ()
end
