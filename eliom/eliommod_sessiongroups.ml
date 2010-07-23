(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomsessiongroups.ml
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



let (>>=) = Lwt.bind
let (!!) = Lazy.force

let make_full_named_group_name_ ~level sitedata g =
  (sitedata.Eliom_common.site_dir_string, level, Ocsigen_lib.Left g)

let make_full_group_name ~level ri site_dir_string ipv4mask ipv6mask = function
  (* The level is the level of group members (`Browser by default). *)
  | None -> (site_dir_string,
             level,
             Ocsigen_lib.Right 
               (Ocsigen_lib.network_of_ip
                  (!!(ri.Ocsigen_extensions.ri_remote_ip_parsed))
                  ipv4mask
                  ipv6mask
               ))
  | Some g -> (site_dir_string, level, Ocsigen_lib.Left g)

let make_persistent_full_group_name ~level ri site_dir_string = function
  | None -> None
  | Some g ->
    Some (Marshal.to_string (site_dir_string, level, Ocsigen_lib.Left g) [])

let getsessgrp a = a
let getperssessgrp a = Marshal.from_string a 0

module type MEMTAB =
  sig
    val add : ?set_max: int -> Eliom_common.sitedata ->
      string -> Eliom_common.cookie_level Eliom_common.sessgrp ->
      string Ocsigen_cache.Dlist.node
    val remove : 'a Ocsigen_cache.Dlist.node -> unit
    val remove_group : Eliom_common.cookie_level Eliom_common.sessgrp -> unit

    (** Groups of browser sessions belongs to a group of groups.
        As these groups are not associated to a cookie,
        we put this information here. *)
    val find_node_in_group_of_groups : 
      Eliom_common.cookie_level Eliom_common.sessgrp ->
      [ `Browser ] Eliom_common.sessgrp Ocsigen_cache.Dlist.node option

    val move :
      ?set_max:int ->
      Eliom_common.sitedata ->
      string Ocsigen_cache.Dlist.node ->
      Eliom_common.cookie_level Eliom_common.sessgrp ->
      string Ocsigen_cache.Dlist.node

    val up : string Ocsigen_cache.Dlist.node -> unit
    val nb_of_groups : unit -> int
    val group_size : Eliom_common.cookie_level Eliom_common.sessgrp -> int
    val set_max : 'a Ocsigen_cache.Dlist.node -> int -> unit
  end

module GroupTable = Hashtbl.Make(struct
  type t = Eliom_common.cookie_level Eliom_common.sessgrp
  let equal = (=)
  let hash = Hashtbl.hash
end)

module Make(A: sig 
              val table : 
                (([ `Browser ] Eliom_common.sessgrp Ocsigen_cache.Dlist.node)
                    option * 
                    (string Ocsigen_cache.Dlist.t)) GroupTable.t
              val close_session : Eliom_common.sitedata -> string -> unit
              val max_tab_per_session : Eliom_common.sitedata -> int
              val max_session_per_group : Eliom_common.sitedata -> int
              val max_session_per_ip : Eliom_common.sitedata -> int
              val clean_session : Eliom_common.sitedata ->
                GroupTable.key ->
                (GroupTable.key -> 'b option) ->
                (string Ocsigen_cache.Dlist.node -> unit) ->
                ('b -> unit) -> unit
            end) : MEMTAB =
struct

  let grouptable = A.table

  let find g = snd (GroupTable.find grouptable g)

  let find_node_in_group_of_groups g =
    try
      fst (GroupTable.find grouptable g)
    with Not_found -> None

  let remove node =
    Ocsigen_cache.Dlist.remove node

  let remove_group sess_grp =
    try
      let cl = find sess_grp in
      let rec close_all cl =
        match Ocsigen_cache.Dlist.oldest cl with
          | Some node -> 
            Ocsigen_cache.Dlist.remove node;
            close_all cl
          | None -> ()
      in
      close_all cl (* will remove the group using finaliser *)
    with Not_found -> ()

  let remove_if_empty sitedata sess_grp cl =
    if Ocsigen_cache.Dlist.size cl = 0 (* finaliser after *)
    then begin
      A.clean_session sitedata sess_grp find_node_in_group_of_groups remove remove;
      GroupTable.remove grouptable sess_grp
    end

  let get_cl ?set_max sitedata sess_grp =
    try
      let cl = find sess_grp in
      (match set_max with
        | None -> ()
        | Some v -> ignore (Ocsigen_cache.Dlist.set_maxsize cl v));
      cl
    with Not_found ->
      (* We create a group *)
      let size = match set_max, sess_grp with
        | None, (_, `Browser, Ocsigen_lib.Left _) ->
          A.max_session_per_group sitedata
        | None, (_, `Tab, Ocsigen_lib.Left _) ->
          A.max_tab_per_session sitedata
        | None, (_, `Browser, Ocsigen_lib.Right _) ->
          A.max_session_per_ip sitedata
        | None, _ -> assert false
        | Some v, _ -> v
      in
      let level = Ocsigen_lib.snd3 sess_grp in
      let cl = Ocsigen_cache.Dlist.create size in
      Ocsigen_cache.Dlist.set_finaliser_after
        (fun node ->
          let name = Ocsigen_cache.Dlist.value node in
          (* First we close all subsessions
             (that is, all sessions in the group associated to the session) *)
          (match level with
            | `Group -> assert false
              (* As there is no table of groups of groups
                 (only one group of groups for each site),
                 the finaliser for these groups is created in eliommod.ml *)
            | `Browser (* We are closing to close a browser session *) ->
              (* First we close all tab sessions in the session (subgrp): *)
              let subgrp = make_full_named_group_name_ ~level sitedata name in
              remove_group subgrp
            | `Tab (* We are closing a browser session *) -> ());
          (* Then we close all session tables: *)
          A.close_session sitedata name;
          (* If the dlist is empty, we remove it from the group table
             (and possibly close the group itself): *)
          remove_if_empty sitedata sess_grp cl)
        cl;
      let node_in_group_of_group =
        match level with
          | `Browser ->
            ignore (Ocsigen_cache.Dlist.add
                      sess_grp sitedata.Eliom_common.group_of_groups);
            Ocsigen_cache.Dlist.newest sitedata.Eliom_common.group_of_groups
          | _ -> None
      in
      GroupTable.add grouptable sess_grp (node_in_group_of_group, cl);
      cl

  let add ?set_max sitedata sess_id sess_grp =
    let cl = get_cl ?set_max sitedata sess_grp in
    ignore (Ocsigen_cache.Dlist.add sess_id cl);
    match Ocsigen_cache.Dlist.newest cl with
      | Some v -> v
      | None -> assert false

  let up node =
    Ocsigen_cache.Dlist.up node

  let move ?set_max sitedata node sess_grp =
(*    if set_max <> None || grp1 <> grp2 then begin *)
    let cl = get_cl ?set_max sitedata sess_grp in
    ignore (Ocsigen_cache.Dlist.move node cl);
    match Ocsigen_cache.Dlist.newest cl with
      | Some v -> v
      | None -> assert false
(*    end
    else [] *)

  let nb_of_groups () = GroupTable.length grouptable

  let group_size sess_grp =
    try
      let cl = find sess_grp in
      Ocsigen_cache.Dlist.size cl
    with Not_found -> 0

  let set_max node i =
    match Ocsigen_cache.Dlist.list_of node with
      | None -> ()
      | Some cl -> ignore (Ocsigen_cache.Dlist.set_maxsize cl i)

end

module Data =
  Make (struct
    let table : (([ `Group ] Eliom_common.sessgrp Ocsigen_cache.Dlist.node)
                    option * 
                    (string Ocsigen_cache.Dlist.t)) GroupTable.t = 
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
      Eliom_common.SessionCookies.remove 
        sitedata.Eliom_common.session_data sess_id;
      (* iterate on all session data tables: *)
      sitedata.Eliom_common.remove_session_data sess_id 
    (* see also in eliommod.ml if you modify this *)

    let max_tab_per_session sitedata =
      fst sitedata.Eliom_common.max_volatile_data_tab_sessions_per_group
    let max_session_per_group sitedata =
      fst sitedata.Eliom_common.max_volatile_data_sessions_per_group
    let max_session_per_ip sitedata =
      fst sitedata.Eliom_common.max_volatile_data_sessions_per_subnet


    let clean_session sitedata sess_grp find_node_in_group_of_groups 
        remove1 remove2 =
      (* We removed the last session from a group.
         Do we want to close the group completely?
         - For browser sessions, yes. No need to keep group data
         when there is no session in the group.
         We remove the group of groups from the site dlist.
         - For tab sessions, yes if the browser cookie is not
         bound is tables and is not in a group (like in Eliommod_gc)
         (means that we do not use the browser session).
      *)
(*VVV See also in Eliommod_gc and 
  Eliom_sessions.close_volatile_session_if_empty.
  Should we use this function here?  
*)
(*VVV remove is not polymorphic enough -> remove1 remove2 *)
      match (sess_grp : GroupTable.key) with
        | (_, `Tab, Ocsigen_lib.Left sess_id) ->
          (try
             let (_, _, _, sgr, sgn) =
               Eliom_common.SessionCookies.find
                 sitedata.Eliom_common.session_data sess_id
             in
             (match !sgr with
               | (_, `Browser, Ocsigen_lib.Right _) (* no group *)
                   when sitedata.Eliom_common.not_bound_in_data_tables
                     sess_id
                     ->
                 remove1 sgn
               | _ -> ()
             )
           with Not_found -> ())
        | (_, `Browser, _) -> 
          (match find_node_in_group_of_groups sess_grp with
            | Some node -> remove2 node | None -> ())
        | _ -> ()

  end)


module Serv =
  Make (struct
    let table : (([ `Group ] Eliom_common.sessgrp Ocsigen_cache.Dlist.node)
                    option * 
                    (string Ocsigen_cache.Dlist.t)) GroupTable.t = 
      GroupTable.create 100
    let close_session sitedata sess_id =
      Eliom_common.SessionCookies.remove 
        sitedata.Eliom_common.session_services sess_id
    let max_tab_per_session sitedata =
      fst sitedata.Eliom_common.max_service_tab_sessions_per_group
    let max_session_per_group sitedata =
      fst sitedata.Eliom_common.max_service_sessions_per_group
    let max_session_per_ip sitedata =
      fst sitedata.Eliom_common.max_service_sessions_per_subnet


    let clean_session sitedata sess_grp find_node_in_group_of_groups 
        remove1 remove2 =
      (* We removed the last session from a group.
         Do we want to close the group completely?
         - For browser sessions, yes. No need to keep group data
         when there is no session in the group.
         We remove the group of groups from the site dlist.
         - For tab sessions, yes if there are no session services
         in the browser service table.
         (means that we do not use the browser session).
      *)
(*VVV We close even if browser session is in a group.
  It is not coherent with data sessions. *)
(*VVV See also in Eliommod_gc and 
  Eliom_sessions.close_service_session_if_empty.
  Should we use this function here?  
*)
(*VVV remove is not polymorphic enough -> remove1 remove2 *)
      match (sess_grp : GroupTable.key) with
        | (_, `Tab, Ocsigen_lib.Left sess_id) ->
          (try

             let (_, tables, _, _, sgr, sgn) =
               Eliom_common.SessionCookies.find
                 sitedata.Eliom_common.session_services sess_id
             in
             if Eliom_common.service_tables_are_empty tables
             then remove1 sgn

           with Not_found -> ())
        | (_, `Browser, _) -> 
          (match find_node_in_group_of_groups sess_grp with
            | Some node -> remove2 node | None -> ())
        | _ -> ()



  end)



type nbmax =
  | Val of int
  | Default
  | Nolimit

let cut n l =
  let rec aux n = function
    | [] -> [], []
    | l when n <= 1 -> [], l
    | a::l ->
        let l1, l2 = aux (n-1) l in
        a::l1, l2
  in
  match n with
    | None -> l, [] (* no limitation *)
    | Some n -> aux n l

module Pers = struct

  let grouptable : (nbmax * string list) Ocsipersist.table Lazy.t =
    lazy (Ocsipersist.open_table "__eliom_session_group_table")
      (* It is lazy because if the module is linked statically,
         the creation of the table must happen after initialisation
         of ocsipersist (after reading the configuration file to know
         the location of the table) *)

  let find g =
    match g with
    | None -> Lwt.return []
    | Some g ->
        Lwt.catch
          (fun () ->
             Ocsipersist.find !!grouptable g >>= fun (_, a) ->
             Lwt.return a)
          (function
            | Not_found -> Lwt.return []
            | e -> Lwt.fail e)

  let add ?set_max defaultmax sess_id sess_grp =
    match sess_grp with
    | Some sg ->
        Lwt.catch
          (fun () ->
            Ocsipersist.find !!grouptable sg >>= fun (max2, cl) ->
            let max, newmax = match set_max with
              | None -> ((match max2 with
                            | Default -> defaultmax
                            | Nolimit -> None
                            | Val m -> Some m), max2)
              | Some None -> None, Nolimit
              | Some (Some v) -> Some v, Val v
            in
            let cl, toclose = cut max cl in
            Ocsipersist.replace_if_exists !!grouptable sg (newmax, (sess_id::cl))
            >>= fun () ->
            Lwt.return toclose)
          (function
            | Not_found ->
                let max = match set_max with
                  | None -> Default
                  | Some None -> Nolimit
                  | Some (Some v) -> Val v
                in
                Ocsipersist.add !!grouptable sg (max, [sess_id]) >>= fun () ->
                Lwt.return []
            | e -> Lwt.fail e)
    | None -> Lwt.return []

  let remove sess_id sess_grp =
    match sess_grp with
    | Some sg ->
        Lwt.catch
          (fun () ->
             Ocsipersist.find !!grouptable sg >>= fun (max, cl) ->
             let newcl = Ocsigen_lib.list_remove_first_if_any sess_id cl in
             (match newcl with
                | [] -> Ocsipersist.remove !!grouptable sg
                | _ -> Ocsipersist.replace_if_exists !!grouptable sg (max, newcl)
             )
          )
          (function
             | Not_found -> Lwt.return ()
             | e -> Lwt.fail e)
    | None -> Lwt.return ()

  let remove_group sess_grp =
    match sess_grp with
    | Some sess_grp -> Ocsipersist.remove !!grouptable sess_grp
    | None -> Lwt.return ()

  let up sess_id grp =
    match grp with
      | None -> Lwt.return ()
      | Some sg ->
          Lwt.catch
            (fun () ->
               Ocsipersist.find !!grouptable sg >>= fun (max, cl) ->
               let newcl = Ocsigen_lib.list_remove_first_if_any sess_id cl in
               Ocsipersist.replace_if_exists !!grouptable sg (max, sess_id::newcl)
            )
            (function
               | Not_found -> Lwt.return ()
               | e -> Lwt.fail e)

  let move ?set_max max sess_id grp1 grp2 =
    if set_max <> None || grp1 <> grp2 then begin
      remove sess_id grp1 >>= fun () ->
      add ?set_max max sess_id grp2
    end
    else Lwt.return []

  let nb_of_groups () = Ocsipersist.length !!grouptable

end
