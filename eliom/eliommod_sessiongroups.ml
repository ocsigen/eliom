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

let make_full_group_name ri site_dir_string = function
  | None -> (site_dir_string, 
             Ocsigen_lib.Right ri.Ocsigen_extensions.ri_remote_inet_addr)
  | Some g -> (site_dir_string, Ocsigen_lib.Left g)

let make_persistent_full_group_name ri site_dir_string = function
  | None -> None
  | Some g -> Some (Marshal.to_string (site_dir_string, Ocsigen_lib.Left g) [])

let getsessgrp a = a
let getperssessgrp a = Marshal.from_string a 0

module type MEMTAB =
  sig
    val add : ?set_max: int -> Eliom_common.sitedata ->
      string -> Eliom_common.sessgrp -> string Ocsigen_cache.Dlist.node
    val remove : 'a Ocsigen_cache.Dlist.node -> unit
    val remove_group : Eliom_common.sessgrp -> unit
    val move :
      ?set_max:int ->
      Eliom_common.sitedata ->
      string Ocsigen_cache.Dlist.node ->
      Eliom_common.sessgrp -> string Ocsigen_cache.Dlist.node
    val up : string Ocsigen_cache.Dlist.node -> unit
    val length : unit -> int
    val set_max : 'a Ocsigen_cache.Dlist.node -> int -> unit
  end

module GroupTable = Hashtbl.Make(struct
  type t = Eliom_common.sessgrp
  let equal = (=)
  let hash = Hashtbl.hash
end)

module Make(A: sig 
              val table : (string Ocsigen_cache.Dlist.t) GroupTable.t
              val close_session : Eliom_common.sitedata -> string -> unit
              val maxgroup : Eliom_common.sitedata -> int
              val maxip : Eliom_common.sitedata -> int
            end) : MEMTAB =
struct

  let grouptable = A.table

  let find g = GroupTable.find grouptable g

  let remove_group sess_grp =
    let cl = find sess_grp in
    let rec close_all cl =
      match Ocsigen_cache.Dlist.oldest cl with
        | Some node -> 
            Ocsigen_cache.Dlist.remove node;
            close_all cl
        | None -> ()
    in
    close_all cl (* will remove the group using finaliser *)

  let remove_if_empty sess_grp node =
    match Ocsigen_cache.Dlist.list_of node with
      | Some cl ->
          if Ocsigen_cache.Dlist.size cl = 0
          then 
            (try
               GroupTable.remove grouptable sess_grp
             with Not_found -> ())
      | None -> ()

  let add ?set_max sitedata sess_id sess_grp =
    let cl = 
      try
        let cl = find sess_grp in
        (match set_max with
           | None -> ()
           | Some v -> ignore (Ocsigen_cache.Dlist.set_maxsize cl v));
        cl
      with Not_found ->
        let size = match set_max, sess_grp with
          | None, (_, Ocsigen_lib.Left _) -> A.maxgroup sitedata
          | None, (_, Ocsigen_lib.Right _) -> A.maxip sitedata
          | Some v, _ -> v
        in
        let cl = Ocsigen_cache.Dlist.create size in
        Ocsigen_cache.Dlist.set_finaliser
          (fun node ->
             A.close_session sitedata (Ocsigen_cache.Dlist.value node);
             remove_if_empty sess_grp node)
          cl;
        GroupTable.add grouptable sess_grp cl;
        cl
    in
    ignore (Ocsigen_cache.Dlist.add sess_id cl);
    match Ocsigen_cache.Dlist.newest cl with
      | Some v -> v
      | None -> assert false

  let remove node =
    Ocsigen_cache.Dlist.remove node

  let up node =
    Ocsigen_cache.Dlist.up node

  let move ?set_max sitedata node grp2 =
(*    if set_max <> None || grp1 <> grp2 then begin *)
    let sess_id = Ocsigen_cache.Dlist.value node in
    remove node;
    add ?set_max sitedata sess_id grp2
(*    end
    else [] *)

  let length () = GroupTable.length grouptable

  let set_max node i =
    match Ocsigen_cache.Dlist.list_of node with
      | None -> ()
      | Some cl -> ignore (Ocsigen_cache.Dlist.set_maxsize cl i)

end

module Data =
  Make (struct
    let table : (string Ocsigen_cache.Dlist.t) GroupTable.t = 
      GroupTable.create 100

    let close_session sitedata sess_id = 
      Eliom_common.SessionCookies.remove 
        sitedata.Eliom_common.session_data sess_id;
      sitedata.Eliom_common.remove_session_data sess_id 
        (* iterates on all
           session data tables *)

    let maxgroup sitedata =
      sitedata.Eliom_common.max_volatile_data_sessions_per_group
    let maxip sitedata =
      sitedata.Eliom_common.max_volatile_data_sessions_per_ip
  end)

module Serv =
  Make (struct
    let table : (string Ocsigen_cache.Dlist.t) GroupTable.t = 
      GroupTable.create 100
    let close_session sitedata sess_id =
      Eliom_common.SessionCookies.remove 
        sitedata.Eliom_common.session_services sess_id
    let maxgroup sitedata =
      sitedata.Eliom_common.max_service_sessions_per_group
    let maxip sitedata =
      sitedata.Eliom_common.max_service_sessions_per_ip
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

  let grouptable : (nbmax * string list) Ocsipersist.table =
    Ocsipersist.open_table "__eliom_session_group_table"

  let find g =
    match g with
    | None -> Lwt.return []
    | Some g ->
        Lwt.catch
          (fun () ->
             Ocsipersist.find grouptable g >>= fun (_, a) ->
             Lwt.return a)
          (function
            | Not_found -> Lwt.return []
            | e -> Lwt.fail e)

  let add ?set_max defaultmax sess_id sess_grp =
    match sess_grp with
    | Some sg ->
        Lwt.catch
          (fun () ->
            Ocsipersist.find grouptable sg >>= fun (max2, cl) ->
            let max, newmax = match set_max with
              | None -> ((match max2 with
                            | Default -> defaultmax
                            | Nolimit -> None
                            | Val m -> Some m), max2)
              | Some None -> None, Nolimit
              | Some (Some v) -> Some v, Val v
            in
            let cl, toclose = cut max cl in
            Ocsipersist.replace_if_exists grouptable sg (newmax, (sess_id::cl))
            >>= fun () ->
            Lwt.return toclose)
          (function
            | Not_found ->
                let max = match set_max with
                  | None -> Default
                  | Some None -> Nolimit
                  | Some (Some v) -> Val v
                in
                Ocsipersist.add grouptable sg (max, [sess_id]) >>= fun () ->
                Lwt.return []
            | e -> Lwt.fail e)
    | None -> Lwt.return []

  let remove sess_id sess_grp =
    match sess_grp with
    | Some sg ->
        Lwt.catch
          (fun () ->
             Ocsipersist.find grouptable sg >>= fun (max, cl) ->
             let newcl = Ocsigen_lib.list_remove_first_if_any sess_id cl in
             (match newcl with
                | [] -> Ocsipersist.remove grouptable sg
                | _ -> Ocsipersist.replace_if_exists grouptable sg (max, newcl)
             )
          )
          (function
             | Not_found -> Lwt.return ()
             | e -> Lwt.fail e)
    | None -> Lwt.return ()

  let remove_group sess_grp =
    match sess_grp with
    | Some sess_grp -> Ocsipersist.remove grouptable sess_grp
    | None -> Lwt.return ()

  let up sess_id grp =
    match grp with
      | None -> Lwt.return ()
      | Some sg ->
          Lwt.catch
            (fun () ->
               Ocsipersist.find grouptable sg >>= fun (max, cl) ->
               let newcl = Ocsigen_lib.list_remove_first_if_any sess_id cl in
               Ocsipersist.replace_if_exists grouptable sg (max, sess_id::newcl)
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

  let length () = Ocsipersist.length grouptable

end
