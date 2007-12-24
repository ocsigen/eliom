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

module type MEMTAB =
  sig
    val find : string option -> string list
    val add : string -> string option -> unit
    val remove : string -> string option -> unit
    val remove_group : string option -> unit
    val move :
      string -> string option -> string option -> unit
    val length : unit -> int
  end

module GroupTable = Hashtbl.Make(struct 
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
end)

module Make(A: sig val table : string list GroupTable.t end) = struct

  let grouptable = A.table
      
  let find g = 
    match g with
    | None -> []
    | Some g ->
        try 
          GroupTable.find grouptable g
        with Not_found -> []
        
  let add sess_id sess_grp =
    match sess_grp with
    | Some sg ->
        let cl = find sess_grp in
        GroupTable.replace grouptable sg (sess_id::cl)
    | None -> ()
          
  let remove sess_id sess_grp =
    match sess_grp with
    | Some sg ->
        let cl = find sess_grp in
        (match cl with
        | [] -> ()
        | _ ->
            GroupTable.replace 
              grouptable sg 
              (Ocsimisc.list_remove_first_if_any sess_id cl))
    | None -> ()
          
  let remove_group sess_grp =
    match sess_grp with
    | Some sess_grp -> GroupTable.remove grouptable sess_grp
    | None -> ()
          
  let move sess_id grp1 grp2 =
    remove sess_id grp1;
    add sess_id grp2

  let length () = GroupTable.length grouptable

end

module Serv =
  Make (struct
    let table : string list GroupTable.t = GroupTable.create 1000
  end)

module Data =
  Make (struct
    let table : string list GroupTable.t = GroupTable.create 1000
  end)

module Pers = struct

  let grouptable : string list Ocsipersist.table = 
    Ocsipersist.open_table "__eliom_session_group_table"
      
  let find g = 
    match g with
    | None -> Lwt.return []
    | Some g ->
        Lwt.catch
          (fun () -> Ocsipersist.find grouptable g)
          (function 
            | Not_found -> Lwt.return []
            | e -> Lwt.fail e)
        
  let add sess_id sess_grp =
    match sess_grp with
    | Some sg ->
        Lwt.catch
          (fun () -> 
            Ocsipersist.find grouptable sg >>= fun cl ->
            Ocsipersist.replace_if_exists grouptable sg (sess_id::cl))
          (function
            | Not_found -> Ocsipersist.add grouptable sg [sess_id]
            | e -> Lwt.fail e)
    | None -> Lwt.return ()
          
  let remove sess_id sess_grp =
    match sess_grp with
    | Some sg ->
        find sess_grp >>= fun cl ->
        (match cl with
        | [] -> Lwt.return ()
        | _ ->
            Ocsipersist.replace_if_exists
              grouptable sg
              (Ocsimisc.list_remove_first_if_any sess_id cl))
    | None -> Lwt.return ()
          
  let remove_group sess_grp =
    match sess_grp with
    | Some sess_grp -> Ocsipersist.remove grouptable sess_grp
    | None -> Lwt.return ()
          
  let move sess_id grp1 grp2 =
    remove sess_id grp1 >>= fun () ->
    add sess_id grp2

  let length () = Ocsipersist.length grouptable

end
