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

type sessgrp = (string * string)
type perssessgrp = string (* pair marshaled *)

let make_full_group_name site_dir_string = function
  | None -> None
  | Some g -> Some (site_dir_string, g)

let make_persistent_full_group_name site_dir_string = function
  | None -> None
  | Some g -> Some (Marshal.to_string (site_dir_string, g) [])

let getsessgrp a = a 
let getperssessgrp a = Marshal.from_string a 0 

type nbmax = 
  | Val of int
  | Default
  | Nolimit

module type MEMTAB =
  sig
    val find : (string * string) option -> string list
    val add : ?set_max: int option -> 
      int option -> string -> (string * string) option -> string list
    val remove : string -> (string * string) option -> unit
    val remove_group : (string * string) option -> unit
    val move :
      ?set_max: int option ->
      int option -> 
      string -> 
      (string * string) option -> 
      (string * string) option -> string list
    val up : string -> (string * string) option -> unit
    val length : unit -> int
  end

module GroupTable = Hashtbl.Make(struct 
  type t = string * string
  let equal = (=)
  let hash = Hashtbl.hash
end)

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

module Make(A: sig val table : (nbmax * string list) GroupTable.t end) = 
struct

  let grouptable = A.table

  let find' g = 
    match g with
    | None -> (Default, [])
    | Some g ->
        try 
          GroupTable.find grouptable g
        with Not_found -> (Default, [])
        
  let find g = snd (find' g)
        
  let add ?set_max defaultmax sess_id sess_grp =
    (* returns the oldest sessions (to close)
       if the number of sessions is too high *)
    match sess_grp with
    | Some sg ->
        let max2, cl = find' sess_grp in
        let max, newmax = match set_max with
          | None -> ((match max2 with
                        | Default -> defaultmax
                        | Nolimit -> None
                        | Val m -> Some m), max2)
          | Some None -> None, Nolimit
          | Some (Some v) -> Some v, Val v
        in
        let cl, toclose = cut max cl in
        GroupTable.replace grouptable sg (newmax, (sess_id::cl));
        toclose
    | None -> []
          
  let remove sess_id sess_grp =
    match sess_grp with
      | Some sg ->
          (try
             let max, cl = GroupTable.find grouptable sg in
             let newcl = Ocsigen_lib.list_remove_first_if_any sess_id cl in
             (match newcl with
                | [] -> GroupTable.remove grouptable sg
                | _ -> GroupTable.replace grouptable sg (max, newcl) 
             )
           with Not_found -> ())
      | None -> ()
          
  let remove_group sess_grp =
    match sess_grp with
    | Some sess_grp -> GroupTable.remove grouptable sess_grp
    | None -> ()
          
  let up sess_id grp =
    match grp with
      | None -> ()
      | Some sg ->
          (try
             let max, cl = GroupTable.find grouptable sg in
             let newcl = Ocsigen_lib.list_remove_first_if_any sess_id cl in
             GroupTable.replace grouptable sg (max, sess_id::newcl) 
           with Not_found -> ())

  let move ?set_max max sess_id grp1 grp2 =
    if set_max <> None || grp1 <> grp2 then begin
      remove sess_id grp1;
      add ?set_max max sess_id grp2
    end
    else []

  let length () = GroupTable.length grouptable

end

module Serv =
  Make (struct
    let table : (nbmax * string list) GroupTable.t = GroupTable.create 1000
  end)

module Data =
  Make (struct
    let table : (nbmax * string list) GroupTable.t = GroupTable.create 1000
  end)

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
