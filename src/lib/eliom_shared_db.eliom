(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015 Vasilis Papavasileiou
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

{shared{
type ('a, 'b) et = Set of ('a * 'b) | Remove of 'a

exception Not_ready

module type TABLE = sig
  type ('a, 'b) t
  val create :
    string ->
    'a Deriving_Json.t Eliom_lib.shared_value ->
    'b Deriving_Json.t Eliom_lib.shared_value ->
    ('a, 'b) t
  val set : ('a, 'b) t -> 'a -> 'b -> unit Lwt.t
  val remove : ('a, 'b) t -> 'a -> unit Lwt.t
  val find : ('a, 'b) t -> 'a -> 'b Lwt.t
  val event : ('a, 'b) t -> ('a, 'b) et Eliom_react.Down.t
end
}}

{client{
module Table : sig
  include TABLE
  val create :
    ('a, 'b) et React.E.t ->
    string ->
    'a Deriving_Json.t Eliom_lib.shared_value ->
    'b Deriving_Json.t Eliom_lib.shared_value ->
    ('a, 'b) t
end = struct
  open Eliom_storage.Local.Json
  type ('a, 'b) t = ('a, 'b) Table.t * ('a, 'b) et React.E.t
  let create e s j j' = Table.create s j j', e
  let set (m, _) id x = Table.set m id x; Lwt.return ()
  let remove (m, _) id = Table.remove m id; Lwt.return ()
  let find (m, _) id = Lwt.return (Table.find m id)
  let event (_, e) = e
end
}}

module Table : TABLE = struct

  type ('a, 'b) t =
    'a Deriving_Json.t *
    'b Ocsipersist.table *
    (('a, 'b) et React.event *
     (?step:React.step -> ('a, 'b) et -> unit))

  let create s j j' =
    Eliom_shared.Value.local j,
    Ocsipersist.open_table s,
    React.E.create ()

  let set (j, a, (v, f)) id x =
    let id' = Deriving_Json.to_string j id in
    lwt () = Ocsipersist.add a id' x in
    f ?step:None (Set (id, x));
    Lwt.return ()

  let remove (j, a, (_, f)) id =
    let id' = Deriving_Json.to_string j id in
    lwt () = Ocsipersist.remove a id' in
    f ?step:None (Remove id);
    Lwt.return ()

  let find (j, a, _) id =
    Ocsipersist.find a (Deriving_Json.to_string j id)

  let event (_, _, (e, _)) =
    Eliom_react.Down.of_react e

end ;;

{shared{
type ('a, 'b) t = ('a, 'b) Table.t Eliom_lib.shared_value
}}

let create prefix j j' =
  let sv = Table.create prefix j j' in
  let e = Table.event sv in
  let cv = {('a, 'b) Table.t{
    let a = Table.create %e %prefix %j %j' in
    let f = function
      | Set (id, x) ->
        Lwt.async (fun () -> Table.set a id x)
      | Remove id ->
        Lwt.async (fun () -> Table.remove a id)
    in
    React.E.map f %e;
    a
  }} in
  Eliom_lib.create_shared_value sv cv

let set a id x =
  let a = Eliom_shared.Value.local a in
  Table.set a id x

let remove a id =
  let a = Eliom_shared.Value.local a in
  Table.remove a id ;;

{shared{
let find m id = Table.find (Eliom_shared.Value.local m) id

let event m = Table.event (Eliom_shared.Value.local m)
}}
