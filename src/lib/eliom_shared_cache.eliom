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
exception Not_ready

module type TABLE = sig
  type ('a, 'b) t
  val create :
    string ->
    'a Deriving_Json.t Eliom_lib.client_value ->
    'b Deriving_Json.t Eliom_lib.client_value ->
    ('a, 'b) t
  val set : ('a, 'b) t -> 'a -> 'b -> unit
  val remove : ('a, 'b) t -> 'a -> unit
  val find : ('a, 'b) t -> 'a -> 'b
end
}}

{client{
module Table : TABLE = struct
  include Eliom_storage.Local.Table
end
}}

module Table : TABLE = struct

  type ('a, 'b) t = unit -> ('a, 'b) Hashtbl.t

  let create s j j' =
    let c = Eliom_reference.Volatile.eref_from_fun
        ~scope:Eliom_common.request_scope
        (fun () -> Hashtbl.create 10)
    in
    fun () -> Eliom_reference.Volatile.get c

  let set h = Hashtbl.add (h ())
  let remove h = Hashtbl.remove (h ())
  let find h = Hashtbl.find (h ())

end

let create prefix j j' =
  {shared# ('a, 'b) Table.t { Table.create %prefix %j %j' }} ;;

{shared{
type ('a, 'b) t = ('a, 'b) Table.t Eliom_lib.shared_value

let do_cache c id v = Table.(remove c id; set c id v)
}}

let do_cache c id v =
  do_cache (Eliom_shared.Value.local c) id v;
  ignore {unit{ do_cache %c %id %v }}

{shared{
let find c get id =
  let f () =
    Lwt.return (Table.find (Eliom_shared.Value.local c) id)
  and g = function
    | Not_found ->
      lwt v = get id in
      do_cache c id v;
      Lwt.return v
    | e ->
      Lwt.fail e
  in
  Lwt.catch f g

let find_if_ready c id =
  try
    Table.find (Eliom_shared.Value.local c) id
  with Not_found ->
    raise Not_ready
}}
