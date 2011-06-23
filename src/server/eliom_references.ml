  (* Ocsigen
   * http://www.ocsigen.org
   * Copyright (C) 2010 Vincent Balat
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
(** {2 Eliom references} *)

open Eliom_state

let (>>=) = Lwt.bind

let pers_ref_store = Ocsipersist.open_store "eliom__persistent_refs"

type 'a eref_kind =
  | Ref of 'a ref
  | Ocsiper of 'a Ocsipersist.t Lwt.t
  | Req of 'a Polytables.key
  | Vol of 'a volatile_table Lazy.t
  | Per of 'a persistent_table

type 'a eref = 'a * 'a eref_kind

let eref ~scope ?secure ?persistent value : 'a eref =
  match (scope:[<Eliom_common.all_scope]) with
    | `Request -> (value, Req (Polytables.make_key ()))
    | `Global ->
      begin
	match persistent with
	  | None -> (value, Ref (ref value))
	  | Some name ->
	    (value, Ocsiper (Ocsipersist.make_persistent
                               ~store:pers_ref_store ~name ~default:value))
      end
    | (#Eliom_common.user_scope as scope) ->
      match persistent with
	| None ->
          (value,
           Vol (lazy (create_volatile_table ~scope ?secure ())))
	| Some name ->
          (value,
           Per (create_persistent_table ~scope ?secure name))

let get (value, table) =
  match table with
    | Req key ->
      let table = Eliom_request_info.get_request_cache () in
      Lwt.return (try Polytables.get ~table ~key with Not_found -> value)
    | Vol t ->
	(match get_volatile_data ~table:(Lazy.force t) () with
         | Data d -> Lwt.return d
         | _ -> Lwt.return value)
    | Per t ->
      (get_persistent_data ~table:t () >>= function
        | Data d -> Lwt.return d
        | _ -> Lwt.return value)
    | Ocsiper r -> r >>= fun r -> Ocsipersist.get r
    | Ref r -> Lwt.return !r

let set (_, table) value =
  match table with
    | Req key ->
      let table = Eliom_request_info.get_request_cache () in
      Polytables.set ~table ~key ~value;
      Lwt.return ()
    | Vol t -> set_volatile_data ~table:(Lazy.force t) value;
      Lwt.return ()
    | Per t -> set_persistent_data ~table:t value
    | Ocsiper r -> r >>= fun r -> Ocsipersist.set r value
    | Ref r -> r := value; Lwt.return ()

let unset (value, table) =
  match table with
    | Req key ->
      let table = Eliom_request_info.get_request_cache () in
      Polytables.remove ~table ~key;
      Lwt.return ()
    | Vol t -> remove_volatile_data ~table:(Lazy.force t) ();
      Lwt.return ()
    | Per t -> remove_persistent_data ~table:t ()
    | Ocsiper r -> r >>= fun r -> Ocsipersist.set r value
    | Ref r -> r := value; Lwt.return ()
