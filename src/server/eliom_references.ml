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
  | Req of 'a Polytables.key
  | Sit of 'a Polytables.key
  | Ref of 'a lazy_t ref
  | Vol of 'a volatile_table Lazy.t
  | Ocsiper of 'a option Ocsipersist.t Lwt.t
  | Ocsiper_sit of 'a Ocsipersist.table
  | Per of 'a persistent_table

type transient = [ `Transient ]
type persistent = [ `Persistent ]

type ('a, 'storage) eref' = (unit -> 'a) * 'a eref_kind
type 'a eref = ('a, [ transient | persistent ]) eref'

module Transient = struct

  type 'a eref = ('a, transient) eref'

  (* TODO With GADTs, drop the [assert false] statements below! *)

  let eref_from_fun ~scope ?secure f : 'a eref =
    f, match scope with
         | `Request -> Req (Polytables.make_key ())
         | `Global -> Ref (ref (Lazy.lazy_from_fun f))
         | `Site -> Sit (Polytables.make_key ())
         | (#Eliom_common.user_scope as scope) ->
           Vol (lazy (create_volatile_table ~scope ?secure ()))

  let eref ~scope ?secure v =
    eref_from_fun ~scope ?secure (fun () -> v)

  let get (f, table : _ eref) =
    match table with
      | Req key ->
        let table = Eliom_request_info.get_request_cache () in
        (try Polytables.get ~table ~key
         with Not_found ->
           let value = f () in
           Polytables.set ~table ~key ~value;
           value)
      | Sit key ->
        let table = Eliom_common.((get_site_data ()).site_value_table) in
        (try Polytables.get ~table ~key
         with Not_found ->
           let value = f () in
           Polytables.set ~table ~key ~value;
           value)
      | Vol t ->
        (match get_volatile_data ~table:(Lazy.force t) () with
         | Data d -> d
         | _ ->
           (let value = f () in
            set_volatile_data ~table:(Lazy.force t) value;
            value))
      | Ref r -> Lazy.force !r
      | _ -> assert false

  let set (_, table : _ eref) value =
    match table with
      | Req key ->
        let table = Eliom_request_info.get_request_cache () in
        Polytables.set ~table ~key ~value;
      | Sit key ->
        let table = Eliom_common.((get_site_data ()).site_value_table) in
        Polytables.set ~table ~key ~value
      | Vol t -> set_volatile_data ~table:(Lazy.force t) value;
      | Ref r -> r := Lazy.lazy_from_val value
      | _ -> assert false

  let modify eref f =
    set eref (f (get eref))

  let unset (f, table : _ eref) =
    match table with
      | Req key ->
        let table = Eliom_request_info.get_request_cache () in
        Polytables.remove ~table ~key;
      | Sit key ->
        let table = Eliom_common.((get_site_data ()).site_value_table) in
        Polytables.remove ~table ~key
      | Vol t -> remove_volatile_data ~table:(Lazy.force t) ();
      | Ref r -> r := Lazy.lazy_from_fun f
      | _ -> assert false

end

let eref_from_fun ~scope ?secure ?persistent f : 'a eref =
  match (scope:[<Eliom_common.all_scope]) with
    | `Request ->
        (Transient.eref_from_fun ~scope ?secure f :> _ eref)
    | `Global ->
      begin
        match persistent with
          | None -> (Transient.eref_from_fun ~scope ?secure f :> _ eref)
          | Some name ->
            (f, Ocsiper (Ocsipersist.make_persistent ~store:pers_ref_store ~name ~default:None))
      end
    | `Site ->
      begin
        match persistent with
          | None -> (Transient.eref_from_fun ~scope ?secure f :> _ eref)
          | Some name ->
              (f, Ocsiper_sit (Ocsipersist.open_table name))
      end
    | (#Eliom_common.user_scope as scope) ->
      match persistent with
        | None ->
          (Transient.eref_from_fun ~scope ?secure f :> _ eref)
        | Some name ->
          (f, Per (create_persistent_table ~scope ?secure name))

let eref ~scope ?secure ?persistent v =
  eref_from_fun ~scope ?secure ?persistent (fun () -> v)

let get_site_id () =
  let sd = Eliom_common.get_site_data () in
  sd.Eliom_common.config_info.Ocsigen_extensions.default_hostname
    ^ ":" ^ sd.Eliom_common.site_dir_string

let get (f, table as eref) =
  match table with
    | Per t ->
      (get_persistent_data ~table:t () >>= function
        | Data d -> Lwt.return d
        | _ ->
            let value = f () in
            set_persistent_data ~table:t value >>= fun () ->
            Lwt.return value)
    | Ocsiper r ->
      (r >>= fun r -> Ocsipersist.get r >>= function
         | Some v -> Lwt.return v
         | None ->
             let value = f () in
             Ocsipersist.set r (Some value) >>= fun () ->
             Lwt.return value)
    | Ocsiper_sit t ->
      (let site_id = get_site_id () in
       try_lwt Ocsipersist.find t site_id
       with Not_found ->
         let value = f () in
         Ocsipersist.add t site_id value >>= fun () ->
         Lwt.return value)
    | _ -> Lwt.return (Transient.get eref)

let set (_, table as eref) value =
  match table with
    | Per t -> set_persistent_data ~table:t value
    | Ocsiper r -> r >>= fun r -> Ocsipersist.set r (Some value)
    | Ocsiper_sit t ->
      Ocsipersist.add t (get_site_id ()) value
    | _ -> Lwt.return (Transient.set eref value)

let modify eref f =
  get eref >>= fun x -> set eref (f x)

let unset (f, table as eref) =
  match table with
    | Per t -> remove_persistent_data ~table:t ()
    | Ocsiper r -> r >>= fun r -> Ocsipersist.set r None
    | Ocsiper_sit t ->
      Ocsipersist.remove t (get_site_id ())
    | _ -> Lwt.return (Transient.unset eref)
