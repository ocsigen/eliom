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
  | Ref of 'a lazy_t ref
  | Ocsiper of 'a option Ocsipersist.t Lwt.t
  | Ocsiper_sit of 'a Ocsipersist.table
  | Req of 'a Polytables.key
  | Sit of 'a Polytables.key
  | Vol of 'a volatile_table Lazy.t
  | Per of 'a persistent_table

type 'a eref = (unit -> 'a) * 'a eref_kind

let eref_from_fun ~scope ?secure ?persistent f : 'a eref =
  match (scope:[<Eliom_common.all_scope]) with
    | `Request -> (f, Req (Polytables.make_key ()))
    | `Global ->
      begin
        match persistent with
          | None -> (f, Ref (ref (Lazy.lazy_from_fun f)))
          | Some name ->
            (f, Ocsiper (Ocsipersist.make_persistent ~store:pers_ref_store ~name ~default:None))
      end
    | `Site ->
      begin
        match persistent with
          | None -> (f, Sit (Polytables.make_key ()))
          | Some name ->
              (f, Ocsiper_sit (Ocsipersist.open_table name))
      end
    | (#Eliom_common.user_scope as scope) ->
      match persistent with
        | None ->
          (f,
           Vol (lazy (create_volatile_table ~scope ?secure ())))
        | Some name ->
          (f,
           Per (create_persistent_table ~scope ?secure name))

let eref ~scope ?secure ?persistent v =
  eref_from_fun ~scope ?secure ?persistent (fun () -> v)

let get_site_id () =
  let sd = Eliom_common.get_site_data () in
  sd.Eliom_common.config_info.Ocsigen_extensions.default_hostname
    ^ ":" ^ sd.Eliom_common.site_dir_string

let get (f, table) =
  match table with
    | Req key ->
      let table = Eliom_request_info.get_request_cache () in
      Lwt.return
        (try Polytables.get ~table ~key
         with Not_found ->
           let value = f () in
           Polytables.set ~table ~key ~value;
           value)
    | Sit key ->
      let table = Eliom_common.((get_site_data ()).site_value_table) in
      Lwt.return
        (try Polytables.get ~table ~key
         with Not_found ->
           let value = f () in
           Polytables.set ~table ~key ~value;
           value)
    | Vol t ->
      (match get_volatile_data ~table:(Lazy.force t) () with
       | Data d -> Lwt.return d
       | _ ->
           Lwt.return
             (let value = f () in
              set_volatile_data ~table:(Lazy.force t) value;
              value))
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
    | Ref r -> Lwt.return (Lazy.force !r)

let set (_, table) value =
  match table with
    | Req key ->
      let table = Eliom_request_info.get_request_cache () in
      Polytables.set ~table ~key ~value;
      Lwt.return ()
    | Sit key ->
      let table = Eliom_common.((get_site_data ()).site_value_table) in
      Lwt.return (Polytables.set ~table ~key ~value)
    | Vol t -> set_volatile_data ~table:(Lazy.force t) value;
      Lwt.return ()
    | Per t -> set_persistent_data ~table:t value
    | Ocsiper r -> r >>= fun r -> Ocsipersist.set r (Some value)
    | Ocsiper_sit t ->
      Ocsipersist.add t (get_site_id ()) value
    | Ref r -> r := Lazy.lazy_from_val value; Lwt.return ()

let modify eref f =
  get eref >>= fun x -> set eref (f x)

let unset (f, table) =
  match table with
    | Req key ->
      let table = Eliom_request_info.get_request_cache () in
      Polytables.remove ~table ~key;
      Lwt.return ()
    | Sit key ->
      let table = Eliom_common.((get_site_data ()).site_value_table) in
      Lwt.return (Polytables.remove ~table ~key)
    | Vol t -> remove_volatile_data ~table:(Lazy.force t) ();
      Lwt.return ()
    | Per t -> remove_persistent_data ~table:t ()
    | Ocsiper r -> r >>= fun r -> Ocsipersist.set r None
    | Ocsiper_sit t ->
      Ocsipersist.remove t (get_site_id ())
    | Ref r -> r := Lazy.lazy_from_fun f; Lwt.return ()
