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

let ( >>= ) = Lwt.bind

module Ocsipersist = struct
  include Eliom_common.Ocsipersist.Store
  include Eliom_common.Ocsipersist.Polymorphic
end

let pers_ref_store = Ocsipersist.open_store "eliom__persistent_refs"

type 'a eref_kind =
  | Req of 'a Polytables.key
  | Sit of 'a Polytables.key
  | Ref of 'a lazy_t ref (* Ocaml reference *)
  | Vol of 'a volatile_table Lazy.t (* Vol. table (group, session, process) *)
  | Ocsiper of 'a option Ocsipersist.t Lwt.t (* Global persist. table *)
  | Ocsiper_sit of 'a Ocsipersist.table Lwt.t (* Persist. table for site *)
  | Per of 'a persistent_table Lwt.t
(* Persist. table for group session or process *)

type volatile = [ `Volatile ]
type persistent = [ `Persistent ]
type ('a, 'storage) eref' = (unit -> 'a) * bool * 'a eref_kind
(* * The function to get the value
   * a boolean true means "it is safe to execute the function from an
     external context" (for example if it is a constant function - eref created
     from a value)
   * the kind of reference (scope, persistence)
*)

type 'a eref = ('a, [ volatile | persistent ]) eref'

exception Eref_not_initialized

module Volatile = struct
  type 'a eref = ('a, volatile) eref'

  (* TODO With GADTs, drop the [assert false] and [failwith] statements below! *)

  let eref_from_fun_ ~ext ~scope ?secure f : 'a eref =
    ( f,
      ext,
      match scope with
      | `Request -> Req (Polytables.make_key ())
      | `Global -> Ref (ref (Lazy.from_fun f))
      | `Site -> Sit (Polytables.make_key ())
      | #Eliom_common.user_scope as scope ->
          Vol (lazy (create_volatile_table ~scope ?secure ())) )

  let eref_from_fun ~scope ?secure f : 'a eref =
    eref_from_fun_ ~ext:false ~scope ?secure f

  let eref ~scope ?secure v =
    eref_from_fun_ ~ext:true ~scope ?secure (fun () -> v)

  let get ((f, _, table) : _ eref) =
    match table with
    | Req key -> (
        let table = Eliom_request_info.get_request_cache () in
        try Polytables.get ~table ~key
        with Not_found ->
          let value = f () in
          Polytables.set ~table ~key ~value;
          value)
    | Sit key -> (
        let table = Eliom_common.((get_site_data ()).site_value_table) in
        try Polytables.get ~table ~key
        with Not_found ->
          let value = f () in
          Polytables.set ~table ~key ~value;
          value)
    | Vol t -> (
        match get_volatile_data ~table:(Lazy.force t) () with
        | Data d -> d
        | _ ->
            let value = f () in
            set_volatile_data ~table:(Lazy.force t) value;
            value)
    | Ref r -> Lazy.force !r
    | _ -> assert false

  let set ((_, _, table) : _ eref) value =
    match table with
    | Req key ->
        let table = Eliom_request_info.get_request_cache () in
        Polytables.set ~table ~key ~value
    | Sit key ->
        let table = Eliom_common.((get_site_data ()).site_value_table) in
        Polytables.set ~table ~key ~value
    | Vol t -> set_volatile_data ~table:(Lazy.force t) value
    | Ref r -> r := Lazy.from_val value
    | _ -> assert false

  let modify eref f = set eref (f (get eref))

  let unset ((f, _, table) : _ eref) =
    match table with
    | Req key ->
        let table = Eliom_request_info.get_request_cache () in
        Polytables.remove ~table ~key
    | Sit key ->
        let table = Eliom_common.((get_site_data ()).site_value_table) in
        Polytables.remove ~table ~key
    | Vol t -> remove_volatile_data ~table:(Lazy.force t) ()
    | Ref r -> r := Lazy.from_fun f
    | _ -> assert false

  module Ext = struct
    let get state (f, ext, table) =
      match table with
      | Vol t -> (
          try
            Eliom_state.Ext.Low_level.get_volatile_data ~state
              ~table:(Lazy.force t)
          with Not_found ->
            if ext then (
              let value = f () in
              Eliom_state.Ext.Low_level.set_volatile_data ~state
                ~table:(Lazy.force t) value;
              value)
            else
              (* I don't want to run f in the wrong context -> I fail *)
              raise Eref_not_initialized)
      | _ -> failwith "wrong eref for this function"

    let set state (_, _, table) value =
      match table with
      | Vol t ->
          Eliom_state.Ext.Low_level.set_volatile_data ~state
            ~table:(Lazy.force t) value
      | _ -> failwith "wrong eref for this function"

    let modify state eref f = set state eref (f (get state eref))

    let unset state ((_, _, table) : _ eref) =
      match table with
      | Vol t ->
          Eliom_state.Ext.Low_level.remove_volatile_data ~state
            ~table:(Lazy.force t)
      | _ -> failwith "wrong eref for this function"
  end
end

let eref_from_fun_ ~ext ~scope ?secure ?persistent f : 'a eref =
  match (scope : [< Eliom_common.all_scope ]) with
  | `Request -> (Volatile.eref_from_fun_ ~ext ~scope ?secure f :> _ eref)
  | `Global -> (
      match persistent with
      | None -> (Volatile.eref_from_fun_ ~ext ~scope ?secure f :> _ eref)
      | Some name ->
          ( f,
            ext,
            Ocsiper
              ( pers_ref_store >>= fun store ->
                Ocsipersist.make_persistent ~store ~name ~default:None ) ))
  | `Site -> (
      match persistent with
      | None -> (Volatile.eref_from_fun_ ~ext ~scope ?secure f :> _ eref)
      | Some name ->
          (*VVV!!! ??? CHECK! *)
          (f, ext, Ocsiper_sit (Ocsipersist.open_table name)))
  | #Eliom_common.user_scope as scope -> (
      match persistent with
      | None -> (Volatile.eref_from_fun_ ~ext ~scope ?secure f :> _ eref)
      | Some name -> (f, ext, Per (create_persistent_table ~scope ?secure name))
      )

let eref_from_fun ~scope ?secure ?persistent f : 'a eref =
  eref_from_fun_ ~ext:false ~scope ?secure ?persistent f

let eref ~scope ?secure ?persistent v =
  eref_from_fun_ ~ext:true ~scope ?secure ?persistent (fun () -> v)

let get_site_id () =
  let sd = Eliom_common.get_site_data () in
  (Eliom_common.get_config_info sd).Ocsigen_extensions.default_hostname ^ ":"
  ^ Eliom_common.get_site_dir_string sd

let get ((f, _, table) as eref) =
  match table with
  | Per t -> (
      t >>= fun t ->
      get_persistent_data ~table:t () >>= function
      | Data d -> Lwt.return d
      | _ ->
          let value = f () in
          set_persistent_data ~table:t value >>= fun () -> Lwt.return value)
  | Ocsiper r -> (
      r >>= fun r ->
      Ocsipersist.get r >>= function
      | Some v -> Lwt.return v
      | None ->
          let value = f () in
          Ocsipersist.set r (Some value) >>= fun () -> Lwt.return value)
  | Ocsiper_sit t ->
      t >>= fun t ->
      let site_id = get_site_id () in
      Lwt.catch
        (fun () -> Ocsipersist.find t site_id)
        (function
          | Not_found ->
              let value = f () in
              Ocsipersist.add t site_id value >>= fun () -> Lwt.return value
          | exc -> Lwt.reraise exc)
  | _ -> Lwt.return (Volatile.get eref)

let set ((_, _, table) as eref) value =
  match table with
  | Per t -> t >>= fun t -> set_persistent_data ~table:t value
  | Ocsiper r -> r >>= fun r -> Ocsipersist.set r (Some value)
  | Ocsiper_sit t -> t >>= fun t -> Ocsipersist.add t (get_site_id ()) value
  | _ -> Lwt.return (Volatile.set eref value)

let modify eref f = get eref >>= fun x -> set eref (f x)

let unset ((_, _, table) as eref) =
  match table with
  | Per t -> t >>= fun t -> remove_persistent_data ~table:t ()
  | Ocsiper r -> r >>= fun r -> Ocsipersist.set r None
  | Ocsiper_sit t -> t >>= fun t -> Ocsipersist.remove t (get_site_id ())
  | _ -> Lwt.return (Volatile.unset eref)

module Ext = struct
  let get state ((f, ext, table) as r) =
    let state = Eliom_state.Ext.untype_state state in
    match table with
    | Vol _ -> Lwt.return (Volatile.Ext.get state r)
    | Per t ->
        t >>= fun t ->
        Lwt.catch
          (fun () ->
            Eliom_state.Ext.Low_level.get_persistent_data ~state ~table:t)
          (function
            | Not_found ->
                if ext (* We can run the function from another state *) then
                  let value = f () in
                  Eliom_state.Ext.Low_level.set_persistent_data ~state ~table:t
                    value
                  >>= fun () -> Lwt.return value
                else Lwt.fail Eref_not_initialized
            | e -> Lwt.fail e)
    | _ -> failwith "wrong eref for this function"

  let set state ((_, _, table) as r) value =
    let state = Eliom_state.Ext.untype_state state in
    match table with
    | Vol _ -> Lwt.return (Volatile.Ext.set state r value)
    | Per t ->
        t >>= fun t ->
        Eliom_state.Ext.Low_level.set_persistent_data ~state ~table:t value
    | _ -> Lwt.fail (Failure "wrong eref for this function")

  let modify state eref f = get state eref >>= fun v -> set state eref (f v)

  let unset state ((_, _, table) as r) =
    let state = Eliom_state.Ext.untype_state state in
    match table with
    | Vol _ -> Lwt.return (Volatile.Ext.unset state r)
    | Per t ->
        t >>= fun t ->
        Eliom_state.Ext.Low_level.remove_persistent_data ~state ~table:t
    | _ -> failwith "wrong eref for this function"
end
