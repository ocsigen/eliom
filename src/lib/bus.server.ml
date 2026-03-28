open Lwt.Syntax

(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010
 * Raphaël Proust
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

module Ecb = Eliom_comet_base

type ('a, 'b) t =
  { stream : 'b Lwt_stream.t
  ; scope : Comet.Channel.comet_scope
  ; name : string option
  ; channel : 'b Comet.Channel.t option
  ; write : 'a -> unit Lwt.t
  ; service : 'a Ecb.bus_send_service
  ; service_registered : bool State.volatile_table option
  ; size : int option
  ; bus_mark : ('a, 'b) t Common.wrapper (* must be the last field ! *) }
[@@warning "-69"]

let register_sender scope service write =
  Registration.Action.register ~scope ~options:`NoReload ~service (fun () x ->
    Lwt_list.iter_s write x)

let internal_wrap (bus : ('a, 'b) t) :
  ('a, 'b) Ecb.wrapped_bus * Common.unwrapper
  =
  let channel =
    match bus.channel with
    | None ->
        Comet.Channel.create ~scope:bus.scope ?name:bus.name ?size:bus.size
          (Lwt_stream.clone bus.stream)
    | Some c -> c
  in
  (match bus.service_registered with
  | None -> ()
  | Some table -> (
    match State.get_volatile_data ~table () with
    | State.Data true -> ()
    | _ ->
        let {service = Ecb.Bus_send_service srv; _} = bus in
        register_sender bus.scope
          (srv
            :> (_, _ list, _, _, _, Service.non_ext, _, _, _, _, _) Service.t)
          bus.write;
        State.set_volatile_data ~table true));
  ( (Comet.Channel.get_wrapped channel, bus.service)
  , Common.make_unwrapper Common.bus_unwrap_id )

let bus_mark () = Common.make_wrapper internal_wrap

let deriving_to_list : 'a Deriving_Json.t -> 'a list Deriving_Json.t =
 fun (type typ) typ ->
  let (typ_list : typ list Deriving_Json.t) =
    let module M = Deriving_Json.Json_list (Deriving_Json.Defaults'' (struct
        type a = typ

        let t = typ
      end))
    in
    M.t
  in
  typ_list

let create_filtered ?scope ?name ?size ~filter typ =
  (*The stream*)
  let stream, push = Lwt_stream.create () in
  let push x =
    let* y = filter x in
    push (Some y); Lwt.return_unit
  in
  let scope =
    match scope with
    | None | Some `Site -> `Site
    | Some (`Client_process n) -> `Client_process n
  in
  let channel =
    match scope with
    | `Site ->
        Some (Comet.Channel.create ~scope ?name ?size (Lwt_stream.clone stream))
    | `Client_process _ -> None
  in
  let typ_list = deriving_to_list typ in
  (*The service*)
  let post_params =
    (Parameter.ocaml "bus_write" typ_list
     : ('a, 'aa, 'aaa) Parameter.params_type)
  in
  let distant_write =
    Service.create ?name
      ~meth:(Service.Post (Parameter.unit, post_params))
      ~path:Service.No_path ()
  in
  let service_registered =
    match scope with
    | `Site ->
        register_sender scope distant_write push;
        None
    | `Client_process _ as scope -> Some (State.create_volatile_table ~scope ())
  in
  (*The bus*)
  let bus =
    { stream
    ; channel
    ; scope
    ; name
    ; write = push
    ; service = Eliom_comet_base.Bus_send_service distant_write
    ; service_registered
    ; bus_mark = bus_mark ()
    ; size }
  in
  bus

let create ?scope ?name ?size typ =
  create_filtered ~filter:Lwt.return ?scope ?name ?size typ

let stream bus =
  match bus.scope with `Site -> bus.stream | `Client_process _ -> bus.stream

let write bus x = bus.write x
