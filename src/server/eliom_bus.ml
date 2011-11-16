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

type 'a t = {
  stream   : 'a Lwt_stream.t;
  scope    : Eliom_comet.Channels.comet_scope;
  name     : string option;
  channel  : 'a Eliom_comet.Channels.t option;
  write    : ('a -> unit);
  service  : 'a Ecb.bus_send_service;
  size     : int option;
  bus_mark : 'a t Eliom_common.wrapper; (* must be the last field ! *)
}

let internal_wrap (bus: 'a t) : 'a Ecb.wrapped_bus * Eliom_common.unwrapper =
  let channel =
    match bus.channel with
      | None ->
	Eliom_comet.Channels.create ~scope:bus.scope ?name:bus.name ?size:bus.size
	  (Lwt_stream.clone bus.stream)
      | Some c -> c
  in
  ( ( Eliom_comet.Channels.get_wrapped channel,
      bus.service ),
    Eliom_common.make_unwrapper Eliom_common.bus_unwrap_id )

let bus_mark () = Eliom_common.make_wrapper internal_wrap

let deriving_to_list : 'a Deriving_Json.t -> 'a list Deriving_Json.t = fun (type typ) typ ->
  let (typ_list:typ list Deriving_Json.t) =
    let module M = Deriving_Json.Json_list(Deriving_Json.Defaults''(struct
      type a = typ
      let t = typ
    end)) in
    M.t
  in
  typ_list

let create ?scope ?name ?size typ =
  (*The stream*)
  let (stream, push) = Lwt_stream.create () in
  let push x = push (Some x) in

  let scope =
    match scope with
      | None
      | Some `Global -> `Global
      | Some `Client_process n -> `Client_process n
  in

  let channel =
    match scope with
      | `Global ->
	Some (Eliom_comet.Channels.create ~scope ?name ?size
		(Lwt_stream.clone stream))
      | `Client_process _ -> None
  in

  let typ_list = deriving_to_list typ in

  (*The service*)
  let post_params =
    (Eliom_parameters.caml "bus_write" typ_list
       : ('a, 'aa, 'aaa) Eliom_parameters.params_type)
  in
  let distant_write = Eliom_services.post_coservice' ?name ~post_params () in
  Eliom_output.Action.register
    (* CCC: TODO: this service should be registered only when the bus
       is sent to the client. The problem now is that: if ~scope is
       client_process, then the bus can't be created globaly. if the
       scope is global the service won't be deleted at any time. then
       if the bus is garbage collected, the service won't. -> memory leak *)
    (* ~scope *)
    ~options:`NoReload
    ~service:distant_write
    (fun () x -> List.iter push x ; Lwt.return ());

  (*The bus*)
  let bus =
    { stream;
      channel;
      scope;
      name;
      write   = push;
      service = distant_write;
      bus_mark = bus_mark ();
      size = size }
  in

  bus

let stream bus =
  match bus.scope with
    | `Global -> Lwt_stream.clone bus.stream
    | `Client_process _ -> bus.stream

let write bus x = bus.write x
