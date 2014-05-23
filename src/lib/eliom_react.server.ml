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

(* Module for event wrapping and related functions *)

open Lwt_react

module Down =
struct

  type 'a stateful =
      {throttling: float option;
       scope: Eliom_common.client_process_scope option;
       react: 'a E.t;
       name: string option;
       size: int option;}

  type 'a stateless = 'a Eliom_comet.Channel.t

  type 'a t' =
    | Stateful of 'a stateful
    | Stateless of 'a stateless

  type 'a t =
      {t : 'a t';
       react_down_mark: 'a t Eliom_common.wrapper;}

  let wrap_stateful
    {throttling=t; scope; react=e; name; size} =
    let ee =
      (match t with
        | None -> e
        | Some t -> E.limit (fun () -> Lwt_unix.sleep t) e)
    in
    let stream = E.to_stream ee in
    let channel = Eliom_comet.Channel.create ?scope ?name ?size stream in
    (channel,Eliom_common.make_unwrapper Eliom_common.react_down_unwrap_id)

  let wrap_stateless channel =
    (channel,Eliom_common.make_unwrapper Eliom_common.react_down_unwrap_id)

  let internal_wrap = function
    | { t = Stateful v } -> wrap_stateful v
    | { t = Stateless v } -> wrap_stateless v

  let react_down_mark () = Eliom_common.make_wrapper internal_wrap

  let stateful ?scope ?throttling ?name ?size (e : 'a E.t) =
    Stateful
      {throttling=throttling;
       scope;
       react=e;
       name=name;
       size=size;
      }

  let stateless ?throttling ?name ?size (e : 'a E.t) =
    let ee =
      (match throttling with
        | None -> e
        | Some t -> E.limit (fun () -> Lwt_unix.sleep t) e)
    in
    let stream = E.to_stream ee in
    Stateless (Eliom_comet.Channel.create ~scope:`Site ?name ?size stream)

  let of_react ?scope ?throttling ?name ?size (e : 'a E.t) =
    let t =
      match scope with
	| Some `Site -> stateless ?throttling ?name ?size e
	| None -> stateful ?throttling ?name ?size e
	| Some ((`Client_process n) as scope) ->
	  stateful ~scope ?throttling ?name ?size e
    in
    { t; react_down_mark=react_down_mark () }

end

module Up =
struct

  type 'a t =
      { event : 'a E.t;
        service :
          (unit,
           'a,
           [ `Nonattached of [ `Post ] Eliom_service.na_s ],
           [ `WithoutSuffix ],
           unit,
           [ `One of 'a Eliom_parameter.ocaml ] Eliom_parameter.param_name,
           [ `Registrable ],
           Eliom_registration.Action.return)
            Eliom_service.service;
        wrapper : 'a t Eliom_common.wrapper }

  let to_react t = t.event

  let internal_wrap t = (t.service, Eliom_common.make_unwrapper Eliom_common.react_up_unwrap_id)

  let up_event_wrapper () = Eliom_common.make_wrapper internal_wrap

  (* An event is created along with a service responsible for it's occurences.
   * function takes a param_type *)
  let create ?scope ?name post_params =
    let (e, push) = E.create () in
    let sp = Eliom_common.get_sp_option () in
    let scope = match sp, scope with
      | _, Some l -> l
      | None, _ -> `Site
      | _ -> (Eliom_common.comet_client_process_scope :> Eliom_common.scope)
    in
    let e_writer = Eliom_service.Http.post_coservice' ?name ~post_params () in
    Eliom_registration.Action.register
      ~scope
      ~options:`NoReload
      ~service:e_writer
      (fun () value -> push value ; Lwt.return ());
    { event = e;
      service = e_writer;
      wrapper = up_event_wrapper () }

end

module S =
struct
  module Down =
  struct

    type 'a stateful =
        {throttling: float option;
	 scope: Eliom_common.client_process_scope option;
         signal: 'a S.t;
         name: string option;}

    type 'a stateless =
	{channel: 'a Eliom_comet.Channel.t;
	 stream: 'a Lwt_stream.t; (* avoid garbage collection *)
	 sl_signal: 'a S.t}

    type 'a t' =
      | Stateful of 'a stateful
      | Stateless of 'a stateless

    type 'a t =
	{ t : 'a t';
	  signal_down_mark: 'a t Eliom_common.wrapper; }

    type 'a store =
        { s : unit S.t Lazy.t; (* to avoid signal GC *)
          mutable value : 'a;
          mutable read : bool;
          condition : unit Lwt_condition.t; }

    let make_store signal =
      let rec store =
        { s = s';
          value = S.value signal;
          read = false;
          condition = Lwt_condition.create (); }
      and s' = lazy (
        S.map (fun v ->
          store.read <- false;
          store.value <- v;
          Lwt_condition.broadcast store.condition ();
          ()) signal)
      in
      ignore (Lazy.force store.s);
      store

    let read_store store =
      let rec aux () =
        if store.read
        then
          begin
            lwt () = Lwt_condition.wait store.condition in
            aux ()
          end
        else
          begin
            store.read <- true;
            Lwt.return (Some store.value)
          end
      in
      aux

    let wrap_stateful
        {throttling=t;
	 scope;
         signal=s;
         name=name} =
      let s : 'a S.t =
        (match t with
          | None -> s
          | Some t -> S.limit (fun () -> Lwt_unix.sleep t) s)
      in
      let store = make_store s in
      let stream = Lwt_stream.from (read_store store) in
      let channel = Eliom_comet.Channel.create_unlimited ?scope ?name stream in
      let value : 'a = S.value s in
      (channel,value,Eliom_common.make_unwrapper Eliom_common.signal_down_unwrap_id)

    let wrap_stateful
        {throttling=t;
         signal=s;
         name=name} =
      let s : 'a S.t =
        (match t with
          | None -> s
          | Some t -> S.limit (fun () -> Lwt_unix.sleep t) s)
      in
      let store = make_store s in
      let stream = Lwt_stream.from (read_store store) in
      let channel = Eliom_comet.Channel.create_unlimited ?name stream in
      let value : 'a = S.value s in
      (channel,value,Eliom_common.make_unwrapper Eliom_common.signal_down_unwrap_id)

    let wrap_stateless
        {sl_signal=s;
         channel} =
      let value : 'a = S.value s in
      (channel,value,Eliom_common.make_unwrapper Eliom_common.signal_down_unwrap_id)

    let internal_wrap = function
      | { t = Stateful v } -> wrap_stateful v
      | { t = Stateless v } -> wrap_stateless v

    let signal_down_mark () = Eliom_common.make_wrapper internal_wrap

    let stateful ?scope
        ?throttling ?name (s : 'a S.t) =
      Stateful
	{throttling=throttling;
	 scope;
	 signal=s;
	 name=name;}

    let stateless ?throttling ?name (s : 'a S.t) =
      let s =
	match throttling with
          | None -> s
          | Some t -> S.limit (fun () -> Lwt_unix.sleep t) s
      in
      let e = S.changes s in
      let stream = E.to_stream e in
      Stateless
	{channel = Eliom_comet.Channel.create_newest ?name stream;
	 stream;
	 sl_signal = s}

    let of_react
	?scope
        ?throttling ?name (s : 'a S.t) =
      let t =
	match scope with
	  | Some `Site -> stateless ?throttling ?name s
	  | None -> stateful ?throttling ?name s
	  | Some ((`Client_process n) as scope) ->
	    stateful ~scope ?throttling ?name s
      in
      { t; signal_down_mark=signal_down_mark () }
  end
end
