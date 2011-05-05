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

  type 'a t =
      {throttling: float option;
       react: 'a E.t;
       name: string option;
       react_down_mark: 'a t Eliom_common.wrapper;}

  let internal_wrap
      {throttling=t;
       react=e;
       name=name} =
    let ee =
      (match t with
        | None -> e
        | Some t -> E.limit (fun () -> Lwt_unix.sleep t) e)
    in
    let stream = E.to_stream ee in
    let channel = Eliom_comet.Channels.create ?name stream in
    (channel,Eliom_common.make_unwrapper Eliom_common.react_down_unwrap_id)

  let react_down_mark () = Eliom_common.make_wrapper internal_wrap

  let of_react
      ?throttling ?name (e : 'a E.t) =
    {throttling=throttling;
     react=e;
     name=name;
     react_down_mark=react_down_mark ()}

end

module Up =
struct

  type 'a t =
      { event : 'a E.t;
        service :
          (unit,
           'a,
           [ `Nonattached of [ `Post ] Eliom_services.na_s ],
           [ `WithoutSuffix ],
           unit,
           [ `One of 'a Eliom_parameters.caml ] Eliom_parameters.param_name,
           [ `Registrable ],
           Eliom_output.Action.return)
            Eliom_services.service;
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
      | None, _ -> `Global
      | _ -> `Client_process
    in
    let e_writer = Eliom_services.post_coservice' ?name ~post_params () in
    Eliom_output.Action.register
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
    type 'a t =
        {throttling: float option;
         signal: 'a S.t;
         name: string option;
         signal_down_mark: 'a t Eliom_common.wrapper;}

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

    let internal_wrap
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
      let channel = Eliom_comet.Channels.create_unlimited ?name stream in
      let value : 'a = S.value s in
      (channel,value,Eliom_common.make_unwrapper Eliom_common.signal_down_unwrap_id)

    let signal_down_mark () = Eliom_common.make_wrapper internal_wrap

    let of_react
        ?throttling ?name (s : 'a S.t) =
      {throttling=throttling;
       signal=s;
       name=name;
       signal_down_mark=signal_down_mark ()}
  end
end
