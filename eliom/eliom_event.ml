(* Ocsigen
 * http://www.ocsigen.org
 * Module server.ml
 * Copyright (C) 2010
 * Raphaël Proust
 * Laboratoire PPS - CNRS Université Paris Diderot
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


(* an event is first associated to a channel, the id of this channel is then
 * wrapped. *)
let wrap_down_event ~sp (e : 'a React.E.t) : 'a Eliom_common_comet.chan_id Eliom_client_types.data_key =
  let c = Eliom_comet.Channels.new_channel () in
  let () =
    let m = React.E.map
              (fun x -> Eliom_comet.Channels.write c x)
              e
    in
    let `R k = React.E.retain e (fun () -> ()) in
      ignore (React.E.retain e (fun () -> k () ; ignore m ; ignore c))
  in
    Eliom_client.wrap ~sp (Eliom_comet.Channels.get_id c)



(* An event is created along with a service responsible for it's occurences. The
* function takes sp and a param_type *)
let create_up_event ~sp post_param =
  let (e, push) = React.E.create () in
  let e_writer =
    Eliom_predefmod.Action.register_new_post_coservice'
      ~options:`NoReload
      ~sp
      ~post_params:post_param
      (fun _ () msg -> push msg ; Lwt.return ())
  in
    (e, e_writer)
