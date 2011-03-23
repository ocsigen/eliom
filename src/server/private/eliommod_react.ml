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

module Down =
struct

  type 'a t =
      {throttling: float option;
       react: 'a React.E.t;
       name: string option;
       react_down_mark: 'a t Eliom_common.wrapper;}

  let wrap
      {throttling=t;
       react=e;
       name=name} =
    let ee =
      (match t with
        | None -> e
        | Some t -> Lwt_event.limit (fun () -> Lwt_unix.sleep t) e)
    in
    let stream = Lwt_event.to_stream ee in
    let channel = Eliom_comet.Channels.create ?name stream in
    Eliom_comet.Channels.wrap channel

  let internal_wrap
      {throttling=t;
       react=e;
       name=name} =
    let ee =
      (match t with
        | None -> e
        | Some t -> Lwt_event.limit (fun () -> Lwt_unix.sleep t) e)
    in
    let stream = Lwt_event.to_stream ee in
    let channel = Eliom_comet.Channels.create ?name stream in
    (channel,Eliom_common.make_unwrapper Eliom_common.react_down_unwrap_id)

  let react_down_mark () = Eliom_common.make_wrapper internal_wrap

  let of_react
      ?throttling ?name (e : 'a React.E.t) =
    {throttling=throttling;
     react=e;
     name=name;
     react_down_mark=react_down_mark ()}

end
