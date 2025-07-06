open Lwt.Syntax

(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010-2011
 * Raphaël Proust
 * Pierre Chambart
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

(* Module for event unwrapping *)
open Lwt_react
open Lwt.Infix

let section = Logs.Src.create "eliom:comet"

module Down = struct
  type 'a t = 'a React.E.t

  let handle_react_exn, set_handle_react_exn_function =
    let pp_exn_option ppf = function
      | Some exn -> Format.fprintf ppf "@\n%s" (Printexc.to_string exn)
      | None -> ()
    in
    let r =
      ref (fun ?exn () ->
        let s =
          "Exception during comet with react. Customize this with Eliom_react.set_handle_react_exn_function. "
        in
        Logs.msg ~src:section Logs.Debug (fun fmt ->
          fmt "%s%a" s pp_exn_option exn);
        Lwt.return_unit)
    in
    (fun ~exn () -> !r ~exn ()), fun f -> r := f

  let internal_unwrap (channel, _unwrapper) =
    (* We want to catch more exceptions here than the usual exceptions caught
       in Eliom_comet. For example Channel_full. *)
    (* We transform the stream into a stream with exception: *)
    let stream = Lwt_stream.wrap_exn channel in
    Lwt.async (fun () ->
      Lwt_stream.iter_s
        (function
          | Error exn ->
              let* () = handle_react_exn ~exn () in
              Lwt.fail exn
          | Ok () -> Lwt.return_unit)
        stream);
    E.of_stream channel

  let () =
    Eliom_unwrap.register_unwrapper Eliom_common.react_down_unwrap_id
      internal_unwrap
end

module Up = struct
  type 'a t = 'a -> unit Lwt.t

  let internal_unwrap (service, _unwrapper) x =
    Eliom_client.call_service ~service () x >|= fun _ -> ()

  let () =
    Eliom_unwrap.register_unwrapper Eliom_common.react_up_unwrap_id
      internal_unwrap
end

module S = struct
  module Down = struct
    type 'a t = 'a React.S.t

    let internal_unwrap (channel, value, _unwrapper) =
      let e = E.of_stream channel in
      S.hold ~eq:(fun _ _ -> false) value e

    let () =
      Eliom_unwrap.register_unwrapper Eliom_common.signal_down_unwrap_id
        internal_unwrap
  end
end

let force_link = ()
