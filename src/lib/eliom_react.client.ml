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
let (>|=) = Lwt.(>|=)

open Lwt_react

let section = Lwt_log_js.Section.make "eliom:comet"


module Down =
struct

  type 'a t = 'a React.E.t

  let handle_react_exn, set_handle_react_exn_function =
  let r = ref (fun ?exn () ->
    let s = "Exception during comet with react. \
             Customize this with Eliom_react.set_handle_react_exn_function. "
    in
    Lwt_log_js.log ~section ~level:Lwt_log_js.Debug ?exn s)
  in
  ((fun ?exn () -> !r ?exn ()),
   (fun f -> r := f))

  let internal_unwrap ( channel, unwrapper ) =
    (* We want to catch more exceptions here than the usual exceptions caught
       in Eliom_comet. For example Channel_full. *)
    (* We transform the stream into a stream with exception: *)
    let stream = Lwt_stream.map_exn channel in
    Lwt.async (fun () -> Lwt_stream.iter_s
                  (function
                    | Lwt_stream.Error exn ->
                      let%lwt () = handle_react_exn ~exn () in
                      Lwt.fail exn
                    | Lwt_stream.Value _ -> Lwt.return_unit)
                  stream);
    E.of_stream channel

  let () =
    Eliom_unwrap.register_unwrapper
      Eliom_common.react_down_unwrap_id internal_unwrap

end

module Up =
struct

  type 'a t = ('a -> unit Lwt.t)

  let internal_unwrap ( service, unwrapper ) =
    fun x -> Eliom_client.call_service ~service () x >|= fun _ -> ()

  let () =
    Eliom_unwrap.register_unwrapper
      Eliom_common.react_up_unwrap_id internal_unwrap

end

module S =
struct
  module Down =
  struct
    type 'a t = 'a React.S.t

    let internal_unwrap ( channel, value, unwrapper ) =
      let e = E.of_stream channel in
      S.hold ~eq:(fun _ _ -> false) value e

    let () = Eliom_unwrap.register_unwrapper Eliom_common.signal_down_unwrap_id internal_unwrap

  end
end

let force_link = ()
