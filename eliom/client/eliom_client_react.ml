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
let (>>=) = Lwt.(>>=)

module Down =
struct

  let unwrap ?wake
        (c : ('a Eliom_common_comet.chan_id * 'b) Eliom_client_types.data_key)
        : 'a React.E.t
    =
    Lwt_event.of_stream (Eliom_client_comet.unwrap ?wake c)

  let internal_unwrap ( channel, unwrapper ) =
    Lwt_event.of_stream channel

  let () = Eliom_client_unwrap.register_unwrapper Eliom_common.react_down_unwrap_id internal_unwrap

end

module Up =
struct

  let unwrap s =
    let service = Eliommod_cli.unwrap s in
    fun x -> Eliom_client.call_service ~service () x >|= fun _ -> ()

  let internal_unwrap ( service, unwrapper ) =
    fun x -> Eliom_client.call_service ~service () x >|= fun _ -> ()

  let () = Eliom_client_unwrap.register_unwrapper Eliom_common.react_up_unwrap_id internal_unwrap

end

let force_link = ()
