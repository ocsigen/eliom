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

open Lwt_react

module Down =
struct

  type 'a t = 'a React.E.t

  let internal_unwrap ( channel, unwrapper ) =
    E.of_stream channel

  let () = Eliom_unwrap.register_unwrapper Eliom_common.react_down_unwrap_id internal_unwrap

end

module Up =
struct

  type 'a t = ('a -> unit Lwt.t)

  let internal_unwrap ( service, unwrapper ) =
    fun x -> Eliom_client.call_service ~service () x >|= fun _ -> ()

  let () = Eliom_unwrap.register_unwrapper Eliom_common.react_up_unwrap_id internal_unwrap

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
