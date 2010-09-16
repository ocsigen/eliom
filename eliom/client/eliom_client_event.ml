(* Ocsigen
 * http://www.ocsigen.org
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

(* Module for event unwrapping *)



module Down =
struct

  let unwrap
        (c : 'a Eliom_common_comet.buffered_chan_id Eliom_client_types.data_key)
        : 'a React.E.t
    =
    let chan : 'a Eliom_common_comet.buffered_chan_id =
      Eliom_client_comet.Dlisted_channels.unwrap c
    in
    let (e, push) = React.E.create () in
    Eliom_client_comet.Dlisted_channels.register
      chan
      (fun s -> push s ; Lwt.return ()) ;
    e

end

module Up =
struct

  let unwrap ~sp s x =
    let service = Eliommod_cli.unwrap s in
    Eliom_client.call_service ~sp ~service () x

end

