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

(* The Comet server extension only provides untyped channels (channels that
 * ransport string content. Here we add type information and we keep only
 * usefull functions. *)

module Ecc = Eliom_common_comet

(* Type of typed channels *)
type 'a chan = Comet.Channels.chan

(* A module that provides primitive for server-side channel handling. The only
 * needed operations are : creating, writing, getting id. *)
module Channels :
sig

  val new_channel : unit -> 'a chan

  (* /!\ Uses Marshaling to pass information /!\ *)
  val write  : 'a chan -> 'a -> unit

  val get_id : 'a chan -> 'a Ecc.chan_id

end = struct

  let new_channel () = Comet.Channels.new_channel ()
  let encode_data r = Ocsigen_lib.encode ~plus:false (Marshal.to_string r [])
  let write c x =
    Comet.Channels.write c
      (Ocsigen_lib.encode ~plus:false (Marshal.to_string x []))
  let get_id c = Comet.Channels.get_id c

end


(* Here is a wrap for channels. This can be used to transmit it to a client. *)
let wrap_channel ~sp (c : 'a chan)
      : 'a Ecc.chan_id Eliom_client_types.data_key =
  Eliom_client.wrap ~sp (Channels.get_id c)


(*TODO: high level functions to handle channels (buffered...)*)
