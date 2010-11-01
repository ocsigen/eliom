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

(* The Ocsigen_Comet server extension only provides untyped channels (channels that
 * transport string content).
 * The first abstraction layer we add here is typped channels. The whole
 * marshalling/unmarshalling process is taken care of automatically. The client
 * dual of this file is eliom_client_comet.ml, located in ./client/, the two
 * modules work together and uses dual marshalling/unmarshalling
 * conventions.
 *
 * WARNING: /!\ Don't forget to adapt the dual file to keep compatibility /!\
 * *)

module Ecc = Eliom_common_comet
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)


(*TODO: move to Ocsigen_lib? *)
let filter_map f l =
  let rec aux ys = function
    | [] -> List.rev ys
    | x :: xs -> match f x with
       | Some y -> aux (y :: ys) xs
       | None -> aux ys xs
  in aux [] l


(* A module that provides primitive for server-side channel handling. The only
 * needed operations are : creating, writing, getting id, watching listener
 * count. This just wraps functions from the Comet module. *)
module Channels :
sig

  (* Type of typed channels *)
  type 'a t = Ocsigen_comet.Channels.t

  val write : 'a t -> 'a -> unit

  val create : ?name:string -> unit -> 'a t

  val get_id : 'a t -> 'a Ecc.chan_id

  val wrap :
    'a t -> 'a Eliom_common_comet.chan_id Eliom_client_types.data_key

end = struct

  let encode s = Marshal.to_string s []

  type 'a t = Ocsigen_comet.Channels.t

  let create ?name () = Ocsigen_comet.Channels.create ?name ()

  let write c x = Ocsigen_comet.Channels.write c (encode x, None)

  let get_id c = Ecc.chan_id_of_string (Ocsigen_comet.Channels.get_id c)

  (* Here is a wrap for channels. This is used by pa_eliom_client syntax
     extension to wrap channels. The associated unwrapping function is in the
     dual file.  *)
  let wrap (c : 'a t) : 'a Ecc.chan_id Eliom_client_types.data_key =
    Eliommod_cli.wrap (get_id c)


end




(* The second abstraction layer we build around Channels is a reliable
 * communication system. This is acheived by watching the number of listeners
 * the channel currently has and sending messages only when it has chances of
 * succeeding.
 * *)

module Dlisted_channels :
sig

  type 'a t

  val write : 'a t -> 'a -> unit

  val create :
       max_size:int
    -> ?timer:float
    -> ?name:string
    -> unit
    -> 'a t

  val get_id : 'a t -> 'a Eliom_common_comet.buffered_chan_id
  (** Returns the unique identifier associated to the channel. *)

  val wrap :
       'a t
    -> 'a Eliom_common_comet.buffered_chan_id Eliom_client_types.data_key

end = struct

  module Dlist = Ocsigen_cache.Dlist

  type 'a t = Ocsigen_comet.Channels.t * 'a Dlist.t

  let create ~max_size ?timer ?name () =
    (Ocsigen_comet.Channels.create ?name (),
     Dlist.create ?timer max_size
    )

  let encode s = Marshal.to_string s []
  let raw_write l (chan, dlist) =
    let (outcome_reader, outcome_writer) = Lwt.task () in
    Ocsigen_comet.Channels.write chan (encode l, Some outcome_writer);
    let _ = (*leak ?*)
      outcome_reader >>= function
        | `Success -> Lwt.return ()
        | `Failure ->
            match Dlist.remove_n_oldest dlist (Dlist.maxsize dlist) with
              | [] -> Lwt.return () (*This case never happens*)
              | ll -> List.iter (fun x -> ignore (Dlist.add x dlist)) l;
                      List.iter (fun x -> ignore (Dlist.add x dlist)) ll;
                      Lwt.return ()
    in
      ()

  let flush ((chan, dlist) as c) =
    match Dlist.remove_n_oldest dlist (Dlist.maxsize dlist) with
      | [] -> ()
      | l -> raw_write l c

  let write ((chan, dlist) as c) x =
    ignore (Dlist.add x dlist);
    if Ocsigen_comet.Channels.listeners chan <= 0
    then () (*TODO: set an observer for listeners and flush as soon as client
                    reconnects*)
    else flush c

  let get_id (c, _) =
    Ecc.buffered_chan_id_of_string (Ocsigen_comet.Channels.get_id c)

  let wrap (c : 'a t)
        : 'a Ecc.buffered_chan_id Eliom_client_types.data_key =
    Eliommod_cli.wrap (get_id c)


end
