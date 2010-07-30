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

(* The Comet server extension only provides untyped channels (channels that
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
  type 'a chan = Comet.Channels.chan

  val create : ?name:string -> 'a React.E.t -> 'a chan

  val really_create : ('a * int option) React.E.t -> 'a chan

  val get_id : 'a chan -> 'a Ecc.chan_id

  val outcomes : 'a chan -> (Ocsigen_stream.outcome * int) React.E.t

  val listeners : 'a chan -> int React.S.t

  val wrap :
    sp:Eliom_sessions.server_params ->
    'a chan -> 'a Eliom_common_comet.chan_id Eliom_client_types.data_key

end = struct

  let encode s = Marshal.to_string s []

  type 'a chan = Comet.Channels.chan
  let create ?name e =
    Comet.Channels.create ?name (React.E.map (fun x -> (encode x, None)) e)
  let really_create e =
    Comet.Channels.create (React.E.map (fun (x, i) -> (encode x, i)) e)
  let get_id c = Ecc.chan_id_of_string (Comet.Channels.get_id c)
  let outcomes c = Comet.Channels.outcomes c
  let listeners c = Comet.Channels.listeners c

  (* Here is a wrap for channels. This is used by pa_eliom_client syntax
     extension to wrap channels. The associated unwrapping function is in the
     dual file.  *)
  let wrap ~sp (c : 'a chan) : 'a Ecc.chan_id Eliom_client_types.data_key =
    Eliommod_cli.wrap ~sp (get_id c)


end




(* The second abstraction layer we build around Channels is a reliable
 * communication system. This is acheived by watching the number of listeners
 * the channel currently has and sending messages only when it has chances of
 * succeeding.
 * *)

module Dlisted_channels :
sig

  type 'a chan

  val create :
       max_size:int
    -> ?timer:float
    -> 'a React.E.t
    -> 'a chan

  val get_id : 'a chan -> 'a Eliom_common_comet.buffered_chan_id
  (** Returns the unique identifier associated to the channel. *)

  val wrap :
       sp:Eliom_sessions.server_params
    -> 'a chan
    -> 'a Eliom_common_comet.buffered_chan_id Eliom_client_types.data_key

end = struct

  module Dlist = Ocsigen_cache.Dlist

  type 'a chan = 'a Channels.chan * int ref

  let create ~max_size ?timer e_pre =
    (*TODO: prevent max_int related error*)
    let index = let i = ref 0 in fun () -> incr i ; !i in

    let dlist = Dlist.create ?timer max_size in

    let (e, raw_push) = React.E.create () in
    let chan = Channels.really_create e in

    (* these are intermediary functions *)
    let prepare_content l =
      let rec aux accu curr_max = function
        | [] -> (List.rev accu, Some curr_max)
        | ((_, i) as v) :: tl -> aux (v :: accu) (max curr_max i) tl
      in
        aux [] (-1) l
    in
    let dlist_push () = match Dlist.remove_n_oldest dlist max_size with
      | [] -> ()
      | l -> List.iter (fun x -> ignore (Dlist.add x dlist)) l ;
             raw_push (prepare_content l)
    in

    (* first: for each positive change in the listener count we flush the dlist
       content into the channel (if any). *)
    let not1 =
      Lwt_event.notify_p
        (fun () ->
           if Dlist.size dlist = 0
           then Lwt.return ()
           else (Lwt.pause () >|= dlist_push)
        )
        (React.E.fmap
           (fun x -> if x > 0 then Some () else None)
           (React.S.changes (Channels.listeners chan))
        )
    in

    (* we also check for listeners before actually pushing *)
    let not2 =
      Lwt_event.notify_p
        (fun x ->
           ignore (Dlist.add (x, index ()) dlist) ;
           Lwt.pause () >|= fun () ->
           if React.S.value (Channels.listeners chan) = 0
           then ()
           else dlist_push ()
        )
        e_pre
    in

    (* finaly we use feedback to remove elements from the dlist when it's ok *)
    let not3 =
      Lwt_event.notify
        (function
           | `Failure, _ -> ()
           | `Success, x ->
               let l = Dlist.remove_n_oldest dlist max_size in
               List.iter
                 (fun ((_, y) as v) ->
                    if x>=y
                    then ()
                    else ignore (Dlist.add v dlist)
                 )
                 l
        )
        (Channels.outcomes chan)
    in

    (* cleaning *)
    (*TODO: find a better way to manage memory. *)
    let collectable = ref 0 in
    let finaliser _ =
      Lwt_event.disable not1 ;
      Lwt_event.disable not2 ;
      Lwt_event.disable not3 ;
    in
    Gc.finalise finaliser collectable ;

    (chan, collectable)

  let get_id (c, _) =
    Ecc.buffered_chan_id_of_string
      (Ecc.string_of_chan_id (Channels.get_id c))

  let wrap ~sp (c : 'a chan)
        : 'a Ecc.buffered_chan_id Eliom_client_types.data_key =
    Eliommod_cli.wrap ~sp (get_id c)


end

