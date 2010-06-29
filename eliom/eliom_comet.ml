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




(* A module that provides primitive for server-side channel handling. The only
 * needed operations are : creating, writing, getting id, watching listener
 * count. This just wraps functions from the Comet module. *)
module Channels :
sig

  (* Type of typed channels *)
  type 'a chan = Comet.Channels.chan

  val create : 'a React.E.t -> 'a chan

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
  let create e =
    Comet.Channels.create (React.E.map (fun x -> (encode x, None)) e)
  let really_create e =
    Comet.Channels.create (React.E.map (fun (x, i) -> (encode x, i)) e)
  let get_id c = Ecc.chan_id_of_string (Comet.Channels.get_id c)
  let outcomes c = Comet.Channels.outcomes c
  let listeners c = Comet.Channels.listeners c

  (* Here is a wrap for channels. This is used by pa_eliom_client syntax
     extension to wrap channels. The associated unwrapping function is in the
     dual file.  *)
  let wrap ~sp (c : 'a chan) : 'a Ecc.chan_id Eliom_client_types.data_key =
    Eliommod_client.wrap ~sp (get_id c)


end





(* The second abstraction layer we build around Channels is a reliable
 * communication system. This is acheived by watching the number of listeners
 * the channel currently has and sending messages only when it has chances of
 * succeeding.
 *
 * Note that this is yet to be tested. *)

module Buffers :
sig

  exception Value_larger_than_buffer_max_size

  type 'a t

  val create :
    max_size:int -> sizer:('a -> int) -> timer:('a -> float option) -> 'a t

  val set_max_size : int -> 'a t -> unit

  val is_empty : 'a t -> bool    (* true if argument is empty *)
  val push : 'a -> 'a t -> unit  (*may raise Value_larger_than_buffer_max_size*)
  val pop : 'a t -> 'a option    (* None if empty buffer *)
  val pop_all : 'a t -> 'a list  (* empty list if empty buffer *)
  val peek : 'a t -> 'a option   (* None if empty buffer *)
  val junk_until : ('a -> bool) -> 'a t -> unit

end = struct

  exception Value_larger_than_buffer_max_size

  type 'a disposable_value =
      {
        mutable dv_val   : 'a option ;
        (*   *) dv_size  : int ;
        mutable dv_timer : unit Lwt.t ;
      }

  type 'a t =
      {
        (*   *) b_queue    : 'a disposable_value Queue.t ;
        mutable b_max_size : int ;
        mutable b_size     : int ;
        (*   *) b_sizer    : 'a -> int ;
        (*   *) b_timer    : 'a -> float option ;
      }

  let set_max_size s t =  t.b_max_size <- s

  let new_disposable_value size timer x =
    let v =
      {
        dv_val  = Some x ;
        dv_size = size ;
        dv_timer = Lwt.return () ;
      }
    in begin
      match timer x with
        | None -> ()
        | Some t ->
            v.dv_timer <- (Lwt_unix.sleep t >|= fun () -> v.dv_val <- None)
    end ; v

  let create ~max_size ~sizer ~timer =
    {
      b_queue    = Queue.create () ;
      b_max_size = max_size ;
      b_size     = 0 ;
      b_sizer    = sizer ;
      b_timer    = timer ;
    }

  let is_empty {b_queue = q} = Queue.is_empty q

  let peek t =
    let rec aux () =
      try
        let p = Queue.peek t.b_queue in
        match p.dv_val with
          | Some _ as x -> x
          | None ->
              t.b_size <- t.b_size - p.dv_size ;
              ignore (Queue.pop t.b_queue) ;
              aux ()
      with
        | Queue.Empty -> None
    in aux ()

  let pop t =
    let rec aux () =
      try
        let v = Queue.pop t.b_queue in
        t.b_size <- t.b_size - v.dv_size ;
        match v.dv_val with
          | None -> aux () (* Value already lost *)
          | Some p -> Lwt.cancel v.dv_timer ; Some p
      with
        | Queue.Empty -> None
    in aux ()

  let pop_all t =
    let rec aux acc = match pop t with
      | None -> acc
      | Some x -> aux (x::acc)
    in
      aux []

  let push x t =
    (* computing the value size *)
    let size = t.b_sizer x in
    (* checking for fitability *)
    if size > t.b_max_size
    then raise Value_larger_than_buffer_max_size
    else
      (* making room for value *)
      let rec aux () =
        if t.b_size + size > t.b_max_size
        then (ignore (pop t) ; aux ())
        else ()
      in
        aux () ;
        t.b_size <- t.b_size + size ;
        Queue.push (new_disposable_value size t.b_timer x) t.b_queue

  let junk_until f t =
    let rec aux () = match peek t with
      | None -> ()
      | Some x ->
          if f x
          then ()
          else (ignore (pop t) ;  aux ())
    in
      aux ()

end


module Buffered_channels :
sig

  type 'a chan

  val create :
       max_size:int -> ?sizer:('a -> int) -> ?timer:('a -> float option)
    -> 'a React.E.t
    -> 'a chan

  val get_id : 'a chan -> 'a Ecc.buffered_chan_id

  val wrap :
    sp:Eliom_sessions.server_params ->
    'a chan -> 'a Ecc.buffered_chan_id Eliom_client_types.data_key

end = struct

  type 'a chan = ('a Channels.chan * int)

  let create ~max_size ?(sizer = fun _ -> 1) ?(timer = fun _ -> None) e_pre =

    (*TODO: prevent max_int related error*)
    let index = let i = ref 0 in fun () -> incr i ; !i in

    let buff =
      Buffers.create
        ~max_size
        ~sizer:(fun (x,_) -> sizer x)
        ~timer:(fun (x,_) -> timer x)
    in
    let (e, raw_push) = React.E.create () in
    let chan = Channels.really_create e in

    (* these are intermediary functions *)
    let prepare_content l =
      let rec aux accu curr_max = function
        | [] -> (accu, Some curr_max)
        | ((_, i) as v) :: tl -> aux (v :: accu) (max curr_max i) tl
      in
        aux [] (-1) l
    in
    let buff_push () = (* side effect: refresh the values in the buffer *)
      match Buffers.pop_all buff with
        | [] -> ()
        | l -> List.iter (fun x -> Buffers.push x buff) l ;
               raw_push (prepare_content l)
    in

    (* first : for each positive change in the listener count we flush the buffer
       content into the channel (if any). *)
    let not1 =
      Lwt_event.notify_p
        (fun () ->
           if Buffers.is_empty buff
           then Lwt.return ()
           else (Lwt.pause () >|= buff_push)
        )
        (React.E.fmap
           (fun x -> if x > 0 then Some () else None)
           (React.S.changes (Channels.listeners chan))
        )
    in

    (* we also check for listeners before actually pushing *)
    let not2 =
      (*TODO: REACTify this... But what about recursion? *)
      Lwt_event.notify_p
        (fun x ->
           Buffers.push (x, index ()) buff ;
           if React.S.value (Channels.listeners chan) = 0
           then Lwt.return ()
           else Lwt.pause () >|= buff_push
        )
        e_pre
    in

    (* finaly we use feedback to empty the buffer when it's ok *)
    let not3 =
      Lwt_event.notify
        (function
           | `Failure, _ -> ()
           | `Success, x -> Buffers.junk_until (fun (_, i) -> i>x) buff
        )
        (Channels.outcomes chan)
    in

    (* cleaning *)
    (*TODO: find a better way to manage memory. *)
    let collectable = Random.int 2 in
    let finaliser _ =
      Lwt_event.disable not1 ;
      Lwt_event.disable not2 ;
      Lwt_event.disable not3 ;
      ignore (Buffers.pop_all buff)
    in
    Gc.finalise finaliser collectable ;

    (chan, collectable)


  let get_id (c, _) =
    Ecc.buffered_chan_id_of_string
      (Ecc.string_of_chan_id (Channels.get_id c))

  let wrap ~sp (c : 'a chan) : 'a Ecc.buffered_chan_id Eliom_client_types.data_key =
    Eliommod_client.wrap ~sp (get_id c)

end

