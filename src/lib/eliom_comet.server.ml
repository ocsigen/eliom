open Eio.Std

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

(* TODO: handle ended stream ( and on client side too ) *)

module Ecb = Eliom_comet_base

let section = Logs.Src.create "eliom:comet"

type chan_id = string

let answer_to_string = Deriving_Json.to_string Eliom_comet_base.answer_json

let encode_downgoing s =
  answer_to_string (Eliom_comet_base.Stateful_messages (Array.of_list s))

let encode_global_downgoing s =
  answer_to_string (Eliom_comet_base.Stateless_messages (Array.of_list s))

let timeout_msg = answer_to_string Eliom_comet_base.Timeout
let state_closed_msg = answer_to_string Eliom_comet_base.State_closed
let error_msg s = answer_to_string (Eliom_comet_base.Comet_error s)
let json_content_type = "application/json"

let marshal (v : 'a) =
  let wrapped = Eliom_wrap.wrap v in
  let value : 'a Eliom_runtime.eliom_comet_data_type = wrapped in
  Eliom_lib.Url.encode ~plus:false (Marshal.to_string value [])

exception New_connection

module Comet_param = struct
  type page = string

  let translate content = content, json_content_type
end

module Comet =
  Eliom_registration.Customize (Eliom_registration.String) (Comet_param)

let comet_path = ["__eliom_comet__"]
let comet_global_path = ["__eliom_comet_global__"]

let fallback_service =
  Eliom_common.lazy_site_value_from_fun @@ fun () ->
  Comet.create ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ~path:(Eliom_service.Path comet_path) (fun () () -> state_closed_msg)

let fallback_global_service =
  Eliom_common.lazy_site_value_from_fun @@ fun () ->
  Comet.create ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ~path:(Eliom_service.Path comet_global_path) (fun () () ->
    error_msg
      "request with no post parameters, or there isn't any registered site comet channel")

let new_id = Eliom_lib.make_cryptographic_safe_string

(* ocsigenserver needs to be modified for this to be configurable:
   the connection is closed after a fixed amount of time
   if the server does not send anything.
   By default it is 20 seconds *)
let timeout_base = 20.
let timeout_jitter = 0.1

let timeout () =
  timeout_base *. (1. +. (timeout_jitter *. (Random.float 2. -. 1.)))

module Stateless : sig
  type channel

  val create : ?name:string -> size:int -> string Eliom_stream.t -> channel
  val get_id : channel -> string
  val get_service : unit -> Eliom_comet_base.comet_service
  val get_kind : newest:bool -> channel -> Eliom_comet_base.stateless_kind
  val chan_id_of_string : string -> 'a Eliom_comet_base.chan_id
end = struct
  type channel_id = string

  module Dlist = Ocsigen_cache.Dlist

  type channel =
    { ch_id : channel_id
    ; mutable ch_index : int
    ; (* the number of messages already added to the channel *)
      ch_content : (string * int) Dlist.t
    ; ch_wakeup : Eio.Condition.t
      (* condition broadcasted when there is a new message *) }

  module Channel_hash = struct
    type t = channel

    let equal c1 c2 = c1.ch_id = c2.ch_id
    let hash c = Hashtbl.hash c.ch_id
  end

  module Weak_channel_table = Weak.Make (Channel_hash)

  let channels = Weak_channel_table.create 0

  let find_channel =
    let dummy_channel =
      { ch_id = ""
      ; ch_index = 0
      ; ch_content = Dlist.create 1
      ; ch_wakeup = Eio.Condition.create () }
    in
    fun ch_id ->
      let dummy = {dummy_channel with ch_id} in
      try Some (Weak_channel_table.find channels dummy) with Not_found -> None

  let wakeup_waiters channel = Eio.Condition.broadcast channel.ch_wakeup

  (* fill the channel with messages from the stream *)
  let run_channel channel stream =
    let channel' = Weak.create 1 in
    Weak.set channel' 0 (Some channel);
    let channel = channel' in
    (* hide non weak reference to be sure not to keep a strong reference *)
    let f msg =
      match Weak.get channel 0 with
      | None ->
          raise Not_found
          (* terminates the loop: remove reference on the stream, etc ... *)
      | Some channel ->
          channel.ch_index <- succ channel.ch_index;
          ignore
            (Dlist.add (msg, channel.ch_index) channel.ch_content : 'a option);
          wakeup_waiters channel
    in
    Fiber.fork
      ~sw:(Stdlib.Option.get (Fiber.get Ocsigen_lib.current_switch))
      (fun () ->
         Fiber.without_binding Eliom_common.sp_key (fun () ->
           Eliom_stream.iter_s f stream))

  let make_name name = "stateless:" ^ name

  let chan_id_of_string name =
    Eliom_comet_base.chan_id_of_string (make_name name)

  let create ?(name = new_id ()) ~size stream =
    let name = make_name name in
    let channel =
      { ch_id = name
      ; ch_index = 0
      ; ch_content = Dlist.create size
      ; ch_wakeup = Eio.Condition.create () }
    in
    run_channel channel stream;
    match find_channel name with
    | Some _ ->
        failwith
          (Printf.sprintf
             "can't create channel %s: a channel with the same name already exists"
             name)
    | None ->
        Weak_channel_table.add channels channel;
        channel

  let get_channel (ch_id, position) =
    match find_channel ch_id with
    | Some channel -> Eliom_lib.Left (channel, position)
    | None -> Right ch_id

  exception
    Finished of (channel_id * (string * int) Eliom_comet_base.channel_data) list

  let queue_take channel last =
    try
      Dlist.fold
        (fun l (v, index) ->
           if index >= last
           then (channel.ch_id, Eliom_comet_base.Data (v, index)) :: l
           else raise (Finished l))
        [] channel.ch_content
    with Finished l -> l

  let get_available_data = function
    | Eliom_lib.Right ch_id ->
        Logs.warn ~src:section (fun fmt ->
          fmt "Stateless channel not found, sending Closed: %s" ch_id);
        [ch_id, Eliom_comet_base.Closed]
    | Eliom_lib.Left (channel, position) -> (
      match position with
      (* the first request of the client should be with i = 1 *)
      (* when the client is requesting the newest data, only return
           one if he don't already have it *)
      | Eliom_comet_base.Newest i when i > channel.ch_index -> []
      | Eliom_comet_base.Newest _ | Eliom_comet_base.Last None -> (
        (* initialisation of external newest channels *)
        match Dlist.newest channel.ch_content with
        | None -> [] (* should not happen *)
        | Some node -> [channel.ch_id, Eliom_comet_base.Data (Dlist.value node)]
        )
      (* when the client is requesting the data after index i return
           all data with index gretter or equal to i*)
      | Eliom_comet_base.After i when i > channel.ch_index -> []
      (* if the requested value is not in the queue anymore, tell
           the client that its request was dropped *)
      | Eliom_comet_base.After i
        when i <= channel.ch_index - Dlist.size channel.ch_content ->
          [channel.ch_id, Eliom_comet_base.Full]
      | Eliom_comet_base.After i -> queue_take channel i
      | Eliom_comet_base.Last (Some n) ->
          let i = channel.ch_index - min (Dlist.size channel.ch_content) n in
          queue_take channel i)

  let has_data = function
    | Eliom_lib.Right _ ->
        true (* a channel was closed: need to tell it to the client *)
    | Eliom_lib.Left (channel, position) -> (
      match position with
      | Eliom_comet_base.Newest i when i > channel.ch_index -> false
      | Eliom_comet_base.Newest _ -> true
      | Eliom_comet_base.After i when i > channel.ch_index -> false
      | Eliom_comet_base.After _ -> true
      | Eliom_comet_base.Last _ when Dlist.size channel.ch_content > 0 -> true
      | Eliom_comet_base.Last _ -> false)

  let really_wait_data requests =
    let rec make_list = function
      | [] -> []
      | Eliom_lib.Left (channel, _) :: q ->
          (fun () -> Eio.Condition.await_no_mutex channel.ch_wakeup)
          :: make_list q
      | Eliom_lib.Right _ :: _ -> assert false
      (* closed channels are considered to have data *)
    in
    Fiber.any (make_list requests)

  let wait_data requests =
    if List.exists has_data requests
    then ()
    else
      Eio.Time.with_timeout_exn
        (Stdlib.Option.get (Fiber.get Ocsigen_lib.env))#clock (timeout ())
        (*VVV How to use mono_clock? *) (fun () -> really_wait_data requests)

  let handle_request () (_, req) =
    match req with
    | Eliom_comet_base.Stateful _ ->
        failwith
          "attempting to request data on stateless service with a stateful request"
    | Eliom_comet_base.Stateless requests ->
        let requests = List.map get_channel (Array.to_list requests) in
        let res =
          try
            let () = wait_data requests in
            List.flatten (List.map get_available_data requests)
          with Eio.Time.Timeout -> []
        in
        encode_global_downgoing res

  let global_service =
    Eliom_common.lazy_site_value_from_fun @@ fun () ->
    (*VVV Why isn't this a POST non-attached coservice? --Vincent *)
    Comet.create_attached_post
      ~post_params:
        Eliom_parameter.(bool "idle" ** Eliom_comet_base.comet_request_param)
      ~fallback:(Eliom_common.force_lazy_site_value fallback_global_service)
      handle_request

  let get_service =
    let queue = ref [] in
    fun () ->
      Eliom_comet_base.Comet_service
        (Eliom_common.force_lazy_site_value global_service, queue)

  let get_id {ch_id; _} = ch_id

  let get_kind ~newest {ch_index; _} =
    if newest
    then Eliom_comet_base.Newest_kind (ch_index + 1)
    else Eliom_comet_base.After_kind (ch_index + 1)
end

(** Register services at site initialization *)
let () =
  Eliommod.register_site_init (fun () ->
    ignore (Eliom_common.force_lazy_site_value fallback_global_service);
    ignore (Eliom_common.force_lazy_site_value fallback_service);
    ignore (Stateless.get_service ()))

(** String channels on which is build the module Channel *)
module Stateful : sig
  type t

  val create :
     ?scope:Eliom_common.client_process_scope
    -> ?name:chan_id
    -> size:int
    -> _ React.event
    -> t

  val create_unlimited :
     ?scope:Eliom_common.client_process_scope
    -> ?name:chan_id
    -> _ Eliom_stream.t
    -> t

  val get_id : t -> string

  type comet_service = Eliom_comet_base.comet_service

  val get_service : t -> comet_service
  val wait_timeout : ?scope:Eliom_common.client_process_scope -> float -> unit
end = struct
  type chan_id = string
  type comet_service = Eliom_comet_base.comet_service
  type internal_comet_service = Eliom_comet_base.internal_comet_service
  type end_request_waiters = unit Eio.Promise.u

  type activity =
    | Active of end_request_waiters list
    (** There is currently a request from the client *)
    | Inactive of float
    (** The last request from the client completed at that time *)

  type waiter = [`Data | `Update] Promise.t

  type channel =
    | Events of
        { queue : string Eliom_comet_base.channel_data Queue.t
        ; (* Reference to the event stream, so that it does not
             get garbage collected *)
          mutable events : Obj.t option }
    | Stream of
        { mutable stream : string Eliom_comet_base.channel_data Eliom_stream.t
        ; mutable waiter : waiter }

  type handler =
    { hd_scope : Eliom_common.client_process_scope
    ; (* id : int; pour tester que ce sont des service differents... *)
      mutable hd_active_channels : (chan_id * channel) list
      (** streams that are currently sent to client *)
    ; mutable hd_unregistered_channels : (chan_id * channel) list
      (** streams that are created on the server side, but client did not register *)
    ; mutable hd_registered_chan_id : chan_id list
      (** the fusion of all the streams from hd_active_channels *)
    ; mutable hd_update_streams_w :
        ([`Data | `Update], exn) result Eio.Promise.u option
      (** used to signal new data or new active streams. *)
    ; hd_service : internal_comet_service
    ; mutable hd_last : string * int
      (** the last message sent to the client, if he sends a request
            with the same number, this message is immediately sent
            back.*)
    ; mutable hd_activity : activity }

  exception Connection_closed

  let set_active handler =
    match handler.hd_activity with
    | Active _ -> ()
    | Inactive _ -> handler.hd_activity <- Active []

  let set_inactive handler =
    match handler.hd_activity with
    | Active l ->
        handler.hd_activity <- Inactive (Unix.gettimeofday ());
        List.iter (fun waiter -> Promise.resolve waiter ()) l
    | Inactive _ -> ()

  let update_inactive handler =
    match handler.hd_activity with
    | Active _ -> ()
    | Inactive _ -> handler.hd_activity <- Inactive (Unix.gettimeofday ())

  let wait_handler_timeout handler t =
    let rec run () =
      match handler.hd_activity with
      | Active l ->
          let waiter, waker =
            Promise.create
              ()
          in
          let t =
            let () = Eio.Promise.await waiter in
            let () = Eio_unix.sleep t in
            run ()
          in
          handler.hd_activity <- Active (waker :: l);
          t
      | Inactive inactive_time ->
          let now = Unix.gettimeofday () in
          if now -. inactive_time > t
          then ()
          else
            let () = Eio_unix.sleep (t -. (now -. inactive_time)) in
            run ()
    in
    run ()

  (** called when a connection is opened, it makes the other
      connection terminate with no data. That way there is at most one
      opened connection to the service. There are new connection
      opened when the client wants to listen to new channels for
      instance. *)
  let new_connection handler =
    set_active handler;
    match handler.hd_update_streams_w with
    | None -> ()
    | Some wakener ->
        handler.hd_update_streams_w <- None;
        Eio.Promise.resolve_error wakener New_connection

  (** called when a new channel is made active. It restarts the thread
      wainting for inputs ( wait_data ) such that it can receive the messages from
      the new channel *)
  let signal_update handler event =
    match handler.hd_update_streams_w with
    | None -> ()
    | Some wakener ->
        handler.hd_update_streams_w <- None;
        Promise.resolve_ok wakener event

  let stream_waiter s =
    Fiber.without_binding Eliom_common.sp_key (fun () ->
      Eio.Switch.run (fun _sw ->
        let _ = Eliom_stream.peek s in
        Eio.Promise.create_resolved `Data))

  (** read up to [n] messages in the list of streams [streams] without blocking. *)
  let read_channels n handler =
    let rec take n acc channels =
      if n = 0
      then acc
      else
        match channels with
        | [] -> acc
        | (id, Events {queue; _}) :: rem ->
            if Queue.is_empty queue
            then take n acc rem
            else take (n - 1) ((id, Queue.take queue) :: acc) channels
        | (id, Stream ({stream; _} as s)) :: rem ->
            let l =
              Fiber.without_binding Eliom_common.sp_key (fun () ->
                Eliom_stream.get_available_up_to n stream)
            in
            if l <> [] then s.waiter <- stream_waiter stream;
            take (n - List.length l) (List.rev_map (fun v -> id, v) l @ acc) rem
    in
    List.rev (take n [] handler.hd_active_channels)

  let wait_channels handler =
    List.fold_left
      (fun acc (_, channel) ->
         match channel with
         | Events _ -> acc
         | Stream {waiter; _} -> waiter :: acc)
      [] handler.hd_active_channels

  (** wait for data on any channel that the client asks. It correctly
      handles new channels the server creates after that the client
      registered them *)
  let rec wait_data wait_closed_connection handler =
    match
      let hd_update_streams, hd_update_streams_w =
        Promise.create
          ()
      in
      handler.hd_update_streams_w <- Some hd_update_streams_w;
      let p, w = Eio.Promise.create () in
      Eliom_lib.fork (fun () ->
        ignore
        @@ Eio.Fiber.List.map
             (fun f ->
                match f () with
                | v -> Eio.Promise.resolve_ok w v
                | exception e -> Eio.Promise.resolve_error w e)
             (wait_closed_connection
             :: (fun () -> Eio.Promise.await_exn hd_update_streams)
             :: List.map
                  (fun p () -> Eio.Promise.await p)
                  (wait_channels handler)));
      Eio.Promise.await_exn p
    with
    | `Data -> handler.hd_update_streams_w <- None
    | `Update -> wait_data wait_closed_connection handler

  let launch_channel handler chan_id channel =
    handler.hd_active_channels <-
      (chan_id, channel) :: handler.hd_active_channels;
    match channel with
    | Events _ -> ()
    | Stream _ -> signal_update handler `Update

  let register_channel handler chan_id =
    Logs.info ~src:section (fun fmt -> fmt "register channel %s" chan_id);
    if not (List.mem_assoc chan_id handler.hd_active_channels)
    then
      try
        let stream = List.assoc chan_id handler.hd_unregistered_channels in
        handler.hd_unregistered_channels <-
          List.remove_assoc chan_id handler.hd_unregistered_channels;
        launch_channel handler chan_id stream
      with Not_found ->
        handler.hd_registered_chan_id <-
          chan_id :: handler.hd_registered_chan_id

  let close_channel' handler chan_id =
    Logs.info ~src:section (fun fmt -> fmt "close channel %s" chan_id);
    handler.hd_active_channels <-
      List.remove_assoc chan_id handler.hd_active_channels;
    handler.hd_unregistered_channels <-
      List.remove_assoc chan_id handler.hd_unregistered_channels;
    handler.hd_registered_chan_id <-
      List.filter (( <> ) chan_id) handler.hd_registered_chan_id

  let wait_closed_connection () =
    let () = Ocsigen_request.connection_closed (Eliom_request_info.get_ri ()) in
    raise Connection_closed

  (* register the service handler.hd_service *)
  let run_handler handler =
    let f () (idle, req) =
      match req with
      | Eliom_comet_base.Stateless _ ->
          failwith
            "attempting to request data on stateful service with a stateless request"
      | Eliom_comet_base.Stateful (Eliom_comet_base.Request_data number) -> (
          Logs.info ~src:section (fun fmt -> fmt "received request %i" number);
          (* if a new connection occurs for a service, we reply
           immediately to the previous with no data. *)
          new_connection handler;
          if snd handler.hd_last = number
          then fst handler.hd_last
          else
            try
              Eio.Time.with_timeout_exn
                (Stdlib.Option.get (Fiber.get Ocsigen_lib.env))#clock
                (*VVV How to use mono_clock? *) (timeout ()) (fun () ->
                let messages =
                  let messages = read_channels 100 handler in
                  if messages <> [] || idle
                  then messages
                  else
                    let () = wait_data (wait_closed_connection ()) handler in
                    read_channels 100 handler
                in
                let message = encode_downgoing messages in
                handler.hd_last <- message, number;
                set_inactive handler;
                message)
            with
            | New_connection ->
                encode_downgoing []
                (* happens if an other connection has been opened on that service *)
                (* CCC in this case, it would be beter to return code 204: no content *)
            | Eio.Time.Timeout -> set_inactive handler; timeout_msg
            | Connection_closed ->
                set_inactive handler;
                (* it doesn't matter what we do here *)
                timeout_msg
            | e -> set_inactive handler; raise e)
      | Eliom_comet_base.Stateful (Eliom_comet_base.Commands commands) ->
          update_inactive handler;
          List.iter
            (function
              | Eliom_comet_base.Register channel ->
                  register_channel handler channel
              | Eliom_comet_base.Close channel -> close_channel' handler channel)
            (Array.to_list commands);
          (* command connections are replied immediately by an
                 empty answer *)
          encode_downgoing []
    in
    let {hd_service = Eliom_comet_base.Internal_comet_service (service, _); _} =
      handler
    in
    Comet.register ~scope:handler.hd_scope ~service f

  (** For each scope there is a reference containing the handler. The
      reference itself are stocked in [handler_ref_table]. This table
      is never cleaned, but it is supposed that this won't be a
      problem as scope should be used in limited number *)

  (* as of now only `Client_process scope are handled: so we only stock scope_hierarchy *)
  type handler_ref_table =
    ( Eliom_common.scope_hierarchy
      , handler option Eliom_reference.Volatile.eref )
      Hashtbl.t

  let handler_ref_table : handler_ref_table = Hashtbl.create 1

  let get_handler_eref scope =
    let scope_hierarchy =
      Eliom_common_base.scope_hierarchy_of_user_scope scope
    in
    try Hashtbl.find handler_ref_table scope_hierarchy
    with Not_found ->
      let eref =
        Eliom_reference.Volatile.eref ~scope:(`Client_process scope_hierarchy)
          None
      in
      Hashtbl.add handler_ref_table scope_hierarchy eref;
      eref

  let get_handler scope =
    let eref = get_handler_eref scope in
    match Eliom_reference.Volatile.get eref with
    | Some t -> t
    | None ->
        let hd_service =
          Eliom_comet_base.Internal_comet_service
            (* CCC ajouter possibilité d'https *)
            ( Eliom_service.create_attached_post
              (*VVV Why is it attached? --Vincent *)
                ~post_params:
                  Eliom_parameter.(
                    bool "idle" ** Eliom_comet_base.comet_request_param)
                ~fallback:(Eliom_common.force_lazy_site_value fallback_service)
                (*~name:"comet" (* CCC faut il mettre un nom ? *)*)
                ()
            , ref [] )
        in
        let handler =
          { hd_scope = scope
          ; hd_active_channels = []
          ; hd_unregistered_channels = []
          ; hd_registered_chan_id = []
          ; hd_service
          ; hd_update_streams_w = None
          ; hd_last = "", -1
          ; hd_activity = Inactive (Unix.gettimeofday ()) }
        in
        Eliom_reference.Volatile.set eref (Some handler);
        Logs.info ~src:section (fun fmt ->
          fmt "Creating new comet handler and registering service for scope");
        run_handler handler;
        handler

  let wait_timeout ?(scope = Eliom_common.comet_client_process_scope) t =
    let hd = get_handler scope in
    wait_handler_timeout hd t

  type t = {ch_handler : handler; ch_id : chan_id}

  let name_of_scope (scope : Eliom_common.user_scope) =
    let sp = Eliom_common.get_sp () in
    let name =
      Eliom_common.make_full_state_name ~sp ~secure:false (*VVV secure? *)
        ~scope
    in
    let pref =
      match scope with
      | `Session_group _ -> "sessiongroup:"
      | `Session _ -> "session:"
      | `Client_process _ -> "clientprocess:"
    in
    Eliom_common.make_full_cookie_name pref name

  let create
        ?(scope = Eliom_common.comet_client_process_scope)
        ?(name = new_id ())
        ~size
        events
    =
    let name = name_of_scope (scope :> Eliom_common.user_scope) ^ name in
    let handler = get_handler scope in
    Logs.info ~src:section (fun fmt -> fmt "create channel %s" name);
    let channel = Events {queue = Queue.create (); events = None} in
    (match channel with
    | Stream _ -> assert false
    | Events channel ->
        channel.events <-
          Some
            (Obj.repr
               (React.E.fold
                  (fun full x ->
                     let queue = channel.queue in
                     full
                     ||
                     if Queue.length queue > size
                     then (
                       channel.events <- None;
                       Queue.clear queue;
                       Queue.push Eliom_comet_base.Full queue;
                       signal_update handler `Data;
                       true)
                     else (
                       Queue.push (Eliom_comet_base.Data (marshal x)) queue;
                       signal_update handler `Data;
                       false))
                  false events)));
    if List.mem name handler.hd_registered_chan_id
    then (
      handler.hd_registered_chan_id <-
        List.filter (( <> ) name) handler.hd_registered_chan_id;
      launch_channel handler name channel)
    else
      handler.hd_unregistered_channels <-
        (name, channel) :: handler.hd_unregistered_channels;
    {ch_handler = handler; ch_id = name}

  let create_unlimited
        ?(scope = Eliom_common.comet_client_process_scope)
        ?(name = new_id ())
        stream
    =
    let name = name_of_scope (scope :> Eliom_common.user_scope) ^ name in
    let handler = get_handler scope in
    Logs.info ~src:section (fun fmt -> fmt "create channel %s" name);
    let stream =
      Fiber.without_binding Eliom_common.sp_key (fun () ->
        Eliom_stream.map (fun x -> Eliom_comet_base.Data (marshal x)) stream)
    in
    let channel = Stream {stream; waiter = stream_waiter stream} in
    if List.mem name handler.hd_registered_chan_id
    then (
      handler.hd_registered_chan_id <-
        List.filter (( <> ) name) handler.hd_registered_chan_id;
      launch_channel handler name channel)
    else
      handler.hd_unregistered_channels <-
        (name, channel) :: handler.hd_unregistered_channels;
    {ch_handler = handler; ch_id = name}

  let get_id {ch_id; _} = ch_id

  let get_service {ch_handler; _} =
    let {hd_service = Ecb.Internal_comet_service (srv, queue); _} =
      ch_handler
    in
    Ecb.Comet_service (srv, queue)
end

module Channel : sig
  type 'a t

  type comet_scope =
    [Eliom_common.site_scope | Eliom_common.client_process_scope]

  val create_from_events :
     ?scope:[< comet_scope]
    -> ?name:string
    -> ?size:int
    -> 'a React.event
    -> 'a t

  val create :
     ?scope:[< comet_scope]
    -> ?name:string
    -> ?size:int
    -> 'a Eliom_stream.t
    -> 'a t

  val create_unlimited :
     ?scope:Eliom_common.client_process_scope
    -> ?name:string
    -> 'a Eliom_stream.t
    -> 'a t

  val create_newest : ?name:string -> 'a Eliom_stream.t -> 'a t
  val get_wrapped : 'a t -> 'a Eliom_comet_base.wrapped_channel

  val external_channel :
     ?history:int
    -> ?newest:bool
    -> prefix:string
    -> name:string
    -> unit
    -> 'a t

  val wait_timeout : ?scope:Eliom_common.client_process_scope -> float -> unit
end = struct
  type 'a channel =
    | Stateless of Stateless.channel
    | Stateless_newest of Stateless.channel
    | Stateful of Stateful.t
    | External of 'a Eliom_comet_base.wrapped_channel

  type 'a t = {channel : 'a channel; channel_mark : 'a t Eliom_common.wrapper}
  [@@warning "-69"]

  let get_wrapped t =
    match t.channel with
    | Stateful channel ->
        Eliom_comet_base.Stateful_channel
          ( Stateful.get_service channel
          , Eliom_comet_base.chan_id_of_string (Stateful.get_id channel) )
    | Stateless channel ->
        Eliom_comet_base.Stateless_channel
          ( Stateless.get_service ()
          , Eliom_comet_base.chan_id_of_string (Stateless.get_id channel)
          , Stateless.get_kind ~newest:false channel )
    | Stateless_newest channel ->
        Eliom_comet_base.Stateless_channel
          ( Stateless.get_service ()
          , Eliom_comet_base.chan_id_of_string (Stateless.get_id channel)
          , Stateless.get_kind ~newest:true channel )
    | External wrapped -> wrapped

  let internal_wrap c =
    ( get_wrapped c
    , Eliom_common.make_unwrapper Eliom_common.comet_channel_unwrap_id )

  let channel_mark () = Eliom_common.make_wrapper internal_wrap

  let create_stateful_channel ?scope ?name ~size events =
    Stateful (Stateful.create ?scope ?name ~size events)

  let create_stateless_channel ?name ~size stream =
    Stateless
      (Stateless.create ?name ~size
         (Fiber.without_binding Eliom_common.sp_key (fun () ->
            Eliom_stream.map marshal stream)))

  let create_stateless_newest_channel ?name stream =
    Stateless_newest
      (Stateless.create ?name ~size:1
         (Fiber.without_binding Eliom_common.sp_key (fun () ->
            Eliom_stream.map marshal stream)))

  let create_stateful ?scope ?name ?(size = 1000) events =
    { channel = create_stateful_channel ?scope ?name ~size events
    ; channel_mark = channel_mark () }

  let create_unlimited ?scope ?name events =
    { channel = Stateful (Stateful.create_unlimited ?scope ?name events)
    ; channel_mark = channel_mark () }

  let create_stateless ?name ?(size = 1000) stream =
    { channel = create_stateless_channel ?name ~size stream
    ; channel_mark = channel_mark () }

  let create_newest ?name stream =
    { channel = create_stateless_newest_channel ?name stream
    ; channel_mark = channel_mark () }

  type comet_scope =
    [Eliom_common.site_scope | Eliom_common.client_process_scope]

  let create_from_events ?scope ?name ?(size = 1000) events =
    match scope with
    | None -> create_stateful ?name ~size events
    | Some (`Client_process _ as scope) ->
        create_stateful ~scope ?name ~size events
    | Some `Site -> create_stateless ?name ~size (Eio_react.E.to_stream events)

  let create ?scope ?name ?(size = 1000) stream =
    match scope with
    | None -> create_stateful ?name ~size (Eio_react.E.of_stream stream)
    | Some (`Client_process _ as scope) ->
        create_stateful ~scope ?name ~size (Eio_react.E.of_stream stream)
    | Some `Site -> create_stateless ?name ~size stream

  let external_channel ?(history = 1) ?(newest = false) ~prefix ~name () =
    let service =
      Eliom_service.extern ~prefix ~path:comet_global_path
        ~meth:
          (Eliom_service.Post
             ( Eliom_parameter.unit
             , Eliom_parameter.(
                 bool "idle" ** Eliom_comet_base.comet_request_param) ))
        ()
    in
    let last = if newest then None else Some history in
    { channel =
        External
          (Eliom_comet_base.Stateless_channel
             ( Eliom_comet_base.Comet_service (service, ref [])
             , Stateless.chan_id_of_string name
             , Eliom_comet_base.Last_kind last ))
    ; channel_mark = channel_mark () }

  let wait_timeout = Stateful.wait_timeout
end
