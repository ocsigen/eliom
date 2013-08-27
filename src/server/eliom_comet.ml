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

open Eliom_lib

(* Shortening names of modules *)
module OFrame  = Ocsigen_http_frame
module OStream = Ocsigen_stream
module OMsg    = Ocsigen_messages
module Ecb     = Eliom_comet_base

type chan_id = string

let encode_downgoing s =
  Eliom_comet_base.Json_answer.to_string
    (Eliom_comet_base.Stateful_messages (Array.of_list s))

let encode_global_downgoing s =
  Eliom_comet_base.Json_answer.to_string
    (Eliom_comet_base.Stateless_messages (Array.of_list s))

let timeout_msg =
 Eliom_comet_base.Json_answer.to_string Eliom_comet_base.Timeout
let process_closed_msg =
  Eliom_comet_base.Json_answer.to_string Eliom_comet_base.Process_closed
let error_msg s =
  Eliom_comet_base.Json_answer.to_string (Eliom_comet_base.Comet_error s)

let json_content_type = "application/json"

exception New_connection

module Comet_param =
struct
  type page = string
  let translate content = Lwt.return (content,json_content_type)
end
module Comet =
  Eliom_registration.Customize ( Eliom_registration.String ) ( Comet_param )

let comet_path = ["__eliom_comet__"]
let comet_global_path = ["__eliom_comet_global__"]

let fallback_service =
  Eliom_common.lazy_site_value_from_fun
    (fun () -> Comet.register_service ~path:comet_path
      ~get_params:Eliom_parameter.unit
      (fun () () -> Lwt.return process_closed_msg))

let fallback_global_service =
  Eliom_common.lazy_site_value_from_fun
    (fun () -> Comet.register_service ~path:comet_global_path
      ~get_params:Eliom_parameter.unit
      (fun () () -> Lwt.return (error_msg "request with no post parameters, or there isn't any registered site comet channel")))

let new_id = make_cryptographic_safe_string

(* ocsigenserver needs to be modified for this to be configurable:
   the connection is closed after a fixed amount of time
   if the server does not send anything.
   By default it is 30 seconds *)
let timeout = 20.

module Stateless : sig

  type channel

  val create : ?name:string -> size:int -> string Lwt_stream.t -> channel

  val get_id : channel -> string

  val get_service : unit -> Ecb.comet_service

  val get_kind : newest:bool -> channel -> Ecb.stateless_kind

  val chan_id_of_string : string -> 'a Ecb.chan_id

end =
struct

  type channel_id = string

  module Dlist = Ocsigen_cache.Dlist

  type channel = {
    ch_id : channel_id;
    mutable ch_index : int; (* the number of messages already added to the channel *)
    ch_content : (string * int) Dlist.t;
    ch_wakeup : unit Lwt_condition.t; (* condition broadcasted when there is a new message *)
  }

  module Channel_hash =
  struct
     type t = channel
     let equal c1 c2 = c1.ch_id = c2.ch_id
     let hash c = Hashtbl.hash c.ch_id
  end

  module Weak_channel_table = Weak.Make(Channel_hash)

  let channels = Weak_channel_table.create 0

  let find_channel =
    let dummy_channel =
      { ch_id = "";
	ch_index = 0;
	ch_content = Dlist.create 1;
	ch_wakeup = Lwt_condition.create (); }
    in
    fun ch_id ->
      let dummy = { dummy_channel with ch_id = ch_id } in
      try
	Some (Weak_channel_table.find channels dummy)
      with
	| Not_found ->
	  None

  let wakeup_waiters channel =
    Lwt_condition.broadcast channel.ch_wakeup ()

  (* fill the channel with messages from the stream *)
  let run_channel channel stream =
    let channel' = Weak.create 1 in
    Weak.set channel' 0 (Some channel);
    let channel = channel' in
    (* hide non weak reference to be sure not to keep a strong reference *)
    let f msg =
      match Weak.get channel 0 with
	| None ->
	  raise_lwt Not_found
	  (* terminates the loop: remove reference on the stream, etc ... *)
	| Some channel ->
	  channel.ch_index <- succ channel.ch_index;
	  ignore (Dlist.add (msg,channel.ch_index) channel.ch_content: 'a option);
	  wakeup_waiters channel;
	  Lwt.return ()
    in
    ignore (Lwt_stream.iter_s f stream:unit Lwt.t)

  let make_name name = "stateless:"^name
  let chan_id_of_string name = Ecb.chan_id_of_string (make_name name)

  let create ?(name=new_id ()) ~size stream =
    let name = make_name name in
    let channel =
      { ch_id = name;
	ch_index = 0;
	ch_content = Dlist.create size;
	ch_wakeup = Lwt_condition.create () }
    in
    run_channel channel stream;
    match find_channel name with
      | Some _ ->
	failwith (Printf.sprintf "can't create channel %s: a channel with the same name already exists" name)
      | None ->
	Weak_channel_table.add channels channel;
	channel

  let get_channel (ch_id,position) =
    match find_channel ch_id with
      | Some channel -> Left (channel,position)
      | None -> Right ch_id

  exception Finished of (channel_id * (string * int) Ecb.channel_data) list

  let queue_take channel last =
    try
      Dlist.fold
	(fun l (v,index) ->
	  if index >= last
	  then (channel.ch_id,Ecb.Data (v,index))::l
	  else raise (Finished l))
	[]
	channel.ch_content
    with
      | Finished l -> l

  let get_available_data = function
    | Right ch_id -> [ch_id, Ecb.Closed]
    | Left (channel,position) ->
      match position with
	(* the first request of the client should be with i = 1 *)
	(* when the client is requesting the newest data, only return
	   one if he don't already have it *)
	| Ecb.Newest i when i > channel.ch_index -> []
	| Ecb.Newest _
	| Ecb.Last None -> (* initialisation of external newest channels *)
	  (match Dlist.newest channel.ch_content with
	    | None -> [] (* should not happen *)
	    | Some node ->
	      [channel.ch_id,Ecb.Data (Dlist.value node)])
	(* when the client is requesting the data after index i return
	   all data with index gretter or equal to i*)
	| Ecb.After i when i > channel.ch_index -> []
	(* if the requested value is not in the queue anymore, tell
	   the client that its request was dropped *)
	| Ecb.After i when i <= channel.ch_index - (Dlist.size channel.ch_content) ->
	  [channel.ch_id,Ecb.Full]
	| Ecb.After i ->
	  queue_take channel i
	| Ecb.Last (Some n) ->
          let i = channel.ch_index - (min (Dlist.size channel.ch_content) n) in
	  queue_take channel i

  let has_data = function
    | Right _ -> true (* a channel was closed: need to tell it to the client *)
    | Left (channel, position) ->
      match position with
	| Ecb.Newest i when i > channel.ch_index -> false
	| Ecb.Newest i -> true
	| Ecb.After i when i > channel.ch_index -> false
	| Ecb.After i -> true
        | Ecb.Last n when (Dlist.size channel.ch_content) > 0 -> true
        | Ecb.Last n -> false

  let really_wait_data requests =
    let rec make_list = function
      | [] -> []
      | (Left (channel,_))::q -> (Lwt_condition.wait channel.ch_wakeup)::(make_list q)
      | (Right _)::q -> assert false (* closed channels are considered to have data *)
    in
    Lwt.pick (make_list requests)

  let wait_data requests =
    if List.exists has_data requests
    then Lwt.return ()
    else
      Lwt_unix.with_timeout timeout
	(fun () -> really_wait_data requests)

  let handle_request () = function
    | Ecb.Stateful _ -> failwith "attempting to request data on stateless service with a stateful request"
    | Ecb.Stateless requests ->
      let requests = List.map get_channel (Array.to_list requests) in
      lwt res =
	try_lwt
          lwt () = wait_data requests in
	  Lwt.return (List.flatten (List.map get_available_data requests))
        with
	  | Lwt_unix.Timeout -> Lwt.return []
      in
      Lwt.return (encode_global_downgoing res)

  let global_service =
    Eliom_common.lazy_site_value_from_fun
      (fun () -> Comet.register_post_service
(*VVV Why isn't this a POST non-attached coservice? --Vincent *)
	~fallback:(Eliom_common.force_lazy_site_value fallback_global_service)
	~post_params:Ecb.comet_request_param
	handle_request)

  let get_service () =
    Eliom_common.force_lazy_site_value global_service

  let get_id {ch_id} = ch_id

  let get_kind ~newest {ch_index} =
    if newest
    then Ecb.Newest_kind (ch_index + 1)
    else Ecb.After_kind (ch_index + 1)

end

module Stateful :
(** String channels on wich is build the module Channel *)
sig

  type t

  val create : ?scope:Eliom_common.client_process_scope ->
    ?name:chan_id -> string Ecb.channel_data Lwt_stream.t -> t

  val get_id : t -> string

  type comet_service = Ecb.comet_service

  val get_service : t -> comet_service

  val close_channel : t -> unit

  val wait_timeout : ?scope:Eliom_common.client_process_scope ->
    float -> unit Lwt.t

end = struct

  type chan_id = string

  type comet_service = Ecb.comet_service

  type internal_comet_service = Ecb.internal_comet_service

  type end_request_waiters = unit Lwt.u

  type activity =
    | Active of end_request_waiters list
    (** There is currently a request from the client *)
    | Inactive of float
    (** The last request from the client completed at that time *)

  type handler =
      {
	hd_scope : Eliom_common.client_process_scope;
	(* id : int; pour tester que ce sont des service differents... *)
	mutable hd_active_streams : ( chan_id * ( string Ecb.channel_data Lwt_stream.t ) ) list;
	(** streams that are currently sent to client *)
	mutable hd_unregistered_streams : ( chan_id * ( string Ecb.channel_data Lwt_stream.t ) ) list;
	(** streams that are created on the server side, but client did not register *)
	mutable hd_registered_chan_id : chan_id list;
	(** the fusion of all the streams from hd_active_streams *)
	mutable hd_update_streams : unit Lwt.t;
	(** thread that wakeup when there are new active streams. *)
	mutable hd_update_streams_w : unit Lwt.u;
	hd_service : internal_comet_service;
	mutable hd_last : string * int;
        (** the last message sent to the client, if he sends a request
	    with the same number, this message is immediately sent
	    back.*)
	mutable hd_activity : activity;
      }

  exception Connection_closed

  let set_active handler =
    match handler.hd_activity with
      | Active _ -> ()
      | Inactive _ -> handler.hd_activity <- Active []

  let set_inactive handler =
    match handler.hd_activity with
      | Active l ->
	handler.hd_activity <- Inactive (Unix.gettimeofday ());
	List.iter (fun waiter -> Lwt.wakeup waiter ()) l
      | Inactive _ -> ()

  let update_inactive handler =
    match handler.hd_activity with
      | Active _ -> ()
      | Inactive _ ->
	handler.hd_activity <- Inactive (Unix.gettimeofday ())

  let wait_handler_timeout handler t =
    let rec run () =
      match handler.hd_activity with
	| Active l ->
	  let waiter,waker = Lwt.task () in
	  let t =
	    lwt () = waiter in
	    lwt () = Lwt_unix.sleep t in
	    run ()
	  in
	  handler.hd_activity <- Active (waker::l);
	  t
	| Inactive inactive_time ->
	  let now = Unix.gettimeofday () in
	  if now -. inactive_time > t
	  then Lwt.return ()
	  else
	    lwt () = Lwt_unix.sleep (t -. (now -. inactive_time)) in
	    run ()
    in
    run ()

  (** called when a connection is opened, it makes the other
      connection terminate with no data. That way there is at most one
      opened connection to the service. There are new connection
      opened when the client wants to listen to new channels for
      instance. *)
  let new_connection handler =
    let t,w = Lwt.task () in
    let wakener = handler.hd_update_streams_w in
    handler.hd_update_streams <- t;
    handler.hd_update_streams_w <- w;
    set_active handler;
    Lwt.wakeup_exn wakener New_connection

  (** called when a new channel is made active. It restarts the thread
      wainting for inputs ( wait_data ) such that it can receive the messages from
      the new channel *)
  let signal_update handler =
    let t,w = Lwt.task () in
    let wakener = handler.hd_update_streams_w in
    handler.hd_update_streams <- t;
    handler.hd_update_streams_w <- w;
    Lwt.wakeup wakener ()

  let wait_streams streams =
    Lwt.pick (List.map (fun (_,s) -> Lwt_stream.peek s) streams)

  (** read up to [n] messages in the list of streams [streams] without blocking. *)
  let read_streams n streams =
    let rec aux acc n streams =
      match streams with
	| [] -> acc
	| (id,stream)::other_streams ->
	  match n with
	    | 0 -> acc
	    | _ ->
	      let l = Lwt_stream.get_available_up_to n stream in
	      let l' = List.map (fun v -> id,v) l in
	      let rest = n - (List.length l) in
	      aux (l'@acc) rest other_streams
    in
    aux [] n streams

  (** wait for data on any channel that the client asks. It correcly
      handles new channels the server creates after that the client
      registered them *)
  let rec wait_data handler =
    Lwt.choose
      [ Lwt.protected (wait_streams handler.hd_active_streams) >>= ( fun _ -> Lwt.return `Data );
	Lwt.protected (handler.hd_update_streams) >>= ( fun _ -> Lwt.return `Update ) ]
    >>= ( function
      | `Data -> Lwt.return ()
      | `Update -> wait_data handler )

  let launch_stream handler (chan_id,stream) =
    handler.hd_active_streams <- (chan_id,stream)::handler.hd_active_streams;
    signal_update handler

  let register_channel handler chan_id =
    OMsg.debug2 (Printf.sprintf "eliom: comet: register channel %s" chan_id);
    if not (List.mem_assoc chan_id handler.hd_active_streams)
    then
      try
	let stream = List.assoc chan_id handler.hd_unregistered_streams in
	handler.hd_unregistered_streams <-
	  List.remove_assoc chan_id handler.hd_unregistered_streams;
	launch_stream handler (chan_id,stream)
      with
	| Not_found ->
	  handler.hd_registered_chan_id <- chan_id::handler.hd_registered_chan_id

  let close_channel' handler chan_id =
    OMsg.debug2 (Printf.sprintf "eliom: comet: close channel %s" chan_id);
    handler.hd_active_streams <- List.remove_assoc chan_id handler.hd_active_streams;
    handler.hd_unregistered_streams <- List.remove_assoc chan_id handler.hd_unregistered_streams;
    handler.hd_registered_chan_id <- List.filter ((<>) chan_id) handler.hd_registered_chan_id;
    signal_update handler

  let wait_closed_connection () =
    let ri = Eliom_request_info.get_ri () in
    lwt () = ri.Ocsigen_extensions.ri_connection_closed in
    raise_lwt Connection_closed

  (* register the service handler.hd_service *)
  let run_handler handler =
    let f () req =
      match req with
      | Ecb.Stateless _ ->
	failwith "attempting to request data on stateful service with a stateless request"
      | Ecb.Stateful (Ecb.Request_data number) ->
	OMsg.debug2 (Printf.sprintf "eliom: comet: received request %i" number);
	(* if a new connection occurs for a service, we reply
	   immediately to the previous with no data. *)

	new_connection handler;
	if snd handler.hd_last = number
	then Lwt.return (fst handler.hd_last)
	else
	  Lwt.catch
	    (fun () -> Lwt_unix.with_timeout timeout
	      (fun () ->
                lwt () = Lwt.choose
                  [ wait_closed_connection ();
		    wait_data handler ] in
		let messages = read_streams 100 handler.hd_active_streams in
		let message = encode_downgoing messages in
		handler.hd_last <- (message,number);
		set_inactive handler;
		Lwt.return message ) )
	    (function
	      | New_connection -> Lwt.return (encode_downgoing [])
	      (* happens if an other connection has been opened on that service *)
	      (* CCC in this case, it would be beter to return code 204: no content *)
	      | Lwt_unix.Timeout ->
		set_inactive handler;
		Lwt.return timeout_msg
              | Connection_closed ->
                set_inactive handler;
                (* it doesn't matter what we do here *)
                raise_lwt Connection_closed
	      | e ->
		set_inactive handler;
		Lwt.fail e )
      | Ecb.Stateful (Ecb.Commands commands) ->
	update_inactive handler;
	List.iter (function
	  | Ecb.Register channel -> register_channel handler channel
	  | Ecb.Close channel -> close_channel' handler channel)
          (Array.to_list commands);
	      (* command connections are replied immediately by an
		 empty answer *)
	Lwt.return (encode_downgoing [])
    in
    Comet.register
      ~scope:handler.hd_scope
      ~service:handler.hd_service
      f


  (** For each scope there is a reference containing the handler. The
      reference itself are stocked in [handler_ref_table]. This table
      is never cleaned, but it is supposed that this won't be a
      problem as scope should be used in limited number *)

  (* as of now only `Client_process scope are handled: so we only stock scope_hierarchy *)
  type handler_ref_table = (Eliom_common.scope_hierarchy,handler option Eliom_reference.eref) Hashtbl.t
  let handler_ref_table : handler_ref_table = Hashtbl.create 1

  (* this is a hack for the create function not to return 'a Lwt.t
     type: This is needed because bus and react create the channel at
     wrapping time, where it is impossible to block *)
  let get_ref eref =
    match Lwt.state (Eliom_reference.get eref) with
      | Lwt.Return v -> v
      | _ ->
	failwith "Eliom_comet: accessing channel references should not be blocking: this is an eliom bug"

  let set_ref eref v =
    match Lwt.state (Eliom_reference.set eref v) with
      | Lwt.Return () -> ()
      | _ ->
	failwith "Eliom_comet: accessing channel references should not be blocking: this is an eliom bug"

  let get_handler_eref scope =
    let scope_hierarchy = Eliom_common_base.scope_hierarchy_of_scope scope in
    try
      Hashtbl.find handler_ref_table scope_hierarchy
    with
      | Not_found ->
	let eref = Eliom_reference.eref ~scope:(`Client_process scope_hierarchy) None in
	Hashtbl.add handler_ref_table scope_hierarchy eref;
	eref

  let get_handler scope =
    let eref = get_handler_eref scope in
    match get_ref eref with
      | Some t -> t
      | None ->
	begin
	  let hd_service =
	    (* CCC ajouter possibilité d'https *)
	    Eliom_service.post_coservice
(*VVV Why is it attached? --Vincent *)
	      ~fallback:(Eliom_common.force_lazy_site_value fallback_service)
	      (*~name:"comet" (* CCC faut il mettre un nom ? *)*)
	      ~post_params:Ecb.comet_request_param
	      ()
	  in
	  let hd_update_streams,hd_update_streams_w = Lwt.task () in
	  let handler = {
	    hd_scope = scope;
	    hd_active_streams = [];
	    hd_unregistered_streams = [];
	    hd_registered_chan_id = [];
	    hd_service;
	    hd_update_streams;
	    hd_update_streams_w;
	    hd_last = "", -1;
	    hd_activity = Inactive (Unix.gettimeofday ());
	  }
	  in
	  set_ref eref (Some handler);
	  run_handler handler;
	  handler
	end

  let wait_timeout ?(scope=Eliom_common.comet_client_process_scope) t =
    let hd = get_handler scope in
    wait_handler_timeout hd t

  type t =
      {
	ch_handler : handler;
	ch_id : chan_id;
        ch_stream : string Ecb.channel_data Lwt_stream.t;
      }

  let close_channel chan =
    close_channel' chan.ch_handler chan.ch_id

  let name_of_scope (scope:Eliom_common.user_scope) =
    let sp = Eliom_common.get_sp () in
    let name = Eliom_common.make_full_state_name
      ~sp ~secure:false (*VVV secure? *) ~scope in
    let pref = match scope with
      | `Session_group _ -> "sessiongroup:"
      | `Session _ -> "session:"
      | `Client_process _ -> "clientprocess:"
    in
    Eliom_common.make_full_cookie_name pref name

  let create ?(scope=Eliom_common.comet_client_process_scope)
      ?(name=new_id ()) stream =
    let name = (name_of_scope (scope:>Eliom_common.user_scope)) ^ name in
    let handler = get_handler scope in
    OMsg.debug2 (Printf.sprintf "eliom: comet: create channel %s" name);
    if List.mem name handler.hd_registered_chan_id
    then
      begin
	handler.hd_registered_chan_id <-
	  List.filter ((<>) name) handler.hd_registered_chan_id;
	launch_stream handler (name,stream)
      end
    else
      handler.hd_unregistered_streams <- (name,stream)::handler.hd_unregistered_streams;
    { ch_handler = handler;
      ch_stream = stream;
      ch_id = name; }

  let get_id {ch_id} =
    ch_id

  let get_service chan =
    (chan.ch_handler.hd_service :> comet_service)

end


module Channel :
sig

  type 'a t

  type comet_scope =
    [ Eliom_common.site_scope
    | Eliom_common.client_process_scope ]

  val create : ?scope:[< comet_scope ] ->
    ?name:string -> ?size:int -> 'a Lwt_stream.t -> 'a t

  val create_unlimited : ?scope:Eliom_common.client_process_scope ->
    ?name:string -> 'a Lwt_stream.t -> 'a t

  val create_newest : ?name:string -> 'a Lwt_stream.t -> 'a t

  val get_wrapped : 'a t -> 'a Ecb.wrapped_channel

  val external_channel : ?history:int -> ?newest:bool ->
    prefix:string -> name:string -> unit -> 'a t

  val wait_timeout : ?scope:Eliom_common.client_process_scope ->
    float -> unit Lwt.t

end = struct

  type 'a channel =
    | Stateless of Stateless.channel
    | Stateless_newest of Stateless.channel
    | Stateful of Stateful.t
    | External of 'a Ecb.wrapped_channel

  type 'a t = {
    channel : 'a channel;
    channel_mark : 'a t Eliom_common.wrapper;
  }

  let get_wrapped t =
    match t.channel with
      | Stateful channel ->
	Ecb.Stateful_channel
	  (Stateful.get_service channel,
	   Ecb.chan_id_of_string (Stateful.get_id channel))
      | Stateless channel ->
	Ecb.Stateless_channel
	  (Stateless.get_service (),
	   Ecb.chan_id_of_string (Stateless.get_id channel),
	   Stateless.get_kind ~newest:false channel)
      | Stateless_newest channel ->
	Ecb.Stateless_channel
	  (Stateless.get_service (),
	   Ecb.chan_id_of_string (Stateless.get_id channel),
	   Stateless.get_kind ~newest:true channel)
      | External wrapped -> wrapped

  let internal_wrap c =
    (get_wrapped c,Eliom_common.make_unwrapper Eliom_common.comet_channel_unwrap_id)

  let channel_mark () = Eliom_common.make_wrapper internal_wrap

  exception Halt

  (* TODO close on full *)
  let limit_stream ~size s =
    let open Lwt in
        let full = ref false in
        let closed = ref false in
        let count = ref 0 in
        let str, push = Lwt_stream.create () in
        let stopper,wake_stopper = wait () in
        let rec loop () =
          ( Lwt_stream.get s <?> stopper ) >>= function
            | Some x ->
              if !count >= size
              then (full := true;
                    ignore (Lwt_stream.get_available str);
                  (* flush the channel *)
                    return ())
              else (incr count; push (Some ( Ecb.Data x )); loop ())
            | None ->
              return ()
        in
        ignore (loop ():'a Lwt.t);
        let res = Lwt_stream.from (fun () ->
          if !full
          then
            if !closed
            then return None
            else ( closed := true;
                   return (Some Ecb.Full) )
          else (decr count;
                Lwt_stream.get str)) in
        Gc.finalise (fun _ -> wakeup_exn wake_stopper Halt) res;
        res

  let marshal (v:'a) =
    let wrapped = Eliom_wrap.wrap v in
    let value : 'a Eliom_types.eliom_comet_data_type = wrapped in
    (Url.encode ~plus:false
       (Marshal.to_string value []))

  let create_stateful_channel ?scope ?name stream =
    Stateful
      (Stateful.create ?scope ?name
	 (Lwt_stream.map
	    (function
	      | Ecb.Closed ->
		OMsg.debug2 (Printf.sprintf "eliom: closed in stateful channels: this is an error: this should not be possible");
		Ecb.Closed
	      | Ecb.Full -> Ecb.Full
	      | Ecb.Data s -> Ecb.Data (marshal s)) stream))

  let create_stateless_channel ?name ~size stream =
    Stateless
      (Stateless.create ?name ~size
	 (Lwt_stream.map marshal stream))

  let create_stateless_newest_channel ?name stream =
    Stateless_newest
      (Stateless.create ?name ~size:1
	 (Lwt_stream.map marshal stream))

  let create_stateful ?scope ?name ?(size=1000) stream =
    let stream = limit_stream ~size stream in
    { channel = create_stateful_channel ?scope ?name stream;
      channel_mark = channel_mark () }

  let create_unlimited ?scope ?name stream =
    let stream = Lwt_stream.map (fun x -> Ecb.Data x) stream in
    { channel = create_stateful_channel ?scope ?name stream;
      channel_mark = channel_mark () }

  let create_stateless ?name ?(size=1000) stream =
    { channel = create_stateless_channel ?name ~size stream;
      channel_mark = channel_mark () }

  let create_newest ?name stream =
    { channel = create_stateless_newest_channel ?name stream;
      channel_mark = channel_mark () }

  type comet_scope =
    [ Eliom_common.site_scope
    | Eliom_common.client_process_scope ]

  let create ?scope ?name ?(size=1000) stream =
    match scope with
      | None -> create_stateful ?name ~size stream
      | Some ((`Client_process n) as scope) -> create_stateful ~scope ?name ~size stream
      | Some `Site -> create_stateless ?name ~size stream

  let external_channel ?(history=1) ?(newest=false) ~prefix ~name () =
    let service = Eliom_service.external_post_service
      ~prefix
      ~path:comet_global_path
      ~get_params:Eliom_parameter.unit
      ~post_params:Ecb.comet_request_param
      ()
    in
    let last = if newest then None else Some history in
    { channel = External (Ecb.Stateless_channel
                            (service,
                             Stateless.chan_id_of_string name,
                             Ecb.Last_kind last));
      channel_mark = channel_mark () }

  let wait_timeout = Stateful.wait_timeout

end
