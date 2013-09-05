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

(* This file is for client-side comet-programming. *)

open Eliom_lib
module Ecb = Eliom_comet_base

module Configuration =
struct
  type configuration_data =
      { active_until_timeout : bool;
        time_between_request_unfocused : (float * float) list option;
        (* (a, b) for a * t + b
           (0, 0) means always active
           None means: no request
           The list is here if there are several configurations
           (we take the min of all values, for a given t)
        *)
        time_after_unfocus : float;
        time_between_request : float; }

  let default_configuration =
    { active_until_timeout = false;
      time_between_request_unfocused = (Some [(0.0078125, 0.)]);
      time_after_unfocus = 180.;
      time_between_request = 0.; }

  type t = int

  type configurations = (int, configuration_data) Hashtbl.t

  let configuration_table = Hashtbl.create 1

  let global_configuration = ref default_configuration

  let config_min c1 c2 =
    { active_until_timeout = c1.active_until_timeout || c2.active_until_timeout;
      time_between_request_unfocused =
        (match
            c1.time_between_request_unfocused, c2.time_between_request_unfocused
         with
           | Some l1, Some l2 -> Some (l1@l2)
           | Some v, None | None, Some v -> Some v
           | None, None -> None);
      time_after_unfocus = max c1.time_after_unfocus c2.time_after_unfocus;
      time_between_request = min c1.time_between_request c2.time_between_request }

  exception C of configuration_data

  let first_conf c =
    try
      ignore (Hashtbl.fold (fun _ v -> raise (C v)) c ());
      assert false
    with
      | C v -> v

  let get_configuration () =
    if Hashtbl.length configuration_table = 0
    then default_configuration
    else Hashtbl.fold (fun _ -> config_min) configuration_table (first_conf configuration_table)

  let update_configuration_waiter, update_configuration_waker =
    let t,u = Lwt.wait () in
    ref t, ref u

  let update_configuration () =
    global_configuration := get_configuration ();
    let t,u = Lwt.wait () in
    update_configuration_waiter := t;
    let wakener = !update_configuration_waker in
    update_configuration_waker := u;
    Lwt.wakeup wakener ()

  let get () = !global_configuration

  let drop_configuration c =
    Hashtbl.remove configuration_table c;
    update_configuration ()

  let new_configuration : unit -> t =
    let r = ref 0 in
    ( fun () ->
      incr r;
      Hashtbl.add configuration_table !r default_configuration;
      update_configuration ();
      !r )

  let set_fun c f =
    try
      Hashtbl.replace configuration_table c ( f (Hashtbl.find configuration_table c));
      update_configuration ();
    with
      | Not_found -> ()

  let set_always_active conf v =
    set_fun conf (fun c -> { c with time_between_request_unfocused =
        if v then Some [0., 0.] else None })

  let set_timeout conf v =
    set_fun conf (fun c -> { c with time_after_unfocus = v })

  let set_active_until_timeout conf v =
    set_fun conf (fun c -> { c with active_until_timeout = v })

  let set_time_between_requests conf v =
    set_fun conf (fun c -> { c with time_between_request = v })

  let set_time_between_requests_when_idle conf v =
    set_fun conf (fun c -> { c with time_between_request_unfocused = Some [v] })

  let sleep_before_next_request focused is_idle active_waiter =
    let time = Sys.time () in
    let sleep_duration () = if is_idle ()
      then (match (get ()).time_between_request_unfocused, focused () with
        | Some ((a, b)::l), Some start ->
          let now = Js.to_float (jsnew Js.date_now ())##getTime() in
          let t = (now -. start) *. 0.001 in (* time from idle start *)
          let v = a *. t +. b in
          let v = List.fold_left (fun v (a, b) -> min v (a *. t +. b)) v l in
          v
        | _ -> 0. (* Configuration changed.
                     We do not sleep and we'll see later. (?) *))
      else (get ()).time_between_request
    in
    let rec aux t =
      lwt () = Lwt.pick [Lwt_js.sleep t;
                         !update_configuration_waiter;
                         (active_waiter ())] in
      let remaining_time = sleep_duration () -. (Sys.time () -. time) in
      if remaining_time > 0.
      then aux remaining_time
      else Lwt.return ()
    in
    let sleep_duration = sleep_duration () in
    if sleep_duration <= 0.
    then Lwt.return ()
    else aux sleep_duration

end


exception Restart
exception Process_closed
exception Channel_closed
exception Channel_full
exception Comet_error of string

type chan_id = string

type stateful_message = ( chan_id * string Ecb.channel_data ) list
type stateless_message = ( chan_id * (string * int) Ecb.channel_data ) list

module Service_handler : sig

  type 'a t

  type 'a kind
  type stateless
  type stateful

  val stateless : stateless kind
  val stateful : stateful kind

  val make : Ecb.comet_service -> 'a kind -> 'a t

  val wait_data : 'a t -> (chan_id * int option * string Ecb.channel_data) list Lwt.t
  (** Returns the messages received in the last request. If the
      channel is stateless, it also returns the message number in the [int option] *)

  val set_activity : 'a t -> [ `Active | `Inactive | `Idle ] -> unit

  val activate : 'a t -> unit

  val is_active : 'a t -> [ `Active | `Inactive | `Idle ]

  val restart : 'a t -> unit

  val add_channel_stateful : stateful t -> chan_id -> unit

  val add_channel_stateless : stateless t -> chan_id -> Ecb.stateless_kind -> unit

  val stop_waiting : stateful t -> chan_id -> unit
  val close : 'a t -> chan_id -> unit

end =
struct
  type activity =
      {
        mutable active : [ `Inactive | `Active | `Idle ];
        (** [!hd.active] is true when the [hd] channel handler is
            receiving data.
            Idle means that the window is not active but we want to
            keep updated from time to time. *)
        mutable focused : float option;
        (** [focused] is None when the page is focused and Some [t]
            when the page lost focus at time [t] (in ms) *)
        mutable active_waiter : unit Lwt.t;
        (** [active_waiter] terminates when the page get focused *)
        mutable active_wakener : unit Lwt.u;
        mutable restart_waiter : unit list Lwt.t;
        mutable restart_wakener : unit list Lwt.u;
        mutable active_channels : String.Set.t;
      }

  type stateful_state = int ref (* id of the next request *)

  type stateless_state_ = {
    count : int;
    position : Ecb.position;
  }

  type stateless_state = (stateless_state_ String.Table.t) ref (* index for each channel of the last message *)

  type channel_state =
    | Stateful_state of stateful_state
    | Stateless_state of stateless_state

  type kind' =
    | Stateful
    | Stateless

  type 'a kind = kind'
  type stateless
  type stateful

  let stateless : stateless kind = Stateless
  let stateful : stateful kind = Stateful

  type 'a t =
      {
        hd_service : Ecb.comet_service;
        hd_state : channel_state;
        hd_kind : 'a kind;
        hd_activity : activity;
      }

  let add_focus_listener f : unit =
    let listener = Dom_html.handler (fun _ ->
      f (); Js.bool false) in
    (Js.Unsafe.coerce (Dom_html.window))##addEventListener(Js.string "focus",listener,Js.bool false)

  let add_blur_listener f : unit =
    let listener = Dom_html.handler (fun _ ->
      f (); Js.bool false) in
    (Js.Unsafe.coerce (Dom_html.window))##addEventListener(Js.string "blur",listener,Js.bool false)

  let set_activity hd v =
    if String.Set.is_empty hd.hd_activity.active_channels
    then hd.hd_activity.active <- `Inactive
    else
      match v with
        | `Inactive -> hd.hd_activity.active <- `Inactive
        | _ ->
          hd.hd_activity.active <- v;
          let t,u = Lwt.wait () in
          hd.hd_activity.active_waiter <- t;
          let wakener = hd.hd_activity.active_wakener in
          hd.hd_activity.active_wakener <- u;
          Lwt.wakeup wakener ()

  let is_active hd = hd.hd_activity.active

  (** register callbacks to 'blur' and 'focus' events of the root
      window. That way, we can tell when the client is active or not and do
      calls to the server only if it is active *)
  let handle_focus handler =
    let focus_callback () =
      handler.hd_activity.focused <- None;
      set_activity handler `Active
    in
    let blur_callback () =
      handler.hd_activity.focused <-
        Some (Js.to_float (jsnew Js.date_now ())##getTime())
    in
    add_focus_listener focus_callback;
    add_blur_listener blur_callback

  let activate hd =
    set_activity hd `Active

  let restart hd =
    let act = hd.hd_activity in
    let t,u = Lwt.wait () in
    act.restart_waiter <- t;
    let wakener = act.restart_wakener in
    act.restart_wakener <- u;
    Lwt.wakeup_exn wakener Restart;
    activate hd

  let max_retries = 5

  let call_service_after_load_end service p1 p2 =
    lwt () = Eliom_client.wait_load_end () in
    Eliom_client.call_service service p1 p2

  let make_request hd =
    match hd.hd_state with
      | Stateful_state count -> (Ecb.Stateful (Ecb.Request_data !count))
      | Stateless_state map ->
        let l = String.Table.fold (fun channel { position } l -> (channel,position)::l) !map [] in
        Ecb.Stateless (Array.of_list l)

  let stop_waiting hd chan_id =
    hd.hd_activity.active_channels <- String.Set.remove chan_id hd.hd_activity.active_channels;
    if String.Set.is_empty hd.hd_activity.active_channels
    then set_activity hd `Inactive

  let update_stateful_state hd message =
    match hd.hd_state with
      | Stateful_state r ->
        incr r;
        List.iter (function
          | ( chan_id, Ecb.Data _ ) -> ()
          | ( chan_id, Ecb.Closed ) ->
            debug "Eliom_comet.update_stateful_state: received Closed: should not happen, this is an eliom bug, please report it"
          | ( chan_id, Ecb.Full ) ->
            stop_waiting hd chan_id) message
      | Stateless_state _ ->
        raise (Comet_error ("update_stateful_state on stateless one"))

  let set_position pos value =
    match pos with
      | Ecb.Newest _ -> Ecb.Newest value
      | Ecb.After _ -> Ecb.After value
      | Ecb.Last None -> Ecb.Newest value
      | Ecb.Last (Some _) -> Ecb.After value

  let position_value pos =
    match pos with
      | Ecb.Newest i
      | Ecb.After i -> i
      | Ecb.Last _ -> 0

  let update_stateless_state hd (message:stateless_message) =
    match hd.hd_state with
      | Stateless_state r ->
        let table =
          List.fold_left
            (fun table -> function
            | ( chan_id, Ecb.Data (_,index)) ->
              (try
                 let state = String.Table.find chan_id table in
                 if position_value state.position < index + 1
                 then String.Table.add chan_id
                   { state with position = (set_position state.position (index + 1)) }
                   table
                 else table
               with
                 | Not_found -> table)
            | ( chan_id, Ecb.Closed )
            | ( chan_id, Ecb.Full ) ->
              stop_waiting hd chan_id;
              String.Table.remove chan_id table)
            !r message
        in
        r := table
      | Stateful_state _ ->
        raise (Comet_error ("update_stateless_state on stateful one"))

  let call_service hd =
    lwt () = Configuration.sleep_before_next_request
      (fun () -> hd.hd_activity.focused)
      (fun () -> hd.hd_activity.active = `Idle)
      (fun () -> hd.hd_activity.active_waiter)
    in
    let request = make_request hd in
    lwt s = call_service_after_load_end hd.hd_service () request in
    Lwt.return (Ecb.Json_answer.from_string s)

  let drop_message_index =
    let aux = function
      | chan, Ecb.Data (m,i) -> ( chan, Some i, Ecb.Data m )
      | chan, (Ecb.Closed as m)
      | chan, (Ecb.Full as m) -> chan, None, m
    in
    List.map aux

  let add_no_index =
    let aux = function
      | chan, (Ecb.Data _ as m)
      | chan, (Ecb.Closed as m)
      | chan, (Ecb.Full as m) -> chan, None, m
    in
    List.map aux

  let update_activity ?(timeout=false) hd =
    match hd.hd_activity.focused with
      | None -> ()
      | Some t ->
        let tbru =
          (Configuration.get ()).Configuration.time_between_request_unfocused
        in
        if not (tbru = Some [0., 0.]) (* if not always active *)
        then begin
          let now = Js.to_float (jsnew Js.date_now ())##getTime() in
          if now -. t >
            (Configuration.get ()).Configuration.time_after_unfocus *. 1000.
          then
            if timeout
              || not (Configuration.get ()).Configuration.active_until_timeout
            then set_activity hd (if tbru = None then `Inactive else `Idle)
        end

  let wait_data hd : (string * int option * string Ecb.channel_data) list Lwt.t =
    let rec aux retries =
      if hd.hd_activity.active = `Inactive
      then
        lwt () = hd.hd_activity.active_waiter in
        aux 0
      else
	begin
	  try_lwt
	    lwt s = Lwt.pick [call_service hd;
			      hd.hd_activity.restart_waiter
			      >>= (fun _ -> error "Eliom_comet: should not happen")] in
	    match s with
	      | Ecb.Timeout ->
		update_activity ~timeout:true hd;
		aux 0
	      | Ecb.Process_closed -> Lwt.fail Process_closed
	      | Ecb.Comet_error e -> Lwt.fail (Comet_error e)
	      | Ecb.Stateless_messages l ->
                let l = Array.to_list l in
		update_stateless_state hd l;
		Lwt.return (drop_message_index l)
	      | Ecb.Stateful_messages l ->
                let l = Array.to_list l in
		update_stateful_state hd l;
		Lwt.return (add_no_index l)
          with
            | Eliom_request.Failed_request 0 ->
              if retries > max_retries
              then
                (debug "Eliom_comet: connection failure";
                 set_activity hd `Inactive;
                 aux 0)
              else
                (Lwt_js.sleep 0.05 >>= (fun () -> aux (retries + 1)))
            | Restart -> debug "Eliom_comet: restart";
              aux 0
            | e -> debug "Eliom_comet: exception %s" (Printexc.to_string e); Lwt.fail e
        end
    in
    update_activity hd;
    aux 0

  let call_commands hd command =
    ignore
      (try_lwt
          call_service_after_load_end hd.hd_service ()
            (Ecb.Stateful (Ecb.Commands command))
       with
         | e -> debug "Eliom_comet: request failed: exception %s" (Printexc.to_string e);
           Lwt.return "")

  let close hd chan_id =
    match hd.hd_state with
      | Stateful_state _ ->
        stop_waiting hd chan_id;
        call_commands hd [|Ecb.Close chan_id|]
      | Stateless_state map ->
        try
          let state = String.Table.find chan_id !map in
          if state.count = 1
          then map := String.Table.remove chan_id !map
          else map := String.Table.add chan_id
            { state with count = state.count - 1 } !map
        with
          | Not_found ->
            debug "Eliom_comet: trying to close a non existent channel: %s" chan_id

  let add_channel_stateful hd chan_id =
    hd.hd_activity.active_channels <- String.Set.add chan_id hd.hd_activity.active_channels;
    call_commands hd [|Eliom_comet_base.Register chan_id|]

  let min_pos = function
    | Ecb.Newest i, Ecb.Newest j -> Ecb.Newest (min i j)
    | Ecb.After i, Ecb.After j -> Ecb.After (min i j)
    | Ecb.Last i, Ecb.Last j -> Ecb.Last (max i j)
    | p, Ecb.Last _ -> p
    | Ecb.Last _, p -> p
    | _ -> error "Eliom_comet: not corresponding position"

  let add_channel_stateless hd chan_id kind =
    let pos =
      match kind with
        | Ecb.Newest_kind i -> Ecb.Newest i
        | Ecb.After_kind i -> Ecb.After i
        | Ecb.Last_kind i -> Ecb.Last i
    in
    hd.hd_activity.active_channels <- String.Set.add chan_id hd.hd_activity.active_channels;
    match hd.hd_state with
      | Stateful_state _ -> assert false
      | Stateless_state map ->
        begin
          let state =
            try
              let old_state = String.Table.find chan_id !map in
              let pos = min_pos (old_state.position,pos) in
              { count = old_state.count + 1;
                position = pos }
            with
              | Not_found ->
                { count = 1;
                  position = pos }
          in
          map := String.Table.add chan_id state !map;
        end;
        restart hd

  let stop_waiting hd chan_id =
    hd.hd_activity.active_channels <- String.Set.remove chan_id hd.hd_activity.active_channels;
    if String.Set.is_empty hd.hd_activity.active_channels
    then set_activity hd `Inactive

  let init_activity () =
    let active_waiter,active_wakener = Lwt.wait () in
    let restart_waiter, restart_wakener = Lwt.wait () in
    {
      active = `Inactive;
      focused = None;
      active_waiter; active_wakener;
      restart_waiter; restart_wakener;
      active_channels = String.Set.empty;
    }

  let make hd_service hd_kind =
    let hd_state = match hd_kind with
        | Stateless -> Stateless_state (ref String.Table.empty)
        | Stateful -> Stateful_state (ref 0)
    in
    let hd = {
      hd_service;
      hd_state;
      hd_kind;
      hd_activity = init_activity ();
    } in
    handle_focus hd;
    hd

end

type 'a handler =
    { hd_service_handler : 'a Service_handler.t;
      hd_stream : ( string * int option * string Ecb.channel_data ) Lwt_stream.t; }

let handler_stream hd =
  Lwt_stream.map_list (fun x -> x)
    (Lwt_stream.from (fun () ->
      lwt s = Service_handler.wait_data hd in Lwt.return (Some s)))

let stateful_handler_table : (Ecb.comet_service, Service_handler.stateful handler) Hashtbl.t
    = Hashtbl.create 1
let stateless_handler_table : (Ecb.comet_service, Service_handler.stateless handler) Hashtbl.t
    = Hashtbl.create 1

let init (service:Ecb.comet_service) kind table =
  let hd_service_handler = Service_handler.make service kind in
  let hd_stream = handler_stream hd_service_handler in
  let hd = { hd_service_handler;
             hd_stream; } in
  Hashtbl.add table service hd;
  hd

let get_stateful_hd (service:Ecb.comet_service) : Service_handler.stateful handler =
  try
    Hashtbl.find stateful_handler_table service
  with
    | Not_found -> init service Service_handler.stateful stateful_handler_table

let get_stateless_hd (service:Ecb.comet_service) : Service_handler.stateless handler =
  try
    Hashtbl.find stateless_handler_table service
  with
    | Not_found -> init service Service_handler.stateless stateless_handler_table

let activate () =
  let f _ { hd_service_handler } = Service_handler.set_activity hd_service_handler `Active in
  Hashtbl.iter f stateless_handler_table;
  Hashtbl.iter f stateful_handler_table

let restart () =
  let f _ { hd_service_handler } = Service_handler.restart hd_service_handler in
  Hashtbl.iter f stateless_handler_table;
  Hashtbl.iter f stateful_handler_table

let close_stateful chan_service chan_id =
  let { hd_service_handler } = get_stateful_hd chan_service in
  Service_handler.close hd_service_handler (Ecb.string_of_chan_id chan_id)

let close = function
  | Ecb.Stateful_channel (chan_service,chan_id) ->
    let { hd_service_handler } = get_stateful_hd chan_service in
    Service_handler.close hd_service_handler (Ecb.string_of_chan_id chan_id)
  | Ecb.Stateless_channel (chan_service,chan_id,kind) ->
    let { hd_service_handler } = get_stateless_hd chan_service in
    Service_handler.close hd_service_handler (Ecb.string_of_chan_id chan_id)

let unmarshal s : 'a = Eliom_unwrap.unwrap (Url.decode s) 0

type position_relation =
  | Equal   (* stateless after channels *)
  | Greater (* stateless newest channels *)

type position =
  | No_position  (* stateful channels*)
  | Position of position_relation * ( int option ref ) (* stateless channels *)

let position_of_kind = function
  | Ecb.After_kind i -> Position (Equal, (ref (Some i)))
  | Ecb.Newest_kind i -> Position (Greater, (ref (Some i)))
  | Ecb.Last_kind None -> Position (Greater, ref None)
  | Ecb.Last_kind (Some _) -> Position (Equal, ref None)

let check_and_update_position position msg_pos data =
  match position, msg_pos, data with
    | No_position, None, _ -> true
    | No_position, Some _, _
    | Position _, None, Ecb.Data _ ->
      error "Eliom_comet: check_position: channel kind and message do not match"
    | Position _, None, (Ecb.Full | Ecb.Closed ) -> true
    | Position (relation,r), Some j, _ ->
      match !r with
        | None -> r := Some (j+1); true
        | Some i ->
          if (match relation with
            | Equal -> j = i
            | Greater -> j >= i)
          then (r := Some (j+1); true )
          else false

(* stateless channels are registered with a position: when a channel
   is registered more than one time, it is possible to receive old
   messages: the position is used to filter them out. *)
let register' hd position (chan_service:Ecb.comet_service) (chan_id:'a Ecb.chan_id) =
  let chan_id = Ecb.string_of_chan_id chan_id in
  let stream = Lwt_stream.filter_map_s
    (function
      | (id,pos,data) when
          id = chan_id
          && check_and_update_position position pos data ->
        ( match data with
          | Ecb.Full ->
            Lwt.fail Channel_full
          | Ecb.Closed ->
            Lwt.fail Channel_closed
          | Ecb.Data x ->
            Lwt.return (Some (unmarshal x:'a)))
      | _ -> Lwt.return None)
    (Lwt_stream.clone hd.hd_stream)
  in
  let protect_and_close t =
    let t' = Lwt.protected t in
    Lwt.on_cancel t' (fun () -> Service_handler.close hd.hd_service_handler chan_id);
    t'
  in
  (* protect the stream from cancels *)
  Lwt_stream.from (fun () -> protect_and_close (Lwt_stream.get stream))

let register_stateful ?(wake=true) service chan_id =
  let hd = get_stateful_hd service in
  let stream = register' hd No_position service chan_id in
  let chan_id = Ecb.string_of_chan_id chan_id in
  Service_handler.add_channel_stateful hd.hd_service_handler chan_id;
  if wake then Service_handler.activate hd.hd_service_handler;
  stream

let register_stateless ?(wake=true) service chan_id kind =
  let hd = get_stateless_hd service in
  let stream = register' hd (position_of_kind kind) service chan_id in
  let chan_id = Ecb.string_of_chan_id chan_id in
  Service_handler.add_channel_stateless hd.hd_service_handler chan_id kind;
  if wake then Service_handler.activate hd.hd_service_handler;
  stream

let register ?(wake=true) (wrapped_chan:'a Ecb.wrapped_channel) =
  match wrapped_chan with
    | Ecb.Stateful_channel (s,c) ->
      register_stateful ~wake s c
    | Ecb.Stateless_channel (s,c,kind) ->
      register_stateless ~wake s c kind

let internal_unwrap ( wrapped_chan, unwrapper ) = register wrapped_chan

let () = Eliom_unwrap.register_unwrapper Eliom_common.comet_channel_unwrap_id internal_unwrap

let is_active () =
(*VVV Check. Isn't it the contrary? (fold from `Inactive?) *)
  let max a b = match a with
    | `Inactive -> `Inactive
    | `Idle -> let b = b () in
               if b = `Active then `Idle else b
    | `Active -> b ()
  in
  let f _ hd active =
    max active (fun () -> Service_handler.is_active hd.hd_service_handler)
  in
  max (Hashtbl.fold f stateless_handler_table `Active)
    (fun () -> Hashtbl.fold f stateful_handler_table `Active)

module Channel =
struct
  type 'a t = 'a Lwt_stream.t
end

let force_link = ()
