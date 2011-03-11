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

let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= )

(* Messages : encoding and decoding, comet protocol *)
module Messages : sig

  exception  Incorrect_encoding

  val decode_downcoming : string -> (string * string) list

end = struct

  exception  Incorrect_encoding

  (* constants *)
  let channel_separator = "\n"
  let field_separator = ":"
  let url_decode x = Ocsigen_lib.urldecode_string x

  (* decoding *)
  let chan_delim_regexp  = Regexp.make channel_separator
  let field_delim_regexp = Regexp.make field_separator

  let decode_downcoming s =
    match s with
      | "" -> []
      | s ->
	let splited = Regexp.split chan_delim_regexp s in
	let splited_twice =
	  Array.map
            (fun s ->
              match Regexp.split field_delim_regexp s with
		| [|chan; msg|] -> (chan, url_decode msg)
		| _ -> raise Incorrect_encoding
            )
            splited
	in
	Array.to_list splited_twice

end

module Configuration =
struct
  type configuration_data =
      { active_until_timeout : bool;
	always_active : bool;
	time_between_request : float; }

  let default_configuration =
    { active_until_timeout = false;
      always_active = false;
      time_between_request = 0.; }

  type t = int

  type configurations = (int,configuration_data) Hashtbl.t

  let configuration_table = Hashtbl.create 1

  let global_configuration = ref default_configuration

  let config_min c1 c2 =
    { active_until_timeout = c1.active_until_timeout || c2.active_until_timeout;
      always_active = c1.always_active || c2.always_active;
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

  let update_configuration_waiter,update_configuration_waker = 
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
    set_fun conf (fun c -> { c with always_active = v })

  let set_active_until_timeout conf v =
    set_fun conf (fun c -> { c with active_until_timeout = v })

  let set_time_between_request conf v =
    set_fun conf (fun c -> { c with time_between_request = v })

  let sleep_before_next_request () =
    let time = Sys.time () in
    let rec aux t =
      lwt () = Lwt.pick [Lwt_js.sleep t;
			 !update_configuration_waiter;] in
      let remaining_time = ( Sys.time () ) -. ( (get ()).time_between_request +. time ) in
      if remaining_time >= 0.
      then Lwt.return ()
      else aux remaining_time
    in
    if (get ()).time_between_request <= 0.
    then Lwt.return ()
    else aux ((get ()).time_between_request)

end

type activity =
    {
      mutable active : bool;
      (** [!hd.hd_active] is true when the page is focused on the
	  browser *)
      mutable focused : bool;
      mutable active_waiter : unit Lwt.t;
      (** [!hd.hd_active_waiter] terminates when the page become
	  focused *)
      mutable active_wakener : unit Lwt.u;
      mutable restart_waiter : string Lwt.t;
      mutable restart_wakener : string Lwt.u;
    }

type handler =
    {
      hd_service : Eliom_common_comet.comet_service;
      hd_stream : (string * string) Lwt_stream.t;
      (** the stream of all messages from the server *)
      hd_commands : Eliom_common_comet.command list -> unit;
      (** [hd.hd_commands commands] sends the commands to the
	  server. It launch a new request to the server
	  immediately. *)
      hd_activity : activity;
    }

let handler = ref None

let add_focus_listener f : unit =
  let listener = Dom_html.handler (fun _ -> 
    f (); Js.bool false) in
  (Js.Unsafe.coerce (Dom_html.window))##addEventListener(Js.string "focus",listener,Js.bool false)

let add_blur_listener f : unit =
  let listener = Dom_html.handler (fun _ -> 
    f (); Js.bool false) in
  (Js.Unsafe.coerce (Dom_html.window))##addEventListener(Js.string "blur",listener,Js.bool false)

let log f =
  Printf.ksprintf (fun s -> Firebug.console##log(Js.string s)) f

let set_activity activity v =
  match v with
    | true ->
      activity.active <- true;
      let t,u = Lwt.wait () in
      activity.active_waiter <- t;
      let wakener = activity.active_wakener in
      activity.active_wakener <- u;
      Lwt.wakeup wakener ()
    | false ->
      activity.active <- false

(** register callbacks to 'blur' and 'focus' events of the root
    window. That way, we can tell when the client is active or not and do
    calls to the server only if it is active *)
let handle_focus handler =
  let focus_callback () =
    handler.hd_activity.focused <- true;
    set_activity handler.hd_activity true
  in
  let blur_callback () =
    handler.hd_activity.focused <- false;
    if not ((Configuration.get ()).Configuration.active_until_timeout
	  || (Configuration.get ()).Configuration.always_active)
    then handler.hd_activity.active <- false
  in
  add_focus_listener focus_callback;
  add_blur_listener blur_callback

exception Restart
exception Timeout
exception Connection_lost

let max_retries = 5

(** wait for data from the server, if also waits until the page is
    focused to make the request *)
let wait_data service activity count =
  let call_service () =
    lwt () = Configuration.sleep_before_next_request () in
    lwt s = Eliom_client.call_service service () (Eliom_common_comet.Request_data !count) in
    match s with
      | "TIMEOUT" -> Lwt.fail Timeout
      | s -> Lwt.return s
  in
  let rec aux retries =
    if activity.active
    then
      begin
	try_lwt
	  lwt s = Lwt.choose [call_service ();
			      activity.restart_waiter ] in
          incr count;
          Lwt.return (Some s)
        with
	  | Eliom_request.Failed_request 0 ->
	    if retries > max_retries
	    then
	      (log "Eliom_client_comet: connection failure";
	       set_activity activity false;
	     aux 0)
	    else
	      (Lwt_js.sleep 0.05 >>= (fun () -> aux (retries + 1)))
	  | Restart -> log "Eliom_client_comet: restart";
	    aux 0
	  | Timeout ->
	    if not activity.focused
	    then
	      if not (Configuration.get ()).Configuration.always_active
	      then set_activity activity false;
	    aux 0
	  | e -> log "Eliom_client_comet: exception"; Lwt.fail e
      end
     else
       lwt () = activity.active_waiter in
       aux 0
  in
  fun () -> aux 0

let service () : Eliom_common_comet.comet_service =
  Eliom_client_unwrap.unwrap (Ocsigen_lib.unmarshal_js_var "comet_service")

let init_activity () =
  let active_waiter,active_wakener = Lwt.wait () in
  let restart_waiter, restart_wakener = Lwt.wait () in
  { 
    active = false;
    focused = true;
    active_waiter; active_wakener;
    restart_waiter; restart_wakener;
  }

let init () =
  (* This reference holds the number of the next request to do. It is
     incremented each time datas are received *)
  let count = ref 0 in
  let hd_activity = init_activity () in
  let hd_service = service () in
  (* the stream on wich are regularily received data from the server *)
  let normal_stream = Lwt_stream.from (wait_data hd_service hd_activity count) in
  (* the stream on wich are received replies of request asking to register new channels *)
  let exceptionnal_stream,push = Lwt_stream.create () in
  let hd_stream =
    Lwt_stream.map_list Messages.decode_downcoming
      ( Lwt_stream.choose [
	normal_stream;
	Lwt_stream.map_s (fun t -> t) exceptionnal_stream] ) in
  (* the function to register and close channels *)
  let hd_commands commands =
    let t =
      Eliom_client.call_service hd_service () 
	(Eliom_common_comet.Commands commands)
    in
    push (Some t);
  in
  let hd =
    { hd_service; hd_stream; hd_commands; hd_activity }
  in
  let _ = Lwt_stream.iter (fun _ -> ()) hd_stream in (* consumes all messages of the stream to avoid memory leaks *)
  handle_focus hd;
  handler := Some hd;
  hd

let get_hd () =
  match !handler with
    | None -> init ()
    | Some hd -> hd

let activate () = set_activity (get_hd ()).hd_activity true

let restart () =
  let act = (get_hd ()).hd_activity in
  let t,u = Lwt.wait () in
  act.restart_waiter <- t;
  let wakener = act.restart_wakener in
  act.restart_wakener <- u;
  Lwt.wakeup_exn wakener Restart;
  activate ()

let close' hd chan_id =
  hd.hd_commands [Eliom_common_comet.Close chan_id]

let close chan_id =
  let hd = get_hd () in
  close' hd (Eliom_common_comet.string_of_chan_id chan_id)

let register ?(wake=true) chan_id =
  let chan_id = Eliom_common_comet.string_of_chan_id chan_id in
  let hd = get_hd () in
  let stream = Lwt_stream.filter_map
    (function
      | (id,data) when id = chan_id -> 
	Some (Marshal.from_string data 0)
      | _ -> 
	None)
    (Lwt_stream.clone hd.hd_stream)
  in
  let protect_and_close t =
    let t' = Lwt.protected t in
    Lwt.on_cancel t' (fun () -> close' hd chan_id);
    t'
  in
  (* protect the stream from cancels *)
  let stream = Lwt_stream.from (fun () -> protect_and_close (Lwt_stream.get stream)) in
  hd.hd_commands [Eliom_common_comet.Register chan_id];
  if wake then activate ();
  stream

let unwrap ?wake key =
  let ( chan_id, unwrapper ) = Eliommod_cli.unwrap key in
  register ?wake chan_id

let internal_unwrap ( chan_id, unwrapper ) = register chan_id

let () = Eliom_client_unwrap.register_unwrapper Eliom_common.comet_channel_unwrap_id internal_unwrap


let is_active () = (get_hd ()).hd_activity.active

