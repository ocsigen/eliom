(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010-2011
 * Raphaël Proust
 * Pierre Chambart
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


(* This file is for client-side comet-programming. *)

let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= )

(* Messages : encoding and decoding, comet protocol *)
module Messages : sig

  exception  Incorrect_encoding

  val encode_upgoing : string list -> string
  val decode_downcoming : string -> (string * string) list

end = struct

  exception  Incorrect_encoding

  (* constants *)
  let channel_separator = "\n"
  let field_separator = ":"
  let url_decode x = Ocsigen_lib.urldecode_string x

  (* encoding *)
  let encode_upgoing = String.concat channel_separator

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
      mutable active_until_timeout : bool;
      mutable always_active : bool;
    }

type handler =
    {
      hd_service : Eliom_common_comet.comet_service;
      hd_stream : (string * string) Lwt_stream.t;
      (** the stream of all messages from the server *)
      hd_new_channels : string list -> unit;
      (** [hd.hd_new_channels chans] registers the channels [chans] to
	  the handler. It launch a new request to the server
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
    if not (handler.hd_activity.active_until_timeout
	  || handler.hd_activity.always_active)
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
let wait_data service activity =
  let call_service () =
    Eliom_client.call_service service () "" >>=
      (function
	| "TIMEOUT" -> Lwt.fail Timeout
	| s -> Lwt.return s)
  in
  let rec aux retries =
    log "call_service";
    if activity.active
    then
      Lwt.catch
	(fun () ->
	  Lwt.choose [
	    call_service ();
	    activity.restart_waiter ]
	  >>=
	   (fun s ->
	     Lwt.return (Some s)))
	(function
	  | Eliom_request.Failed_request 0 ->
	    if retries > max_retries
	    then
	      (log "Eliom_client_comet: connection failure";
	       set_activity activity false;
	       aux 0)
	    else
	      (Lwt_js.sleep 0.05 >>= (fun () -> aux (retries + 1)))
	  | Restart ->
	    aux 0
	  | Timeout ->
	    if not activity.focused
	    then
	      if not activity.always_active
	      then set_activity activity false;
	    aux 0
	  | e -> log "Eliom_client_comet: exception"; Lwt.fail e)
    else
      activity.active_waiter >>= (fun () -> aux 0)
  in
  fun () -> aux 0

let service () : Eliom_common_comet.comet_service =
  Eliommod_cli.unwrap (Ocsigen_lib.unmarshal_js_var "comet_service")

let init_activity () =
  let active_waiter,active_wakener = Lwt.wait () in
  let restart_waiter, restart_wakener = Lwt.wait () in
  { 
    active = false;
    focused = true;
    active_waiter; active_wakener;
    restart_waiter; restart_wakener;
    active_until_timeout = false;
    always_active = false
  }

let init () =
  let hd_activity = init_activity () in
  let hd_service = service () in
  (* the stream on wich are regularily received data from the server *)
  let normal_stream = Lwt_stream.from (wait_data hd_service hd_activity) in
  (* the stream on wich are received replies of request asking to register new channels *)
  let exceptionnal_stream,push = Lwt_stream.create () in
  let hd_stream = 
    Lwt_stream.map_list Messages.decode_downcoming
      ( Lwt_stream.choose [
	normal_stream;
	Lwt_stream.map_s (fun t -> t) exceptionnal_stream] ) in
  (* the function to register new channels *)
  let hd_new_channels chans =
    let chans = Messages.encode_upgoing chans in
    let t = Eliom_client.call_service hd_service () chans in
    push (Some t);
  in
  let hd =
    { hd_service; hd_stream; hd_new_channels; hd_activity }
  in
  let _ = Lwt_stream.iter (fun _ -> ()) hd_stream in (* consumes all messages of the stream to avoid memory leaks *)
  handle_focus hd;
  handler := Some hd;
  hd

let get_hd () =
  match !handler with
    | None -> init ()
    | Some hd -> hd

let add_channel handler channel =
  handler.hd_new_channels [channel]

let activate () = set_activity (get_hd ()).hd_activity true

let restart () = 
  let act = (get_hd ()).hd_activity in
  let t,u = Lwt.wait () in
  act.restart_waiter <- t;
  let wakener = act.restart_wakener in
  act.restart_wakener <- u;
  Lwt.wakeup_exn wakener Restart

let register chan_id =
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
  add_channel hd chan_id;
  activate ();
  stream

let unwrap chan_id =
  let chan_id = Eliommod_cli.unwrap chan_id in
  register chan_id

let active_until_timeout v = (get_hd ()).hd_activity.active_until_timeout <- v
let always_active v = (get_hd ()).hd_activity.always_active <- v

let is_active () = (get_hd ()).hd_activity.active
