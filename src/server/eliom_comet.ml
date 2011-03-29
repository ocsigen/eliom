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

open Eliom_pervasives

(* Shortening names of modules *)
module OFrame  = Ocsigen_http_frame
module OStream = Ocsigen_stream
module OMsg    = Ocsigen_messages
module Ecb     = Eliom_comet_base

(* infix monad binders *)
let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= ) (* AKA map, AKA lift *)

type chan_id = string

module Messages :
  (* All about messages from between clients and server *)
  (*
   * The server sends result to the client in the form of a list of :
   * channel_id ^ ":" ^ value ^ { ";" ^ channel_id ^ " " ^ value }*
   * where channel_id is the id of a channel that the client registered upon and
   * value is the string that was written upon the associated channel.
   * *)
sig

  val encode_downgoing :
    chan_id list
    -> (chan_id * string) list
    -> string
    (* Encode outgoing messages : the first argument is the list of channels
     * that have already been collected.
     * The results is the stream to send to the client*)

  val encode_ended : chan_id list -> string

end = struct

  (* constants *)
  let channel_separator = "\n"
  let field_separator = ":"
  let ended_message = "ENDED_CHANNEL"
  let channel_separator_regexp = Netstring_pcre.regexp channel_separator

  let url_encode x = Url.encode ~plus:false x

  let encode1 (c, s) =
    c ^ field_separator ^ url_encode s

  let encode l = String.concat channel_separator (List.map encode1 l)

  let encode_ended l =
    String.concat
      channel_separator
      (List.map (fun c -> c ^ field_separator ^ ended_message) l)

  let encode_downgoing ended_channels s =
    match ended_channels with
      | [] -> encode s
      | e -> encode_ended e
        ^ channel_separator
        ^ encode s

end

module Cometreg_ = struct
  open XHTML.M
  open XHTML_types
  open Ocsigen_http_frame

  type page = (string * string)

  type options = unit

  type return = Eliom_services.http

  let send_appl_content = Eliom_services.XAlways

  let code_of_code_option = function
    | None -> 200
    | Some c -> c

  let send ?options ?charset ?code 
      ?content_type ?headers content =
    Ocsigen_senders.Text_content.result_of_content content >>= fun r ->
    Lwt.return
      {r with
        res_cookies= Eliom_request_info.get_user_cookies ();
        res_code= code_of_code_option code;
        res_charset= (match charset with
          | None ->  Some (Eliom_config.get_config_default_charset ())
          | _ -> charset);
        res_content_type= (match content_type with
          | None -> r.res_content_type
          | _ -> content_type
        );
        res_headers= (match headers with
          | None -> r.res_headers
          | Some headers -> 
            Http_headers.with_defaults headers r.res_headers
        );
      }

end

module Comet = Eliom_mkreg.MakeRegister(Cometreg_)



module Raw_channels :
(** String channels on wich is build the module Channels *)
sig

  type t

  val create : ?name:chan_id -> string Lwt_stream.t -> t
  val get_id : t -> string

  type comet_service = Ecb.comet_service

  val get_service : unit -> comet_service
  val get_service_data_key : unit -> comet_service Eliom_types.data_key

  val close_channel : t -> unit

end = struct

  type chan_id = string

  type comet_service = Ecb.comet_service

  type handler =
      {
	(* id : int; pour tester que ce sont des service differents... *)
	mutable hd_active_streams : ( chan_id * ( string Lwt_stream.t ) ) list;
	(** streams that are currently sent to client *)
	mutable hd_unregistered_streams : ( chan_id * ( string Lwt_stream.t ) ) list;
	(** streams that are created on the server side, but client did not register *)
	mutable hd_registered_chan_id : chan_id list;
	(** the fusion of all the streams from hd_active_streams *)
	mutable hd_update_streams : unit Lwt.t;
	(** thread that wakeup when there are new active streams. *)
	mutable hd_update_streams_w : unit Lwt.u;
	hd_service : comet_service;
	hd_service_data_key : comet_service Eliom_types.data_key;
	mutable hd_last : string * int;
        (** the last message sent to the client, if he sends a request
	    with the same number, this message is immediately sent
	    back.*)
      }

  let handler_key : handler Polytables.key = Polytables.make_key ()

  type t =
      {
	ch_id : chan_id;
        ch_stream : string Lwt_stream.t;
      }

  exception New_connection

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

  let new_id = String.make_cryptographic_safe
  let content_type = "text/plain"

  let timeout = 20.
  
  (** Returns the handler for the current application.
      It is created if it does not exists. *)
  let get_handler () =
    let sp = Eliom_common.get_sp () in
    let cpi = Lazy.force sp.Eliom_common.sp_client_process_info in
    let table = cpi.Eliom_common.cpi_references in
    try
      Polytables.get ~table ~key:handler_key
    with
      | Not_found ->
	begin
	  let hd_service =
	    (* XXX ajouter possibilité d'https *)
	    Eliom_services.post_coservice'
	      ~name:"comet" (* VVV faut il mettre un nom ? *)
	      ~post_params:Ecb.comet_request_param
	      ()
	  in
	  let hd_service_data_key = Eliommod_cli.wrap hd_service in
	  let hd_update_streams,hd_update_streams_w = Lwt.task () in
	  let handler = {
	    hd_active_streams = [];
	    hd_unregistered_streams = [];
	    hd_registered_chan_id = [];
	    hd_service;
	    hd_service_data_key;
	    hd_update_streams;
	    hd_update_streams_w;
	    hd_last = "", -1;
	  }
	  in
	  let f () = function
	    | Ecb.Request_data number ->
	      OMsg.debug2 (Printf.sprintf "eliom: comet: received request %i" number);
	      (* if a new connection occurs for a service, we reply
		 immediately to the previous with no data. *)
	      new_connection handler;
	      if snd handler.hd_last = number
	      then Lwt.return (fst handler.hd_last,content_type)
	      else
		Lwt.catch
		  ( fun () -> Lwt_unix.with_timeout timeout
		    (fun () ->
		      wait_data handler >>= ( fun _ ->
			let messages = read_streams 100 handler.hd_active_streams in
			  let message = Messages.encode_downgoing [] messages in
			  handler.hd_last <- (message,number);
			  Lwt.return ( message, content_type ) ) ) )
		    ( function
		      | New_connection -> Lwt.return ("",content_type)
		      (* happens if an other connection has been opened on that service *)
		      (* CCC in this case, it would be beter to return code 204: no content *)
		      | Lwt_unix.Timeout -> Lwt.return ("TIMEOUT",content_type)
		      | e -> Lwt.fail e )
	    | Ecb.Commands commands ->
	      List.iter (function
		| Ecb.Register channel -> register_channel handler channel
		| Ecb.Close channel -> close_channel' handler channel) commands;
		(* command connections are replied immediately by an
		   empty answer *)
	      Lwt.return ("",content_type)
	  in
	  Comet.register
	    ~scope:`Client_process
	    ~service:hd_service
	    f;
	  Polytables.set ~table ~key:handler_key ~value:handler;
	  handler
	end

  let close_channel chan =
    let handler = get_handler () in
    close_channel' handler chan.ch_id

  let create ?(name=new_id ()) stream =
    let handler = get_handler () in
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
    { ch_stream = stream;
      ch_id = name; }

  let get_id { ch_id } = ch_id

  let get_service () =
    (get_handler ()).hd_service

  let get_service_data_key () =
    (get_handler ()).hd_service_data_key

end


module Channels :
sig

  type +'a t

  val create : ?name:string -> ?size:int -> 'a Lwt_stream.t -> 'a t
  val create_unlimited : ?name:string -> 'a Lwt_stream.t -> 'a t
  val wrap : 'a t -> ( 'a Ecb.chan_id * Eliom_common.unwrapper ) Eliom_types.data_key
  val get_id : 'a t -> 'a Ecb.chan_id

end = struct

  type +'a t = {
    channel : Raw_channels.t;
    channel_mark : 'a t Eliom_common.wrapper;
  }

  let get_id t =
    Ecb.chan_id_of_string (Raw_channels.get_id t.channel)

  let wrap c =
    Eliommod_cli.wrap (get_id c,Eliom_common.empty_unwrapper)

  let internal_wrap c =
    (get_id c,Eliom_common.make_unwrapper Eliom_common.comet_channel_unwrap_id)

  let channel_mark () = Eliom_common.make_wrapper internal_wrap

  exception Halt

  (* TODO close on full *)
  let limit_stream ~size s =
    let open Lwt in
	let count = ref 0 in
	let str, push = Lwt_stream.create () in
	let stopper,wake_stopper = wait () in
	let rec loop () =
	  ( Lwt_stream.get s <?> stopper ) >>= function
	    | Some x ->
	      if !count >= size
	      then (push (Some Ecb.Full); return ())
	      else (incr count; push (Some ( Ecb.Data x )); loop ())
	    | None ->
              return ()
	in
	let decount e = decr count; e in
	ignore (loop ():'a Lwt.t);
	let res = Lwt_stream.map decount str in
	Gc.finalise (fun _ -> wakeup_exn wake_stopper Halt) res;
	res

  let create_channel ?name stream =
    (* TODO: addapt channels to dynamic wrapping: it would be able to send more types *)
    Raw_channels.create ?name
      (Lwt_stream.map (fun x -> Marshal.to_string x []) stream)

  let create ?name ?(size=1000) stream =
    let stream = limit_stream ~size stream in
    { channel = create_channel ?name stream;
      channel_mark = channel_mark () }

  let create_unlimited ?name stream =
    let stream = Lwt_stream.map (fun x -> Ecb.Data x) stream in
    { channel = create_channel ?name stream;
      channel_mark = channel_mark () }

end

let get_service = Raw_channels.get_service

type comet_handler = Raw_channels.comet_service
let init () = Raw_channels.get_service ()
