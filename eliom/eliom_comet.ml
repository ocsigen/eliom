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

(* Shortening names of modules *)
module OFrame  = Ocsigen_http_frame
module OStream = Ocsigen_stream
module OLib    = Ocsigen_lib
module OMsg    = Ocsigen_messages

(* infix monad binders *)
let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= ) (* AKA map, AKA lift *)

type chan_id = string

module Messages :
  (* All about messages from between clients and server *)
  (*
   * The client sends a POST request with a "registration" parameter containing
   * a list of channel ids. Separator for the list are semi-colon : ';'.
   *
   * The server sends result to the client in the form of a list of :
   * channel_id ^ ":" ^ value ^ { ";" ^ channel_id ^ " " ^ value }*
   * where channel_id is the id of a channel that the client registered upon and
   * value is the string that was written upon the associated channel.
   * *)
sig

  val decode_upcomming :
    string -> chan_id list
    (* decode incomming message : the result is the list of the new channels to listen
       to. *)

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

  let decode_upcomming s =
    Netstring_pcre.split channel_separator_regexp s

  let url_encode x = OLib.encode ~plus:false x

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
  open Xhtmltypes
  open Ocsigen_http_frame

  type page = (string * string)

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options () = Lwt.return ()

  let do_appl_xhr = Eliom_services.XAlways

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

  type comet_service = Eliom_common_comet.comet_service

  val get_service : unit -> comet_service
  val get_service_data_key : unit -> comet_service Eliom_client_types.data_key

end = struct

  type chan_id = string

  type comet_service = Eliom_common_comet.comet_service

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
	hd_service_data_key : comet_service Eliom_client_types.data_key;
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

  let new_id = Ocsigen_lib.make_cryptographic_safe_string
  let content_type = "text/plain"

  let timeout = 20.
  
  (** Returns the handler for the current application. It is created if it does not exists. *)
  let get_handler () =
    let sp = Eliom_common.get_sp () in
    let cpi =
      match sp.Eliom_common.sp_client_process_info with
	| None -> raise (Eliom_common.Eliom_site_information_not_available "need to be called inside an application")
	| Some cpi -> cpi
    in
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
	      ~post_params:(Eliom_parameters.prod (Eliom_parameters.string "registration") (Eliom_parameters.int "number"))
	      ()
	  in
	  let hd_service_data_key = Eliom_services.wrap hd_service in
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
	  let f () (input,number) =
	    match input with
	      | "" ->
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
			  (* VVV here need to handle closed streams *)
			  let message = Messages.encode_downgoing [] messages in
			  handler.hd_last <- (message,number);
			  Lwt.return ( message, content_type ) ) ) )
		    ( function
		      | New_connection -> Lwt.return ("",content_type)
		      (* happens if an other connection has been opened on that service *)
		      (*VVV in this case, it would be beter to return code 204: no content *)
		      | Lwt_unix.Timeout -> Lwt.return ("TIMEOUT",content_type)
		      | e -> Lwt.fail e )
	      | _ ->
		(* connections registering new channels are replied
		   immediately by an empty answer *)
		let new_channels = Messages.decode_upcomming input in
		List.iter (register_channel handler) new_channels;
		Lwt.return ("",content_type)
	  in
	  Comet.register
	    ~scope:`Client_process
	    ~service:hd_service
	    f;
	  Polytables.set ~table ~key:handler_key ~value:handler;
	  handler
	end

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

  val create : ?name:string -> 'a Lwt_stream.t -> 'a t
  val wrap : 'a t -> 'a Eliom_common_comet.chan_id Eliom_client_types.data_key
  val get_id : 'a t -> 'a Eliom_common_comet.chan_id

end = struct

  type +'a t = {
    channel : Raw_channels.t;
  }

  let create ?name stream =
    { channel = Raw_channels.create ?name
	(Lwt_stream.map (fun x -> Marshal.to_string x []) stream) }

  let get_id t =
    Eliom_common_comet.chan_id_of_string (Raw_channels.get_id t.channel)

  let wrap c = Eliommod_cli.wrap (get_id c)

end

let get_service = Raw_channels.get_service

type comet_handler = Raw_channels.comet_service Eliom_client_types.data_key
let init () = Raw_channels.get_service_data_key ()
