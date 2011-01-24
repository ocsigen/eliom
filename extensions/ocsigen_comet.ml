(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010
 * Raphaël Proust
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

(* Comet extension for Ocsigen server
 * ``Comet'' is a set of <strike>hacks</strike> techniques providing basic
 * server-to-client communication. Using HTTP, it is not possible for the server
 * to send a message to the client, it is only possible to answer a client's
 * request.
 *
 * This implementation is to evolve and will change a lot with HTML5's
 * WebSockets support.
 *)


(*** PREAMBLE ***)

(* Shortening names of modules *)
module OFrame  = Ocsigen_http_frame
module OStream = Ocsigen_stream
module OX      = Ocsigen_extensions
module OLib    = Ocsigen_lib
module OConf   = Ocsigen_config
module OMsg    = Ocsigen_messages

(* infix monad binders *)
let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= ) (* AKA map, AKA lift *)

(* small addition to the standard library *)
let map_rev_accu_split func lst accu1 accu2 =
  let rec aux accu1 accu2 = function
    | [] -> (accu1, accu2)
    | x :: xs -> match func x with
        | OLib.Left y -> aux (y :: accu1) accu2 xs
        | OLib.Right y -> aux accu1 (y :: accu2) xs
  in
    aux accu1 accu2 lst


(*** EXTENSION OPTIONS ***)


(* timeout for comet connections : if no value has been written in the ellapsed
 * time, connection will be closed. Should be equal to client timeout. *)
let timeout_ref = ref 20.
let get_timeout () = !timeout_ref

(* the size initialization for the channel hashtable *)
let tbl_initial_size = 16

let max_virtual_channels_ref = ref None
let get_max_virtual_channels () = !max_virtual_channels_ref

let rec parse_options = function
  | [] -> ()
  | ("max_virtual_channels", "") :: tl ->
        max_virtual_channels_ref := None ; parse_options tl
  | ("max_virtual_channels", s) :: tl ->
        max_virtual_channels_ref := Some (int_of_string s) ; parse_options tl
  | _ :: _ -> raise (OX.Error_in_config_file "Unexpected data in config file")



(*** CORE ***)

module Channels :
sig

  exception Too_many_virtual_channels
    (* raised when calling [create] while [max_virtual_channels] is [Some x] and
     * creating a new channel would make the virtual channel count greater than
     * [x]. *)
  exception Non_unique_channel_name
    (* raised when creating a channel with a name already associated. *)

  type t
    (* the type of channels :
     * channels can be written on or read from using the following functions
     *)
  type chan_id = string

  val create : ?name:string -> unit -> t
  val read : t -> (string * OStream.outcome Lwt.u option) Lwt.t
  val write : t -> (string * OStream.outcome Lwt.u option) -> unit

  val listeners : t -> int
    (* The up-to-date count of registered clients *)
  val send_listeners : t -> int -> unit
    (* [send_listeners c i] adds [i] to [listeners c]. [i] may be negative. *)

  val find_channel : chan_id -> t
    (* may raise Not_found if the channel was collected or never created.
     * Basically ids are meant for clients to tell a server to start listening
     * to it. *)
  val get_id : t -> chan_id
    (* [find_channel (get_id ch)] returns [ch] if the channel wasn't destroyed
     * that is. *)

end = struct

  exception Too_many_virtual_channels
  exception Non_unique_channel_name

  type chan_id = string
    type t =
        {
                  ch_id : chan_id ;
          mutable ch_read  : (string * OStream.outcome Lwt.u option) Lwt.t ;
          mutable ch_write : (string * OStream.outcome Lwt.u option) Lwt.u;
          mutable ch_listeners : int ;
        }
  module Dummy = struct
    (*module added to avoid Ctbl.t cyclicity*)
    type tt = t
  end

  let get_id ch = ch.ch_id

  (* In order to being able to retrieve channels by there IDs, let's have a map
   * *)
  module CTbl =
    Weak.Make
      (struct
         type t = Dummy.tt
         let equal { ch_id = i } { ch_id = j } = i = j
         let hash { ch_id = c } = Hashtbl.hash c
       end)

  (* storage and ID manipulation *)
  let ctbl = CTbl.create tbl_initial_size

  let new_id = Ocsigen_lib.make_cryptographic_safe_string

  (* because Hashtables allow search for elements with a corresponding hash, we
   * have to create a dummy channel in order to retreive the original channel.
   * Is there a KISSer way to do that ? *)
  let (dummy1, dummy2) = Lwt.task ()
  let dummy_chan i =
    {
      ch_id = i ;
      ch_read  = dummy1 ;
      ch_write = dummy2 ;
      ch_listeners = 0 ;
    }

  (* May raise Not_found *)
  let find_channel i =
    CTbl.find ctbl (dummy_chan i)

  (* virtual channel count *)
  let (chan_count, incr_chan_count, decr_chan_count) =
    let cc = ref 0 in
    ((fun () -> !cc), (fun () -> incr cc), (fun _ -> decr cc))
  let maxed_out_virtual_channels () = match get_max_virtual_channels () with
    | None -> false
    | Some y -> chan_count () >= y


  (* creation : newly created channel is stored in the map as a side effect *)
  let do_create name =
    if maxed_out_virtual_channels ()
    then (OMsg.warning "Too many virtual channels, associated exception raised";
          raise Too_many_virtual_channels)
    else
      let (read , write ) = Lwt.task () in
      let ch =
        {
          ch_id = name ;
          ch_read  = read  ;
          ch_write = write ;
          ch_listeners = 0 ;
        }
      in
        incr_chan_count ();
        CTbl.add ctbl ch;
        Gc.finalise decr_chan_count ch;
        ch

  let write ch x =
    let (read, write) = Lwt.task () in
    let old_write = ch.ch_write in
    ch.ch_write <- write ;
    ch.ch_read  <- read  ;
    Lwt.wakeup old_write x

  let create ?name () = match name with
    | None -> do_create (new_id ())
    | Some n ->
        try ignore (find_channel n) ; raise Non_unique_channel_name
        with Not_found -> do_create n

  (* reading a channel : just getting a hang on the reader thread *)
  let read ch = ch.ch_read

  (* listeners *)
  let listeners ch = ch.ch_listeners
  let send_listeners ch x = ch.ch_listeners <- ch.ch_listeners + x

end


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
    OX.request -> (Channels.t list * Channels.chan_id list) Lwt.t
    (* decode incomming message : the result is the list of channels to listen
       to (on the left) or to signal non existence (on the right). *)

  val encode_downgoing :
       Channels.chan_id list
    -> (Channels.t * string * OStream.outcome Lwt.u option) list option
    -> string OStream.t
    (* Encode outgoing messages : the first argument is the list of channels
     * that have already been collected.
     * The results is the stream to send to the client*)

  val encode_ended : Channels.chan_id list -> string

end = struct

  (* constants *)
  let channel_separator = "\n"
  let field_separator = ":"
  let ended_message = "ENDED_CHANNEL"
  let channel_separator_regexp = Netstring_pcre.regexp channel_separator
  let url_encode x = OLib.encode ~plus:false x

  let decode_string s accu1 accu2 =
    map_rev_accu_split
      (fun s ->
         try OLib.Left (Channels.find_channel s)
         with | Not_found -> OLib.Right s
      )
      (Netstring_pcre.split channel_separator_regexp s)
      accu1
      accu2

  let decode_param_list params =
    let rec aux ((tmp_reg, tmp_end) as tmp) = function
      | [] -> (tmp_reg, tmp_end)
      | ("registration", s) :: tl -> aux (decode_string s tmp_reg tmp_end) tl
      | _ :: tl -> aux tmp tl
    in
      aux ([], []) params

  let decode_upcomming r =
    (* RRR This next line makes it fail with Ocsigen_unsupported_media, hence
     * the http_frame low level version *)
    (* r.OX.request_info.OX.ri_post_params r.OX.request_config *)
    Lwt.catch
      (fun () ->
         match r.OX.request_info.OX.ri_http_frame.OFrame.frame_content with
           | None ->
               Lwt.return []
           | Some body ->
               Lwt.return (OStream.get body) >>=
               OStream.string_of_stream
                 (OConf.get_maxrequestbodysizeinmemory ()) >|=
               Ocsigen_lib.fixup_url_string >|=
               Netencoding.Url.dest_url_encoded_parameters
      )
      (function
         | OStream.String_too_large -> Lwt.fail OLib.Input_is_too_large
         | e -> Lwt.fail e
      )
      >|= decode_param_list

  let encode1 (c, s, _) =
    Channels.get_id c ^ field_separator ^ url_encode s

  let encode l = String.concat channel_separator (List.map encode1 l)

  let encode_ended l =
    String.concat
      channel_separator
      (List.map (fun c -> c ^ field_separator ^ ended_message) l)

  let stream_result_notification s outcome =
    Lwt_list.iter_p
      (function
         (*when write has been made with outcome notifier*)
         | (c, _, Some x) -> (Lwt.wakeup x outcome ; Lwt.return ())
         (*when it hasn't*)
         | (_, _, None) -> Lwt.return ()
      )
      s

  let encode_downgoing e = function
    | None -> OStream.of_string (encode_ended e)
    | Some s ->
        let stream =
          OStream.of_string
            (match e with
               | [] -> encode s
               | e ->   encode_ended e
                      ^ field_separator
                      ^ encode s
            )
        in
        OStream.add_finalizer stream (stream_result_notification s) ;
        stream

end

module Security :
sig

  val set_timeout : ?reset:bool -> float -> unit
    (* Set the [timeout] constant for new connections. Existing connections are
     * not affected unless [?reset] is [Some true] *)

  val deactivate : unit -> unit
    (* Stop serving comet connections and kill all current connections. *)

  val activate : unit -> unit
    (* (Re)start serving connections *)

  val activated : unit -> bool
    (* activation state *)

  val kill : unit React.E.t
    (* The event reflecting willingness to kill connections *)

  val command_function : string -> string list -> unit Lwt.t
    (* To be registered with Ocsigen_extension.register_command_function *)

end = struct

  let (kill, kill_all_connections) = React.E.create ()

  let activated, activate, deactivate =
    let activated = ref true in
      ((fun () -> !activated),
       (fun () -> if !activated
                  then ()
                  else (OMsg.warning "Comet is being activated";
                        activated := true)),
       (fun () -> if !activated
                  then (OMsg.warning "Comet is being deactivated";
                        activated := false;
                        kill_all_connections ())
                  else ())
      )

  let warn_kill =
    React.E.map
     (fun () -> OMsg.warning "Comet connections kill notice is being sent.")
      kill
  let `R _ = React.E.retain kill (fun () -> ignore warn_kill)

  let set_timeout ?(reset=false) f =
    timeout_ref := f ;
    if reset
    then kill_all_connections ()
    else ()

  let command_function_ _ = function
    | ["deactivate"] -> deactivate ()
    | ["activate"]   -> activate ()
    | "set_timeout" :: f :: tl ->
        (try
           set_timeout
             ~reset:(match tl with
                       | ["KILL"] -> true
                       | [] -> false
                       | _ -> raise OX.Unknown_command
             )
             (float_of_string f)
         with Failure _ -> raise OX.Unknown_command)
    | _ -> raise OX.Unknown_command

  let command_function x y = command_function_ x y; Lwt.return ()

end

module Main :
  (* a client can wait for all the channels on which it
   * is registered and return with the first result. *)
sig

  val main : OX.request -> unit -> OFrame.result Lwt.t
  (* treat an incoming request from a client. The unit part is for partial
   * application in Ext_found parameter. *)

end = struct

  let frame_503 () =
    Lwt.return
      { (OFrame.default_result ()) with
            OFrame.res_stream = (OStream.of_string "", None);
            OFrame.res_code = 503; (*Service Unavailable*)
            OFrame.res_content_length = None;
            OFrame.res_content_type = Some "text/plain";
      }

  exception Kill

  (* Once channel list is obtain, use this function to return a thread that
   * terminates when one of the channel is written upon. *)
  let treat_decoded = function
    | [], [] -> (* error : empty request *)
        OMsg.debug (fun () -> "Incorrect or empty Comet request");
        Lwt.return
          { (OFrame.default_result ()) with
               OFrame.res_stream =
                 (OStream.of_string "Empty or incorrect registration", None) ;
               OFrame.res_code = 400 ;(* BAD REQUEST *)
               OFrame.res_content_type = Some "text/plain" ;
               OFrame.res_content_length = None ;
          }

    | [], (_::_ as ended) -> (* All channels are closed *)
        let end_notice = Messages.encode_ended ended in
        OMsg.debug (fun () -> "Comet request served");
        Lwt.return
          { (OFrame.default_result ()) with
               OFrame.res_stream = (OStream.of_string end_notice, None) ;
               OFrame.res_content_length = None ;
               OFrame.res_content_type = Some "text/plain" ;
          }

    | (_::_ as active), ended -> (* generic case *)
        let choosed =
          let readings =
            (List.map
               (fun c -> Channels.read c >|= fun (v,x) -> (c, v, x))
               active
            )
          in
          (*wait for one thread to terminate and get all terminated threads  *)
          Lwt.choose readings >>= fun _ -> Lwt.nchoose readings
        in
        List.iter (fun c -> Channels.send_listeners c 1) active ;
        Lwt.catch
          (fun () ->
             Lwt.choose
               [ (choosed >|= fun x -> Some x);
                 (Lwt_unix.sleep (get_timeout ()) >|= fun () -> None);
                 (Lwt_event.next Security.kill >>= fun () -> Lwt.fail Kill);
               ] >|= fun x ->
             List.iter (fun c -> Channels.send_listeners c (-1)) active ;
             let s = Messages.encode_downgoing ended x in
             OMsg.debug (fun () -> "Comet request served");
             { (OFrame.default_result ()) with
                   OFrame.res_stream = (s, None) ;
                   OFrame.res_content_length = None ;
                   OFrame.res_content_type = Some "text/plain" ;
             }
          )
          (function
             | Kill -> (* Comet stopped for security *)
                 List.iter (fun c -> Channels.send_listeners c (-1)) active ;
                 OMsg.debug (fun () -> "Killed Comet request handling");
                 frame_503 ()
             | e -> Lwt.fail e
          )


  (* This is just a mashup of the other functions in the module. *)
  let main r () =
    if Security.activated ()
    then
      (OMsg.debug (fun () -> "Serving Comet request");
       Messages.decode_upcomming r >>= treat_decoded)
    else
      (OMsg.debug (fun () -> "Refusing Comet request (Comet deactivated)");
       frame_503 ())

end

let rec has_comet_content_type = function
  | [] -> false
  | ("application", "x-ocsigen-comet") :: _ -> true
  | _ :: tl -> has_comet_content_type tl

(*Only for debugging purpose*)
let rec debug_content_type = function
  | [] -> ""
  | (s1,s2) :: tl -> s1 ^ "/" ^ s2 ^ "\n" ^ debug_content_type tl



(*** MAIN FUNCTION ***)

let main = function

  | OX.Req_not_found (_, rq) -> (* Else check for content type *)
      begin match rq.OX.request_info.OX.ri_content_type with
        | Some (hd, tl) when has_comet_content_type (hd :: tl) ->
            OMsg.debug (fun () -> "Comet message: " ^ debug_content_type (hd :: tl));
            Lwt.return (OX.Ext_found (Main.main rq))

        | Some (hd, tl) ->
            OMsg.debug (fun () -> "Non comet message: " ^ debug_content_type (hd :: tl));
            Lwt.return OX.Ext_do_nothing
        | None ->
            OMsg.debug (fun () -> "Non comet message: no content type");
            Lwt.return OX.Ext_do_nothing
      end

  | OX.Req_found _ -> (* If recognized by some other extension... *)
      Lwt.return OX.Ext_do_nothing (* ...do nothing *)




(*** EPILOGUE ***)

(* registering extension and the such *)
let parse_config _ _ _ = function
  | Simplexmlparser.Element ("comet", attrs, []) ->
      parse_options attrs ;
      main
  | Simplexmlparser.Element (t, _, _) ->
      raise (OX.Bad_config_tag_for_extension t)
  | _ ->
      raise (OX.Error_in_config_file "Unexpected data in config file")
let site_creator (_ : OX.virtual_hosts) _ = parse_config
let user_site_creator (_ : OX.userconf_info) = site_creator

(* registering extension *)
let () = OX.register_extension
  ~name:"comet"
  ~fun_site:site_creator
  ~user_fun_site:user_site_creator
  ()
let () = OX.register_command_function
           ~prefix:"comet"
           Security.command_function
