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

(* Comet server extension for ocsigen *)

(* Shortening names of modules *)
module OFrame  = Ocsigen_http_frame
module OStream = Ocsigen_stream
module OX      = Ocsigen_extensions
module OLib    = Ocsigen_lib
module Pxml    = Simplexmlparser

(* infix monad binders *)
let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= ) (* AKA map, AKA lift *)

(* A few React related functions... TODO: move to some appropriate place *)

let create_half_primitive evt =
  let (prim_evt, prim_push) = React.E.create () in
  (React.E.select [evt ; prim_evt], prim_push)

let branch f e =
  let ee = React.E.map f e in
  (React.E.map fst ee, React.E.map snd ee)



(* a tiny deforestating addition to Lwt library : filter_map *)
let filter_map_rev_s func lst =
  let rec aux accu = function
    | [] -> Lwt.return accu
    | x::xs -> func x >>= (function
        | Some y -> aux (y :: accu) xs
        | None -> aux accu xs)
  in aux [] lst
let filter_map_s f l =
  filter_map_rev_s f l >|= List.rev

let filter_map f l =
  let rec aux accu = function
    | [] -> List.rev accu
    | x::xs -> (match f x with
                  | None -> aux accu xs
                  | Some y -> aux (y::accu) xs
      )
  in aux [] l

let filter_map_accu func lst accu =
  let rec aux accu = function
    | [] -> accu
    | x :: xs -> match func x with
        | Some y -> aux (y :: accu) xs
        | None -> aux accu xs
  in
    aux accu lst

(* timeout for comet connections : if no value has been written in the ellapsed
 * time, connection will be closed. Should be equal to client timeout. *)
(* TODO: make value customizable via conf file *)
let timeout = 20.

(* the size initialization for the channel hashtable *)
(* TODO: make value customizable via conf file *)
let tbl_initial_size = 42

(* HOWTO limit the number of connections (to avoid DOS) ? *)
module Channels :
sig

  type chan
    (* the type of channels :
     * channels can be written on or read from using the following functions
     *)

  val create : unit -> chan
    (* creating a fresh virtual channel, a client can request registraton to  *)

  val write  : chan -> string -> unit
    (* [write ch v] sends [v] over [ch] all clients currently listening to [ch]
     * are being sent [v]. *)
  val read : chan -> string React.E.t
    (* [read ch] is an event with occurrences for each call to [write]. The
     * carried value is the argument given to [write]. This is the way for
     * clients to listen on a channel. *)

  (* MEMORY LEAKS ? *)
  val notify : chan -> string -> unit
    (* [notify c s] sends [s] to [notification c] event. *)
  val notification : chan -> string React.E.t
    (* [notification c] is an event with an [x] occurrence for each call to
     * [notify c x]. *)

  val find_channel : string -> chan
    (* may raise Not_found if the channel was collected or never created.
     * Basically ids are meant for clients to tell a server to start listening
     * to it. *)
  val get_id : chan -> string
    (* [find_channel (get_id ch)] returns [ch] if the channel wasn't destroyed
     * that is. *)

end = struct

  type chan =
      {
        ch_id : string ;
        ch_tell_client  : string -> unit ;
        ch_client_event : string React.E.t ;
        ch_tell_server  : string -> unit ;
        ch_server_event : string React.E.t ;
      }

  let get_id ch = ch.ch_id

  (* In order to being able to retrieve channels by there IDs, let's have a map
   * *)
  module CTbl =
    Weak.Make
      (struct
         type t = chan
         let equal { ch_id = i } { ch_id = j } = i = j
         let hash { ch_id = c } = Hashtbl.hash c
       end)

  (* storage and ID manipulation *)
  let ctbl = CTbl.create tbl_initial_size

  let new_id = Ocsigen_lib.make_cryptographic_safe_string

  (* because Hashtables allow search for elements with a corresponding hash, we
   * have to create a dummy channel in order to retreive the original channel.
   * Is there a KISSer way to do that ? *)
  let (dummy1, dummy2) = React.E.create ()
  let dummy_chan i =
    {
      ch_id = i ;
      ch_tell_client  = dummy2 ;
      ch_client_event = dummy1 ;
      ch_tell_server  = dummy2 ;
      ch_server_event = dummy1 ;
    }

  (* May raise Not_found *)
  let find_channel i =
    CTbl.find ctbl (dummy_chan i)

  (* creation : newly created channel is stored in the map as a side effect *)
  let create () =
    let (client_event, tell_client) = React.E.create () in
    let (server_event, tell_server) = React.E.create () in
    let ch =
      {
        ch_id = new_id () ;
        ch_tell_client  = tell_client  ;
        ch_client_event = client_event ;
        ch_tell_server  = tell_server  ;
        ch_server_event = server_event ;
      }
    in CTbl.add ctbl ch ; ch

  (* writing on a channel : wakeup the writer with the given value and reload
   * both reader and writer. *)
  let write ch v = ch.ch_tell_client v

  (* reading a channel : just getting a hang on the reader thread *)
  let read ch = ch.ch_client_event

  (* notification *)
  let notify ch v = ch.ch_tell_server v

  let notification ch = ch.ch_server_event

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

  exception Incorrect_encoding

  val decode_upcomming :
    OX.request -> (Channels.chan list * (unit -> unit) list) Lwt.t
    (* decode incomming message : the result is the list of channels to listen
       to. *)

  val encode_downgoing :
    (Channels.chan * string) list option -> string OStream.t Lwt.t
    (* Encode outgoing messages : results in the stream to send to the client *)

end = struct

  exception Incorrect_encoding

  let channel_separator_regexp = Netstring_pcre.regexp ";"
  let separator_regexp = Netstring_pcre.regexp ":;"

  (* string -> Channels.chan list -> Channels.chan list *)
  let decode_registration_string s accu =
    filter_map_accu
      (fun s ->
         try Some (Channels.find_channel s)
         with | Not_found -> None
      )
      (Netstring_pcre.split channel_separator_regexp s)
      accu

  let decode_notification_string s accu =
    let rec aux accu = function
      | [] -> accu
      |    Netstring_pcre.Text c
        :: Netstring_pcre.Delim ":"
        :: Netstring_pcre.Text s
        :: Netstring_pcre.Delim ";"
        :: tl 
      |    Netstring_pcre.Text c
        :: Netstring_pcre.Delim ":"
        :: Netstring_pcre.Text s
        :: ([] as tl)
         -> aux
              (   (fun () ->
                     try Channels.notify (Channels.find_channel c) s
                     with | Not_found -> ())
               :: accu)
             tl
      | _ -> (* Client encoding error : stop *) []
    in
      aux accu (Netstring_pcre.full_split separator_regexp s)

  let unopt v d = match v with
    | Some x -> x
    | None -> d

  (* (string * string) list -> (Channels.chan list * (unit -> unit) list) *)
  let decode_param_list params =
    let rec aux tmp_reg tmp_not = function
      | [] -> (unopt tmp_reg [], unopt tmp_not [])
      | ("registration", s) :: tl ->
          begin 
            aux
              (Some (decode_registration_string s (unopt tmp_reg []))) tmp_not
              tl
          end
      | ("notification", s) :: tl ->
          begin
            aux
              tmp_reg (Some (decode_notification_string s (unopt tmp_not [])))
              tl
          end
      | _ :: tl -> aux tmp_reg tmp_not tl
    in
      aux None None params

  (* OX.request -> Channels.chan list Lwt.t *)
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
               OStream.string_of_stream >|=
               Ocsigen_lib.fixup_url_string >|=
               Netencoding.Url.dest_url_encoded_parameters
      )
      (function
         | OStream.String_too_large -> Lwt.fail OLib.Input_is_too_large
         | e -> Lwt.fail e
      )
      >|= decode_param_list


  let encode_downgoing lo =
    let str = match lo with
      | None -> ""
      | Some l -> String.concat ";"
                    (List.map (fun (c,s) -> Channels.get_id c ^ ":" ^ s) l)
    in
    Lwt.return (OStream.of_string str)

end

module Main :
  (* using React.merge, a client can wait for all the channels on which it
   * is registered and return with the first result. *)
sig

  val main : OX.request -> unit -> OFrame.result Lwt.t
  (* treat an incoming request from a client. The unit part is for partial
   * application in Ext_found parameter. *)

end = struct

  (* A timeout that that return a choosen value instead of failing *)
  let armless_timeout t r =
    Lwt.catch
      (fun () -> Lwt_unix.timeout t)
      (function
         | Lwt_unix.Timeout -> Lwt.return r
         | exc -> Lwt.fail exc
      )

  (* Once channel list is obtain, use this function to return a thread that
   * terminates when one of the channel is written upon. *)
  let treat_decoded chans notifications =
    (*RRR: [merged] must be created before performing [notifications]. If not,
     * an application's message written on one of the merged channels as a
     * REACTion of notifications won't reach the client. *)
    let merged =
        (Lwt_event.next (
           React.E.merge
             (fun acc v -> v :: acc)
             []
             (List.map
                (fun c -> React.E.map (fun v -> (c, v)) (Channels.read c))
                chans)
         ) >|= fun x -> Some x)
    in
    (*TODO: find a way to send all the notifications simultaneously *)
    List.iter (fun f -> f ()) notifications ;
    Lwt.choose [ merged ; (Lwt_unix.sleep timeout >|= fun () -> None) ] >>=
    Messages.encode_downgoing

  (* This is just a mashup of the other functions in the module. *)
  let main r () =
    Messages.decode_upcomming r >>=
    OLib.uncurry2 treat_decoded >>= fun stream ->
      let res = OFrame.default_result () in
        Lwt.return
          { res with
                OFrame.res_stream = (stream, None) ;
                OFrame.res_content_length = None ;
                OFrame.res_content_type = Some "text/html" ;
          }


end

let rec has_comet_content_type = function
  | [] -> false
  | ("application", "x-ocsigen-comet") :: _ -> true
  | _ :: tl -> has_comet_content_type tl

let main = function

  | OX.Req_found _ -> (* If recognized by some other extension... *)
      Lwt.return OX.Ext_do_nothing (* ...do nothing *)

  | OX.Req_not_found (_, rq) -> (* Else check for content type *)
      match rq.OX.request_info.OX.ri_content_type with
        | Some (hd,tl) ->
            if has_comet_content_type (hd :: tl)
            then Lwt.return (OX.Ext_found (Main.main rq))
            else Lwt.return OX.Ext_do_nothing
        | None -> Lwt.return OX.Ext_do_nothing





(* registering extension and the such *)
let begin_init () = ()
let end_init () = ()
let parse_config _ _ _ = function
  | Pxml.Element ("comet", [], []) -> main
  | Pxml.Element (t, _, _) -> raise (OX.Bad_config_tag_for_extension t)
  | _ -> raise (OX.Error_in_config_file "Unexpected data in config file")
let site_creator (hostpattern : OX.virtual_hosts) = parse_config
let user_site_creator (path : OX.userconf_info) = site_creator

(* registering extension *)
let () = OX.register_extension
  ~name:"comet"
  ~fun_site:site_creator
  ~user_fun_site:user_site_creator
  ()
