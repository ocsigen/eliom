(* Comet server extension for ocsigen *)
(* /!\ PROTOTYPE /!\ *)
(* For now only an UDP style connection is available :
 * a module can use :
 *   Comet.Channels.new_channel to obtain a channel
 *   Comet.Channels.get_id to communicate a channel's id to a client
 *   Comet.Channels.write to send a message to all connected clients currently
 *     listening on this channel
 *   Comet.Channels.destroy_channel if a channel is not needed anymore
 *
 * Clients have to start a new connection from time to time (see [timeout] in
 * order to recheck the server. The connection is then held by the server
 * waiting for a channel to be written on. Every client listening to this
 * channel is then sent the value written on the channel. If a client is
 * disconnected, nothing happens !
 *
 * Eg :
 *
 * let ch = Comet.Channels.new_channel () (* as a global value for the module *)
 *
 * let serve_page () =
 *   generate_script_using_ch (Comet.Channels.get_id ch) >>= fun script ->
 *   serve_page_with_script_to_client script other_parameter
 *
 * let rec tick =
 *   let count = ref 0 in
 *   fun () ->
 *     incr c ;
 *     Comet.Channels.write ch (string_of_int !count) ;
 *     Lwt_unix.sleep 10. >>= tick
 *
 *)

(* Shortening names of modules : basically Osmthg is for Ocsigen_smthg *)
module OFrame = Ocsigen_http_frame
module OStream = Ocsigen_stream
module OX = Ocsigen_extensions
module Pxml = Simplexmlparser

(* infix monad binder *)
let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= )

(* a tiny deforestating addition to Lwt library : take_filter_map *)
let take_filter_map_rev_s limit func lst =
  let rec aux count accu = function
    | [] -> Lwt.return accu
    | x::xs -> func x >>= function
        | Some y ->
            if succ count >= limit
            then Lwt.return (y :: accu)
            else aux (succ count) (y :: accu) xs
        | None -> aux count accu xs
  in aux 0 [] lst
let take_filter_map_s lim f l =
  take_filter_map_rev_s lim f l >|= List.rev

(* timeout for comet connections : if no value has been written in the ellapsed
 * time, connection will be closed. Should be equal to client timeout. *)
(* TODO: make value customizable via conf file *)
let timeout = 20.

(* the maximum number of channels a client can register upon. An application
 * really needs one channel and can use multiplexing (as will be provided soon
 * in eliom_comet !) in order to acheive multichannel listening. *)
(* TODO: make value customizable via conf file *)
let chan_number_upper_limit = 5

(* HOWTO limit the number of connections (to avoid DOS) ? *)
module Channels :
sig

  type t
    (* the type of channels :
     * channels can be written on or read from using the following functions
     *)

  val new_channel : unit -> t
    (* creating a fresh channel *)
  val write  : t -> string -> unit
    (* [write ch v] sends [v] over [ch] all clients currently listening to [ch]
     * are being sent [v]. *)
  val read : t -> string Lwt.t
    (* [read ch] will return whenever a value is written on [ch]. The result is
     * the value written on it. This is the way for clients to listen on a
     * channel. Note that while channels can be used to emulates events the
     * implementation does not really suits it. *)

  val find_channel : int -> t
    (* may raise Not_found if the channel was destroyed or never created.
     * Basically ids are meant for clients to tell a server to start listening
     * to it. *)
  val get_id : t -> int
    (* [find_channel (get_id ch)] returns [ch] if the channel wasn't destroyed
     * that is. *)

  val destroy_channel : t -> unit
    (* Makes the subsequent calls to find_channel fail for this channel, also
     * makes the channel GCable as far as this module is concerned. If other
     * references are kept, it obviously won't be. Remember to destroy any
     * channel. *)
    (* TODO: have the channels automattically made collectable. Weak ? Channel
     * creator keeping the one and only reference and passing a retrieval
     * function when creating ? *)

end = struct

  (* In order to being able to retrieve channels by there IDs, let's have a map
   * *)
  module CMap = Map.Make (struct type t = int let compare = compare end)

  type t =
      {
        ch_id : int ;
        mutable ch_reader : string Lwt.t ;
        mutable ch_writer : string Lwt.u ;
      }

  (* storage and ID manipulation *)
  let cmap : t CMap.t ref = ref CMap.empty
  (* TODO: generate id randomly for security reasons *)
  let new_id = let c = ref 0 in fun () -> incr c ; !c

  let find_channel i = CMap.find i !cmap

  let get_id ch = ch.ch_id

  let destroy_channel ch =
    cmap := CMap.remove ch.ch_id !cmap

  (* creation : newly created channel is stored in the map as a side effect *)
  let new_channel () =
    let (reader, writer) = Lwt.wait () in
    let ch =
      {
        ch_id = new_id () ;
        ch_reader = reader ;
        ch_writer = writer ;
      }
    in cmap := CMap.add ch.ch_id ch !cmap ; ch

  (* writing on a channel : wakeup the writer with the given value and reload
   * both reader and writer. *)
  let write ch v =
    Lwt.wakeup ch.ch_writer v ;
    let (reader, writer) = Lwt.wait () in
    ch.ch_writer <- writer ;
    ch.ch_reader <- reader

  (* reading a channel : just getting a hang on the reader thread *)
  let read ch = ch.ch_reader

end


module Messages :
  (* All about messages from between clients and server *)
sig

  val decode_incomming :
    OX.request_info -> Channels.t list Lwt.t
    (* decode incomming message : the result is the list of channels to listen
     * to. *)

  val encode_outgoing :
    (Channels.t * string) option list -> string OStream.t Lwt.t

end = struct

  let decode_string s =
    (* Is the split cooperative enough ? *)
    let re = Netstring_pcre.regexp ";" in
    Lwt.return (Netstring_pcre.split re s) >>=
    take_filter_map_s
      chan_number_upper_limit
      (fun s ->
         Lwt.catch
           (fun () ->
              Lwt.return (Some (Channels.find_channel (int_of_string s)))
           )
           (function
              | Not_found -> Lwt.return None
              | exc -> Lwt.fail exc
           )
      )

  let decode_incomming { OX.ri_http_frame = { OFrame.frame_content = fc } } =
    match fc with
      | None -> Lwt.return []
      | Some fc -> OStream.string_of_stream (OStream.get fc) >>= decode_string

  let rec encode_outgoing_step l =
    let rec aux accu = function (*TODO: use a fold *)
      | [] -> accu
      | None :: tl -> aux accu tl
      | Some (chan, str) :: tl ->
          aux
            (  accu
             ^ "[" ^ string_of_int (Channels.get_id chan) ^ " : " ^ str ^ "]"
            )
            tl
    in aux "" l
 
  let encode_outgoing l =
    Lwt.return (OStream.of_string (encode_outgoing_step l))


end

module Main :
  (* binding Lwt threads *)
  (* using Lwt.choose, a client can wait for all the channels thread on which it
   * is bound and return with the first result. *)
sig

  val treat_incoming :
    OX.request_info -> OFrame.result Lwt.t
    (* treat an incoming request from a client *)

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
  let treat_decoded chans =
    Lwt.choose
      (   armless_timeout timeout None
       :: List.map
            (fun chan ->
               Channels.read chan >>= fun value ->
               Lwt.return (Some (chan, value))
            )
            chans
      ) >>=
    fun x -> Lwt.return [ x ] >>= (*TODO: replace Lwt.choose with a proper "select" and delete this line *)
    Messages.encode_outgoing

  (* This is just a mashup of the other functions in the module. *)
  let treat_incoming r =
    Messages.decode_incomming r >>= treat_decoded >>= fun stream ->
      let res = OFrame.default_result () in
        Lwt.return
          { res with
                OFrame.res_stream = (stream, None) ;
                OFrame.res_content_length = None ;
                OFrame.res_content_type = Some "text" ;
          }


end

let comet_regexp = Netstring_pcre.regexp ".*x-ocsigen-comet.*"
let main = function
  | OX.Req_found _ ->
      Lwt.return OX.Ext_do_nothing
  | OX.Req_not_found (_, rq) ->
      match rq.OX.request_info.OX.ri_content_type_string with
        | Some s ->
            begin match Netstring_pcre.string_match comet_regexp s 0 with
                | Some _ ->
                    Lwt.return
                      (OX.Ext_found
                         (fun () -> Main.treat_incoming rq.OX.request_info)
                      )
                | None -> Lwt.return OX.Ext_do_nothing
            end
        | None -> Lwt.return OX.Ext_do_nothing


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
