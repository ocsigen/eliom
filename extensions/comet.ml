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
let filter_map_rev_s func lst =
  let rec aux accu = function
    | [] -> Lwt.return accu
    | x::xs -> func x >>= function
        | Some y -> aux (y :: accu) xs
        | None -> aux accu xs
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

  val new_channel : unit -> chan
    (* creating a fresh virtual channel, a client can request registraton to  *)
  val write  : chan -> string -> unit
    (* [write ch v] sends [v] over [ch] all clients currently listening to [ch]
     * are being sent [v]. *)
  val read : chan -> string Lwt.t
    (* [read ch] will return whenever a value is written on [ch]. The result is
     * the value written on it. This is the way for clients to listen on a
     * channel. Note that while channels can be used to emulates events the
     * implementation does not really suits it. *)

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
        mutable ch_reader : string Lwt.t ;
        mutable ch_writer : string Lwt.u ;
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
  let (dummy_r, dummy_w) = Lwt.wait ()
  let dummy_chan i =
    {
      ch_id = i ;
      ch_reader = dummy_r ;
      ch_writer = dummy_w ;
    }

  (* May raise Not_found *)
  let find_channel i =
    CTbl.find ctbl (dummy_chan i)

  (* creation : newly created channel is stored in the map as a side effect *)
  let new_channel () =
    let (reader, writer) = Lwt.wait () in
    let ch =
      {
        ch_id = new_id () ;
        ch_reader = reader ;
        ch_writer = writer ;
      }
    in CTbl.add ctbl ch ; ch

  (* writing on a channel : wakeup the writer with the given value and reload
   * both reader and writer. *)
  let write ch v =
    Lwt.wakeup ch.ch_writer v ; (* DO NOT cooperate here
                                   (to avoid using twice the same wakener)
                                 *)
    let (reader, writer) = Lwt.wait () in
    ch.ch_writer <- writer ;
    ch.ch_reader <- reader

  (* reading a channel : just getting a hang on the reader thread *)
  let read ch = ch.ch_reader

end


module Messages :
  (* All about messages from between clients and server *)
  (* 
   * The client has to send a registration request : the content-type for the
   * HTTP request has to contain x-ocsigen-comet, the content of the HTTP
   * request must be a list of (channel) ids separated by a semi-colon.
   *
   * The server sends result to the client in the form of a list of :
   * channel_id ^ " : " ^ value ^ { ";" ^ channel_id ^ " : " ^ value }*
   * where channel_id is the id of a channel that the client registered upon and
   * value is the string that was written upon the associated channel.
   * *)
sig

  exception Incorrect_encoding

  val decode_incomming :
    OX.request_info -> Channels.chan list Lwt.t
    (* decode incomming message : the result is the list of channels to listen
     * to. *)

  val encode_outgoing :
    (Channels.chan * string) option list -> string OStream.t Lwt.t

end = struct
  (*TODO: improve safety by "catching" improperly encoded messages *)

  exception Incorrect_encoding

  let decode_string s =
    (* Is the split cooperative enough ? It must be, the server uses it too *)
    let re = Netstring_pcre.regexp ";" in
    Lwt.return (Netstring_pcre.split re s) >>=
    filter_map_s
      (fun s ->
         Lwt.catch
           (fun () ->
              Lwt.return (Some (Channels.find_channel s))
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
    (*TODO: make a one pass version *)
    String.concat ";"
      (filter_map
         (function
            | None -> None
            | Some (chan, str) -> Some (Channels.get_id chan ^ ":" ^ str)
         )
         l
      )
 
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
    Lwt.nchoose (* seems useless for it only one thread return *)
      (   armless_timeout timeout None
       :: List.map
            (fun chan ->
               Channels.read chan >>= fun value ->
               Lwt.return (Some (chan, value))
            )
            chans
      ) >>=
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
