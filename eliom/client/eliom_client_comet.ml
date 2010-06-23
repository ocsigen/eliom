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


(* This file is for client-side comet-programming. *)

module Ecc = Eliom_common_comet

(* binders *)
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)


(* Messages : encoding and decoding, comet protocol *)
module Messages : sig

  exception  Incorrect_encoding

  val encode_upgoing : Ecc.chan_id_pre list -> string
  val decode_downcoming : string -> (Ecc.chan_id_pre * string) list

end = struct

  exception  Incorrect_encoding

  let channel_separator = ";"

  let encode_upgoing = String.concat channel_separator

  (* Right now we use Obrowser's Regexp module *)
  let down_chan_delimiter_regexp = Regexp.make ";"
  let down_msg_delimiter_regexp = Regexp.make ":"

  let decode_downcoming s =
    (*TODO: make one pass (or two...)*)
    let splited = Regexp.split down_chan_delimiter_regexp s in
    let splited_twice =
      Array.map (Regexp.split down_msg_delimiter_regexp) splited
    in
    let prepared =
      Array.map
        (function
           | [| chan; msg |] -> (chan, msg)
           | _ -> raise Incorrect_encoding
        )
        splited_twice
    in
      Array.to_list prepared

end


(* Engine : the main loop for comet channel management *)
module Engine :
sig

  (* [register c f] registers to the channel [c], calling [f] on each server
   * pushed value.
   * It should only be called once per channel_id.
   * If the engine isn't running at this point, it is started. *)
  val register : Ecc.chan_id_pre -> (string -> unit Lwt.t) -> unit

  (* [unregister c] cancels all registration on channel [c] *)
  val unregister : Ecc.chan_id_pre -> unit

  (* [registered c] is [true] if [c] has already been registered, else it's
   * [false] *)
  val registered : Ecc.chan_id_pre -> bool

  (* started is true when the engine is running and false otherwise *)
  val running : bool React.S.t

  val start : unit -> unit
  val stop : unit -> unit

end = struct

  (* Primitive events for the reactive engine *)
  let (start_e, start) = React.E.create ()
  let (stop_e,  stop)  = React.E.create ()


  (* Managing registration set (map actually) *)

  module Cmap =
    Map.Make (struct type t = Ecc.chan_id_pre let compare = compare end)

  let cmap = ref Cmap.empty

  let register c f = cmap := Cmap.add c f !cmap ; start ()
  let unregister c = cmap := Cmap.remove c !cmap
  let registered c = Cmap.mem c !cmap

  let list_registered () = Cmap.fold (fun k _ l -> k :: l) !cmap []


  (* derivated events and signals *)
  let running =
    React.S.hold
      false
      (React.E.select
         [ React.E.map (fun () -> true) start_e ;
           React.E.map (fun () -> false) stop_e ; ]
      )

  (* action *)
  let rec run () = match list_registered () with
      | [] -> Lwt.pause () >|= stop
      | regs ->
          let up_msg = Messages.encode_upgoing regs in
          Lwt.catch (fun () ->
            (*TODO: treat server errors properly : exponential waiting time *)
            (*TODO: get rid of alert in error handling *)

            Lwt_obrowser.http_post_with_content_type
              "./"
              "application/x-ocsigen-comet"
              [("registration", up_msg)]            >|= snd >|=
            Messages.decode_downcoming              >>=
            Lwt_list.iter_s
              (fun (c,m) ->
                 try (Cmap.find c !cmap) m
                 with Not_found -> Lwt.return ()
              )                                     >>=
            run

          ) (function
               | Messages.Incorrect_encoding -> Lwt_obrowser.sleep 2. >>= run
               | e -> stop () ;
                      Dom_html.window##alert (Js.string (Printexc.to_string e)) ;
                      Lwt.fail e
          )

  (* Loops and notifications *)
  let _ =
    let run_thread = ref None in
    Lwt_event.always_notify_s
      (function
         | true  -> run_thread := Some (run ()) ; Lwt.return ()
         | false -> match !run_thread with
             | Some t -> Lwt.cancel t ; run_thread := None ; Lwt.return ()
             | None -> Lwt.return ()
      )
      (React.S.changes running)


end


(* Unwrapping of channels *)

let unwrap (c : 'a Ecc.chan_id Eliom_client_types.data_key) : 'a Ecc.chan_id =
  Eliom_obrowser.unwrap c


module Registration :
sig

  val register : 'a Ecc.chan_id -> ('a -> unit Lwt.t) -> unit
  val unregister : 'a Ecc.chan_id -> unit

end = struct

  let register c f =
    Engine.register c
      (fun x -> f (Marshal.from_string (Ocsigen_lib.urldecode_string x) 0))
  let unregister c = Engine.unregister c

end
