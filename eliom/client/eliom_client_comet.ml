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


(* comet specific constants *)
let content_type = "application/x-ocsigen-comet"


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
    let splited = Regexp.split chan_delim_regexp s in
    let splited_twice =
      Array.map
        (fun s ->
           let a = Regexp.split field_delim_regexp s in
           match a with
             | [|chan; msg|] -> (chan, url_decode msg)
             | _ -> raise Incorrect_encoding
        )
        splited
    in
      Array.to_list splited_twice

end


(* Engine : the main loop for comet channel management *)
module Engine :
sig

  (* [register c f] registers to the channel [c], calling [f] on each server
   * pushed value.
   * If the engine isn't running at this point, it is started. *)
  val register : string -> (string -> unit Lwt.t) -> unit

  (* [unregister c] cancels all registration on channel [c] *)
  val unregister : string -> unit

  (* [registered c] is [true] if [c] has already been registered, else it's
   * [false] *)
  val registered : string -> bool

  (* started is true when the engine is running and false otherwise *)
  val running : bool React.S.t

  val start : unit -> unit
  val stop : unit -> unit

end = struct

  (* Primitive events for the reactive engine *)
  let (start_e,   start  ) = React.E.create ()
  let (stop_e,    stop   ) = React.E.create ()
  let (restart_e, restart) = React.E.create ()

  (* derivated events and signals *)
  let running =
    React.S.hold
      false
      (React.E.select
         [ React.E.map (fun () -> true) start_e ;
           React.E.map (fun () -> false) stop_e ; ]
      )

  let running_changes =
    React.E.select
      [
        React.S.changes running;
        React.E.map (fun () -> true) restart_e;
      ]


  (* Managing registration set (map actually) *)

  module Cmap =
    Map.Make (struct type t = string let compare = compare end)

  let cmap = ref Cmap.empty

  let registered c = Cmap.mem c !cmap
  let register c f =
    if registered c                       (* if already registered...         *)
    then cmap := Cmap.add c f !cmap       (*...just change the closure...     *)
    else (cmap := Cmap.add c f !cmap ;    (*...else don't forget to restart ! *)
          restart ())
  let unregister c = cmap := Cmap.remove c !cmap

  let list_registered () = Cmap.fold (fun k _ l -> k :: l) !cmap []

  let more_slp f = if f >= 64. then 128. else 2. *. f

  (* action *)
  let rec run slp wt = match list_registered () with
      | [] -> Lwt.pause () >|= stop
      | regs ->
          let up_msg = Messages.encode_upgoing regs in
          Lwt.catch (fun () ->

          (* make asynchronous request *)
            let async =
              XmlHttpRequest.send
                ~content_type
                ~post_args:[("registration", up_msg)]
                "./"
            in
            Lwt.on_cancel wt (fun () -> Lwt.cancel async) ;
            async >>= fun (code, msg) ->
          (* check returned code *)
            match code / 100 with
          (* treat failure *)
              | 0 | 3 | 4 -> (stop () ; Lwt.return ())
          (* treat semi-failure *)
              | 1 -> run slp wt
              | 5 -> begin (* server error *)
                   Lwt_js.sleep (1. +. Random.float slp) >>= fun () ->
                   run (more_slp slp) wt
                end
          (* treat success *)
              | 2 ->
                  (if msg = "" (* no server message *)
                   then run 1. wt
                   else begin
                     List.iter
                       (fun (c,m) ->
                          (*RRR: create the thread immediatly so that canceling
                           * can't happen here !*)
                          try
                            if m = "ENDED_CHANNEL"
                            then unregister c
                            else let _ = (Cmap.find c !cmap) m in ()
                          with Not_found -> ()
                       )
                       (Messages.decode_downcoming msg) ;
                       run 1. wt
                   end)
              | _ -> (stop () ; Lwt.return ())

          ) (function
               | Messages.Incorrect_encoding ->
                   (Lwt_js.sleep (1. +. Random.float slp) >>= fun () ->
                    run (more_slp slp) wt)
               | Lwt.Canceled -> Lwt.return ()
               | e ->
                   (stop () ;
                    Dom_html.window##alert (Js.string (Printexc.to_string e)) ;
                    Lwt.fail e)
          )

  (* Loops and notifications *)
  let _ =
    let run_wk = ref None in
    Lwt_event.always_notify
      (function
         | true  ->
             let (wt, wk) = Lwt.task () in
             begin match !run_wk with
   (*  start*) | None   -> run_wk := Some wk ; let _ = run 1. wt in ()
   (*restart*) | Some u -> Lwt.wakeup_exn u Lwt.Canceled ;             (*stop *)
                           run_wk := Some wk ; let _ = run 1. wt in () (*start*)
             end
         | false ->
            begin match !run_wk with
   (*  stop*) | Some u -> Lwt.wakeup_exn u Lwt.Canceled ; run_wk := None ;
   (*ignore*) | None   -> ()
            end
      )
      running_changes


end


module Channels =
struct

  let unwrap (c : 'a Ecc.chan_id Eliom_client_types.data_key) : 'a Ecc.chan_id =
    Eliommod_cli.unwrap c

  let decode s = Marshal.from_string s 0
  let register c f =
    Engine.register
      (Ecc.string_of_chan_id c)
      (fun x -> f (decode x))
  let unregister c = Engine.unregister (Ecc.string_of_chan_id c)

end

module Dlisted_channels =
struct

  let unwrap (c : 'a Ecc.buffered_chan_id Eliom_client_types.data_key)
        : 'a Ecc.buffered_chan_id =
    Eliommod_cli.unwrap c

  let decode s = Marshal.from_string s 0
  let register c f =
    (*TODO: use second composant*)
    Engine.register
      (Ecc.string_of_buffered_chan_id c)
      (fun l -> Lwt_list.iter_s (fun (x, _) -> f x) (decode l))
  let unregister c = Engine.unregister (Ecc.string_of_buffered_chan_id c)

end

