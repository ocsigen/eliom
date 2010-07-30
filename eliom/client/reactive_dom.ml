(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Raphael Proust
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

(* This module is intended for wrapping Dom related values into higher level
 * reactive values. *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

module Engine : sig

  val set_timer : float -> unit

  val register : (unit -> unit) -> unit
  (*TODO? make unregisterable *)

end = struct

  let timer = ref 0.2

  let set_timer f = timer := f

  let registered = ref []

  let register f = registered := f :: !registered

  let rec poll () =
    Lwt_js.sleep !timer >>= fun () ->
    List.iter (fun f -> f ()) !registered;
    poll ()

  let _ = poll ()

end


let signalify (cb : (unit -> 'a)) : 'a React.S.t =
  let (s, set_s) = React.S.create (cb ()) in
  ignore (Engine.register (fun () -> set_s (cb ())));
  s

let eventify_mouse target typ f =
  let (e, push_e) = React.E.create () in
  ignore (Events.listen target typ (fun n e -> push_e (f n e)));
  e


let eventify_keyboard target typ f =
  let (e, push_e) = React.E.create () in
  ignore (Events.listen target typ (fun n e -> push_e (f n e)));
  e

