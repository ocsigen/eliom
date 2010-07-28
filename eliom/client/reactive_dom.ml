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

let eventify_mouse
      (x : <onclick :     ('self Js.t, Dom_html.mouseEvent Js.t) Dom_html.event_listener Js.writeonly_prop;
            ondblclick :  ('self Js.t, Dom_html.mouseEvent Js.t) Dom_html.event_listener Js.writeonly_prop;
            onmousedown : ('self Js.t, Dom_html.mouseEvent Js.t) Dom_html.event_listener Js.writeonly_prop;
            onmouseup :   ('self Js.t, Dom_html.mouseEvent Js.t) Dom_html.event_listener Js.writeonly_prop;
            onmouseover : ('self Js.t, Dom_html.mouseEvent Js.t) Dom_html.event_listener Js.writeonly_prop;
            onmousemove : ('self Js.t, Dom_html.mouseEvent Js.t) Dom_html.event_listener Js.writeonly_prop;
            onmouseout :  ('self Js.t, Dom_html.mouseEvent Js.t) Dom_html.event_listener Js.writeonly_prop;
           .. > Js.t
      )
      (evt :
         [ `Click
         | `Dblclick
         | `Mousedown
         | `Mouseup
         | `Mouseover
         | `Mousemove
         | `Mouseout ]
      )
      (f : #Dom_html.mouseEvent Js.t -> 'a)
      : 'a React.E.t =
  let (e, push_e) = React.E.create () in
  let handler = Dom_html.handler (fun b -> push_e (f b); Js._false) in
  begin match evt with
    | `Click -> x##onclick <- handler
    | `Dblclick -> x##ondblclick <- handler
    | `Mousedown -> x##onmousedown <- handler
    | `Mouseup -> x##onmouseup <- handler
    | `Mouseover -> x##onmouseover <- handler
    | `Mousemove -> x##onmousemove <- handler
    | `Mouseout -> x##onmouseout <- handler
  end; e

let eventify_keyboard
      (x : <onkeypress : ('self Js.t, Dom_html.keyboardEvent Js.t) Dom_html.event_listener Js.writeonly_prop;
            onkeydown :  ('self Js.t, Dom_html.keyboardEvent Js.t) Dom_html.event_listener Js.writeonly_prop;
            onkeyup :    ('self Js.t, Dom_html.keyboardEvent Js.t) Dom_html.event_listener Js.writeonly_prop;
            .. > Js.t
      )
      (evt :
         [ `Keypress
         | `Keydown
         | `Keyup ]
      )
      (f : #Dom_html.keyboardEvent Js.t -> 'a)
      : 'a React.E.t =
  let (e, push_e) = React.E.create () in
  let handler = Dom_html.handler (fun b -> push_e (f b); Js._false) in
  begin match evt with
    | `Keypress -> x##onkeypress <- handler
    | `Keydown -> x##onkeydown <- handler
    | `Keyup -> x##onkeyup <- handler
  end; e


