(* Ocsigen
 * http://www.ocsigen.org
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

(* Module for event wrapping and related functions *)


module Down =
struct

  let wrap ~sp e =
    (*TODO: use optionnal argument for max_size. *)
    let chan = Eliom_comet.Dlisted_channels.create ~max_size:5 e in
    let `R r = React.E.retain e (fun () -> ()) in
    let `R _ = React.E.retain e (fun () -> r () ; ignore chan) in
    Eliom_comet.Dlisted_channels.wrap ~sp chan

end
