(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_predefmod
 * Copyright (C) 2007 Vincent Balat
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

type appl_service = Eliom_service.appl_service
type http_service = Eliom_service.http_service
type non_caml_service = Eliom_service.non_caml_service

type input_type =
  [
  | `Url
  | `Tel
  | `Text
  | `Time
  | `Search
  | `Password
  | `Checkbox
  | `Range
  | `Radio
  | `Submit
  | `Reset
  | `Number
  | `Hidden
  | `Month
  | `Week
  | `File
  | `Email
  | `Image
  | `Datetime_local
  | `Datetime
  | `Date
  | `Color
  | `Button]

type button_type =
    [ `Button
    | `Reset
    | `Submit
    ]

(*BB Has nothing to do with Eliom_registration in fact, should live in something like Eliom_content_base. *)
module Html5_forms : sig
  module F : "sigs/eliom_html5_forms.mli"
  module D : "sigs/eliom_html5_forms.mli"
end
