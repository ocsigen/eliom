(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2016 Vasilis Papavasileiou
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

module Html : Eliom_registration_sigs.S
  with type page = Html_types.html Eliom_content.Html.elt
   and type options = unit

module Action : Eliom_registration_sigs.S
  with type page = unit
   and type options = [ `Reload | `NoReload ]

module App (P : Eliom_registration_sigs.APP_PARAM) : sig
  val application_name : string
  include module type of Html
end

(**/**)

module type Base = sig
  type return = Eliom_service.non_ocaml
end

module Block5 : Base
module Html_text : Base
module CssText : Base
module Text : Base
module String : Base
module Unit : Base
module String_redirection : Base
module Any : Base
module Streamlist : Base

module Ocaml : sig
  type 'a return = 'a Eliom_service.ocaml
end

module Redirection : sig
  type 'a return = 'a
end
