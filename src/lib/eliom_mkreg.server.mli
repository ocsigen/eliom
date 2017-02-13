(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_mkreg
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


(** This module defines the functor to use to creates modules
    generating functions to register services for your own types of
    pages.  It is used for example in {!Eliom_registration}.  *)

(** {2 Creates modules to register services for one type of pages} *)
module Make
    (Pages: Eliom_registration_sigs.PARAM
     with type frame := Ocsigen_response.t) :
  Eliom_registration_sigs.S_with_create
  with type page = Pages.page
   and type options = Pages.options
   and type result = Pages.result

(** {2 Creating modules to register services for one type of
    parametrised pages} *)
module Make_poly
    (Pages : Eliom_registration_sigs.PARAM_POLY
     with type frame := Ocsigen_response.t) :
  Eliom_registration_sigs.S_poly_with_create
  with type 'a page   = 'a Pages.page
   and type options   = Pages.options
   and type 'a return = 'a Pages.return

(**/**)
val suffix_redir_uri_key : string Polytables.key
