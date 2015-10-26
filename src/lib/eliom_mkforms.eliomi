(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_mkforms
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

{shared{

(** This module defines the functor to use to creates modules
    generating form widgets for your own types of pages.  It is used
    for example in {!Eliom_registration}. *)

module MakeForms(Pages: Eliom_form_sigs.PARAM) :
  Eliom_form_sigs.S
  with type +'a elt := 'a Pages.elt
   and type +'a attrib := 'a Pages.attrib
   and type uri = Pages.uri

}}
