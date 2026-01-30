(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015 Vasilis Papavasileiou
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

module type LINKS_AND_FORMS = sig
  type +'a elt
  type +'a attrib
  type uri
  type (_, _, _) star
  type 'a form_param

  type ('a, 'b, 'c) lazy_star =
    ?a:'a attrib list -> 'b elt list Eliom_lazy.request -> 'c elt

  val lazy_form :
    ( [< Html_types.form_attrib]
      , [< Html_types.form_content_fun]
      , [> Html_types.form] )
      lazy_star

  include
    Eliom_form_sigs.LINKS
    with type +'a elt := 'a elt
     and type +'a attrib := 'a attrib
     and type uri := uri

  module Form :
    Eliom_form_sigs.S
    with type +'a elt := 'a elt
     and type +'a attrib := 'a attrib
     and type uri := uri
     and type 'a param = 'a form_param
end
