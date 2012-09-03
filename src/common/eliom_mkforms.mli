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

open Eliom_lib

(** This module defines the functor to use to creates modules
   generating form widgets for your own types of pages.
   It is used for example in {!Eliom_registration}.
 *)

open Lwt
open Eliom_parameter
open Eliom_service

module type FORMS_PARAM = "sigs/eliom_forms_param.mli"

module MakeForms(Pages: FORMS_PARAM) : sig

  type uri = Pages.uri

  include  "sigs/eliom_forms.mli"
      subst type uri := Pages.uri
        and type pcdata_elt := Pages.pcdata_elt

        and type form_elt := Pages.form_elt
        and type form_content_elt := Pages.form_content_elt
        and type form_content_elt_list := Pages.form_content_elt_list
        and type form_attrib_t := Pages.form_attrib_t

        and type 'a a_elt := 'a Pages.a_elt
        and type 'a a_content_elt := 'a Pages.a_content_elt
        and type 'a a_content_elt_list := 'a Pages.a_content_elt_list
        and type a_attrib_t := Pages.a_attrib_t

        and type link_elt := Pages.link_elt
        and type link_attrib_t := Pages.link_attrib_t

        and type script_elt := Pages.script_elt
        and type script_attrib_t := Pages.script_attrib_t

        and type textarea_elt := Pages.textarea_elt
        and type textarea_attrib_t := Pages.textarea_attrib_t

        and type input_elt := Pages.input_elt
        and type input_attrib_t := Pages.input_attrib_t

        and type select_elt := Pages.select_elt
        and type select_attrib_t := Pages.select_attrib_t

        and type button_elt := Pages.button_elt
        and type button_content_elt := Pages.button_content_elt
        and type button_content_elt_list := Pages.button_content_elt_list
        and type button_attrib_t := Pages.button_attrib_t

        and type optgroup_attrib_t := Pages.optgroup_attrib_t
        and type option_attrib_t := Pages.option_attrib_t

	and type input_type_t := Pages.input_type_t
        and type button_type_t := Pages.button_type_t

        and type for_attrib := Pages.for_attrib

end
