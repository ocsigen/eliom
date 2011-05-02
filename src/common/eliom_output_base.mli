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

open Eliom_pervasives

module type HTML5_FORMS = "sigs/eliom_forms.mli"
  subst type uri := HTML5_types.uri
    and type pcdata_elt := HTML5_types.pcdata HTML5.M.elt

    and type form_elt := [> HTML5_types.form ] HTML5.M.elt
    and type form_content_elt := HTML5_types.form_content HTML5.M.elt
    and type form_content_elt_list := HTML5_types.form_content HTML5.M.elt list
    and type form_attrib_t := HTML5_types.form_attrib HTML5.M.attrib list

    and type 'a a_elt := [> 'a HTML5_types.a ] HTML5.M.elt
    and type 'a a_content_elt := 'a HTML5.M.elt
    and type 'a a_content_elt_list := 'a HTML5.M.elt list
    and type a_attrib_t := HTML5_types.a_attrib HTML5.M.attrib list

    and type link_elt := [> HTML5_types.link ] HTML5.M.elt
    and type link_attrib_t := HTML5_types.link_attrib HTML5.M.attrib list

    and type script_elt := [> HTML5_types.script ] HTML5.M.elt
    and type script_attrib_t := HTML5_types.script_attrib HTML5.M.attrib list

    and type textarea_elt := [> HTML5_types.textarea ] HTML5.M.elt
    and type textarea_attrib_t := HTML5_types.textarea_attrib HTML5.M.attrib list

    and type input_elt := [> HTML5_types.input ] HTML5.M.elt
    and type input_attrib_t := HTML5_types.input_attrib HTML5.M.attrib list

    and type select_elt := [> HTML5_types.select ] HTML5.M.elt
    and type select_attrib_t := HTML5_types.select_attrib HTML5.M.attrib list

    and type button_elt := [> HTML5_types.button ] HTML5.M.elt
    and type button_content_elt := HTML5_types.button_content HTML5.M.elt
    and type button_content_elt_list := HTML5_types.button_content HTML5.M.elt list
    and type button_attrib_t := HTML5_types.button_attrib HTML5.M.attrib list

    and type optgroup_attrib_t := [ HTML5_types.common | `Disabled ] HTML5.M.attrib list
    and type option_attrib_t := HTML5_types.option_attrib HTML5.M.attrib list

    and type input_type_t :=
      [< `Hidden
      | `Password
      | `Submit
      | `Text ]

    and type raw_input_type_t :=
      [< `Button
      | `Hidden
      | `Password
      | `Reset
      | `Submit
      | `Text ]

    and type button_type_t :=
      [< `Button
      | `Reset
      | `Submit
      ]

module Html5_forms : HTML5_FORMS

(**/**)

module type HTML5_FORMS_CLOSED = "sigs/eliom_forms.mli"
  subst type uri := HTML5_types.uri
    and type pcdata_elt := HTML5_types.pcdata HTML5.M.elt

    and type form_elt := HTML5_types.form HTML5.M.elt
    and type form_content_elt := HTML5_types.form_content HTML5.M.elt
    and type form_content_elt_list := HTML5_types.form_content HTML5.M.elt list
    and type form_attrib_t := HTML5_types.form_attrib HTML5.M.attrib list

    and type 'a a_elt := 'a HTML5_types.a HTML5.M.elt
    and type 'a a_content_elt := 'a HTML5.M.elt
    and type 'a a_content_elt_list := 'a HTML5.M.elt list
    and type a_attrib_t := HTML5_types.a_attrib HTML5.M.attrib list

    and type link_elt := HTML5_types.link HTML5.M.elt
    and type link_attrib_t := HTML5_types.link_attrib HTML5.M.attrib list

    and type script_elt := HTML5_types.script HTML5.M.elt
    and type script_attrib_t := HTML5_types.script_attrib HTML5.M.attrib list

    and type textarea_elt := HTML5_types.textarea HTML5.M.elt
    and type textarea_attrib_t := HTML5_types.textarea_attrib HTML5.M.attrib list

    and type input_elt := HTML5_types.input HTML5.M.elt
    and type input_attrib_t := HTML5_types.input_attrib HTML5.M.attrib list

    and type select_elt := HTML5_types.select HTML5.M.elt
    and type select_attrib_t := HTML5_types.select_attrib HTML5.M.attrib list

    and type button_elt := HTML5_types.button HTML5.M.elt
    and type button_content_elt := HTML5_types.button_content HTML5.M.elt
    and type button_content_elt_list := HTML5_types.button_content HTML5.M.elt list
    and type button_attrib_t := HTML5_types.button_attrib HTML5.M.attrib list

    and type optgroup_attrib_t := [ HTML5_types.common | `Disabled ] HTML5.M.attrib list
    and type option_attrib_t := HTML5_types.option_attrib HTML5.M.attrib list

    and type input_type_t :=
      [ `Button
      | `Checkbox
      | `File
      | `Hidden
      | `Image
      | `Password
      | `Radio
      | `Reset
      | `Submit
      | `Text
      ]

    and type raw_input_type_t :=
      [ `Button
      | `Checkbox
      | `File
      | `Hidden
      | `Image
      | `Password
      | `Radio
      | `Reset
      | `Submit
      | `Text
      ]

    and type button_type_t :=
      [ `Button
      | `Reset
      | `Submit
      ]

module Html5_forms_closed : HTML5_FORMS_CLOSED

module Open_Html5_forms :
  functor (Html5_forms_closed : HTML5_FORMS_CLOSED) -> HTML5_FORMS

(**/**)
