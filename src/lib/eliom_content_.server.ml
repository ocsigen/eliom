(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2012 Vincent Balat, Benedikt Becker
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
open Eliom_content_core

module type Forms = "sigs/eliom_forms.mli"

module Xml = Eliom_content_core.Xml

module Svg = struct

  module F = Svg.F
  module D = Svg.D
  module R = struct
    module Raw = Eliom_csreact_content.Svg.R
    include Raw
    let pcdata _ = `Unimplemented
  end

  module Id = Svg.Id

  type +'a elt = 'a F.elt
  type 'a wrap = 'a F.wrap
  type 'a list_wrap = 'a F.list_wrap
  type 'a attrib = 'a F.attrib
  type uri = F.uri

  module Printer = Svg.Printer

end

module Html5 = struct

  module F = struct
    include Html5.F
    include Eliom_registration_base.Html5_forms.F
    let raw_form = form
    let form = get_form
    let input = string_input
    let select = string_select ?required:None
  end

  module D = struct
    include Html5.D
    include Eliom_registration_base.Html5_forms.D
    let raw_form = form
    let form = get_form
    let input = string_input
    let select = string_select ?required:None
  end

  module R = Eliom_csreact_content.Html5.R

  module Custom_data = Eliom_content_core.Html5.Custom_data

  module Id = Html5.Id

  module Printer = Html5.Printer

  type +'a elt = 'a F.elt
  type 'a wrap = 'a F.wrap
  type 'a list_wrap = 'a F.list_wrap
  type 'a attrib = 'a F.attrib
  type uri = F.uri
end

module Html_text = struct
  module Forms_base = struct

    type uri = string
    type pcdata_elt = string

    type form_elt = string
    type form_content_elt = string
    type form_content_elt_list = string
    type form_attrib_t = string

    type +'a a_elt = string
    type +'a a_elt_list = string
    type +'a a_content_elt = string
    type +'a a_content_elt_list = string
    type a_attrib_t = string

    type link_elt = string
    type link_attrib_t = string

    type script_elt = string
    type script_attrib_t = string

    type textarea_elt = string
    type textarea_attrib_t = string

    type input_elt = string
    type input_attrib_t = string
    let a_input_required `Required = "required"
    let input_attrib_append = (^)

    type select_elt = string
    type select_content_elt = string
    type select_content_elt_list = string
    type select_attrib_t = string
    let select_content_cons = (^)
    let a_select_required `Required = "required"
    let select_attrib_append = (^)

    type button_elt = string
    type button_content_elt = string
    type button_content_elt_list = string
    type button_attrib_t = string

    type option_elt = string
    type option_elt_list = string
    type optgroup_attrib_t = string
    type option_attrib_t = string

    type input_type_t = string
    type button_type_t = string

    let hidden = "hidden"
    (* let text = "text" let password = "password" *)
    let checkbox = "checkbox"
    let radio = "radio"
    let submit = "submit"
    let file = "file"
    let image = "image"

    let buttonsubmit = "submit"

    let uri_of_string x = x ()

    let empty_seq = ""
    let cons_form a l = a^l

    let map_option f =
      List.fold_left (fun d a -> d^(f a)) ""

    let map_optgroup f a l =
      ((f a), List.fold_left (fun d a -> d^(f a)) "" l)

    let select_content_of_option = id

    let make_pcdata = id

    let make_a ?(a="") ?href l : 'a a_elt =
      let a = match href with
        | None -> a
        | Some v -> " href=\""^v^"\" "^a
      in
      "<a "^a^">"^(* List.fold_left (^) "" l *) l^"</a>"

    let make_get_form ?(a="") ~action elts : form_elt =
      "<form method=\"get\" action=\""^ action ^"\""^a^">"^
      Eliom_lazy.force elts^"</form>"

    let make_post_form ?(a="") ~action ?id ?(inline = false) elts
        : form_elt =
      let aa = "enctype=\"multipart/form-data\" "
          (* Always Multipart!!! How to test if there is a file?? *)
        ^(match id with
          None -> a
        | Some i -> " id="^i^" "^a)
      in
      "<form method=\"post\" action=\""^ action ^"\""^
      (if inline then "style=\"display: inline\"" else "")^aa^">"^
      Eliom_lazy.force elts^"</form>"

    let empty_seq = ""
    let cons_hidden_fieldset fields content =
      "<fieldset style=\"display: none;\">"
      ^ Eliom_lib.String.concat "" fields
      ^ "</fieldset>"
      ^ content

    let make_input ?(a="") ?(checked=false) ~typ ?name ?src ?value () =
      let a2 = match value with
        None -> a
      | Some v -> " value="^v^" "^a
      in
      let a2 = match name with
        None -> a2
      | Some v -> " name="^v^" "^a2
      in
      let a2 = match src with
        None -> a2
      | Some v -> " src="^v^" "^a2
      in
      let a2 = if checked then " checked=\"checked\" "^a2 else a2 in
      "<input type=\""^typ^"\" "^a2^"/>"

    let make_button ?(a="") ~button_type ?name ?value c =
      let a2 = match value with
        None -> a
      | Some v -> " value="^v^" "^a
      in
      let a2 = match name with
        None -> a2
      | Some v -> " name="^v^" "^a2
      in
      "<button type=\""^button_type^"\" "^a2^">"^c^"</button>"

    let make_textarea ?(a="") ~name:name ?(value="") () =
      "<textarea name=\""^name^"\" "^a^">"^value^"</textarea>"

    let make_select ?(a="") ~multiple ~name elt elts =
      "<select "^(if multiple then "multiple=\"multiple\" " else "")^
      "name=\""^name^"\" "^a^">"^elt^elts^"</select>"

    let make_option ?(a="") ~selected ?value c =
      let a = match value with
        None -> a
      | Some v -> " value="^v^" "^a
      in
      "<option "^(if selected then "selected=\"selected\" " else "")^
      a^">"^c^"</option>"

    let make_optgroup ?(a="") ~label elt elts =
      "<optgroup label=\""^label^"\" "^
      a^">"^elt^elts^"</optgroup>"


    let make_css_link ?(a="") ~uri () =
      "<link href=\""^uri^" type=\"text/css\" rel=\"stylesheet\" "^a^"/>"

    let make_js_script ?(a="") ~uri () =
      "<script src=\""^uri^" contenttype=\"text/javascript\" "^a^"></script>"

    type for_attrib = string
    let make_for_attrib name = "for=\""^name^"\""

  end
  include Eliom_mkforms.MakeForms(Forms_base)
end
