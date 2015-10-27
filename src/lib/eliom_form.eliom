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

{shared{

module type S = sig

  include Html5_sigs.T
    with type 'a Xml.W.t = 'a
     and type 'a Xml.W.tlist = 'a list

  type ('a, 'b, 'c) lazy_star =
    ?a: (('a attrib) list) ->
    ('b elt) list Eliom_lazy.request ->
    'c elt

  val lazy_form:
    ([< Html5_types.form_attrib ],
     [< Html5_types.form_content_fun ],
     [> Html5_types.form ]) lazy_star

  val uri_of_fun : (unit -> string) -> Xml.uri

end

module type Attribs = sig

  type +'a attrib

  val attrib_of_service :
    string ->
    ([ `A | `Form_get | `Form_post] *
     (bool * string list) option *
     string option) option Eliom_lazy.request ->
    Html5_types.form_attrib attrib

  val attrib_onclick :
    (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value ->
    Html5_types.a_attrib attrib

end

module type S_with_attribs = sig
  include S
  include Attribs with type +'a attrib := 'a attrib
end

module Html5_forms_base (Html5 : S) = struct

  type +'a elt = 'a Html5.elt
  type +'a attrib = 'a Html5.attrib

  type uri = Html5.uri

  let a_input_required () = Html5.a_required `Required
  let a_select_required () = Html5.a_required `Required

  open Html5

  let uri_of_string = Html5.uri_of_fun

  let select_content_of_option a =
    (a :> Html5_types.select_content elt)

  let make_pcdata s = pcdata s

  let make_a ?(a = []) ?href l =
    let a = match href with
      | None -> a
      | Some href -> a_href href :: a
    in
    Html5.a ~a l

  let make_empty_form_content () = fieldset []

  let make_get_form ?(a = []) ~action elts =
    let a = a_method `Get :: a_action action :: a in
    Html5.lazy_form ~a:a elts

  let make_post_form ?(a = []) ~action ?id ?(inline = false) elts =
    let a =
      match id with
      | None -> a
      | Some i -> a_id i :: a
    in
    let a =
      Html5.a_enctype "multipart/form-data" ::
      (* Always Multipart!!! How to test if there is a file?? *)
      a_action action ::
      a_method `Post ::
      if inline then a_class ["inline"] :: a else a
    in
    lazy_form ~a elts

  let cons_hidden_fieldset fields content =
    let fieldset =
      Html5.fieldset
        ~a:[a_style "display: none;"]
        fields in
    fieldset :: content

  let make_input ?(a = [])
      ?(checked=false) ~typ ?name ?src ?value () =
    let a = match value with
      | None -> a
      | Some v -> a_value v :: a
    in
    let a = match name with
      | None -> a
      | Some v -> a_name v :: a
    in
    let a = match src with
      | None -> a
      | Some v -> a_src v :: a
    in
    let a = if checked then a_checked `Checked :: a else a in
    let a = a_input_type typ :: a in
    input ~a ()

  let make_button ?(a = []) ~button_type ?name ?value c =
    let a = match value with
      | None -> a
      | Some v -> a_text_value v :: a
    in
    let a = match name with
      | None -> a
      | Some v -> a_name v :: a
    in
    button ~a:(a_button_type button_type :: a) c

  let make_textarea ?(a = []) ~name ?(value = "") () =
    let a = a_name name :: a in
    textarea ~a (pcdata value)

  let make_select ?(a = []) ~multiple ~name elt elts =
    let a = if multiple then a_multiple `Multiple :: a else a in
    let a = a_name name :: a in
    select ~a (elt :: elts)

  let make_option ?(a = []) ~selected ?value c =
    let a = match value with
      | None -> a
      | Some v -> a_text_value v :: a
    in
    let a = if selected then a_selected `Selected :: a else a in
    option ~a c

  let make_optgroup ?(a = []) ~label elt elts =
    optgroup ~label ~a (elt :: elts)

  let make_css_link ?(a = []) ~uri () =
    let a = a_mime_type "text/css" :: a in
    link ~href:uri ~rel:[`Stylesheet] ~a ()

  let make_js_script ?(a = []) ~uri () =
    let a = a_mime_type "text/javascript" :: a_src uri :: a in
    script ~a (pcdata "")

  let make_for_attrib = a_for

end

module MakeApplForms

    (Forms: sig
       include Eliom_form_sigs.S
       include Attribs with type +'a attrib := 'a attrib
     end) = struct

  include Forms

  let make_info ~https kind service =
    let f () =
      match Eliom_service.xhr_with_cookies service with
      | None ->
        None
      | Some tmpl ->
        Some (kind, Eliom_uri.make_cookies_info (https, service), tmpl)
    in
    Eliom_lazy.from_fun f

  let get_xhr = function
    | Some xhr -> xhr
    | None -> Eliom_config.get_default_links_xhr ()

  let a_onclick_service info = attrib_of_service "onclick" info

  let a_onsubmit_service info = attrib_of_service "onsubmit" info

  let a ?absolute ?absolute_path ?https ?(a = [])
      ~service ?hostname ?port ?fragment ?keep_nl_params ?nl_params
      ?xhr
      content getparams =
    let xhr = get_xhr xhr in
    let a =
      match xhr, Eliom_service.get_client_fun_ service with
      | true, _
      | _, Some _ ->
        attrib_onclick
          {{ fun ev ->
             if not (Eliom_client.middleClick ev) then begin
               Dom.preventDefault ev;
               Dom_html.stopPropagation ev;
               Lwt.async (fun () ->
                 Eliom_client.change_page
                   ?absolute:%absolute
                   ?absolute_path:%absolute_path
                   ?https:%https
                   ~service:%service
                   ?hostname:%hostname
                   ?port:%port
                   ?fragment:%fragment
                   ?keep_nl_params:%keep_nl_params
                   ?nl_params:%nl_params
                 %getparams ())
             end
           }} :: a
      | _ -> a
    in
    Forms.a
      ?absolute ?absolute_path ?https ~a ~service ?hostname ?port
      ?fragment ?keep_nl_params ?nl_params ~xhr
      content getparams

  let warn_client_service service =
    if Eliom_service.get_client_fun_ service <> None then
      Eliom_lib.debug
        "Client side services not implemented with forms. \
         Please do it manually using \
         Eliom_client.change_page, or contribute."

  let get_form
      ?absolute ?absolute_path ?https ?(a = []) ~service ?hostname
      ?port ?fragment ?keep_nl_params ?nl_params ?xhr
      contents =
    let a =
      if get_xhr xhr then
        let info = make_info ~https `Form_get service in
        a_onsubmit_service info :: a
      else
        a
    in
    warn_client_service service;
    Forms.get_form
      ?absolute ?absolute_path ?https ~a ~service ?hostname ?port
      ?fragment ?keep_nl_params ?nl_params
      contents

  let lwt_get_form
      ?absolute ?absolute_path ?https ?(a = []) ~service ?hostname
      ?port ?fragment ?keep_nl_params ?nl_params ?xhr
      contents =
    let a =
      if get_xhr xhr then
        let info = make_info ~https `Form_get service in
        a_onsubmit_service info :: a
      else
        a
    in
    warn_client_service service;
    Forms.lwt_get_form
      ?absolute ?absolute_path ?https ~a ~service ?hostname ?port
      ?fragment ?nl_params ?keep_nl_params
      contents

  let post_form
      ?absolute ?absolute_path ?https ?(a = []) ~service ?hostname
      ?port ?fragment ?keep_nl_params ?keep_get_na_params ?nl_params
      ?xhr
      contents getparams =
    let a =
      if get_xhr xhr then
        let info = make_info ~https `Form_post service in
        a_onsubmit_service info :: a
      else
        a
    in
    warn_client_service service;
    Forms.post_form
      ?absolute ?absolute_path ?https ~a ~service ?hostname ?port
      ?fragment ?keep_nl_params ?keep_get_na_params ?nl_params
      contents getparams

  let lwt_post_form
      ?absolute ?absolute_path ?https ?(a = []) ~service ?hostname
      ?port ?fragment ?keep_nl_params ?keep_get_na_params ?nl_params
      ?xhr
      contents getparams =
    let a =
      if get_xhr xhr then
        let info = make_info ~https `Form_post service in
        a_onsubmit_service info :: a
      else
        a
    in
    warn_client_service service;
    Forms.lwt_post_form
      ?absolute ?absolute_path ?https ~a ~service ?hostname ?port
      ?fragment ?keep_nl_params ?keep_get_na_params ?nl_params
      contents getparams

end

module Make (H : S_with_attribs) = struct
  include MakeApplForms(struct
      include Eliom_mkforms.MakeForms(Html5_forms_base(H))
      let attrib_of_service = H.attrib_of_service
      let attrib_onclick = H.attrib_onclick
      type uri = H.uri
    end)
end

}}
