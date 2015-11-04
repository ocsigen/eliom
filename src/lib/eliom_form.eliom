(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_form
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
module type Html5 = sig

  include Html5_sigs.T
    with type 'a Xml.W.t = 'a
     and type 'a Xml.W.tlist = 'a list
     and type Xml.mouse_event_handler =
           (Dom_html.mouseEvent Js.t -> unit) Eliom_lib.client_value

  type ('a, 'b, 'c) lazy_star =
    ?a: (('a attrib) list) ->
    ('b elt) list Eliom_lazy.request ->
    'c elt

  val lazy_form:
    ([< Html5_types.form_attrib ],
     [< Html5_types.form_content_fun ],
     [> Html5_types.form ]) lazy_star

  val uri_of_fun : (unit -> string) -> Xml.uri

  val attrib_of_service :
    string ->
    ([ `A | `Form_get | `Form_post] *
     (bool * string list) option *
     string option) option Eliom_lazy.request ->
    Html5_types.form_attrib attrib

end

let get_xhr = function
  | Some xhr -> xhr
  | None -> Eliom_config.get_default_links_xhr ()

module Make_links (Html5 : Html5) = struct

  type +'a elt = 'a Html5.elt
  type +'a attrib = 'a Html5.attrib
  type uri = Html5.uri

  let make_uri
      ?absolute
      ?absolute_path
      ?https ~service ?hostname ?port ?fragment
      ?keep_nl_params ?nl_params gp =
    Html5.uri_of_fun @@ fun () ->
    Eliom_uri.make_string_uri
      ?absolute ?absolute_path
      ?https ?fragment ~service
      ?hostname ?port ?keep_nl_params ?nl_params gp

  let uri_of_string = Html5.uri_of_fun

  let a ?absolute ?absolute_path ?https ?(a = [])
      ~service ?hostname ?port ?fragment ?keep_nl_params ?nl_params
      ?xhr
      content getparams =
    let a =
      let href =
        Html5.uri_of_fun @@ fun () ->
        Eliom_uri.make_string_uri
          ?absolute ?absolute_path ?https ~service ?hostname ?port
          ?fragment ?keep_nl_params ?nl_params getparams
      in
      let href = Html5.a_href href in
      match get_xhr xhr, Eliom_service.get_client_fun_ service with
      | true, _
      | _, Some _ ->
        let f = {{ fun ev ->
          if not (Eliom_client.middleClick ev) then begin
            Dom.preventDefault ev;
            Dom_html.stopPropagation ev;
            Lwt.async @@ fun () ->
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
              %getparams ()
          end }}
        in
        Html5.a_onclick f :: href :: a
      | _ ->
        href :: a
    in
    Html5.a ~a content

  let css_link ?(a = []) ~uri () =
    let a = Html5.a_mime_type "text/css" :: a in
    Html5.link ~href:uri ~rel:[`Stylesheet] ~a ()

  let js_script ?(a = []) ~uri () =
    let a =
      Html5.a_mime_type "text/javascript" ::
      Html5.a_src uri ::
      a
    in
    Html5.script ~a (Html5.pcdata "")

end

module Make (Html5 : Html5) = struct

  type +'a elt = 'a Html5.elt
  type +'a attrib = 'a Html5.attrib
  type uri = Html5.uri

  open Html5

  let id = Eliom_lib.id

  let make_post_form ?(a = []) ~action ?id ?(inline = false) elts =
    let a =
      match id with
      | None -> a
      | Some id -> a_id id :: a
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
    Html5.fieldset ~a:[a_style "display: none;"] fields :: content

  let make_input ?(a = []) ?(checked = false) ~typ ?name ?src ?value () =
    let a = match value with
      | None -> a
      | Some value -> a_value value :: a
    in
    let a = match name with
      | None -> a
      | Some name -> a_name name :: a
    in
    let a = match src with
      | None -> a
      | Some src -> a_src src :: a
    in
    let a = if checked then a_checked `Checked :: a else a in
    let a = a_input_type typ :: a in
    input ~a ()

  let make_button ?(a = []) ~button_type ?name ?value c =
    let a = match value with
      | None -> a
      | Some value -> a_text_value value :: a
    in
    let a = match name with
      | None -> a
      | Some name -> a_name name :: a
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

  (** Functions to construct web pages: *)

  let make_post_uri_components = Eliom_uri.make_post_uri_components

  let get_form_
      bind return
      ?absolute ?absolute_path ?https ?a ~service ?hostname ?port
      ?fragment ?(nl_params = Eliom_parameter.empty_nl_params_set)
      ?keep_nl_params
      f =

    let issuffix, paramnames =
      Eliom_service.get_get_params_type_ service |>
      Eliom_parameter.make_params_names
    in

    let components =
      Eliom_lazy.from_fun @@ fun () ->
      Eliom_uri.make_uri_components_
        ?absolute ?absolute_path ?https ~service ?hostname ?port
        ?fragment ~nl_params ?keep_nl_params
        ()
    in

    let uri =
      Html5.uri_of_fun @@ fun () ->
      let uri, _, fragment = Eliom_lazy.force components in
      let uri =
        if issuffix then
          if uri.[String.length uri - 1] = '/' then
            uri^Eliom_common.eliom_nosuffix_page
          else
            String.concat "/" [uri; Eliom_common.eliom_nosuffix_page]
        else
          uri
      in
      match fragment with
      | None -> uri
      | Some f -> String.concat "#" [uri; Eliom_lib.Url.encode f]
    in

    bind (f paramnames) @@ fun inside ->
    let inside =
      Eliom_lazy.from_fun @@ fun () ->
      let (_, hiddenparams, _) = Eliom_lazy.force components
      and f (n, v) =
        let name = n
        and value = Eliommod_parameters.to_string v
        and typ = `Hidden in
        make_input ~typ ~name ~value ()
      in
      cons_hidden_fieldset (List.map f hiddenparams) inside
    and a =
      let a' = [a_method `Get; a_action uri] in
      match a with Some a -> a' @ a | _ -> a'
    in
    return (Html5.lazy_form ~a inside)

  let get_form
      ?absolute ?absolute_path ?https ?a ~service ?hostname ?port
      ?fragment ?keep_nl_params ?nl_params ?xhr f =
    get_form_
      (fun x f -> f x) (fun x -> x)
      ?absolute ?absolute_path
      ?https ?a ~service ?keep_nl_params
      ?nl_params ?hostname ?port ?fragment f

  let post_form_
      bind return
      ?absolute ?absolute_path ?https ?a ~service ?hostname ?port
      ?fragment ?(nl_params = Eliom_parameter.empty_nl_params_set)
      ?(keep_nl_params : [ `All | `Persistent | `None ] option)
      ?keep_get_na_params
      f getparams =

    let _, paramnames =
      Eliom_service.get_post_params_type_ service |>
      Eliom_parameter.make_params_names
    in

    let components =
      Eliom_lazy.from_fun @@ fun () ->
      Eliom_uri.make_post_uri_components_
        ?absolute ?absolute_path ?https ~service ?hostname ?port
        ?fragment ?keep_nl_params ~nl_params ?keep_get_na_params
        getparams
        ()
    in

    bind (f paramnames) @@ fun inside ->
    let inside =
      Eliom_lazy.from_fun @@ fun () ->
      let (_, _, _, hiddenparams) = Eliom_lazy.force components
      and f (name, value) =
        let value = Eliommod_parameters.to_string value in
        make_input ~typ:`Hidden ~name ~value ()
      in
      cons_hidden_fieldset (List.map f hiddenparams) inside
    and action =
      Html5.uri_of_fun @@ fun () ->
      let (uri, g, r, _) = Eliom_lazy.force components in
      Eliom_uri.make_string_uri_from_components (uri, g, r)
    in
    return (make_post_form ?a ~action inside)

  let post_form
      ?absolute ?absolute_path ?https ?a ~service ?hostname ?port
      ?fragment ?keep_nl_params ?keep_get_na_params ?nl_params ?xhr
      f getparams =
    post_form_ (fun x f -> f x) (fun x -> x)
      ?absolute ?absolute_path ?https ?a ~service ?hostname ?port
      ?fragment ?keep_get_na_params
      ?keep_nl_params ?nl_params
      f getparams

  let option_map f = function Some x -> Some (f x) | None -> None

  let gen_input ?a ~input_type
      ?value ?src ?name string_of =
    let name = option_map Eliom_parameter.string_of_param_name name
    and value = option_map string_of value in
    make_input ?a ?value ~typ:input_type ?name ?src ()

  let input ?a ~input_type ?name ?value y =
    let f = Eliom_parameter_base.string_of_atom y in
    gen_input ?a ~input_type ?value ?name f

  let int_input ?a ~input_type
      ?name ?value () =
    gen_input ?a ~input_type ?value ?name string_of_int

  let int32_input ?a ~input_type
      ?name ?value () =
    gen_input ?a ~input_type ?value ?name Int32.to_string

  let int64_input ?a ~input_type
      ?name ?value () =
    gen_input ?a ~input_type ?value ?name Int64.to_string

  let float_input ?a ~input_type
      ?name ?value () =
    gen_input ?a ~input_type ?value ?name Xml_print.string_of_number

  let string_input ?a ~input_type
      ?name ?value () =
    gen_input ?a ~input_type ?value ?name id

  let user_type_input string_of ?a ~input_type
      ?name ?value () =
    gen_input ?a ~input_type ?value ?name string_of

  let raw_input ?a ~input_type ?name ?value () =
    make_input ?a ?value ~typ:input_type ?name ()

  let file_input ?a ~name () =
    make_input ?a ~typ:`File
      ~name:(Eliom_parameter.string_of_param_name name) ()
  (* value attribute not supported by browsers for security reasons *)

  let image_input ?a ~name ~value ?src y =
    let f = Eliom_parameter_base.string_of_atom y in
    gen_input ?a ~input_type:`Image ~name ~value ?src f

  let int_image_input ?a ~name ~value ?src () =
    gen_input ?a ~input_type:`Image ~name ~value ?src string_of_int

  let int32_image_input ?a ~name ~value ?src () =
    gen_input ?a ~input_type:`Image ~name ~value ?src
      Int32.to_string

  let int64_image_input ?a ~name ~value ?src () =
    gen_input ?a ~input_type:`Image ~name ~value ?src
      Int64.to_string

  let float_image_input ?a ~name ~value ?src () =
    gen_input ?a ~input_type:`Image ~name ~value ?src
      Xml_print.string_of_number

  let string_image_input ?a ~name ~value ?src () =
    gen_input ?a ~input_type:`Image ~name ~value ?src id

  let user_type_image_input string_of ?a ~name ~value ?src () =
    gen_input ?a ~input_type:`Image ~name ~value ?src string_of

  let raw_image_input ?a ~(name : string) ~value ?src () =
    make_input ?a ~value ~typ:`Image ?src ~name ()

  let checkbox ?a ?checked ~name ~value y =
    let name = Eliom_parameter.string_of_param_name name
    and value = Eliom_parameter_base.string_of_atom y value
    and typ = `Checkbox in
    make_input ?a ?checked ~typ ~name ~value ()

  let bool_checkbox ?a ?checked ~name () =
    make_input ?a ?checked ~typ:`Checkbox
      ~name:(Eliom_parameter.string_of_param_name name) ()

  let int_checkbox ?a ?checked ~name ~value () =
    make_input ?a ?checked ~typ:`Checkbox
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(string_of_int value) ()

  let int32_checkbox ?a ?checked ~name ~value () =
    make_input ?a ?checked ~typ:`Checkbox
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(Int32.to_string value) ()

  let int64_checkbox ?a ?checked ~name ~value () =
    make_input ?a ?checked ~typ:`Checkbox
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(Int64.to_string value) ()

  let float_checkbox ?a ?checked ~name ~value () =
    make_input ?a ?checked ~typ:`Checkbox
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(Xml_print.string_of_number value) ()

  let string_checkbox ?a ?checked ~name ~value () =
    make_input ?a ?checked ~typ:`Checkbox
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value ()

  let user_type_checkbox string_of ?a ?checked ~name ~value () =
    make_input ?a ?checked ~typ:`Checkbox
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(string_of value) ()

  let raw_checkbox ?a ?checked ~name ~value () =
    make_input ?a ?checked ~typ:`Checkbox ~name ~value ()

  let radio ?a ?checked ~name ~value y =
    let name = Eliom_parameter.string_of_param_name name
    and value = Eliom_parameter_base.string_of_atom y value
    and typ = `Radio in
    make_input ?a ?checked ~typ ~name ~value ()

  let string_radio ?a ?checked ~name ~value () =
    make_input
      ?a ?checked ~typ:`Radio
      ~name:(Eliom_parameter.string_of_param_name name) ~value ()

  let string_radio_required ?a ?checked ~name ~value () =
    let a =
      let required = Html5.a_required `Required in
      match a with
      | None -> [required]
      | Some a -> required :: a
    in
    make_input
      ~a ?checked ~typ:`Radio
      ~name:(Eliom_parameter.string_of_param_name name) ~value ()

  let int_radio ?a ?checked ~name ~value () =
    make_input
      ?a ?checked ~typ:`Radio
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(string_of_int value) ()

  let int32_radio ?a ?checked ~name ~value () =
    make_input
      ?a ?checked ~typ:`Radio
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(Int32.to_string value) ()

  let int64_radio ?a ?checked ~name ~value () =
    make_input
      ?a ?checked ~typ:`Radio
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(Int64.to_string value) ()

  let float_radio ?a ?checked ~name ~value () =
    make_input
      ?a ?checked ~typ:`Radio
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(Xml_print.string_of_number value) ()

  let user_type_radio string_of ?a ?checked ~name ~value () =
    make_input
      ?a ?checked ~typ:`Radio
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(string_of value) ()

  let raw_radio ?a ?checked ~(name : string) ~value () =
    make_input ?a ?checked ~typ:`Radio ~name ~value ()

  let button ?a ~name ~value y c =
    let name = Eliom_parameter.string_of_param_name name
    and value = Eliom_parameter_base.string_of_atom y value
    and button_type = `Submit in
    make_button ?a ~button_type ~name ~value c

  let string_button ?a ~name ~value c =
    make_button ?a ~button_type:`Submit
      ~name:(Eliom_parameter.string_of_param_name name) ~value c

  let int_button ?a ~name ~value c =
    make_button ?a ~button_type:`Submit
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(string_of_int value) c

  let int32_button ?a ~name ~value c =
    make_button ?a ~button_type:`Submit
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(Int32.to_string value) c

  let int64_button ?a ~name ~value c =
    make_button ?a ~button_type:`Submit
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(Int64.to_string value) c

  let float_button ?a ~name ~value c =
    make_button ?a ~button_type:`Submit
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(Xml_print.string_of_number value) c

  let user_type_button string_of ?a ~name ~value c =
    make_button ?a ~button_type:`Submit
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value:(string_of value) c

  let raw_button ?a ~button_type ~name ~value c =
    make_button ?a ~button_type ~name ~value c

  let button_no_value ?a ~button_type c =
    make_button ?a ~button_type c

  let textarea ?a ~name =
    make_textarea ?a ~name:(Eliom_parameter.string_of_param_name name)

  let raw_textarea ?a ~name =
    make_textarea ?a ~name

  type 'a soption =
    Html5_types.option_attrib attrib list
    * 'a (* Content (or value if the following is present) *)
    * Html5_types.pcdata elt option (* if content different from value *)
    * bool (* selected *)

  type 'a select_opt =
    | Optgroup of
        [ Html5_types.common | `Disabled ] attrib list
        * string (* label *)
        * 'a soption
        * 'a soption list
    | Option of 'a soption

  let gen_select ?a ?(multiple=false) ?required ~name
      (fl : 'a select_opt) (ol : 'a select_opt list) string_of =

    let a = match required with
      | None -> a
      | Some _ ->
        let required = Html5.a_required `Required in
        match a with
        | Some a -> Some (required :: a)
        | None -> Some [required]
    in

    let normalize_selected l =
      (* We change the list of option to have exactly one selected
         item.  We do this because the behaviour of browsers differs.
         We select the first one if nothing is selected.  We select
         the first selected if several are selected.  Thus all
         browsers will behave the same way.  *)
      let aux1 found ((a, b, c, selected) as line) =
        if found then
          (a, b, c, false), true
        else
          line, selected
      in
      let rec aux2 found = function
        | line :: l ->
          let line, found = aux1 found line in
          let l, found = aux2 found l in
          line :: l, found
        | [] ->
          [], found
      in
      let rec aux found = function
        | Option line :: l ->
          let line, found = aux1 found line in
          let l, found = aux found l in
          Option line :: l, found
        | Optgroup (a, b, fl, ol) :: l ->
          let fl, found = aux1 found fl in
          let ol, found = aux2 found ol in
          let l, found = aux found l in
          Optgroup (a, b, fl, ol) :: l, found
        | [] ->
          [], found
      in
      let select_first = function
        | Option (a, b, c, _) -> Option (a, b, c, true)
        | Optgroup (a, b, (c, d, e, _), ol) ->
          Optgroup (a, b, (c, d, e, true), ol)
      in
      let newl, found = aux false l in
      if found then
        List.hd newl, List.tl newl, true
      else
        let first = List.hd newl in
        (* We select the first one by default *)
        let first =
          match required with
          | None -> select_first first
          | _ -> first
        in
        first, (List.tl newl), false
    in

    let fl, ol, has_selected =
      if multiple then
        fl, ol, let _, _, hs = normalize_selected (fl :: ol) in hs
      else
        normalize_selected (fl :: ol)
    in
    let make_opt (a, cv, co, sel) =
      (match co with
       | None ->
         make_option ~a ~selected:sel (pcdata (string_of cv))
       | Some c -> make_option ~a ~selected:sel ~value:(string_of cv) c)
    in
    let make_optg = function
      | Option o ->
        make_opt o
      | Optgroup (a, label, og1, ogl) ->
        make_optgroup ~a ~label (make_opt og1) (List.map make_opt ogl)
    in
    let fl2, ol2 = make_optg fl, List.map make_optg ol in
    let fl3, ol3 =
      match required with
      | None -> fl2, ol2
      | Some label ->
        make_option ~selected:(not has_selected) ~value:"" label,
        fl2 :: ol2
    in
    make_select ?a ~multiple ~name fl3 ol3

  let select ?a ?required ~name y fl ol =
    let multiple = false
    and name = Eliom_parameter.string_of_param_name name
    and f = Eliom_parameter_base.string_of_atom y in
    gen_select ?a ?required ~multiple ~name fl ol f

  let raw_select ?a ?required ~(name : string)
      (fl : string select_opt) (ol : string select_opt list) =
    gen_select ?a ?required ~multiple:false ~name fl ol id

  let int_select ?a ?required ~name
      (fl : int select_opt) (ol : int select_opt list) =
    gen_select ?a ?required ~multiple:false
      ~name:(Eliom_parameter.string_of_param_name name)
      fl ol string_of_int

  let int32_select ?a ?required ~name
      (fl : int32 select_opt) (ol : int32 select_opt list) =
    gen_select ?a ?required ~multiple:false
      ~name:(Eliom_parameter.string_of_param_name name)
      fl ol Int32.to_string

  let int64_select ?a ?required ~name
      (fl : int64 select_opt) (ol : int64 select_opt list) =
    gen_select ?a ?required ~multiple:false
      ~name:(Eliom_parameter.string_of_param_name name)
      fl ol Int64.to_string

  let float_select ?a ?required ~name
      (fl : float select_opt) (ol : float select_opt list) =
    gen_select ?a ~multiple:false
      ~name:(Eliom_parameter.string_of_param_name name)
      fl ol Xml_print.string_of_number

  let string_select ?a ?required ~name
      (fl : string select_opt) (ol : string select_opt list) =
    gen_select ?a ?required ~multiple:false
      ~name:(Eliom_parameter.string_of_param_name name) fl ol id

  let user_type_select string_of ?a ?required ~name (fl : 'a select_opt)
      (ol : 'a select_opt list) =
    gen_select ?a ?required ~multiple:false
      ~name:(Eliom_parameter.string_of_param_name name)
      fl ol string_of

  let multiple_select ?a ?required ~name y fl ol =
    let multiple = true
    and name = Eliom_parameter.string_of_param_name name
    and f = Eliom_parameter_base.string_of_atom y in
    gen_select ?a ?required ~multiple ~name fl ol f

  let raw_multiple_select ?a ?required ~(name : string)
      (fl : string select_opt) (ol : string select_opt list) =
    gen_select ?a ?required ~multiple:true ~name fl ol id

  let int_multiple_select ?a ?required ~name
      (fl : int select_opt) (ol : int select_opt list) =
    gen_select ?a ?required ~multiple:true
      ~name:(Eliom_parameter.string_of_param_name name)
      fl ol string_of_int

  let int32_multiple_select ?a ?required ~name
      (fl : int32 select_opt) (ol : int32 select_opt list) =
    gen_select ?a ?required ~multiple:true
      ~name:(Eliom_parameter.string_of_param_name name)
      fl ol Int32.to_string

  let int64_multiple_select ?a ?required ~name
      (fl : int64 select_opt) (ol : int64 select_opt list) =
    gen_select ?a ?required ~multiple:true
      ~name:(Eliom_parameter.string_of_param_name name)
      fl ol Int64.to_string

  let float_multiple_select ?a ?required ~name
      (fl : float select_opt) (ol : float select_opt list) =
    gen_select ?a ?required ~multiple:true
      ~name:(Eliom_parameter.string_of_param_name name)
      fl ol Xml_print.string_of_number

  let string_multiple_select ?a ?required ~name
      (fl : string select_opt) (ol : string select_opt list) =
    gen_select ?a ?required ~multiple:true
      ~name:(Eliom_parameter.string_of_param_name name) fl ol id

  let user_type_multiple_select string_of ?a ?required
      ~name (fl : 'a select_opt)
      (ol : 'a select_opt list) =
    gen_select ?a ?required ~multiple:true
      ~name:(Eliom_parameter.string_of_param_name name)
      fl ol string_of

  let make_info ~https kind service =
    let f () =
      match Eliom_service.xhr_with_cookies service with
      | None ->
        None
      | Some tmpl ->
        Some (kind, Eliom_uri.make_cookies_info (https, service), tmpl)
    in
    Eliom_lazy.from_fun f

  let a_onclick_service info = Html5.attrib_of_service "onclick" info

  let a_onsubmit_service info = Html5.attrib_of_service "onsubmit" info

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
    get_form
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
    get_form_ Lwt.bind Lwt.return
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
    post_form
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
    post_form_ Lwt.bind Lwt.return
      ?absolute ?absolute_path ?https ~a ~service ?hostname ?port
      ?fragment ?keep_get_na_params ?keep_nl_params ?nl_params
      contents getparams

  let a_for = a_for

end
}}
