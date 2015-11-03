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

open Eliom_lib
open Eliom_parameter
open Eliom_service
open Eliom_uri

module MakeForms(Pages : Eliom_form_sigs.PARAM) = struct

  type +'a elt = 'a Pages.elt

  type +'a attrib = 'a Pages.attrib

  type uri = Pages.uri

  (** Functions to construct web pages: *)

  let make_proto_prefix = make_proto_prefix

  let make_string_uri = make_string_uri

  let make_uri_components = make_uri_components

  let make_post_uri_components = make_post_uri_components

  let uri_of_string = Pages.uri_of_string

  let make_uri
      ?absolute
      ?absolute_path
      ?https ~service ?hostname ?port ?fragment
      ?keep_nl_params ?nl_params gp =
    Pages.uri_of_string
      (fun () ->
	make_string_uri
          ?absolute ?absolute_path
          ?https ?fragment ~service
          ?hostname ?port ?keep_nl_params ?nl_params gp)


  let a ?absolute ?absolute_path ?https ?a ~service ?hostname ?port ?fragment
      ?keep_nl_params ?nl_params ?xhr content getparams =
    let href =
      Pages.uri_of_string
	(fun () ->
	  make_string_uri
	    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
	    ?keep_nl_params ?nl_params getparams)
    in
    Pages.make_a ?a ~href content

  let get_form_
        bind return
	?absolute ?absolute_path ?https ?a ~service ?hostname ?port ?fragment
	?(nl_params = Eliom_parameter.empty_nl_params_set) ?keep_nl_params
	f =

    let getparamstype = get_get_params_type_ service in
    let issuffix, paramnames = make_params_names getparamstype in

    let components =
      Eliom_lazy.from_fun
	(fun () ->
	  make_uri_components_
            ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
            ~nl_params ?keep_nl_params
            () )
    in

    let uri =
      Pages.uri_of_string
	(fun () ->
	  let (uri, hiddenparams, fragment) = Eliom_lazy.force components in
	  let uri =
	    if issuffix then
	      if uri.[String.length uri - 1] = '/'
	      then uri^Eliom_common.eliom_nosuffix_page
	      else String.concat "/" [uri; Eliom_common.eliom_nosuffix_page]
	    else uri
	  in
	  match fragment with
	  | None -> uri
	  | Some f -> String.concat "#" [uri; Url.encode f])
    in

    bind (f paramnames)
      (fun inside ->
	let inside =
	  Eliom_lazy.from_fun
	    (fun () ->
	      let (uri, hiddenparams, fragment) = Eliom_lazy.force components in
	      Pages.cons_hidden_fieldset
		(List.map
		   (fun (n, v) ->
		     Pages.make_input
		       ~typ:`Hidden
		       ~name:n ~value:(Eliommod_parameters.to_string v) ())
		   hiddenparams)
		inside)
	in
	return (Pages.make_get_form ?a ~action:uri inside))

  let get_form
      ?absolute ?absolute_path ?https ?a ~service ?hostname ?port ?fragment
      ?keep_nl_params ?nl_params ?xhr f =
    get_form_
      (fun x f -> f x) (fun x -> x)
      ?absolute ?absolute_path
      ?https ?a ~service ?keep_nl_params
      ?nl_params ?hostname ?port ?fragment f

  let lwt_get_form
      ?absolute ?absolute_path ?https ?a ~service ?hostname ?port ?fragment
      ?keep_nl_params ?nl_params ?xhr f =
    get_form_
      Lwt.bind Lwt.return
      ?absolute ?absolute_path ?https ?a ~service ?hostname ?port ?fragment
      ?nl_params ?keep_nl_params f

  let post_form_
      bind return
      ?absolute ?absolute_path ?https ?a ~service ?hostname ?port ?fragment
      ?(nl_params = Eliom_parameter.empty_nl_params_set)
      ?(keep_nl_params : [ `All | `Persistent | `None ] option)
      ?keep_get_na_params
      f getparams =

    let getparamstype = get_post_params_type_ service in
    let _, paramnames = make_params_names getparamstype in

    let components =
      Eliom_lazy.from_fun
	(fun () ->
	  make_post_uri_components_
            ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
            ?keep_nl_params ~nl_params ?keep_get_na_params
            getparams
            ())
    in

    bind (f paramnames)
      (fun inside ->
         let inside =
	   Eliom_lazy.from_fun
	     (fun () ->
	       let (uri, getparams, fragment, hiddenparams) =
		 Eliom_lazy.force components in
	      Pages.cons_hidden_fieldset
		(List.map
		   (fun (n,v) ->
		     (Pages.make_input
			~typ:`Hidden
			~name:n ~value:(Eliommod_parameters.to_string v) ()))
		   hiddenparams)
	      inside) in
         let uri =
	   Pages.uri_of_string
	     (fun () ->
	       let (uri, getparams, fragment, hiddenparams) =
		 Eliom_lazy.force components in
               make_string_uri_from_components (uri, getparams, fragment))
         in
         return (Pages.make_post_form ?a ~action:uri inside))

  let post_form
      ?absolute ?absolute_path ?https ?a ~service ?hostname ?port ?fragment
      ?keep_nl_params ?keep_get_na_params ?nl_params ?xhr f getparams =
    post_form_
      (fun x f -> f x) (fun x -> x)
      ?absolute ?absolute_path ?https ?a ~service ?hostname ?port
      ?fragment ?keep_get_na_params
      ?keep_nl_params ?nl_params
      f getparams

  let lwt_post_form
      ?absolute ?absolute_path ?https ?a ~service ?hostname ?port ?fragment
      ?keep_nl_params ?keep_get_na_params
      ?nl_params
      ?xhr f getparams =
    post_form_ Lwt.bind Lwt.return
      ?absolute ?absolute_path
      ?https ?a ~service ?hostname ?port
      ?fragment ?keep_get_na_params ?keep_nl_params ?nl_params
      f getparams

  let js_script = Pages.make_js_script
  let css_link = Pages.make_css_link

  let gen_input ?a ~input_type
      ?value ?src ?name string_of =
    let name = match name with
      | None -> None
      | Some n -> Some (string_of_param_name n)
    in
    (match value with
     | None ->
         Pages.make_input ?a ~typ:input_type ?name ?src ()
     | Some v ->
         Pages.make_input
           ?a
           ~value:(string_of v)
           ~typ:input_type
           ?src
           ?name
           ())

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
    (match value with
     | None ->
         Pages.make_input ?a ~typ:input_type ?name ()
     | Some v ->
         Pages.make_input
           ?a
           ~value:v
           ~typ:input_type
           ?name
           ())

  let file_input ?a ~name () =
    Pages.make_input ?a ~typ:`File ~name:(string_of_param_name name) ()
      (* value attribute not supported by browsers for security reasons *)

  let image_input ?a ~name ~value ?src y =
    let f = Eliom_parameter_base.string_of_atom y in
    gen_input ?a ~input_type:`Image ~name ~value ?src f

  let int_image_input ?a ~name ~value ?src () =
    gen_input ?a ~input_type:`Image ~name
      ~value ?src string_of_int

  let int32_image_input ?a ~name ~value ?src () =
    gen_input ?a ~input_type:`Image ~name
      ~value ?src Int32.to_string

  let int64_image_input ?a ~name ~value ?src () =
    gen_input ?a ~input_type:`Image ~name
      ~value ?src Int64.to_string

  let float_image_input ?a ~name ~value ?src () =
    gen_input ?a ~input_type:`Image ~name
      ~value ?src Xml_print.string_of_number

  let string_image_input ?a ~name ~value ?src () =
    gen_input ?a ~input_type:`Image ~name
      ~value ?src id

  let user_type_image_input string_of ?a ~name ~value ?src () =
    gen_input ?a ~input_type:`Image ~name
      ~value ?src string_of

  let raw_image_input ?a ~(name : string) ~value ?src () =
    Pages.make_input
      ?a
      ~value
      ~typ:`Image
      ?src
      ~name
      ()

  let checkbox ?a ?checked ~name ~value y =
    let name = string_of_param_name name
    and value = Eliom_parameter_base.string_of_atom y value
    and typ = `Checkbox in
    Pages.make_input ?a ?checked ~typ ~name ~value ()

  let bool_checkbox ?a ?checked ~name () =
    Pages.make_input ?a ?checked ~typ:`Checkbox
      ~name:(string_of_param_name name) ()

  let int_checkbox ?a ?checked ~name ~value () =
    Pages.make_input ?a ?checked ~typ:`Checkbox
      ~name:(string_of_param_name name) ~value:(string_of_int value) ()

  let int32_checkbox ?a ?checked ~name ~value () =
    Pages.make_input ?a ?checked ~typ:`Checkbox
      ~name:(string_of_param_name name) ~value:(Int32.to_string value) ()

  let int64_checkbox ?a ?checked ~name ~value () =
    Pages.make_input ?a ?checked ~typ:`Checkbox
      ~name:(string_of_param_name name) ~value:(Int64.to_string value) ()

  let float_checkbox ?a ?checked ~name ~value () =
    Pages.make_input ?a ?checked ~typ:`Checkbox
      ~name:(string_of_param_name name) ~value:(Xml_print.string_of_number value) ()

  let string_checkbox ?a ?checked ~name ~value () =
    Pages.make_input ?a ?checked ~typ:`Checkbox
      ~name:(string_of_param_name name) ~value ()

  let user_type_checkbox string_of ?a ?checked ~name ~value () =
    Pages.make_input ?a ?checked ~typ:`Checkbox
      ~name:(string_of_param_name name) ~value:(string_of value) ()

  let raw_checkbox ?a ?checked ~name ~value () =
    Pages.make_input ?a ?checked ~typ:`Checkbox
      ~name:name ~value ()

  let radio ?a ?checked ~name ~value y =
    let name = string_of_param_name name
    and value = Eliom_parameter_base.string_of_atom y value
    and typ = `Radio in
    Pages.make_input ?a ?checked ~typ ~name ~value ()

  let string_radio ?a ?checked ~name ~value () =
    Pages.make_input
      ?a ?checked ~typ:`Radio
      ~name:(string_of_param_name name) ~value ()

  let string_radio_required ?a ?checked ~name ~value () =
    let a =
      let required = Pages.a_input_required () in
      match a with
        | None -> [required]
        | Some a -> required :: a
    in
    Pages.make_input
      ~a ?checked ~typ:`Radio
      ~name:(string_of_param_name name) ~value ()

  let int_radio ?a ?checked ~name ~value () =
    Pages.make_input
      ?a ?checked ~typ:`Radio
      ~name:(string_of_param_name name) ~value:(string_of_int value) ()

  let int32_radio ?a ?checked ~name ~value () =
    Pages.make_input
      ?a ?checked ~typ:`Radio
      ~name:(string_of_param_name name) ~value:(Int32.to_string value) ()

  let int64_radio ?a ?checked ~name ~value () =
    Pages.make_input
      ?a ?checked ~typ:`Radio
      ~name:(string_of_param_name name) ~value:(Int64.to_string value) ()

  let float_radio ?a ?checked ~name ~value () =
    Pages.make_input
      ?a ?checked ~typ:`Radio
      ~name:(string_of_param_name name) ~value:(Xml_print.string_of_number value) ()

  let user_type_radio string_of ?a ?checked ~name ~value () =
    Pages.make_input
      ?a ?checked ~typ:`Radio
      ~name:(string_of_param_name name) ~value:(string_of value) ()

  let raw_radio ?a ?checked ~(name : string) ~value () =
    Pages.make_input
      ?a ?checked ~typ:`Radio
      ~name:name ~value:value ()

  let button ?a ~name ~value y c =
    let name = string_of_param_name name
    and value = Eliom_parameter_base.string_of_atom y value
    and button_type = `Submit in
    Pages.make_button ?a ~button_type ~name ~value c

  let string_button ?a ~name ~value c =
    Pages.make_button ?a ~button_type:`Submit
      ~name:(string_of_param_name name) ~value c

  let int_button ?a ~name ~value c =
    Pages.make_button ?a ~button_type:`Submit
      ~name:(string_of_param_name name) ~value:(string_of_int value) c

  let int32_button ?a ~name ~value c =
    Pages.make_button ?a ~button_type:`Submit
      ~name:(string_of_param_name name) ~value:(Int32.to_string value) c

  let int64_button ?a ~name ~value c =
    Pages.make_button ?a ~button_type:`Submit
      ~name:(string_of_param_name name) ~value:(Int64.to_string value) c

  let float_button ?a ~name ~value c =
    Pages.make_button ?a ~button_type:`Submit
      ~name:(string_of_param_name name) ~value:(Xml_print.string_of_number value) c

  let user_type_button string_of ?a ~name ~value c =
    Pages.make_button ?a ~button_type:`Submit
      ~name:(string_of_param_name name) ~value:(string_of value) c

  let raw_button ?a ~button_type ~name ~value c =
    Pages.make_button ?a ~button_type ~name ~value c

  let button_no_value ?a ~button_type c =
    Pages.make_button ?a ~button_type c

  let textarea ?a ~name =
    Pages.make_textarea ?a ~name:(string_of_param_name name)

  let raw_textarea ?a ~name =
    Pages.make_textarea ?a ~name



  type 'a soption =
      Html5_types.option_attrib Pages.attrib list
      * 'a (* Content (or value if the following is present) *)
      * Html5_types.pcdata Pages.elt option (* if content different from value *)
      * bool (* selected *)

  type 'a select_opt =
    | Optgroup of
        [ Html5_types.common | `Disabled ] Pages.attrib list
        * string (* label *)
        * 'a soption
        * 'a soption list
    | Option of 'a soption

  let gen_select ?a ?(multiple=false) ?required ~name
      (fl : 'a select_opt) (ol : 'a select_opt list) string_of =

    let a = match required with
      | None -> a
      | Some _ ->
        let required = Pages.a_select_required () in
        match a with
        | Some a -> Some (required :: a)
        | None -> Some [required]
    in

    let normalize_selected l =
      (* We change the list of option to have exactly one selected item.
         We do this because the behaviour of browsers differs.
         We select the first one if nothing is selected.
         We select the first selected if several are selected.
         Thus all browsers will behave the same way.
      *)
      let aux1 trouve ((a, b, c, selected) as line) =
        if trouve
        then ((a, b, c, false), true)
        else if selected
        then (line, true)
        else (line, false)
      in
      let rec aux2 trouve = function
        | line::l ->
            let (line, trouve) = aux1 trouve line in
            let (l, trouve) = aux2 trouve l in
            (line::l, trouve)
        | [] -> ([], trouve)
      in
      let rec aux trouve = function
        | (Option line)::l ->
            let (line, trouve) = aux1 trouve line in
            let (l, trouve) = aux trouve l in
            ((Option line)::l, trouve)
        | (Optgroup (a, b, fl, ol))::l ->
            let (fl, trouve) = aux1 trouve fl in
            let (ol, trouve) = aux2 trouve ol in
            let (l, trouve) = aux trouve l in
            ((Optgroup (a, b, fl, ol))::l, trouve)
        | [] -> ([], trouve)
      in
      let select_first = function
        | Option (a, b, c, _) -> Option (a, b, c, true)
        | Optgroup (a, b, (c, d, e, _), ol) ->
            Optgroup (a, b, (c, d, e, true), ol)
      in
      let (newl, trouve) = aux false l in
      if trouve
      then ((List.hd newl), (List.tl newl), true)
      else
        let first = List.hd newl in
        (* We select the first one by default *)
        let first =
          if required = None then
            select_first first
          else
            first
        in
        (first, (List.tl newl), false)
    in


    let (fl, ol, has_selected) =
      if multiple
      then (fl, ol, let _, _, hs = normalize_selected (fl :: ol) in hs)
      else normalize_selected (fl::ol)
    in
    let make_opt (a, cv, co, sel) =
      (match co with
       | None -> Pages.make_option ~a ~selected:sel
           (Pages.make_pcdata (string_of cv))
       | Some c -> Pages.make_option ~a ~selected:sel
           ~value:(string_of cv) c)
    in
    let make_optg = function
      | Option o ->
        Pages.select_content_of_option (make_opt o)
      | Optgroup (a, label, og1, ogl) ->
        Pages.make_optgroup
          ~a ~label (make_opt og1) (List.map make_opt ogl)
    in
    let fl2, ol2 = make_optg fl, List.map make_optg ol in
    let fl3, ol3 =
      match required with
      | None -> fl2, ol2
      | Some label ->
        let placeholder =
          Pages.make_option ~selected:(not has_selected) ~value:"" label
        in
        Pages.select_content_of_option placeholder,
        fl2 :: ol2
    in
    Pages.make_select ?a ~multiple ~name fl3 ol3

  let select ?a ?required ~name y fl ol =
    let multiple = false
    and name = string_of_param_name name
    and f = Eliom_parameter_base.string_of_atom y in
    gen_select ?a ?required ~multiple ~name fl ol f

  let raw_select ?a ?required ~(name : string)
      (fl : string select_opt) (ol : string select_opt list) =
    gen_select ?a ?required ~multiple:false ~name fl ol id

  let int_select ?a ?required ~name
      (fl : int select_opt) (ol : int select_opt list) =
    gen_select ?a ?required ~multiple:false
      ~name:(string_of_param_name name) fl ol string_of_int

  let int32_select ?a ?required ~name
      (fl : int32 select_opt) (ol : int32 select_opt list) =
    gen_select ?a ?required ~multiple:false
      ~name:(string_of_param_name name) fl ol Int32.to_string

  let int64_select ?a ?required ~name
      (fl : int64 select_opt) (ol : int64 select_opt list) =
    gen_select ?a ?required ~multiple:false
      ~name:(string_of_param_name name) fl ol Int64.to_string

  let float_select ?a ?required ~name
      (fl : float select_opt) (ol : float select_opt list) =
    gen_select ?a ~multiple:false
      ~name:(string_of_param_name name) fl ol Xml_print.string_of_number

  let string_select ?a ?required ~name
      (fl : string select_opt) (ol : string select_opt list) =
    gen_select ?a ?required ~multiple:false
      ~name:(string_of_param_name name) fl ol id

  let user_type_select string_of ?a ?required ~name (fl : 'a select_opt)
      (ol : 'a select_opt list) =
    gen_select ?a ?required ~multiple:false
      ~name:(string_of_param_name name) fl ol string_of

  let multiple_select ?a ?required ~name y fl ol =
    let multiple = true
    and name = string_of_param_name name
    and f = Eliom_parameter_base.string_of_atom y in
    gen_select ?a ?required ~multiple ~name fl ol f

  let raw_multiple_select ?a ?required ~(name : string)
      (fl : string select_opt) (ol : string select_opt list) =
    gen_select ?a ?required ~multiple:true ~name fl ol id

  let int_multiple_select ?a ?required ~name
      (fl : int select_opt) (ol : int select_opt list) =
    gen_select ?a ?required ~multiple:true
      ~name:(string_of_param_name name) fl ol string_of_int

  let int32_multiple_select ?a ?required ~name
      (fl : int32 select_opt) (ol : int32 select_opt list) =
    gen_select ?a ?required ~multiple:true
      ~name:(string_of_param_name name) fl ol Int32.to_string

  let int64_multiple_select ?a ?required ~name
      (fl : int64 select_opt) (ol : int64 select_opt list) =
    gen_select ?a ?required ~multiple:true
      ~name:(string_of_param_name name) fl ol Int64.to_string

  let float_multiple_select ?a ?required ~name
      (fl : float select_opt) (ol : float select_opt list) =
    gen_select ?a ?required ~multiple:true
      ~name:(string_of_param_name name) fl ol Xml_print.string_of_number

  let string_multiple_select ?a ?required ~name
      (fl : string select_opt) (ol : string select_opt list) =
    gen_select ?a ?required ~multiple:true
      ~name:(string_of_param_name name) fl ol id

  let user_type_multiple_select string_of ?a ?required
      ~name (fl : 'a select_opt)
      (ol : 'a select_opt list) =
    gen_select ?a ?required ~multiple:true
      ~name:(string_of_param_name name) fl ol string_of

  let a_for = Pages.make_for_attrib

end

}}
