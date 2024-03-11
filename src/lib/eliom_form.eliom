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

open%shared Js_of_ocaml
[%%client.start]

let read_params form y =
  Eliom_parameter.reconstruct_params_form (Form.form_elements form) y

let error_handler =
  ref @@ fun _ -> Lwt.fail_with "Cannot parse params for client-side service"

let set_error_handler f = error_handler := f

let iter_contents y ev f =
  let fls () = Lwt.return_false in
  Js.Opt.case ev##.target fls @@ fun target ->
  Js.Opt.case (Dom_html.CoerceTo.form target) fls @@ fun target ->
  match read_params target y with
  | Some v ->
      let%lwt () = f v in
      Lwt.return_true
  | None -> !error_handler ()

type client_form_handler = Eliom_client.client_form_handler

let make_hdlr_get service : client_form_handler =
 fun ev ->
  match Eliom_service.client_fun service with
  | None -> Lwt.return_false
  | Some _ ->
      iter_contents (Eliom_service.get_params_type service) ev @@ fun g ->
      Eliom_client.change_page ~service g ()

let make_hdlr_post service g : client_form_handler =
 fun ev ->
  match Eliom_service.client_fun service with
  | None -> Lwt.return_false
  | Some _ ->
      iter_contents (Eliom_service.post_params_type service) ev @@ fun p ->
      Eliom_client.change_page ~service g p

[%%server type client_form_handler]
[%%shared.start]

module type Html = sig
  include
    Html_sigs.T
    with type 'a Xml.W.t = 'a
     and type 'a Xml.W.tlist = 'a list
     and type Xml.mouse_event_handler =
      (Dom_html.mouseEvent Js.t -> unit) Eliom_client_value.t

  type ('a, 'b, 'c) lazy_star =
    ?a:'a attrib list -> 'b elt list Eliom_lazy.request -> 'c elt

  val lazy_form :
    ( [< Html_types.form_attrib]
      , [< Html_types.form_content_fun]
      , [> Html_types.form] )
      lazy_star

  val uri_of_fun : (unit -> string) -> Xml.uri

  val attrib_of_service :
     string
    -> ([`A | `Form_get | `Form_post]
       * (bool * string list) option
       * string option
       * Eliom_lib.poly)
         option
         Eliom_lazy.request
    -> Html_types.form_attrib attrib

  val to_elt : 'a elt -> Eliom_content_core.Xml.elt
end

let get_xhr = function
  | Some xhr -> xhr
  | None -> Eliom_config.get_default_links_xhr ()

module Make_links (Html : Html) = struct
  type +'a attrib = 'a Html.attrib

  let make_uri ?absolute ?absolute_path ?https ~service ?hostname ?port
      ?fragment ?keep_nl_params ?nl_params gp
    =
    Html.uri_of_fun @@ fun () ->
    Eliom_uri.make_string_uri ?absolute ?absolute_path ?https ?fragment ~service
      ?hostname ?port ?keep_nl_params ?nl_params gp

  let uri_of_string = Html.uri_of_fun

  let a ?absolute ?absolute_path ?https ?(a = []) ~service ?hostname ?port
      ?fragment ?keep_nl_params ?nl_params ?xhr content getparams
    =
    let a =
      let a = (a :> Html_types.a_attrib attrib list) in
      let href =
        Html.uri_of_fun @@ fun () ->
        Eliom_uri.make_string_uri ?absolute ?absolute_path ?https ~service
          ?hostname ?port ?fragment ?keep_nl_params ?nl_params getparams
      in
      let href = Html.a_href href in
      if get_xhr xhr
      then
        let f =
          [%client.unsafe
            fun ev ->
              if not (Eliom_client.middleClick ev)
              then (
                Dom.preventDefault ev;
                Dom_html.stopPropagation ev;
                Lwt.async @@ fun () ->
                Eliom_client.change_page ?absolute:~%absolute
                  ?absolute_path:~%absolute_path ?https:~%https
                  ~service:~%service ?hostname:~%hostname ?port:~%port
                  ?fragment:~%fragment ?keep_nl_params:~%keep_nl_params
                  ?nl_params:~%nl_params ~%getparams ())]
        in
        Html.a_onclick f :: href :: a
      else href :: a
    in
    Html.a ~a content

  let css_link ?(a = []) ~uri () =
    let a =
      Html.a_mime_type "text/css" :: (a :> Html_types.link_attrib attrib list)
    in
    Html.link ~href:uri ~rel:[`Stylesheet] ~a ()

  let js_script ?(a = []) ~uri () =
    let a =
      Html.a_script_type `Javascript
      :: Html.a_src uri
      :: (a :> Html_types.script_attrib attrib list)
    in
    Html.script ~a (Html.txt "")
end

type _ param =
  | Atom : 'a Eliom_parameter_base.atom -> 'a param
  | User : ('a -> string) -> 'a param

module Make (Html : Html) = struct
  type 'a param' = 'a param
  type 'a param = 'a param'

  let string_of_param = function
    | Atom a -> Eliom_parameter_base.string_of_atom a
    | User f -> f

  let float = Atom Eliom_parameter_base.TFloat
  let int = Atom Eliom_parameter_base.TInt
  let int32 = Atom Eliom_parameter_base.TInt32
  let int64 = Atom Eliom_parameter_base.TInt64
  let nativeint = Atom Eliom_parameter_base.TNativeint
  let bool = Atom Eliom_parameter_base.TBool
  let string = Atom Eliom_parameter_base.TString
  let user f = User f

  open Html

  let make_post_form ?(a = []) ~action ?id ?(inline = false) elts =
    let a = match id with None -> a | Some id -> a_id id :: a in
    let a =
      Html.a_enctype "multipart/form-data"
      :: (* Always Multipart!!! How to test if there is a file?? *)
         a_action action
      :: a_method `Post
      :: (if inline then a_class ["inline"] :: a else a)
    in
    lazy_form ~a elts

  let cons_hidden_fieldset fields content =
    Html.fieldset ~a:[a_style "display: none;"] fields :: content

  let make_input ?(a = []) ?(checked = false) ~typ ?name ?src ?value () =
    let a = (a :> Html_types.input_attrib attrib list) in
    let a = match value with None -> a | Some value -> a_value value :: a in
    let a = match name with None -> a | Some name -> a_name name :: a in
    let a = match src with None -> a | Some src -> a_src src :: a in
    let a = if checked then a_checked () :: a else a in
    let a = a_input_type typ :: a in
    input ~a ()

  let make_button ?(a = []) ~button_type ?name ?value c =
    let a = (a :> Html_types.button_attrib attrib list) in
    let a =
      match value with None -> a | Some value -> a_text_value value :: a
    in
    let a = match name with None -> a | Some name -> a_name name :: a in
    button ~a:(a_button_type button_type :: a) c

  let make_textarea ?(a = []) ~name ?(value = "") () =
    let a = a_name name :: (a :> Html_types.textarea_attrib attrib list) in
    textarea ~a (txt value)

  let make_select ?(a = []) ~multiple ~name elt elts =
    let a = if multiple then a_multiple () :: a else a in
    let a = a_name name :: a in
    select ~a (elt :: elts)

  let make_option ?(a = []) ~selected ?value c =
    let a = match value with None -> a | Some v -> a_text_value v :: a in
    let a = if selected then a_selected () :: a else a in
    option ~a c

  let make_optgroup ?(a = []) ~label elt elts = optgroup ~label ~a (elt :: elts)

  (** Functions to construct web pages: *)

  let make_post_uri_components = Eliom_uri.make_post_uri_components

  let get_form_ bind return ?absolute ?absolute_path ?https ?a ~service
      ?hostname ?port ?fragment
      ?(nl_params = Eliom_parameter.empty_nl_params_set) ?keep_nl_params f
    =
    let issuffix, paramnames =
      Eliom_parameter.make_params_names (Eliom_service.get_params_type service)
    in
    let components =
      Eliom_lazy.from_fun @@ fun () ->
      Eliom_uri.make_uri_components_ ?absolute ?absolute_path ?https ~service
        ?hostname ?port ?fragment ~nl_params ?keep_nl_params ()
    in
    let uri =
      Html.uri_of_fun @@ fun () ->
      let uri, _, fragment = Eliom_lazy.force components in
      let uri =
        if issuffix
        then
          if uri.[String.length uri - 1] = '/'
          then uri ^ Eliom_common.eliom_nosuffix_page
          else String.concat "/" [uri; Eliom_common.eliom_nosuffix_page]
        else uri
      in
      match fragment with
      | None -> uri
      | Some f -> String.concat "#" [uri; Eliom_lib.Url.encode f]
    in
    bind (f paramnames) @@ fun inside ->
    let inside =
      Eliom_lazy.from_fun @@ fun () ->
      let _, hiddenparams, _ = Eliom_lazy.force components
      and f (n, v) =
        let name = n
        and value = Eliommod_parameters.to_string v
        and typ = `Hidden in
        make_input ~typ ~name ~value ()
      in
      cons_hidden_fieldset (List.map f hiddenparams)
        (inside :> Html_types.form_content elt list)
    and a =
      let a' = [a_method `Get; a_action uri] in
      match a with Some a -> a' @ a | _ -> a'
    in
    return (Html.lazy_form ~a inside)

  let get_form ?absolute ?absolute_path ?https ?a ~service ?hostname ?port
      ?fragment ?keep_nl_params ?nl_params ?xhr:_ f
    =
    get_form_
      (fun x f -> f x)
      (fun x -> x)
      ?absolute ?absolute_path ?https ?a ~service ?keep_nl_params ?nl_params
      ?hostname ?port ?fragment f

  let post_form_ bind return ?absolute ?absolute_path ?https ?a ~service
      ?hostname ?port ?fragment
      ?(nl_params = Eliom_parameter.empty_nl_params_set)
      ?(keep_nl_params : [`All | `Persistent | `None] option)
      ?keep_get_na_params f get_params
    =
    let _, paramnames =
      Eliom_parameter.make_params_names (Eliom_service.post_params_type service)
    in
    let components =
      Eliom_lazy.from_fun @@ fun () ->
      Eliom_uri.make_post_uri_components_ ?absolute ?absolute_path ?https
        ~service ?hostname ?port ?fragment ?keep_nl_params ~nl_params
        ?keep_get_na_params get_params ()
    in
    bind (f paramnames) @@ fun inside ->
    let inside =
      Eliom_lazy.from_fun @@ fun () ->
      let _, _, _, hiddenparams = Eliom_lazy.force components
      and f (name, value) =
        let value = Eliommod_parameters.to_string value in
        make_input ~typ:`Hidden ~name ~value ()
      in
      cons_hidden_fieldset (List.map f hiddenparams)
        (inside :> Html_types.form_content elt list)
    and action =
      Html.uri_of_fun @@ fun () ->
      let uri, g, r, _ = Eliom_lazy.force components in
      Eliom_uri.make_string_uri_from_components (uri, g, r)
    in
    return (make_post_form ?a ~action inside)

  let post_form ?absolute ?absolute_path ?https ?a ~service ?hostname ?port
      ?fragment ?keep_nl_params ?keep_get_na_params ?nl_params ?xhr:_ f
      getparams
    =
    post_form_
      (fun x f -> f x)
      (fun x -> x)
      ?absolute ?absolute_path ?https ?a ~service ?hostname ?port ?fragment
      ?keep_get_na_params ?keep_nl_params ?nl_params f getparams

  let option_map f = function Some x -> Some (f x) | None -> None

  let gen_input ?a ~input_type ?value ?src ?name string_of =
    let name = option_map Eliom_parameter.string_of_param_name name
    and value = option_map string_of value in
    make_input ?a ?value ~typ:input_type ?name ?src ()

  let input ?a ~input_type ?name ?value y =
    let f = string_of_param y in
    gen_input ?a ~input_type ?value ?name f

  let file_input ?a ~name () =
    make_input ?a ~typ:`File
      ~name:(Eliom_parameter.string_of_param_name name)
      ()
  (* value attribute not supported by browsers for security reasons *)

  let image_input ?a ~name ?src () =
    make_input ?a ~typ:`Image
      ~name:(Eliom_parameter.string_of_param_name name)
      ?src ()

  let checkbox ?a ?checked ~name ~value y =
    let name = Eliom_parameter.string_of_param_name name
    and value = string_of_param y value
    and typ = `Checkbox in
    make_input ?a ?checked ~typ ~name ~value ()

  let bool_checkbox_one ?a ?checked ~name () =
    let typ = `Checkbox and name = Eliom_parameter.string_of_param_name name in
    make_input ?a ?checked ~typ ~name ()

  let radio ?a ?checked ~name ~value y =
    let name = Eliom_parameter.string_of_param_name name
    and value = string_of_param y value
    and typ = `Radio in
    make_input ?a ?checked ~typ ~name ~value ()

  let string_radio_required ?a ?checked ~name ~value () =
    let a =
      let required = Html.a_required () in
      match a with
      | None -> [required]
      | Some a -> required :: (a :> Html_types.input_attrib attrib list)
    in
    make_input ~a ?checked ~typ:`Radio
      ~name:(Eliom_parameter.string_of_param_name name)
      ~value ()

  let button ?a ~button_type ~name ~value y c =
    let name = Eliom_parameter.string_of_param_name name
    and value = string_of_param y value in
    make_button ?a ~button_type ~name ~value c

  let button_no_value ?a ~button_type c = make_button ?a ~button_type c

  let textarea ?a ~name =
    make_textarea ?a ~name:(Eliom_parameter.string_of_param_name name)

  type 'a soption =
    Html_types.option_attrib attrib list
    * 'a
    (* Content (or value if the following is present) *)
    * Html_types.pcdata elt option
    (* if content different from value *)
    * bool
  (* selected *)

  type 'a select_opt =
    | Optgroup of
        [Html_types.common | `Disabled] attrib list
        * string (* label *)
        * 'a soption
        * 'a soption list
    | Option of 'a soption

  let gen_select ?a ?(multiple = false) ?required ~name (fl : 'a select_opt)
      (ol : 'a select_opt list) string_of
    =
    let a = (a :> Html_types.select_attrib attrib list option) in
    let a =
      match required with
      | None -> a
      | Some _ -> (
          let required = Html.a_required () in
          match a with Some a -> Some (required :: a) | None -> Some [required])
    in
    let normalize_selected l =
      (* We change the list of option to have exactly one selected
         item.  We do this because the behaviour of browsers differs.
         We select the first one if nothing is selected.  We select
         the first selected if several are selected.  Thus all
         browsers will behave the same way.  *)
      let aux1 found ((a, b, c, selected) as line) =
        if found then (a, b, c, false), true else line, selected
      in
      let rec aux2 found = function
        | line :: l ->
            let line, found = aux1 found line in
            let l, found = aux2 found l in
            line :: l, found
        | [] -> [], found
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
        | [] -> [], found
      in
      let select_first = function
        | Option (a, b, c, _) -> Option (a, b, c, true)
        | Optgroup (a, b, (c, d, e, _), ol) ->
            Optgroup (a, b, (c, d, e, true), ol)
      in
      let newl, found = aux false l in
      if found
      then List.hd newl, List.tl newl, true
      else
        let first = List.hd newl in
        (* We select the first one by default *)
        let first =
          match required with None -> select_first first | _ -> first
        in
        first, List.tl newl, false
    in
    let fl, ol, has_selected =
      if multiple
      then
        ( fl
        , ol
        , let _, _, hs = normalize_selected (fl :: ol) in
          hs )
      else normalize_selected (fl :: ol)
    in
    let make_opt (a, cv, co, sel) =
      match co with
      | None -> make_option ~a ~selected:sel (txt (string_of cv))
      | Some c -> make_option ~a ~selected:sel ~value:(string_of cv) c
    in
    let make_optg = function
      | Option o -> make_opt o
      | Optgroup (a, label, og1, ogl) ->
          make_optgroup ~a ~label (make_opt og1) (List.map make_opt ogl)
    in
    let fl2, ol2 = make_optg fl, List.map make_optg ol in
    let fl3, ol3 =
      match required with
      | None -> fl2, ol2
      | Some label ->
          make_option ~selected:(not has_selected) ~value:"" label, fl2 :: ol2
    in
    make_select ?a ~multiple ~name fl3 ol3

  let select ?a ?required ~name y fl ol =
    let multiple = false
    and name = Eliom_parameter.string_of_param_name name
    and f = string_of_param y in
    gen_select ?a ?required ~multiple ~name fl ol f

  let multiple_select ?a ?required ~name y fl ol =
    let multiple = true
    and name = Eliom_parameter.string_of_param_name name
    and f = string_of_param y in
    gen_select ?a ?required ~multiple ~name fl ol f

  let make_info ~https kind service hdlr =
    let f () =
      match Eliom_service.xhr_with_cookies service with
      | None -> None
      | Some tmpl ->
          Some
            ( (kind : [`Form_get | `Form_post] :> [`Form_get | `Form_post | `A])
            , Eliom_uri.make_cookies_info (https, service)
            , tmpl
            , Eliom_lib.to_poly hdlr )
    in
    Eliom_lazy.from_fun f

  let a_onsubmit_service info = Html.attrib_of_service "onsubmit" info

  let get_form ?absolute ?absolute_path ?https ?(a = []) ~service ?hostname
      ?port ?fragment ?keep_nl_params ?nl_params ?xhr contents
    =
    let a =
      let a = (a :> Html_types.form_attrib attrib list) in
      if get_xhr xhr
      then
        let hdlr =
          [%client.unsafe (make_hdlr_get ~%service : client_form_handler)]
        in
        let info = make_info ~https `Form_get service hdlr in
        a_onsubmit_service info :: a
      else a
    in
    get_form ?absolute ?absolute_path ?https ~a ~service ?hostname ?port
      ?fragment ?keep_nl_params ?nl_params contents

  let lwt_get_form ?absolute ?absolute_path ?https ?(a = []) ~service ?hostname
      ?port ?fragment ?keep_nl_params ?nl_params ?xhr contents
    =
    let a =
      let a = (a :> Html_types.form_attrib attrib list) in
      if get_xhr xhr
      then
        let hdlr =
          [%client.unsafe (make_hdlr_get ~%service : client_form_handler)]
        in
        let info = make_info ~https `Form_get service hdlr in
        a_onsubmit_service info :: a
      else a
    in
    get_form_ Lwt.bind Lwt.return ?absolute ?absolute_path ?https ~a ~service
      ?hostname ?port ?fragment ?nl_params ?keep_nl_params contents

  let post_form ?absolute ?absolute_path ?https ?(a = []) ~service ?hostname
      ?port ?fragment ?keep_nl_params ?keep_get_na_params ?nl_params ?xhr
      contents getparams
    =
    let a =
      let a = (a :> Html_types.form_attrib attrib list) in
      if get_xhr xhr
      then
        let hdlr =
          [%client.unsafe
            (make_hdlr_post ~%service ~%getparams : client_form_handler)]
        in
        let info = make_info ~https `Form_post service hdlr in
        a_onsubmit_service info :: a
      else a
    in
    post_form ?absolute ?absolute_path ?https ~a ~service ?hostname ?port
      ?fragment ?keep_nl_params ?keep_get_na_params ?nl_params contents
      getparams

  let lwt_post_form ?absolute ?absolute_path ?https ?(a = []) ~service ?hostname
      ?port ?fragment ?keep_nl_params ?keep_get_na_params ?nl_params ?xhr
      contents getparams
    =
    let a =
      let a = (a :> Html_types.form_attrib attrib list) in
      if get_xhr xhr
      then
        let hdlr =
          [%client.unsafe
            (make_hdlr_post ~%service ~%getparams : client_form_handler)]
        in
        let info = make_info ~https `Form_post service hdlr in
        a_onsubmit_service info :: a
      else a
    in
    post_form_ Lwt.bind Lwt.return ?absolute ?absolute_path ?https ~a ~service
      ?hostname ?port ?fragment ?keep_get_na_params ?keep_nl_params ?nl_params
      contents getparams
end
