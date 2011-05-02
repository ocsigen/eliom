(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_output
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

open Eliom_services
open Eliom_parameters

let code_of_code_option = function
  | None -> 200
  | Some c -> c

(******************************************************************************)
(******************************************************************************)

include Eliom_output_base

module type HTML5_REGISTRATION = "sigs/eliom_reg.mli"
  subst type page    := HTML5_types.html HTML5.M.elt
    and type options := unit
    and type return  := Eliom_services.http

module Html5_make_reg_base
  (Html5_content : Ocsigen_http_frame.HTTP_CONTENT
                   with type t = HTML5_types.html HTML5.M.elt) = struct

  open HTML5.M
  open HTML5_types

  type page = xhtml elt

  type options = unit

  type return = Eliom_services.http

  let send_appl_content = Eliom_services.XNever

  module Html5_content = struct

    include Html5_content

    let add_css (a : 'a) : 'a =
      let css =
        HTML5.M.toelt
          (HTML5.M.style
             [HTML5.M.pcdata "\n.";
             HTML5.M.pcdata Eliom_common.inline_class_name;
             HTML5.M.pcdata " {display: inline}\n.";
             HTML5.M.pcdata Eliom_common.nodisplay_class_name;
             HTML5.M.pcdata " {display: none}\n"])
      in
      let rec aux = function
        | { XML.elt = XML.Node ("head",al,el) } as e::l ->
            { e with XML.elt = XML.Node ("head",al,css::el) }::l
        | e::l -> e::(aux l)
        | [] -> []
      in
      HTML5.M.tot
        (match HTML5.M.toelt a with
           | { XML.elt = XML.Node ("html",al,el) } as e ->
               { e with XML.elt = XML.Node ("html",al,aux el) }
           | e -> e)

    let get_etag ?options c = get_etag (add_css c)

    let result_of_content ?options c = result_of_content ?options (add_css c)

  end

  let send ?(options = ()) ?charset ?code
      ?content_type ?headers content =
    Html5_content.result_of_content content >>= fun r ->
    let open Ocsigen_http_frame in
    Lwt.return
      {r with
         res_cookies= Eliom_request_info.get_user_cookies ();
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None -> Some (Eliom_config.get_config_default_charset ())
                         | _ -> charset
                      );
         res_content_type= (match content_type with
                              | None -> r.res_content_type
                              | _ -> content_type
                           );
         res_headers= (match headers with
                         | None -> r.res_headers
                         | Some headers ->
                             Http_headers.with_defaults headers r.res_headers
                      );
      }

end

module Html5_reg_base =
  Html5_make_reg_base(Ocsigen_senders.Make_XML_Content(XML)(HTML5.M))

module Html5_registration = Eliom_mkreg.MakeRegister(Html5_reg_base)

module Html5 = struct
  include Html5_forms
  include Html5_registration
end

(******************************************************************************)
(******************************************************************************)

type basic_input_type =
    [ `Hidden
    | `Password
    | `Submit
    | `Text
    ]

type full_input_type =
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

type button_type =
    [ `Button
    | `Reset
    | `Submit
    ]

module type XHTML_FORMS = "sigs/eliom_forms.mli"
  subst type uri := XHTML_types.uri
    and type pcdata_elt := XHTML_types.pcdata XHTML.M.elt

    and type form_elt := [> XHTML_types.form ] XHTML.M.elt
    and type form_content_elt := XHTML_types.form_content XHTML.M.elt
    and type form_content_elt_list := XHTML_types.form_content XHTML.M.elt list
    and type form_attrib_t := XHTML_types.form_attrib XHTML.M.attrib list

    and type 'a a_elt := [> XHTML_types.a ] XHTML.M.elt
    and type 'a a_content_elt := XHTML_types.a_content XHTML.M.elt
    and type 'a a_content_elt_list := XHTML_types.a_content XHTML.M.elt list
    and type a_attrib_t := XHTML_types.a_attrib XHTML.M.attrib list

    and type link_elt := [> XHTML_types.link ] XHTML.M.elt
    and type link_attrib_t := XHTML_types.link_attrib XHTML.M.attrib list

    and type script_elt := [> XHTML_types.script ] XHTML.M.elt
    and type script_attrib_t := XHTML_types.script_attrib XHTML.M.attrib list

    and type textarea_elt := [> XHTML_types.textarea ] XHTML.M.elt
    and type textarea_attrib_t := XHTML_types.textarea_attrib XHTML.M.attrib list

    and type input_elt := [> XHTML_types.input ] XHTML.M.elt
    and type input_attrib_t := XHTML_types.input_attrib XHTML.M.attrib list

    and type select_elt := [> XHTML_types.select ] XHTML.M.elt
    and type select_attrib_t := XHTML_types.select_attrib XHTML.M.attrib list

    and type button_elt := [> XHTML_types.button ] XHTML.M.elt
    and type button_content_elt := XHTML_types.button_content XHTML.M.elt
    and type button_content_elt_list := XHTML_types.button_content XHTML.M.elt list
    and type button_attrib_t := XHTML_types.button_attrib XHTML.M.attrib list

    and type optgroup_attrib_t := [ XHTML_types.common | `Disabled ] XHTML.M.attrib list
    and type option_attrib_t := XHTML_types.option_attrib XHTML.M.attrib list

    and type input_type_t := [< basic_input_type ]
    and type raw_input_type_t := [< basic_input_type | `Button | `Reset ]
    and type button_type_t := [< button_type ]

module type XHTML_FORMS_CLOSED = "sigs/eliom_forms.mli"
  subst type uri := XHTML_types.uri
    and type pcdata_elt := XHTML_types.pcdata XHTML.M.elt

    and type form_elt := XHTML_types.form XHTML.M.elt
    and type form_content_elt := XHTML_types.form_content XHTML.M.elt
    and type form_content_elt_list := XHTML_types.form_content XHTML.M.elt list
    and type form_attrib_t := XHTML_types.form_attrib XHTML.M.attrib list

    and type 'a a_elt := XHTML_types.a XHTML.M.elt
    and type 'a a_content_elt := XHTML_types.a_content XHTML.M.elt
    and type 'a a_content_elt_list := XHTML_types.a_content XHTML.M.elt list
    and type a_attrib_t := XHTML_types.a_attrib XHTML.M.attrib list

    and type link_elt := XHTML_types.link XHTML.M.elt
    and type link_attrib_t := XHTML_types.link_attrib XHTML.M.attrib list

    and type script_elt := XHTML_types.script XHTML.M.elt
    and type script_attrib_t := XHTML_types.script_attrib XHTML.M.attrib list

    and type textarea_elt := XHTML_types.textarea XHTML.M.elt
    and type textarea_attrib_t := XHTML_types.textarea_attrib XHTML.M.attrib list

    and type input_elt := XHTML_types.input XHTML.M.elt
    and type input_attrib_t := XHTML_types.input_attrib XHTML.M.attrib list

    and type select_elt := XHTML_types.select XHTML.M.elt
    and type select_attrib_t := XHTML_types.select_attrib XHTML.M.attrib list

    and type button_elt := XHTML_types.button XHTML.M.elt
    and type button_content_elt := XHTML_types.button_content XHTML.M.elt
    and type button_content_elt_list := XHTML_types.button_content XHTML.M.elt list
    and type button_attrib_t := XHTML_types.button_attrib XHTML.M.attrib list

    and type optgroup_attrib_t := [ XHTML_types.common | `Disabled ] XHTML.M.attrib list
    and type option_attrib_t := XHTML_types.option_attrib XHTML.M.attrib list

    and type input_type_t := [ full_input_type ]
    and type raw_input_type_t := [ full_input_type ]
    and type button_type_t := [ button_type ]

module type XHTML_REGISTRATION = "sigs/eliom_reg.mli"
  subst type page    := XHTML_types.xhtml XHTML.M.elt
    and type options := unit
    and type return  := Eliom_services.http

(*****************************************************************************)
(*****************************************************************************)

module Xhtml_forms_base = struct

  open XHTML.M
  open XHTML_types

  type uri = XHTML_types.uri
  type pcdata_elt = XHTML_types.pcdata XHTML.M.elt

  type form_elt = XHTML_types.form XHTML.M.elt
  type form_content_elt = XHTML_types.form_content XHTML.M.elt
  type form_content_elt_list = XHTML_types.form_content XHTML.M.elt list
  type form_attrib_t = XHTML_types.form_attrib XHTML.M.attrib list

  type 'a a_elt = XHTML_types.a XHTML.M.elt
  type 'a a_elt_list = XHTML_types.a XHTML.M.elt list
  type 'a a_content_elt = XHTML_types.a_content XHTML.M.elt
  type 'a a_content_elt_list = XHTML_types.a_content XHTML.M.elt list
  type a_attrib_t = XHTML_types.a_attrib XHTML.M.attrib list

  type link_elt = XHTML_types.link XHTML.M.elt
  type link_attrib_t = XHTML_types.link_attrib XHTML.M.attrib list

  type script_elt = XHTML_types.script XHTML.M.elt
  type script_attrib_t = XHTML_types.script_attrib XHTML.M.attrib list

  type textarea_elt = XHTML_types.textarea XHTML.M.elt
  type textarea_attrib_t = XHTML_types.textarea_attrib XHTML.M.attrib list

  type input_elt = XHTML_types.input XHTML.M.elt
  type input_attrib_t = XHTML_types.input_attrib XHTML.M.attrib list

  type select_elt = XHTML_types.select XHTML.M.elt
  type select_content_elt = XHTML_types.select_content XHTML.M.elt
  type select_content_elt_list = XHTML_types.select_content XHTML.M.elt list
  type select_attrib_t = XHTML_types.select_attrib XHTML.M.attrib list

  type button_elt = XHTML_types.button XHTML.M.elt
  type button_content_elt = XHTML_types.button_content XHTML.M.elt
  type button_content_elt_list = XHTML_types.button_content XHTML.M.elt list
  type button_attrib_t = XHTML_types.button_attrib XHTML.M.attrib list

  type option_elt = XHTML_types.selectoption XHTML.M.elt
  type option_elt_list = XHTML_types.selectoption XHTML.M.elt list
  type optgroup_attrib_t = [ XHTML_types.common | `Disabled ] XHTML.M.attrib list
  type option_attrib_t = XHTML_types.option_attrib XHTML.M.attrib list

  type input_type_t = full_input_type
  type raw_input_type_t = full_input_type
  type button_type_t = button_type

  let hidden = `Hidden
  let checkbox = `Checkbox
  let radio = `Radio
  let submit = `Submit
  let file = `File
  let image = `Image

  let buttonsubmit = `Submit

  let uri_of_string = Uri.uri_of_string

  let empty_seq = []
  let cons_form a l = a::l

  let map_option = List.map
  let map_optgroup f a l = ((f a), List.map f l)
  let select_content_of_option a = (a :> select_content_elt)

  let make_pcdata s = pcdata s

  let make_a ?(a=[]) ?href ?onclick l : 'a a_elt =
    let a = match href with
      | None -> a
      | Some v -> (a_href (uri_of_string v))::a
    in
    let a = match onclick with
      | None -> a
      | Some v -> (a_onclick v)::a
    in
    XHTML.M.a ~a l

  let make_get_form ?(a=[]) ~action ?onsubmit elt1 elts : form elt =
    let a = (match onsubmit with
      | None -> a
      | Some s -> (a_onsubmit s)::a)
    in
    let r =
      form ~a:((a_method `Get)::a)
        ~action:(uri_of_string action) elt1 elts
    in
  (* if onsubmit is true, the node ref must exist: *)
    if onsubmit <> None then ignore (XML.ref_node (XHTML.M.toelt r));
    r


  let make_post_form ?(a=[]) ~action ?onsubmit ?id ?(inline = false) elt1 elts
      : form elt =
    let a = (match onsubmit with
      | None -> a
      | Some s -> (a_onsubmit s)::a)
    in
    let aa = (match id with
    | None -> a
    | Some i -> (a_id i)::a)
    in
    let r =
      form ~a:((XHTML.M.a_enctype "multipart/form-data")::
             (* Always Multipart!!! How to test if there is a file?? *)
                  (a_method `Post)::
                  (if inline then (a_class ["inline"])::aa else aa))
        ~action:(uri_of_string action) elt1 elts
    in
  (* if onsubmit is true, the node ref must exist: *)
    if onsubmit <> None then ignore (XML.ref_node (XHTML.M.toelt r));
    r

  let make_hidden_field content =
    let c = match content with
      | None -> []
      | Some c -> [c]
    in
    (div ~a:[a_class ["eliom_nodisplay"]] c :> form_content elt)

  let make_empty_form_content () = p [pcdata ""] (**** à revoir !!!!! *)

  let remove_first = function
    | a::l -> a,l
    | [] -> (make_empty_form_content ()), []

  let make_input ?(a=[]) ?(checked=false) ~typ ?name ?src ?value () =
    let a2 = match value with
    | None -> a
    | Some v -> (a_value v)::a
    in
    let a2 = match name with
    | None -> a2
    | Some v -> (a_name v)::a2
    in
    let a2 = match src with
    | None -> a2
    | Some v -> (a_src v)::a2
    in
    let a2 = if checked then (a_checked `Checked)::a2 else a2 in
    input ~a:((a_input_type typ)::a2) ()

  let make_button ?(a = []) ~button_type ?name ?value c =
    let a = match value with
    | None -> a
    | Some v -> (a_value v)::a
    in
    let a = match name with
    | None -> a
    | Some v -> (a_name v)::a
    in
    button ~a:((a_button_type button_type)::a) c

  let make_textarea ?(a=[]) ~name ?(value="") ~rows ~cols () =
    let a3 = (a_name name)::a in
    textarea ~a:a3 ~rows ~cols (pcdata value)

  let make_select ?(a=[]) ~multiple ~name elt elts =
    let a = if multiple then (a_multiple `Multiple)::a else a in
    select ~a:((a_name name)::a) elt elts

  let make_option ?(a=[]) ~selected ?value c =
    let a = match value with
    | None -> a
    | Some v -> (a_value v)::a
    in
    let a = if selected then (a_selected `Selected)::a else a in
    option ~a c

  let make_optgroup ?(a=[]) ~label elt elts =
    optgroup ~label ~a elt elts

  let make_css_link ?(a=[]) ~uri () =
    link ~a:((a_href uri)::
             (a_type "text/css")::(a_rel [`Stylesheet])::a) ()

  let make_js_script ?(a=[]) ~uri () =
    script ~a:((a_src uri)::a) ~contenttype:"text/javascript" (pcdata "")

(*
  let register_event_a node = XML.register_event (XHTML.M.toelt node)
  let register_event_form node = XML.register_event (XHTML.M.toelt node)
*)
(*POSTtabcookies* forms with tab cookies in POST params:

  let add_tab_cookies_to_get_form _ () =
    failwith "add_tab_cookies_to_get_form not implemented for xhtml1"

  let add_tab_cookies_to_post_form _ () =
    failwith "add_tab_cookies_to_post_form not implemented for xhtml1"

  let add_tab_cookies_to_get_form_id_string = "not implemented for xhtml1"

  let add_tab_cookies_to_post_form_id_string =
    add_tab_cookies_to_get_form_id_string
*)

  let make_a_with_onclick ?a ?cookies_info s =
    failwith "make_a_with_onclick not implemented for xhtml1"

  let make_get_form_with_onsubmit ?a ?cookies_info x y =
    failwith "make_get_form_with_onsubmit implemented for xhtml1"

  let make_post_form_with_onsubmit ?a ?cookies_info x y =
    failwith "make_post_form_with_onsubmit not implemented for xhtml1"

  let client_capable = false

end

(*****************************************************************************)
(*****************************************************************************)

module Xhtml_forms : XHTML_FORMS = struct

  open XHTML.M
  open XHTML_types

  module Xhtml_forms_closed : XHTML_FORMS_CLOSED =
    Eliom_mkforms.MakeForms(Xhtml_forms_base)

  (* As we want -> [> a ] elt and not -> [ a ] elt (etc.), as found in
     Xhtmlforms_closed, we introduce explicit coercion.  *)

  include Xhtml_forms_closed

  let a = (a :
             ?absolute:bool ->
            ?absolute_path:bool ->
            ?https:bool ->
            ?a:a_attrib attrib list ->
            service:('get, unit, [< get_service_kind ],
                     [< suff ], 'gn, 'pn,
                     [< registrable ], 'return) service ->
            ?hostname:string ->
            ?port:int ->
            ?fragment:string ->
            ?keep_nl_params:[ `All | `Persistent | `None ] ->
            ?nl_params: Eliom_parameters.nl_params_set ->
            ?no_appl:bool ->
            a_content elt list -> 'get ->
            a XHTML.M.elt :>
              ?absolute:bool ->
            ?absolute_path:bool ->
            ?https:bool ->
            ?a:a_attrib attrib list ->
            service:('get, unit, [< get_service_kind ],
                     [< suff ], 'gn, 'pn,
                     [< registrable ], 'return) service ->
            ?hostname:string ->
            ?port:int ->
            ?fragment:string ->
            ?keep_nl_params:[ `All | `Persistent | `None ] ->
            ?nl_params: Eliom_parameters.nl_params_set ->
            ?no_appl:bool ->
            a_content elt list -> 'get ->
            [> a] XHTML.M.elt)

  let css_link = (css_link :
                    ?a:(link_attrib attrib list) ->
                   uri:uri -> unit -> link elt :>
                   ?a:(link_attrib attrib list) ->
                   uri:uri -> unit -> [> link ] elt)

  let js_script = (js_script :
                     ?a:(script_attrib attrib list) ->
                    uri:uri -> unit -> script elt :>
                    ?a:(script_attrib attrib list) ->
                    uri:uri -> unit -> [> script ] elt)

  let make_uri = (make_uri :
                    ?absolute:bool ->
                   ?absolute_path:bool ->
                   ?https:bool ->
                   service:('get, unit, [< get_service_kind ],
                            [< suff ], 'gn, unit,
                            [< registrable ], 'return) service ->
                   ?hostname:string ->
                   ?port:int ->
                   ?fragment:string ->
                   ?keep_nl_params:[ `All | `Persistent | `None ] ->
                   ?nl_params: Eliom_parameters.nl_params_set ->
                   'get -> uri)

  let get_form = (get_form :
                    ?absolute:bool ->
                   ?absolute_path:bool ->
                   ?https:bool ->
                   ?a:form_attrib attrib list ->
                   service:('get, unit, [< get_service_kind ],
                            [<suff ], 'gn, 'pn,
                            [< registrable ], 'return) service ->
                   ?hostname:string ->
                   ?port:int ->
                   ?fragment:string ->
                   ?keep_nl_params:[ `All | `Persistent | `None ] ->
                   ?nl_params: Eliom_parameters.nl_params_set ->
                   ?no_appl:bool ->
		   ('gn -> form_content elt list) -> form elt :>
                   ?absolute:bool ->
                   ?absolute_path:bool ->
                   ?https:bool ->
                   ?a:form_attrib attrib list ->
                   service:('get, unit, [< get_service_kind ],
                            [<suff ], 'gn, 'pn,
                            [< registrable ], 'return) service ->
                   ?hostname:string ->
                   ?port:int ->
                   ?fragment:string ->
                   ?keep_nl_params:[ `All | `Persistent | `None ] ->
                   ?nl_params: Eliom_parameters.nl_params_set ->
                   ?no_appl:bool ->
                   ('gn -> form_content elt list) -> [> form ] elt)


  let lwt_get_form = (lwt_get_form :
                        ?absolute:bool ->
                       ?absolute_path:bool ->
                       ?https:bool ->
                       ?a:form_attrib attrib list ->
                       service:('get, unit, [< get_service_kind ],
                                [<suff ], 'gn, 'pn,
                                [< registrable ], 'return) service ->
                       ?hostname:string ->
                       ?port:int ->
                       ?fragment:string ->
                       ?keep_nl_params:[ `All | `Persistent | `None ] ->
                       ?nl_params: Eliom_parameters.nl_params_set ->
                       ?no_appl:bool ->
                       ('gn -> form_content elt list Lwt.t) -> form elt Lwt.t :>
                       ?absolute:bool ->
                       ?absolute_path:bool ->
                       ?https:bool ->
                       ?a:form_attrib attrib list ->
                       service:('get, unit, [< get_service_kind ],
                                [<suff ], 'gn, 'pn,
                                [< registrable ], 'return) service ->
                       ?hostname:string ->
                       ?port:int ->
                       ?fragment:string ->
                       ?keep_nl_params:[ `All | `Persistent | `None ] ->
                       ?nl_params: Eliom_parameters.nl_params_set ->
                       ?no_appl:bool ->
                       ('gn -> form_content elt list Lwt.t) ->
                       [> form ] elt Lwt.t)


  let post_form = (post_form :
                     ?absolute:bool ->
                    ?absolute_path:bool ->
                    ?https:bool ->
                    ?a:form_attrib attrib list ->
                    service:('get, 'post, [< post_service_kind ],
                             [< suff ], 'gn, 'pn,
                             [< registrable ], 'return) service ->
                    ?hostname:string ->
                    ?port:int ->
                    ?fragment:string ->
                    ?keep_nl_params:[ `All | `Persistent | `None ] ->
                    ?keep_get_na_params:bool ->
                    ?nl_params: Eliom_parameters.nl_params_set ->
                    ?no_appl:bool ->
                    ('pn -> form_content elt list) -> 'get -> form elt :>
                    ?absolute:bool ->
                    ?absolute_path:bool ->
                    ?https:bool ->
                    ?a:form_attrib attrib list ->
                    service:('get, 'post, [< post_service_kind ],
                             [< suff ], 'gn, 'pn,
                             [< registrable ], 'return) service ->
                    ?hostname:string ->
                    ?port:int ->
                    ?fragment:string ->
                    ?keep_nl_params:[ `All | `Persistent | `None ] ->
                    ?keep_get_na_params:bool ->
                    ?nl_params: Eliom_parameters.nl_params_set ->
                    ?no_appl:bool ->
                    ('pn -> form_content elt list) -> 'get -> [> form ] elt)

  let lwt_post_form = (lwt_post_form :
                         ?absolute:bool ->
                        ?absolute_path:bool ->
                        ?https:bool ->
                        ?a:form_attrib attrib list ->
                        service:('get, 'post, [< post_service_kind ],
                                 [< suff ], 'gn, 'pn,
                                 [< registrable ], 'return) service ->
                        ?hostname:string ->
                        ?port:int ->
                        ?fragment:string ->
                        ?keep_nl_params:[ `All | `Persistent | `None ] ->
                        ?keep_get_na_params:bool ->
                        ?nl_params: Eliom_parameters.nl_params_set ->
                        ?no_appl:bool ->
                        ('pn -> form_content elt list Lwt.t) ->
                        'get -> form elt Lwt.t :>
                        ?absolute:bool ->
                        ?absolute_path:bool ->
                        ?https:bool ->
                        ?a:form_attrib attrib list ->
                        service:('get, 'post, [< post_service_kind ],
                                 [< suff ], 'gn, 'pn,
                                 [< registrable ], 'return) service ->
                        ?hostname:string ->
                        ?port:int ->
                        ?fragment:string ->
                        ?keep_nl_params:[ `All | `Persistent | `None ] ->
                        ?keep_get_na_params:bool ->
                        ?nl_params: Eliom_parameters.nl_params_set ->
                        ?no_appl:bool ->
                        ('pn -> form_content elt list Lwt.t) -> 'get ->
                        [> form ] elt Lwt.t)


  type basic_input_type =
      [
      | `Hidden
      | `Password
      | `Submit
      | `Text ]

  type full_input_type =
      [ `Button
      | `Checkbox
      | `File
      | `Hidden
      | `Image
      | `Password
      | `Radio
      | `Reset
      | `Submit
      | `Text ]

  let int_input = (int_input :
		     ?a:input_attrib attrib list -> input_type:full_input_type ->
		    ?name:'a -> ?value:int -> unit -> input elt :>
		    ?a:input_attrib attrib list -> input_type:[< basic_input_type] ->
		    ?name:'a -> ?value:int -> unit -> [> input ] elt)

  let int32_input = (int32_input :
		       ?a:input_attrib attrib list -> input_type:full_input_type ->
		      ?name:'a -> ?value:int32 -> unit -> input elt :>
		      ?a:input_attrib attrib list -> input_type:[< basic_input_type] ->
		      ?name:'a -> ?value:int32 -> unit -> [> input ] elt)

  let int64_input = (int64_input :
		       ?a:input_attrib attrib list -> input_type:full_input_type ->
		      ?name:'a -> ?value:int64 -> unit -> input elt :>
		      ?a:input_attrib attrib list -> input_type:[< basic_input_type] ->
		      ?name:'a -> ?value:int64 -> unit -> [> input ] elt)

  let float_input = (float_input :
		       ?a:input_attrib attrib list -> input_type:full_input_type ->
		      ?name:'a -> ?value:float -> unit -> input elt :>
		      ?a:input_attrib attrib list -> input_type:[< basic_input_type] ->
		      ?name:'a -> ?value:float -> unit -> [> input ] elt)

  let string_input = (string_input :
			?a:input_attrib attrib list -> input_type:full_input_type ->
		       ?name:'a -> ?value:string -> unit -> input elt :>
		       ?a:input_attrib attrib list -> input_type:[< basic_input_type] ->
		       ?name:'a -> ?value:string -> unit -> [> input ] elt)

  let user_type_input = (user_type_input :
                           ('a -> string) ->
			  ?a:input_attrib attrib list -> input_type:full_input_type ->
			  ?name:'b -> ?value:'a -> unit -> input elt :>
                          ('a -> string) ->
			  ?a:input_attrib attrib list -> input_type:[< basic_input_type] ->
			  ?name:'b -> ?value:'a -> unit -> [> input ] elt)

  let raw_input = (raw_input :
		     ?a:input_attrib attrib list -> input_type:full_input_type ->
		    ?name:string -> ?value:string -> unit -> input elt :>
		    ?a:input_attrib attrib list ->
		    input_type:[< basic_input_type | `Button | `Reset ] ->
		    ?name:string -> ?value:string -> unit -> [> input ] elt)

  let file_input = (file_input :
		      ?a:input_attrib attrib list -> name:'a ->
		     unit -> input elt :>
		     ?a:input_attrib attrib list -> name:'a ->
		     unit -> [> input ] elt)

  let image_input = (image_input :
		       ?a:input_attrib attrib list -> name:'a ->
		      ?src:uri -> unit -> input elt :>
		      ?a:input_attrib attrib list -> name:'a ->
		      ?src:uri -> unit -> [> input ] elt)

  let int_image_input = (int_image_input :
			   ?a:input_attrib attrib list ->
			  name:'a -> value:int ->
			  ?src:uri -> unit -> input elt :>
			  ?a:input_attrib attrib list ->
			  name:'a -> value:int ->
			  ?src:uri -> unit -> [> input ] elt)

  let int32_image_input = (int32_image_input :
			     ?a:input_attrib attrib list ->
			    name:'a -> value:int32 ->
			    ?src:uri -> unit -> input elt :>
			    ?a:input_attrib attrib list ->
			    name:'a -> value:int32 ->
			    ?src:uri -> unit -> [> input ] elt)

  let int64_image_input = (int64_image_input :
			     ?a:input_attrib attrib list ->
			    name:'a -> value:int64 ->
			    ?src:uri -> unit -> input elt :>
			    ?a:input_attrib attrib list ->
			    name:'a -> value:int64 ->
			    ?src:uri -> unit -> [> input ] elt)

  let float_image_input = (float_image_input :
			     ?a:input_attrib attrib list ->
			    name:'a -> value:float ->
			    ?src:uri -> unit -> input elt :>
			    ?a:input_attrib attrib list ->
			    name:'a -> value:float ->
			    ?src:uri -> unit -> [> input ] elt)

  let string_image_input = (string_image_input :
			      ?a:input_attrib attrib list ->
			     name:'a -> value:string ->
			     ?src:uri -> unit -> input elt :>
			     ?a:input_attrib attrib list ->
			     name:'a -> value:string ->
			     ?src:uri -> unit -> [> input ] elt)

  let user_type_image_input = (user_type_image_input :
				 ('a -> string) ->
				?a:input_attrib attrib list ->
				name:'b -> value:'a ->
				?src:uri -> unit -> input elt :>
				('a -> string) ->
				?a:input_attrib attrib list ->
				name:'b -> value:'a ->
				?src:uri -> unit -> [> input ] elt)

  let raw_image_input = (raw_image_input :
			   ?a:input_attrib attrib list ->
			  name:string -> value:string -> ?src:uri -> unit -> input elt :>
			  ?a:input_attrib attrib list ->
			  name:string -> value:string -> ?src:uri -> unit -> [> input ] elt)

  let bool_checkbox = (bool_checkbox :
			 ?a:(input_attrib attrib list ) -> ?checked:bool ->
			name:'a -> unit -> input elt :>
			?a:(input_attrib attrib list ) -> ?checked:bool ->
			name:'a -> unit -> [> input ] elt)

  let int_checkbox = (int_checkbox :
			?a:input_attrib attrib list -> ?checked:bool ->
		       name:[ `Set of int ] param_name -> value:int -> unit -> input elt :>
		       ?a:input_attrib attrib list -> ?checked:bool ->
		       name:[ `Set of int ] param_name -> value:int -> unit -> [> input ] elt)

  let int32_checkbox = (int32_checkbox :
			  ?a:input_attrib attrib list -> ?checked:bool ->
			 name:[ `Set of int32 ] param_name -> value:int32 -> unit -> input elt :>
			 ?a:input_attrib attrib list -> ?checked:bool ->
			 name:[ `Set of int32 ] param_name -> value:int32 -> unit -> [> input ] elt)

  let int64_checkbox = (int64_checkbox :
			  ?a:input_attrib attrib list -> ?checked:bool ->
			 name:[ `Set of int64 ] param_name -> value:int64 -> unit -> input elt :>
			 ?a:input_attrib attrib list -> ?checked:bool ->
			 name:[ `Set of int64 ] param_name -> value:int64 -> unit -> [> input ] elt)

  let float_checkbox = (float_checkbox :
			  ?a:input_attrib attrib list -> ?checked:bool ->
			 name:[ `Set of float ] param_name -> value:float -> unit -> input elt :>
			 ?a:input_attrib attrib list -> ?checked:bool ->
			 name:[ `Set of float ] param_name -> value:float -> unit -> [> input ] elt)

  let string_checkbox = (string_checkbox :
			   ?a:input_attrib attrib list -> ?checked:bool ->
			  name:[ `Set of string ] param_name -> value:string -> unit -> input elt :>
			  ?a:input_attrib attrib list -> ?checked:bool ->
			  name:[ `Set of string ] param_name -> value:string -> unit -> [> input ] elt)

  let user_type_checkbox = (user_type_checkbox :
			      ('a -> string) ->
			     ?a:input_attrib attrib list -> ?checked:bool ->
			     name:[ `Set of 'a ] param_name -> value:'a -> unit -> input elt :>
			     ('a -> string) ->
			     ?a:input_attrib attrib list -> ?checked:bool ->
			     name:[ `Set of 'a ] param_name -> value:'a -> unit -> [> input ] elt)

  let raw_checkbox = (raw_checkbox :
			?a:input_attrib attrib list -> ?checked:bool ->
		       name:string -> value:string -> unit -> input elt :>
		       ?a:input_attrib attrib list -> ?checked:bool ->
		       name:string -> value:string -> unit -> [> input ] elt)


  let string_radio = (string_radio :
			?a:(input_attrib attrib list ) -> ?checked:bool ->
		       name:'a -> value:string -> unit -> input elt :>
		       ?a:(input_attrib attrib list ) -> ?checked:bool ->
		       name:'a -> value:string -> unit -> [> input ] elt)

  let int_radio = (int_radio :
                     ?a:(input_attrib attrib list ) -> ?checked:bool ->
		    name:'a -> value:int -> unit -> input elt :>
                    ?a:(input_attrib attrib list ) -> ?checked:bool ->
		    name:'a -> value:int -> unit -> [> input ] elt)

  let int32_radio = (int32_radio :
                       ?a:(input_attrib attrib list ) -> ?checked:bool ->
		      name:'a -> value:int32 -> unit -> input elt :>
                      ?a:(input_attrib attrib list ) -> ?checked:bool ->
		      name:'a -> value:int32 -> unit -> [> input ] elt)

  let int64_radio = (int64_radio :
                       ?a:(input_attrib attrib list ) -> ?checked:bool ->
		      name:'a -> value:int64 -> unit -> input elt :>
                      ?a:(input_attrib attrib list ) -> ?checked:bool ->
		      name:'a -> value:int64 -> unit -> [> input ] elt)

  let float_radio = (float_radio :
                       ?a:(input_attrib attrib list ) -> ?checked:bool ->
		      name:'a -> value:float -> unit -> input elt :>
                      ?a:(input_attrib attrib list ) -> ?checked:bool ->
		      name:'a -> value:float -> unit -> [> input ] elt)

  let user_type_radio = (user_type_radio :
			   ('a -> string) ->
                          ?a:(input_attrib attrib list ) -> ?checked:bool ->
			  name:'b -> value:'a -> unit -> input elt :>
			  ('a -> string) ->

                          ?a:(input_attrib attrib list ) -> ?checked:bool ->
			  name:'b -> value:'a -> unit -> [> input ] elt)

  let raw_radio = (raw_radio :
                     ?a:(input_attrib attrib list ) -> ?checked:bool ->
		    name:string -> value:string -> unit -> input elt :>
                    ?a:(input_attrib attrib list ) -> ?checked:bool ->
		    name:string -> value:string -> unit -> [> input ] elt)

  let textarea = (textarea :
		    ?a:textarea_attrib attrib list ->
		   name:'a -> ?value:string ->
		   rows:int -> cols:int ->
		   unit -> textarea elt :>
		   ?a:textarea_attrib attrib list ->
		   name:'a -> ?value:string ->
		   rows:int -> cols:int ->
		   unit -> [> textarea ] elt)

  let raw_textarea = (raw_textarea :
			?a:textarea_attrib attrib list ->
		       name:string -> ?value:string ->
		       rows:int -> cols:int ->
		       unit -> textarea elt :>
		       ?a:textarea_attrib attrib list ->
		       name:string -> ?value:string ->
		       rows:int -> cols:int ->
		       unit -> [> textarea ] elt)

  let raw_select = (raw_select :
		      ?a:select_attrib attrib list ->
		     name:string ->
		     string select_opt ->
		     string select_opt list -> select elt :>
		     ?a:select_attrib attrib list ->
		     name:string ->
		     string select_opt ->
		     string select_opt list -> [> select ] elt)

  let int_select = (int_select :
		      ?a:select_attrib attrib list ->
		     name:'a ->
		     int select_opt ->
		     int select_opt list -> select elt :>
		     ?a:select_attrib attrib list ->
		     name:'a ->
		     int select_opt ->
		     int select_opt list -> [> select ] elt)

  let int32_select = (int32_select :
			?a:select_attrib attrib list ->
		       name:'a ->
		       int32 select_opt ->
		       int32 select_opt list -> select elt :>
		       ?a:select_attrib attrib list ->
		       name:'a ->
		       int32 select_opt ->
		       int32 select_opt list -> [> select ] elt)

  let int64_select = (int64_select :
			?a:select_attrib attrib list ->
		       name:'a ->
		       int64 select_opt ->
		       int64 select_opt list -> select elt :>
		       ?a:select_attrib attrib list ->
		       name:'a ->
		       int64 select_opt ->
		       int64 select_opt list -> [> select ] elt)

  let float_select = (float_select :
			?a:select_attrib attrib list ->
		       name:'a ->
		       float select_opt ->
		       float select_opt list -> select elt :>
		       ?a:select_attrib attrib list ->
		       name:'a ->
		       float select_opt ->
		       float select_opt list -> [> select ] elt)

  let string_select = (string_select :
			 ?a:select_attrib attrib list ->
			name:'a ->
			string select_opt ->
			string select_opt list -> select elt :>
			?a:select_attrib attrib list ->
			name:'a ->
			string select_opt ->
			string select_opt list -> [> select ] elt)

  let user_type_select = (user_type_select :
			    ('a -> string) ->
			   ?a:select_attrib attrib list ->
			   name:'b ->
			   'a select_opt ->
			   'a select_opt list -> select elt :>
			   ('a -> string) ->
			   ?a:select_attrib attrib list ->
			   name:'b ->
			   'a select_opt ->
			   'a select_opt list -> [> select ] elt)


  let raw_multiple_select = (raw_multiple_select :
			       ?a:select_attrib attrib list ->
			      name:string ->
			      string select_opt ->
			      string select_opt list -> select elt :>
			      ?a:select_attrib attrib list ->
			      name:string ->
			      string select_opt ->
			      string select_opt list -> [> select ] elt)

  let int_multiple_select = (int_multiple_select :
			       ?a:select_attrib attrib list ->
			      name:'a ->
			      int select_opt ->
			      int select_opt list -> select elt :>
			      ?a:select_attrib attrib list ->
			      name:'a ->
			      int select_opt ->
			      int select_opt list -> [> select ] elt)

  let int32_multiple_select = (int32_multiple_select :
				 ?a:select_attrib attrib list ->
				name:'a ->
				int32 select_opt ->
				int32 select_opt list -> select elt :>
				?a:select_attrib attrib list ->
				name:'a ->
				int32 select_opt ->
				int32 select_opt list -> [> select ] elt)

  let int64_multiple_select = (int64_multiple_select :
				 ?a:select_attrib attrib list ->
				name:'a ->
				int64 select_opt ->
				int64 select_opt list -> select elt :>
				?a:select_attrib attrib list ->
				name:'a ->
				int64 select_opt ->
				int64 select_opt list -> [> select ] elt)

  let float_multiple_select = (float_multiple_select :
				 ?a:select_attrib attrib list ->
				name:'a ->
				float select_opt ->
				float select_opt list -> select elt :>
				?a:select_attrib attrib list ->
				name:'a ->
				float select_opt ->
				float select_opt list -> [> select ] elt)

  let string_multiple_select = (string_multiple_select :
				  ?a:select_attrib attrib list ->
				 name:'a ->
				 string select_opt ->
				 string select_opt list -> select elt :>
				 ?a:select_attrib attrib list ->
				 name:'a ->
				 string select_opt ->
				 string select_opt list -> [> select ] elt)

  let user_type_multiple_select = (user_type_multiple_select :
				     ('a -> string) ->
				    ?a:select_attrib attrib list ->
				    name:'b ->
				    'a select_opt ->
				    'a select_opt list -> select elt :>
				    ('a -> string) ->
				    ?a:select_attrib attrib list ->
				    name:'b ->
				    'a select_opt ->
				    'a select_opt list -> [> select ] elt)

  type button_type =
      [ `Button
      | `Reset
      | `Submit
      ]

  let string_button = (string_button :
			 ?a:button_attrib attrib list ->
			name:'a -> value:string ->
			button_content elt list -> button elt :>
			?a:button_attrib attrib list ->
			name:'a -> value:string ->
			button_content elt list -> [> button ] elt)

  let int_button = (int_button :
		      ?a:button_attrib attrib list ->
		     name:'a -> value:int ->
		     button_content elt list -> button elt :>
		     ?a:button_attrib attrib list ->
		     name:'a -> value:int ->
		     button_content elt list -> [> button ] elt)

  let int32_button = (int32_button :
			?a:button_attrib attrib list ->
		       name:'a -> value:int32 ->
		       button_content elt list -> button elt :>
		       ?a:button_attrib attrib list ->
		       name:'a -> value:int32 ->
		       button_content elt list -> [> button ] elt)

  let int64_button = (int64_button :
			?a:button_attrib attrib list ->
		       name:'a -> value:int64 ->
		       button_content elt list -> button elt :>
		       ?a:button_attrib attrib list ->
		       name:'a -> value:int64 ->
		       button_content elt list -> [> button ] elt)

  let float_button = (float_button :
			?a:button_attrib attrib list ->
		       name:'a -> value:float ->
		       button_content elt list -> button elt :>
		       ?a:button_attrib attrib list ->
		       name:'a -> value:float ->
		       button_content elt list -> [> button ] elt)

  let user_type_button = (user_type_button :
			    ('a -> string) ->
			   ?a:button_attrib attrib list ->
			   name:'b -> value:'a ->
			   button_content elt list -> button elt :>
			   ('a -> string) ->
			   ?a:button_attrib attrib list ->
			   name:'b -> value:'a ->
			   button_content elt list -> [> button ] elt)

  let raw_button = (raw_button :
		      ?a:button_attrib attrib list ->
		     button_type:button_type ->
		     name:string -> value:string ->
		     button_content elt list -> button elt :>
		     ?a:button_attrib attrib list ->
		     button_type:[< button_type ] ->
		     name:string -> value:string ->
		     button_content elt list -> [> button ] elt)

  let button = (button :
		  ?a:button_attrib attrib list ->
		 button_type:button_type ->
		 button_content elt list -> button elt :>
		 ?a:button_attrib attrib list ->
		 button_type:[< button_type ] ->
		 button_content elt list -> [> button ] elt)

end

(*****************************************************************************)
(*****************************************************************************)

module Xhtml_make_reg_base
  (Xhtml_content : Ocsigen_http_frame.HTTP_CONTENT
   with type t = XHTML_types.xhtml XHTML.M.elt) = struct

  open XHTML.M
  open XHTML_types

  type page = xhtml elt

  type return = Eliom_services.http

  module Xhtml_content = struct

    include Xhtml_content

    let add_css (a : 'a) : 'a =
      let css =
        XHTML.M.toelt
          (XHTML.M.style ~contenttype:"text/css"
             [XHTML.M.pcdata "\n.";
             XHTML.M.pcdata Eliom_common.inline_class_name;
             XHTML.M.pcdata " {display: inline}\n.";
             XHTML.M.pcdata Eliom_common.nodisplay_class_name;
             XHTML.M.pcdata " {display: none}\n"])
      in
      let rec aux = function
        | { XML.elt = XML.Node ("head",al,el) } as e::l ->
            { e with XML.elt = XML.Node ("head",al,css::el) }::l
        | e::l -> e::(aux l)
        | [] -> []
      in
      XHTML.M.tot
        (match XHTML.M.toelt a with
           | { XML.elt = XML.Node ("html",al,el) } as e ->
               { e with XML.elt = XML.Node ("html",al,aux el) }
           | e -> e)

    let get_etag ?options c = get_etag (add_css c)

    let result_of_content ?options c = result_of_content ?options (add_css c)

  end

  type options = unit

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers content =
    let open Ocsigen_http_frame in
    Xhtml_content.result_of_content content >>= fun r ->
    Lwt.return
      {r with
         res_cookies= Eliom_request_info.get_user_cookies ();
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None -> Some (Eliom_config.get_config_default_charset ())
                         | _ -> charset
                      );
         res_content_type= (match content_type with
                              | None -> r.res_content_type
                              | _ -> content_type
                           );
         res_headers= (match headers with
                         | None -> r.res_headers
                         | Some headers ->
                             Http_headers.with_defaults headers r.res_headers
                      );
      }

end

module Xhtml_reg_base =
  Xhtml_make_reg_base(Ocsigen_senders.Make_XML_Content(XML)(XHTML.M))

module Xhtml_registration = Eliom_mkreg.MakeRegister(Xhtml_reg_base)

module Xhtml = struct
  include Xhtml_forms
  include Xhtml_registration
end


(****************************************************************************)
(****************************************************************************)

module Make_TypedXML_Registration
  (XML: XML_sigs.Iterable)
  (TypedXML: XML_sigs.TypedXML with module XML := XML)
  (E : sig type content end) = struct

    module type REGISTRATION = "sigs/eliom_reg.mli"
      subst type page    := E.content TypedXML.elt list
	    and type options := unit
	    and type return  := Eliom_services.http

    module Format = XML_print.MakeTyped(XML)(TypedXML)(Ocsigen_stream.StringStream)

    let result_of_content_subxhtml get_etag c =
      let x = Format.print_list c in
      let default_result = Ocsigen_http_frame.default_result () in
      Lwt.return
        {default_result with
          Ocsigen_http_frame.res_content_length = None;
          res_content_type = Some "text/html";
          res_etag = get_etag c;
          res_headers= Http_headers.dyn_headers;
          res_stream = (x, None)
        }

    module Cont_content =
      (* Pasted from ocsigen_senders.ml and modified *)
      struct
        type t = E.content TypedXML.elt list

        let get_etag_aux x = None

        let get_etag ?options c = None

        let result_of_content c = result_of_content_subxhtml get_etag c

      end

    module Cont_reg_base = struct

      type page = E.content TypedXML.elt list

      type options = unit

      type return = Eliom_services.http

      let send_appl_content = Eliom_services.XNever

      let send ?options ?charset ?code
          ?content_type ?headers content =
        Cont_content.result_of_content content >>= fun r ->
        let open Ocsigen_http_frame in
        Lwt.return
          {r with
	     res_cookies= Eliom_request_info.get_user_cookies ();
             res_code= code_of_code_option code;
             res_charset= (match charset with
                           | None -> Some (Eliom_config.get_config_default_charset ())
                           | _ -> charset);
             res_content_type= (match content_type with
				| None -> r.res_content_type
				| _ -> content_type
                               );
             res_headers= (match headers with
                           | None -> r.res_headers
                           | Some headers ->
                               Http_headers.with_defaults
				 headers r.res_headers
                          );

          }

    end

    include Eliom_mkreg.MakeRegister(Cont_reg_base)

  end

module Blocks = Make_TypedXML_Registration(XML)(XHTML.M)(struct
  type content = XHTML_types.body_content
end)

module type BLOCKS_REGISTRATION = Blocks.REGISTRATION

module Blocks5 = Make_TypedXML_Registration(XML)(HTML5.M)(struct
  type content = HTML5_types.body_content
end)

module type BLOCKS5_REGISTRATION = Blocks5.REGISTRATION

(****************************************************************************)
(****************************************************************************)

module Text_reg_base = struct

  type page = (string * string)

  type options = unit

  type return = Eliom_services.http

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers content =
    Ocsigen_senders.Text_content.result_of_content content >>= fun r ->
    let open Ocsigen_http_frame in
    Lwt.return
      {r with
         res_cookies= Eliom_request_info.get_user_cookies ();
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None ->  Some (Eliom_config.get_config_default_charset ())
                         | _ -> charset);
         res_content_type= (match content_type with
                              | None -> r.res_content_type
                              | _ -> content_type
                           );
         res_headers= (match headers with
                         | None -> r.res_headers
                         | Some headers ->
                             Http_headers.with_defaults headers r.res_headers
                      );
      }

end

module Text = Eliom_mkreg.MakeRegister(Text_reg_base)

(****************************************************************************)
(****************************************************************************)

module CssText_reg_base = struct

  type page = string

  type options = unit

  type return = Eliom_services.http

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code
     ?content_type ?headers content =
    Ocsigen_senders.Text_content.result_of_content (content, "text/css")
    >>= fun r ->
    let open Ocsigen_http_frame in
    Lwt.return
      {r with
         res_cookies= Eliom_request_info.get_user_cookies ();
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None -> Some (Eliom_config.get_config_default_charset ())
                         | _ -> charset);
         res_content_type= (match content_type with
                              | None -> r.res_content_type
                              | _ -> content_type
                           );
         res_headers= (match headers with
                         | None -> r.res_headers
                         | Some headers ->
                             Http_headers.with_defaults
                               headers r.res_headers
                      );
      }

end

module CssText = Eliom_mkreg.MakeRegister(CssText_reg_base)

(****************************************************************************)
(****************************************************************************)

module type HTMLTEXT_REGISTRATION = "sigs/eliom_reg.mli"
  subst type page    := string
    and type options := unit
    and type return  := Eliom_services.http

module type HTMLTEXT_FORMS = "sigs/eliom_forms.mli"
  subst type uri := string
    and type pcdata_elt := string

    and type form_elt := string
    and type form_content_elt := string
    and type form_content_elt_list := string
    and type form_attrib_t := string

    and type 'a a_elt := string
    and type 'a a_elt_list := string
    and type 'a a_content_elt := string
    and type 'a a_content_elt_list := string
    and type a_attrib_t := string

    and type link_elt := string
    and type link_attrib_t := string

    and type script_elt := string
    and type script_attrib_t := string

    and type textarea_elt := string
    and type textarea_attrib_t := string

    and type input_elt := string
    and type input_attrib_t := string

    and type select_elt := string
    and type select_content_elt := string
    and type select_content_elt_list := string
    and type select_attrib_t := string

    and type button_elt := string
    and type button_content_elt := string
    and type button_content_elt_list := string
    and type button_attrib_t := string

    and type option_elt := string
    and type option_elt_list := string
    and type optgroup_attrib_t := string
    and type option_attrib_t := string

    and type input_type_t := string
    and type raw_input_type_t := string
    and type button_type_t := string

module HtmlText_reg_base = struct

  type page = string

  type options = unit

  type return = Eliom_services.http

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers content =
    Ocsigen_senders.Text_content.result_of_content (content, "text/html")
    >>= fun r ->
    let open Ocsigen_http_frame in
    Lwt.return
      {r with
         res_cookies= Eliom_request_info.get_user_cookies ();
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None -> Some (Eliom_config.get_config_default_charset ())
                         | _ -> charset);
         res_content_type= (match content_type with
                              | None -> r.res_content_type
                              | _ -> content_type
                           );
         res_headers= (match headers with
                         | None -> r.res_headers
                         | Some headers ->
                             Http_headers.with_defaults headers r.res_headers
                      );
      }

end

module HtmlText_forms_base = struct

  type uri = string
  type pcdata_elt = string

  type form_elt = string
  type form_content_elt = string
  type form_content_elt_list = string
  type form_attrib_t = string

  type 'a a_elt = string
  type 'a a_elt_list = string
  type 'a a_content_elt = string
  type 'a a_content_elt_list = string
  type a_attrib_t = string

  type link_elt = string
  type link_attrib_t = string

  type script_elt = string
  type script_attrib_t = string

  type textarea_elt = string
  type textarea_attrib_t = string

  type input_elt = string
  type input_attrib_t = string

  type select_elt = string
  type select_content_elt = string
  type select_content_elt_list = string
  type select_attrib_t = string

  type button_elt = string
  type button_content_elt = string
  type button_content_elt_list = string
  type button_attrib_t = string

  type option_elt = string
  type option_elt_list = string
  type optgroup_attrib_t = string
  type option_attrib_t = string

  type input_type_t = string
  type raw_input_type_t = string
  type button_type_t = string

  let hidden = "hidden"
(*  let text = "text"
  let password = "password" *)
  let checkbox = "checkbox"
  let radio = "radio"
  let submit = "submit"
  let file = "file"
  let image = "image"

  let buttonsubmit = "submit"

  let uri_of_string x = x

  let empty_seq = ""
  let cons_form a l = a^l

  let map_option f =
    List.fold_left (fun d a -> d^(f a)) ""

  let map_optgroup f a l =
    ((f a), List.fold_left (fun d a -> d^(f a)) "" l)

  let select_content_of_option = id

  let make_pcdata = id

  let make_a ?(a="") ?href ?onclick l : 'a a_elt =
    let a = match href with
      | None -> a
      | Some v -> " href=\""^v^"\" "^a
    in
    let a = match onclick with
      | None -> a
      | Some v -> " onclick=\""^v^"\" "^a
    in
    "<a "^a^">"^(* List.fold_left (^) "" l *) l^"</a>"

  let make_get_form ?(a="") ~action ?onsubmit elt1 elts : form_elt =
    let a = match onsubmit with
      | None -> a
      | Some v -> " onsubmit=\""^v^"\" "^a
    in
    "<form method=\"get\" action=\""^(uri_of_string action)^"\""^a^">"^
    elt1^(*List.fold_left (^) "" elts *) elts^"</form>"

  let make_post_form ?(a="") ~action ?onsubmit ?id ?(inline = false) elt1 elts
      : form_elt =
    let a = match onsubmit with
      | None -> a
      | Some v -> " onsubmit=\""^v^"\" "^a
    in
    let aa = "enctype=\"multipart/form-data\" "
        (* Always Multipart!!! How to test if there is a file?? *)
      ^(match id with
        None -> a
      | Some i -> " id="^i^" "^a)
    in
    "<form method=\"post\" action=\""^(uri_of_string action)^"\""^
    (if inline then "style=\"display: inline\"" else "")^aa^">"^
    elt1^(* List.fold_left (^) "" elts*) elts^"</form>"

  let make_hidden_field content =
    let content = match content with
      | None -> ""
      | Some c -> c
    in
    "<div style=\"display: none\""^content^"</div>"

  let remove_first l = "",l

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

  let make_textarea ?(a="") ~name:name ?(value="") ~rows ~cols () =
    "<textarea name=\""^name^"\" rows=\""^(string_of_int rows)^
    "\" cols=\""^(string_of_int cols)^"\" "^a^">"^value^"</textarea>"

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

(*
  let register_event_a elt ev callback v =
    failwith "register_event_a not implemented for text"

  let register_event_form elt ev callback v =
    failwith "register_event_form not implemented for text"
*)

(*POSTtabcookies* forms with tab cookies in POST params:

  let add_tab_cookies_to_get_form _ () =
    failwith "add_tab_cookies_to_get_form not implemented for text"

  let add_tab_cookies_to_post_form _ () =
    failwith "add_tab_cookies_to_post_form not implemented for text"

  let add_tab_cookies_to_get_form_id_string = "not implemented for text"

  let add_tab_cookies_to_post_form_id_string =
    add_tab_cookies_to_get_form_id_string
*)
  let make_a_with_onclick ?a ?cookies_info s =
    failwith "make_a_with_onclick not implemented for text"

  let make_get_form_with_onsubmit ?a ?cookies_info x y =
    failwith "make_get_form_with_onsubmit implemented for text"

  let make_post_form_with_onsubmit ?a ?cookies_info x y =
    failwith "make_post_form_with_onsubmit not implemented for text"

  let client_capable = false

end

module HtmlText_registration : HTMLTEXT_REGISTRATION =
  Eliom_mkreg.MakeRegister(HtmlText_reg_base)
module HtmlText_forms : HTMLTEXT_FORMS =
  Eliom_mkforms.MakeForms(HtmlText_forms_base)

module HtmlText = struct
  include HtmlText_registration
  include HtmlText_forms
end

(****************************************************************************)
(****************************************************************************)

(** Actions are like services, but do not generate any page. The current
   page is reloaded (but if you give the optional parameter
    [~options:`NoReload] to the registration function).
 *)
module Action_reg_base = struct

  type page = unit

  type options = [ `Reload | `NoReload ]

  type return = Eliom_services.http

  let send_appl_content = Eliom_services.XAlways
  (* The post action service will decide later *)

  let send_directly =
  (* send bypassing the following directives
     in the configuration file (they have already been taken into account) *)
    fun ri res ->
      Polytables.set
        ri.Ocsigen_extensions.ri_request_cache Eliom_common.found_stop_key ();
      res

  let send
      ?(options = `Reload) ?charset ?(code = 204)
      ?content_type ?headers () =
    let user_cookies = Eliom_request_info.get_user_cookies () in
    if options = `NoReload
    then
      let open Ocsigen_http_frame in
      let empty_result = Ocsigen_http_frame.empty_result () in
      Lwt.return
        {empty_result with
          res_cookies= user_cookies;
          res_code= code;
          res_content_type= (match content_type with
            | None -> empty_result.res_content_type
            | _ -> content_type
          );
          res_headers= (match headers with
            | None -> empty_result.res_headers
            | Some headers ->
              Http_headers.with_defaults
                headers empty_result.res_headers
          );
        }
    else
      (* It is an action, we reload the page.
         To do that, we retry without POST params.
         If no post param at all, we retry
         without GET non_att info.
         If no GET non_att info, we retry without
         GET state.
         If no GET state,
         we do not reload, otherwise it will loop.
      *)
      (* be very careful while re-reading this *)
      let sp = Eliom_common.get_sp () in
      let sitedata = Eliom_request_info.get_sitedata_sp sp in
      let si = Eliom_request_info.get_si sp in
      let ri = Eliom_request_info.get_request_sp sp in
      let open Ocsigen_extensions in
      let open Ocsigen_http_frame in
      (match (si.Eliom_common.si_nonatt_info,
              si.Eliom_common.si_state_info,
              ri.request_info.ri_method) with
          | (Eliom_common.RNa_no,
             (Eliom_common.RAtt_no, Eliom_common.RAtt_no),
             Ocsigen_http_frame.Http_header.GET) ->
            let empty_result = Ocsigen_http_frame.empty_result () in
            Lwt.return empty_result
          | _ ->
            let all_cookie_info = sp.Eliom_common.sp_cookie_info
            in
            Eliommod_cookies.compute_new_ri_cookies
              (Unix.time ())
              ri.request_info.ri_sub_path
              (Lazy.force ri.request_info.ri_cookies)
              all_cookie_info
              user_cookies
            >>= fun ric ->

            Eliommod_cookies.compute_cookies_to_send
              sitedata
              all_cookie_info
              user_cookies
            >>= fun all_new_cookies ->

            (* Now tab cookies:
               As tab cookies are sent only by Eliom_app services,
               we just need to keep them in rc.
               If the fallback service is not Eliom_app, they will
               be lost.
            *)
            let rc = Eliom_request_info.get_request_cache_sp sp in
            Polytables.set
              ~table:rc
              ~key:Eliom_common.tab_cookie_action_info_key
              ~value:(sp.Eliom_common.sp_tab_cookie_info,
                      sp.Eliom_common.sp_user_tab_cookies,
                      si.Eliom_common.si_tab_cookies
              );

            (* Now removing some parameters to decide the following service: *)
            (match
                (si.Eliom_common.si_nonatt_info,
                 si.Eliom_common.si_state_info,
                 ri.request_info.ri_method)
             with
               | (Eliom_common.RNa_get_ _,
                  (_, Eliom_common.RAtt_no),
                  Ocsigen_http_frame.Http_header.GET)
               | (Eliom_common.RNa_get' _,
                  (_, Eliom_common.RAtt_no),
                  Ocsigen_http_frame.Http_header.GET)
                  (* no post params, GET na coservice *)
               | (Eliom_common.RNa_no,
                  (_, Eliom_common.RAtt_no),
                  Ocsigen_http_frame.Http_header.GET)
                    (* no post params, GET attached coservice *)
             ->
                 Polytables.set
                   ri.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache
                   Eliom_common.eliom_params_after_action
                   (si.Eliom_common.si_all_get_params,
                    si.Eliom_common.si_all_post_params, (* is Some [] *)
                    si.Eliom_common.si_nl_get_params,
                    si.Eliom_common.si_nl_post_params,
                    si.Eliom_common.si_all_get_but_nl (*204FORMS*,
                    si.Eliom_common.si_internal_form *))
(*VVV Also put all_cookie_info in this,
  to avoid update_cookie_table and get_cookie_info (?)
*)
               ;
               let ri =
                 {ri.request_info with
                   ri_cookies= lazy ric;
                   ri_get_params =
                     lazy si.Eliom_common.si_other_get_params;
                 (* Here we modify ri,
                    thus the request can be taken by other extensions,
                    with its new parameters *)
                 }
               in
               Eliommod_pagegen.update_cookie_table sitedata all_cookie_info
               >>= fun () ->
               send_directly ri (Ocsigen_extensions.compute_result
                                   ~previous_cookies:all_new_cookies ri)


               | (Eliom_common.RNa_post_ _, (_, _), _)
               | (Eliom_common.RNa_post' _, (_, _), _) ->
                      (* POST na coservice *)
                      (* retry without POST params *)

                 Polytables.set
                   ri.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache
                   Eliom_common.eliom_params_after_action
                   (si.Eliom_common.si_all_get_params,
                    si.Eliom_common.si_all_post_params,
                    si.Eliom_common.si_nl_get_params,
                    si.Eliom_common.si_nl_post_params,
                    si.Eliom_common.si_all_get_but_nl (*204FORMS*,
                    si.Eliom_common.si_internal_form *))
                 ;
                 let ri =
                   {ri.request_info with
                     ri_method = Ocsigen_http_frame.Http_header.GET;
                     ri_cookies= lazy ric;
                     ri_get_params =
                       lazy si.Eliom_common.si_other_get_params;
                     ri_post_params = Some (fun _ -> Lwt.return []);
                     ri_files = Some (fun _ -> Lwt.return []);
                   }
                 in
                 Eliommod_pagegen.update_cookie_table sitedata all_cookie_info
                 >>= fun () ->
                 send_directly ri (Ocsigen_extensions.compute_result
                                     ~previous_cookies:all_new_cookies ri)

               | _ ->
                 (* retry without POST params *)
(*VVV
  Warning: is it possible to have an Eliom service with POST method
  but no POST parameter?
  --> may loop...
  (we impose GET to prevent that)
*)
                 Polytables.set
                   ri.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache
                   Eliom_common.eliom_params_after_action
                   (si.Eliom_common.si_all_get_params,
                    si.Eliom_common.si_all_post_params,
                    si.Eliom_common.si_nl_get_params,
                    si.Eliom_common.si_nl_post_params,
                    si.Eliom_common.si_all_get_but_nl (*204FORMS*,
                    si.Eliom_common.si_internal_form *))
                 ;
                 let ri =
                   {ri.request_info with
                     ri_method = Ocsigen_http_frame.Http_header.GET;
                     ri_cookies= lazy ric;
                     ri_get_params =
                       lazy si.Eliom_common.si_other_get_params;
                     ri_post_params = Some (fun _ -> Lwt.return []);
                     ri_files = Some (fun _ -> Lwt.return []);
                   }
                 in
                 Eliommod_pagegen.update_cookie_table sitedata all_cookie_info
                 >>= fun () ->
                 send_directly ri (Ocsigen_extensions.compute_result
                                     ~previous_cookies:all_new_cookies ri))
      )

end

module Action = Eliom_mkreg.MakeRegister(Action_reg_base)

(** Unit services are like services, do not generate any page, and do not
    reload the page. To be used carefully. Probably not usefull at all.
    (Same as {!Eliom_output.Action} with [`NoReload] option).
 *)

module Unit_reg_base = struct

  type page = unit
  type options = unit
  type return = Eliom_services.http

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?(code = 204)
      ?content_type ?headers content =
    let open Ocsigen_http_frame in
    let empty_result = Ocsigen_http_frame.empty_result () in
    Lwt.return
      {empty_result with
         res_cookies= Eliom_request_info.get_user_cookies ();
         res_code= code;
         res_content_type= (match content_type with
                              | None -> empty_result.res_content_type
                              | _ -> content_type
                           );
         res_headers= (match headers with
                         | None -> empty_result.res_headers
                         | Some headers ->
                             Http_headers.with_defaults
                               headers empty_result.res_headers
                      );
      }

end

module Unit = Eliom_mkreg.MakeRegister(Unit_reg_base)

(* Any is a module allowing to register services that decide themselves
   what they want to send.
 *)
module Any_reg_base = struct

  type page = Ocsigen_http_frame.result
  type options = unit
  type return = Eliom_services.http

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers res =
    let open Ocsigen_http_frame in
    Lwt.return
      {res with
         res_cookies=
          Ocsigen_cookies.add_cookies
            (Eliom_request_info.get_user_cookies ())
            res.res_cookies;
         res_charset= (match charset with
                         | None -> res.res_charset
                         | _ -> charset);
         res_content_type= (match content_type with
                              | None -> res.res_content_type
                              | _ -> content_type
                           );
         res_headers= (match headers with
                         | None -> res.res_headers
                         | Some headers ->
                             Http_headers.with_defaults
                               headers res.res_headers
                      );
      }

end

module Any = Eliom_mkreg.MakeRegister(Any_reg_base)

(* Files is a module allowing to register services that send files *)
module Files_reg_base = struct

  type page = string
  type options = unit
  type return = Eliom_services.http

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers filename =
    let sp = Eliom_common.get_sp () in
    let request = Eliom_request_info.get_request_sp sp in
    let file =
      try Ocsigen_local_files.resolve request filename
      with
        | Ocsigen_local_files.Failed_403 (* XXXBY : maybe we should signal a true 403? *)
        | Ocsigen_local_files.Failed_404
        | Ocsigen_local_files.NotReadableDirectory ->
            raise Eliom_common.Eliom_404
    in
    Ocsigen_local_files.content ~request ~file >>= fun r ->
    let open Ocsigen_http_frame in
    let open Ocsigen_extensions in
    Lwt.return
      { r with
          res_cookies = Eliom_request_info.get_user_cookies ();
          res_code = code_of_code_option code;
          res_charset = (match charset with
                           | None ->
                               Some (Ocsigen_charset_mime.find_charset
                                       filename
                                       (Eliom_config.get_config_info_sp sp).charset_assoc)
                           | _ -> charset);
          res_content_type= (match content_type with
                               | None -> r.res_content_type
                               | _ -> content_type
                            );
          res_headers= (match headers with
                          | None -> r.res_headers
                          | Some headers ->
                              Http_headers.with_defaults
                                headers r.res_headers
                       );

      }

end

module Files = Eliom_mkreg.MakeRegister(Files_reg_base)

(****************************************************************************)
(****************************************************************************)

module Streamlist_reg_base = struct

  type page = (((unit -> (string Ocsigen_stream.t) Lwt.t) list) * string)
  type options = unit
  type return = Eliom_services.http

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers content =
    let open Ocsigen_http_frame in
    Ocsigen_senders.Streamlist_content.result_of_content content >>= fun r ->
    Lwt.return
      {r with
         res_cookies= Eliom_request_info.get_user_cookies ();
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None ->  Some (Eliom_config.get_config_default_charset ())
                         | _ -> charset);
         res_content_type= (match content_type with
                              | None -> r.res_content_type
                              | _ -> content_type
                           );
         res_headers= (match headers with
                         | None -> r.res_headers
                         | Some headers ->
                             Http_headers.with_defaults
                               headers r.res_headers
                      );
      }

end

module Streamlist = Eliom_mkreg.MakeRegister(Streamlist_reg_base)

(****************************************************************************)
(****************************************************************************)

module Customize
  (B : sig type options type return type page end)
  (R : "sigs/eliom_reg.mli" subst type options := B.options
			      and type return  := B.return
			      and type page    := B.page)
  (T : sig type page val translate : page -> B.page Lwt.t end) = struct

  let make_eh = function
    | None -> None
    | Some eh -> Some (fun l -> eh l >>= T.translate)

  let make_service_handler f g p = f g p >>= T.translate

  let send ?options ?charset ?code ?content_type ?headers content =
    T.translate content >>= fun c ->
    R.send ?options ?charset ?code ?content_type ?headers c

  let register
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ~service
      ?error_handler
      (f : ('get -> 'post -> 'return Lwt.t)) =
    R.register
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ~service
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let register_service
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?https
      ?priority
      ~path
      ~get_params
      ?error_handler
      f =
    R.register_service
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?https
      ?priority
      ~path
      ~get_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let register_coservice
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_state_name
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~fallback
      ~get_params
      ?error_handler
      f =
    R.register_coservice
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_state_name
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~fallback:(Eliom_services.untype_service_ fallback)
      ~get_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let register_coservice'
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_state_name
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~get_params
      ?error_handler
      f =
    R.register_coservice'
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_state_name
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~get_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let register_post_service
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?https
      ?priority
      ~fallback
      ~post_params
      ?error_handler
      f =
    R.register_post_service
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?https
      ?priority
      ~fallback:(Eliom_services.untype_service_ fallback)
      ~post_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let register_post_coservice
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_state_name
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~fallback
      ~post_params
      ?error_handler
      f =
    R.register_post_coservice
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_state_name
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~fallback:(Eliom_services.untype_service_ fallback)
      ~post_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let register_post_coservice'
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_state_name
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?keep_get_na_params
      ?https
      ~post_params
      ?error_handler
      f =
    R.register_post_coservice'
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_state_name
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?keep_get_na_params
      ?https
      ~post_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

end

(****************************************************************************)
(****************************************************************************)

module Caml_reg_base = struct

  type page = string
  type options = unit
  type return = Eliom_services.http

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers content =
    Text.send ?options ?charset ?code
      ?content_type ?headers
      (content,
       Eliom_services.eliom_appl_answer_content_type)

end

module Caml = struct

  module M = Eliom_mkreg.MakeRegister(Caml_reg_base)

  let make_eh = function
    | None -> None
    | Some eh ->
        Some (fun l ->
                eh l >>= fun r ->
                Lwt.return (Eliom_types.encode_eliom_data r))

  let make_service_handler f =
    fun g p ->
      f g p >>= fun r ->
      Lwt.return (Eliom_types.encode_eliom_data r)

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers content =
    M.send ?options ?charset ?code
      ?content_type ?headers (Eliom_types.encode_eliom_data content)

  let register
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ~(service : ('get, 'post,
                   [< internal_service_kind ],
                   [< suff ], 'gn, 'pn, [ `Registrable ],
                   'return Eliom_parameters.caml) service)
      ?(error_handler : ((string * exn) list -> 'return Lwt.t) option)
      (f : ('get -> 'post -> 'return Lwt.t)) =
    M.register
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ~service:(Eliom_services.untype_service_ service)
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)


  let register_service
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?https
      ?priority
      ~path
      ~get_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_service
                                      ?scope
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?state_name
                                      ?secure_session
                                      ?https
				      ?priority
                                      ~path
                                      ~get_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_coservice
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_state_name
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~fallback
      ~get_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_coservice
                                      ?scope
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?state_name
                                      ?secure_session
                                      ?name
                                      ?csrf_safe
                                      ?csrf_state_name
                                      ?csrf_scope
                                      ?csrf_secure
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~fallback:(Eliom_services.untype_service_ fallback)
                                      ~get_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_coservice'
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_state_name
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~get_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_coservice'
                                      ?scope
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?state_name
                                      ?secure_session
                                      ?name
                                      ?csrf_safe
                                      ?csrf_state_name
                                      ?csrf_scope
                                      ?csrf_secure
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~get_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))


  let register_post_service
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?https
      ?priority
      ~fallback
      ~post_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_post_service
                                      ?scope
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?state_name
                                      ?secure_session
                                      ?https
				      ?priority
                                      ~fallback:(Eliom_services.untype_service_ fallback)
                                      ~post_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_post_coservice
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_state_name
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~fallback
      ~post_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_post_coservice
                                      ?scope
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?state_name
                                      ?secure_session
                                      ?name
                                      ?csrf_safe
                                      ?csrf_state_name
                                      ?csrf_scope
                                      ?csrf_secure
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~fallback:(Eliom_services.untype_service_ fallback)
                                      ~post_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_post_coservice'
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?state_name
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_state_name
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?keep_get_na_params
      ?https
      ~post_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_post_coservice'
                                      ?scope
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?state_name
                                      ?secure_session
                                      ?name
                                      ?csrf_safe
                                      ?csrf_state_name
                                      ?csrf_scope
                                      ?csrf_secure
                                      ?max_use
                                      ?timeout
                                      ?keep_get_na_params
                                      ?https
                                      ~post_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))


end

(****************************************************************************)
(****************************************************************************)

type appl_service_params =
    {
      ap_title: string;
      ap_container : 'a.
        ((([< HTML5_types.common ] as 'a) HTML5.M.attrib list) option *
           (HTML5_types.body_content HTML5.M.elt ->
            HTML5_types.body_content HTML5.M.elt list))
        option;
      ap_body_attributes :
        'a. (([< HTML5_types.common ] as 'a) HTML5.M.attrib list) option;
      ap_headers_before : HTML5_types.head_content_fun HTML5.M.elt list;
      ap_headers_after : HTML5_types.head_content_fun HTML5.M.elt list
    }

type appl_service_options =
    {
      do_not_launch : bool; (** Do not launch the client side program
                                       if it is not already launched.
                                       Default: [false]. *)
    }

let default_appl_service_options = {do_not_launch = false; }

module type APPL_PARAMS = sig
     val application_name : string
     val params : appl_service_params
end

let default_appl_params =
  { ap_title = "Eliom application";
    ap_container = None;
    ap_body_attributes = None;
    ap_headers_before = [];
    ap_headers_after = [];
  }

let comet_service_key = Polytables.make_key ()

(*CPE* change_page_event
let change_current_page_key : ('a -> unit) Polytables.key =
  Polytables.make_key ()
*)

module Eliom_appl_reg_make_param
  (Xhtml_content
     : Ocsigen_http_frame.HTTP_CONTENT with type t = [ `Html ] HTML5.M.elt)
  (Appl_params : APPL_PARAMS) = struct

  open HTML5.M
  open HTML5_types

  type page = body_content elt list
  type options = appl_service_options
  type return = Eliom_services.appl_service

  let eliom_appl_state_name = "__eliom_appl_name"

  let get_tab_cook sp =
    Eliommod_cookies.compute_cookies_to_send
      sp.Eliom_common.sp_sitedata
      sp.Eliom_common.sp_tab_cookie_info
      sp.Eliom_common.sp_user_tab_cookies

  let create_page
      ~options ~sp cpi params cookies_to_send
      (*CPE* change_page_event *) content =
    let do_not_launch = options.do_not_launch
        (* ||
           (Ocsigen_cookies.length tab_cookies_to_send > 1)
        (* If there are cookies, we launch the application *)
           Actually, no, we trust options ...
           Because we must decide whether to launch
           the application or not before creating links and forms.
        *)
    in
    let body, container_node = match params.ap_container with
      | None -> let b = HTML5.M.body ?a:params.ap_body_attributes content in
                (b, (HTML5.M.toelt b))
      | Some (a, container) ->
        let d = HTML5.M.div ?a content in
        (HTML5.M.body
           ?a:params.ap_body_attributes
           (container d),
         (HTML5.M.toelt d))
    in
    ignore (Eliom_xml.make_node_id container_node); (* The ref must be created
						       for container before
						       calling make_ref_tree! *)

    HTML5.M.html
      (HTML5.M.head (HTML5.M.title (HTML5.M.pcdata params.ap_title))
         (
           params.ap_headers_before@
           HTML5.M.style
             [
               HTML5.M.pcdata
                 "\n.eliom_inline {display: inline}\n.eliom_nodisplay {display: none}\n"]::

             (* This will do a redirection if there is a #! in the URL *)
             HTML5.M.script
             (cdata_script
                ("// Redirect if the URL contains #! while loading the page
function redir () {
  var str_url = window.location.toString() ;
  try{
    var match = str_url.match(\"((https?://[^/]*).*)/[^#/?]*(\\\\?.*)?#!((https?://[^/]*/)?(/)?(.*))$\");
          //but what if there's a # the search ?
    if(match) {
      if(match[5]) { // full absolute
        window.location = match[4];
      }
      else
      if(match[6]) { // absolute path
        window.location = match[2] + match[4];
      }
      else { // relative
        window.location = match[1] + \"/\" + match[4] ;
      }
    }
  } catch(e) {} ;
};
redir ();"))::

	 let onload_form_creators =
	   Eliommod_cli.wrap (Eliom_services.get_onload_form_creators
                                Appl_params.application_name sp) in
	 let eliom_appl_page_data =
           Eliom_wrap.wrap (Eliommod_cli.get_eliom_appl_page_data_ sp)
         in
	 Eliom_xml.mark_sent (HTML5.M.toelt body);
	 let contents_to_send = Eliom_xml.contents_to_send () in

             if not do_not_launch
             then
               HTML5.M.script
                 (cdata_script
                    (
                      String.concat
                        ""
                        [
                          "var container_node = \'";
                          (let reqnum = Eliom_request_info.get_request_id_sp sp in
                           (Eliom_types.jsmarshal
                              (Eliom_types.to_data_key_
                                 (*(reqnum, XML.ref_node container_node))*)
                                 (reqnum, Eliom_xml.make_node_id container_node))
                           )) ; "\'; \n";

                          "var eliom_data = \'" ;
                          (Eliom_types.jsmarshal
                             ((Left
                                 (*(XML.make_ref_tree (HTML5.M.toelt body)),*)
                                 (Eliom_xml.make_ref_tree (HTML5.M.toelt body)),
                            (* Warning: due to right_to_left evaluation,
                               make_ref_tree is called before the previous
                               items. Do not create new node refs in
                               previous items!
                            *)
                               contents_to_send,
                               eliom_appl_page_data,
                               cookies_to_send,
                               onload_form_creators,
                               Eliom_services.get_onload sp,
                               Eliom_services.get_onunload sp,
                               Eliommod_cli.client_si sp.Eliom_common.sp_si
                              ) :
                                 Eliom_types.eliom_data_type
                             )
                          ) ; "\'; \n" ;

			  "var comet_service = \'" ;
                          (Eliom_types.jsmarshal
			     (Eliom_wrap.wrap
				(Polytables.get
                                   ~table:cpi.Eliom_common.cpi_references
				   ~key:comet_service_key)
                             )) ; "\'; \n" ;

(*CPE* change_page_event
                          "var change_page_event = \'" ;
                          (Eliom_client_types.jsmarshal
                             (Eliommod_react.Down.wrap
                                (Eliommod_react.Down.of_react change_page_event)
                             )
                          ) ; "\'; \n" ;
*)

                          "var sitedata = \'" ;
                          (Eliom_types.jsmarshal
                             (Eliommod_cli.client_sitedata sp)
                          ) ; "\'; \n"

                        ]

                    )
                 ) ::
               (* Javascript program: *)
               HTML5.M.script
                   ~a:[a_src (Xhtml.make_uri
                                ~service:(Eliom_services.static_dir ())
                                [Appl_params.application_name ^ ".js"])]
                 (pcdata "")::
                 params.ap_headers_after
             else params.ap_headers_before@params.ap_headers_after

         ))
      body

  let send_appl_content = Eliom_services.XSame_appl Appl_params.application_name


  let get_eliom_page_content ~options sp content =
    get_tab_cook sp >>= fun tab_cookies_to_send ->
    let onload_form_creators =
      Eliommod_cli.wrap (Eliom_services.get_onload_form_creators Appl_params.application_name sp) in
    let eliom_appl_page_data = (Eliom_wrap.wrap (Eliommod_cli.get_eliom_appl_page_data_ sp)) in
    List.iter (fun x -> Eliom_xml.mark_sent (HTML5.M.toelt x)) content;
    let contents_to_send = Eliom_xml.contents_to_send () in

(*VVV Here we do not send a stream *)
    Lwt.return
      ((Right
          (Eliom_xml.make_ref_tree_list (HTML5.M.toeltl content)),
	contents_to_send,
        eliom_appl_page_data,
        tab_cookies_to_send,
        onload_form_creators,
        Eliom_services.get_onload sp,
        Eliom_services.get_onunload sp,
        Eliommod_cli.client_si sp.Eliom_common.sp_si
       ),
     (*VVV Use another serialization format than XML for the page? *)
       let b = Buffer.create 512 in
       HTML5.P.print_list ~output:(Buffer.add_string b) content;
       Buffer.contents b)


  let send ?(options = default_appl_service_options) ?charset ?code
      ?content_type ?headers content =
    let sp = Eliom_common.get_sp () in
(*    let si = Eliom_request_info.get_si sp in *)
    let cpi = Lazy.force sp.Eliom_common.sp_client_process_info in
    let content_only =
      (* If the name of the application sent by the browser
         corresponds to the name of the application of the service,
         we send only the content of the page.
      *)
      sp.Eliom_common.sp_client_appl_name = Some Appl_params.application_name
    in
    (if content_only
     then begin
       let rc = Eliom_request_info.get_request_cache_sp sp in
       let url_to_display =
         "/"^
         try Polytables.get ~table:rc ~key:Eliom_mkreg.suffix_redir_uri_key
       (* If it is a suffix service with redirection, the uri has already been
          computed in rc *)
         with Not_found -> Eliom_request_info.get_full_url_sp sp
       (* Otherwise, the full url has already been recomputed
          without internal form info and taking "to_be_considered_as_get"
          into account*)
       in
       get_eliom_page_content ~options sp content >>= fun data ->
(*204FORMS* old implementation of forms with 204 and change_page_event

       if si.Eliom_common.si_internal_form
       then begin (* It was an internal form.
                     We want to change only the content.
                     But the browser is not doing an xhr.
                     We send 204 No Content
                     and use the change_page_event to update the content. *)
         let change_current_page = Polytables.get
	   ~table:cpi.Eliom_common.cpi_references ~key:change_current_page_key
         in
         change_current_page (Eliom_services.EAContent (data, url_to_display));
         Lwt.return (Ocsigen_http_frame.empty_result ())
       end
       else *)
       Caml.send (EAContent (data, url_to_display))
     end
     else begin
       (* We launch the client side process *)
       let comet_service = Eliom_comet.init () in
       Polytables.set
         ~table:cpi.Eliom_common.cpi_references
         ~key:comet_service_key
	 ~value:comet_service;
(*CPE* change_page_event
       let change_page_event, change_current_page =
         (* This event allows the server to ask the client to change
            current page content *)
         React.E.create ()
       in
       Polytables.set
         ~table:cpi.Eliom_common.cpi_references
	 ~key:change_current_page_key
	 ~value:change_current_page;
*)
       Eliom_state.set_cookie
         ~cookie_scope:`Client_process
         ~name:Eliom_common.appl_name_cookie_name
         ~value:Appl_params.application_name ();
       get_tab_cook sp >>= fun tab_cookies_to_send ->
(*VVV for now not possible to give other params for one page *)
       let page =
         create_page
           ~options ~sp cpi Appl_params.params tab_cookies_to_send
           (*CPE* change_page_event *) content
       in
       Xhtml_content.result_of_content page
       >>= fun r ->
        let open Ocsigen_http_frame in
        Lwt.return
          {r with
            res_cookies= Eliom_request_info.get_user_cookies ();
            res_code= code_of_code_option code;
            res_charset= (match charset with
              | None -> Some (Eliom_config.get_config_default_charset ())
              | _ -> charset
            );
            res_content_type= (match content_type with
              | None -> r.res_content_type
              | _ -> content_type
            );
            res_headers= (match headers with
              | None -> r.res_headers
              | Some headers ->
                Http_headers.with_defaults headers r.res_headers
            );
          }
     end
    )

end

module Eliom_appl (Appl_params : APPL_PARAMS) = struct

  module Eliom_appl_reg_param =
    Eliom_appl_reg_make_param
      (Ocsigen_senders.Make_XML_Content(XML)(HTML5.M))
      (Appl_params)

  module Eliom_appl_registration = Eliom_mkreg.MakeRegister(Eliom_appl_reg_param)

  include Eliom_appl_registration

  (** Unique identifier for this application.
      It is the application name.
      Warning: do not mix up with the "application instance id",
      that is unique for each instance of the application.
  *)
  let application_name = Appl_params.application_name

end

(*****************************************************************************)

(** Redirection services are like services, but send a redirection instead
 of a page.

   The HTTP/1.1 RFC says:
   If the 301 status code is received in response to a request other than GET or HEAD, the user agent MUST NOT automatically redirect the request unless it can be confirmed by the user, since this might change the conditions under which the request was issued.

   Here redirections are done towards services without parameters.
   (possibly preapplied).

 *)
module String_redir_reg_base = struct

  type page = HTML5.M.uri
  type options = [ `Temporary | `Permanent ]
  type return = Eliom_services.http

  let send_appl_content = Eliom_services.XAlways
  (* actually, the service will decide itself *)

  let send ?(options = `Permanent) ?charset ?code
      ?content_type ?headers content =
    let uri = Uri.string_of_uri content in
    let empty_result = Ocsigen_http_frame.empty_result () in
    let cookies = Eliom_request_info.get_user_cookies () in
    let content_type = match content_type with
      | None -> empty_result.Ocsigen_http_frame.res_content_type
      | _ -> content_type
    in
    let headers = match headers with
      | None -> empty_result.Ocsigen_http_frame.res_headers
      | Some headers ->
        Http_headers.with_defaults
          headers empty_result.Ocsigen_http_frame.res_headers
    in

    (* Now we decide the kind of redirection we do.
       If the request is an xhr done by a client side Eliom program,
       we do not send an HTTP redirection.
       In that case, we send a full xhr redirection.
       If the application to which belongs the destination service is the same,
       then it is ok, otherwise, there will be another redirection ...
    *)
    match Eliom_request_info.get_sp_client_appl_name () with
        (* the appl name as sent by browser *)
      | None -> (* the browser did not ask application eliom data,
                   we send a regular redirection *)
        let code = match code with
          | Some c -> c
          | None ->
            if options = `Temporary
            then 307 (* Temporary move *)
            else 301 (* Moved permanently *)
        in
        Lwt.return
          {empty_result with
            Ocsigen_http_frame.res_cookies= cookies;
            res_code= code;
            res_location = Some uri;
            res_content_type= content_type;
            res_headers= headers;
          }
      | _ ->
        Lwt.return
          {empty_result with
            Ocsigen_http_frame.res_cookies= cookies;
            res_content_type= content_type;
            res_headers=
              Http_headers.add
                (Http_headers.name Eliom_common.full_xhr_redir_header)
                uri headers
          }


end

module String_redirection = Eliom_mkreg.MakeRegister(String_redir_reg_base)

module Redir_reg_base = struct

  type page =
      (unit, unit, Eliom_services.get_service_kind,
       [ `WithoutSuffix ],
       unit, unit, Eliom_services.registrable, Eliom_services.http)
        Eliom_services.service

  type options = [ `Temporary | `Permanent ]

  type return = Eliom_services.http

  let send_appl_content = Eliom_services.XAlways
  (* actually, the service will decide itself *)

  let send ?(options = `Permanent) ?charset ?code
      ?content_type ?headers service =
    let uri = lazy (Xhtml.make_string_uri ~absolute:true ~service ()) in
    let empty_result = Ocsigen_http_frame.empty_result () in
    let cookies = Eliom_request_info.get_user_cookies () in
    let content_type = match content_type with
      | None -> empty_result.Ocsigen_http_frame.res_content_type
      | _ -> content_type
    in
    let headers = match headers with
      | None -> empty_result.Ocsigen_http_frame.res_headers
      | Some headers ->
        Http_headers.with_defaults
          headers empty_result.Ocsigen_http_frame.res_headers
    in

    (* Now we decide the kind of redirection we do.
       If the request is an xhr done by a client side Eliom program,
       we do not send an HTTP redirection.
       In that case, we send:
       - a full xhr redirection if the application to which belongs
       the destination service is the same (thus it will send back tab cookies)
       - a half xhr redirection otherwise
    *)
    match Eliom_request_info.get_sp_client_appl_name () with
      (* the appl name as sent by browser *)
      | None -> (* the browser did not ask application eliom data,
                   we send a regular redirection *)
        let code = match code with
          | Some c -> c
          | None ->
            if options = `Temporary
            then 307 (* Temporary move *)
            else 301 (* Moved permanently *)
        in
        Lwt.return
          {empty_result with
            Ocsigen_http_frame.res_cookies= cookies;
            res_code= code;
            res_location = Some (Lazy.force uri);
            res_content_type= content_type;
            res_headers= headers; }

      | Some anr ->
        (* the browser asked application eliom data
           for the application called anr *)
(*204FORMS* old implementation of forms with 204 and change_page_event
        let sp = Eliom_common.get_sp () in
        let si = Eliom_request_info.get_si sp in
        if si.Eliom_common.si_internal_form
        then begin
        (* If it comes from a form, we use the change_page_event *)
          let cpi = Lazy.force sp.Eliom_common.sp_client_process_info in
          let change_current_page = Polytables.get
	    ~table:cpi.Eliom_common.cpi_references ~key:change_current_page_key
          in
          (match Eliom_services.get_send_appl_content service with
            (* the appl name of the destination service *)
            | Eliom_services.XSame_appl an when (an = anr) ->
            (* Same appl, we do a full xhr redirection
               (not an http redirection, because we want to
               send back tab cookies) *)
              change_current_page (Eliom_services.EAFullRedir
                                     (Eliom_services.pre_wrap service))

            | Eliom_services.XAlways ->
            (* It is probably an action, or a void coservice. Full xhr again *)
              change_current_page (Eliom_services.EAFullRedir
                                     (Eliom_services.pre_wrap service))
            | _ -> (* No application, or another application.
                      We ask the browser to do an HTTP redirection. *)
              change_current_page
                (Eliom_services.EAHalfRedir (Lazy.force uri)));
          Lwt.return (Ocsigen_http_frame.empty_result ())
        end
        else
*)
        (* If it comes from an xhr, we use answer with a special header field *)
          match Eliom_services.get_send_appl_content service with
          (* the appl name of the destination service *)
            | Eliom_services.XSame_appl an when (an = anr) ->
            (* Same appl, we do a full xhr redirection
               (not an http redirection, because we want to
               send back tab cookies) *)
              Lwt.return
                {empty_result with
                  Ocsigen_http_frame.res_cookies= cookies;
                  res_content_type= content_type;
                  res_headers=
                    Http_headers.add
                      (Http_headers.name Eliom_common.full_xhr_redir_header)
                      (Lazy.force uri) headers
                }

            | Eliom_services.XAlways ->
            (* It is probably an action, or a void coservice. Full xhr again *)
              Lwt.return
                {empty_result with
                  Ocsigen_http_frame.res_cookies= cookies;
                  res_content_type= content_type;
                  res_headers=
                    Http_headers.add
                      (Http_headers.name Eliom_common.full_xhr_redir_header)
                      (Lazy.force uri) headers
                }

            | _ -> (* No application, or another application.
                      We ask the browser to do an HTTP redirection. *)
              Lwt.return
                {empty_result with
                  Ocsigen_http_frame.res_cookies= cookies;
                  res_content_type= content_type;
                  res_headers=
                    Http_headers.add
                      (Http_headers.name Eliom_common.half_xhr_redir_header)
                      (Lazy.force uri) headers
                }


end


module Redirection = Eliom_mkreg.MakeRegister(Redir_reg_base)

(*****************************************************************************)
