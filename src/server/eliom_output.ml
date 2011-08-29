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

include Eliom_output_base

(******************************************************************************)
(* Send return types                                                          *)
(******************************************************************************)

module Result_types :
sig
  type ('a, 'b) kind
  val cast_result : Ocsigen_http_frame.result -> ('a, 'b) kind
  val cast_kind : ('a, 'b) kind -> Ocsigen_http_frame.result
  val cast_kind_lwt : ('a, 'b) kind Lwt.t -> Ocsigen_http_frame.result Lwt.t
  val cast_result_lwt : Ocsigen_http_frame.result Lwt.t -> ('a, 'b) kind Lwt.t
  val cast_function_kind : ('c -> ('a, 'b) kind Lwt.t) -> ('c -> ('d, 'e) kind Lwt.t)
  val cast_function_http : ('c -> ('a, 'b) kind Lwt.t) -> ('c -> Ocsigen_http_frame.result Lwt.t)
end
=
struct
  type ('a, 'b) kind = Ocsigen_http_frame.result
  let cast_result x = x
  let cast_kind x = x
  let cast_kind_lwt x = x
  let cast_result_lwt x = x
  let cast_function_kind x = x
  let cast_function_http x = x
end

type ('a, 'b) kind = ('a, 'b) Result_types.kind
type 'a application_content = [`Appl of 'a]
type block_content
type browser_content = [`Browser]
type 'a caml_content
type unknown_content

let cast_unknown_content_kind (x:(unknown_content, http_service) kind) : ('a, 'b) kind =
  Result_types.cast_result ( Result_types.cast_kind x)
let cast_http_result = Result_types.cast_result

(******************************************************************************)
(******************************************************************************)

module Html5_make_reg_base
  (Html5_content : Ocsigen_http_frame.HTTP_CONTENT
                   with type t = HTML5_types.html HTML5.M.elt) = struct

  open HTML5.M
  open HTML5_types

  type page = xhtml elt

  type options = unit

  type return = http_service

  type result = (browser_content, http_service) kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_services.XNever

  let send ?(options = ()) ?charset ?code
      ?content_type ?headers content =
    lwt r = Html5_content.result_of_content content in
    let open Ocsigen_http_frame in
    Lwt.return
      {r with
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

  let make_a ?(a=[]) ?href l : 'a a_elt =
    let a = match href with
      | None -> a
      | Some v -> lazy_a_href v :: a
    in
    XHTML.M.a ~a l

  let make_empty_form_content () = p [pcdata ""] (**** à revoir !!!!! *)
  let remove_first = function
    | a::l -> a,l
    | [] -> (make_empty_form_content ()), []

  let make_get_form ?(a=[]) ~action elts : form elt =
    let elts = Eliom_lazy.from_fun (fun () -> remove_first (Eliom_lazy.force elts)) in
    let elt1 = Eliom_lazy.from_fun (fun () -> fst (Eliom_lazy.force elts))
    and elts = Eliom_lazy.from_fun (fun () -> snd (Eliom_lazy.force elts)) in
    let r =
      lazy_form ~a:((a_method `Get)::a) ~action:action elt1 elts
    in
    r

  let make_post_form ?(a=[]) ~action ?id ?(inline = false) elts
      : form elt =
    let aa = (match id with
    | None -> a
    | Some i -> (a_id i)::a)
    in
    let elts = Eliom_lazy.from_fun (fun () -> remove_first (Eliom_lazy.force elts)) in
    let elt1 = Eliom_lazy.from_fun (fun () -> fst (Eliom_lazy.force elts))
    and elts = Eliom_lazy.from_fun (fun () -> snd (Eliom_lazy.force elts)) in
    let r =
      lazy_form ~a:((XHTML.M.a_enctype "multipart/form-data")::
             (* Always Multipart!!! How to test if there is a file?? *)
                  (a_method `Post)::
                  (if inline then (a_class ["inline"])::aa else aa))
        ~action:action elt1 elts
    in
    r

  let make_hidden_field content =
    let c = match content with
      | None -> []
      | Some c -> [c]
    in
    (div ~a:[a_class ["eliom_nodisplay"]] c :> form_content elt)


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

end

(*****************************************************************************)
(*****************************************************************************)

module Xhtml_forms = struct

  open XHTML.M
  open XHTML_types

  module Xhtml_forms_closed = Eliom_mkforms.MakeForms(Xhtml_forms_base)

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
  type return = http_service
  type result = (browser_content, http_service) kind
  type options = unit

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code ?content_type ?headers content =
    lwt r = Xhtml_content.result_of_content content in
    Lwt.return
      {r with
         Ocsigen_http_frame.
         res_code    = code_of_code_option code;
         res_charset = (match charset with
           | None -> Some (Eliom_config.get_config_default_charset ())
           | _ -> charset);
         res_content_type = (match content_type with
           | None -> r.Ocsigen_http_frame.res_content_type
           | _ -> content_type
         );
         res_headers = (match headers with
           | None -> r.Ocsigen_http_frame.res_headers
           | Some headers ->
             Http_headers.with_defaults headers r.Ocsigen_http_frame.res_headers
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

      type return = http_service

      type result = (block_content, http_service) kind

      let result_of_http_result = Result_types.cast_result

      let send_appl_content = Eliom_services.XNever

      let send ?options ?charset ?code ?content_type ?headers content =
        lwt r = Cont_content.result_of_content content in
        Lwt.return
          { r with
	    Ocsigen_http_frame.
            res_code    = code_of_code_option code;
            res_charset = (match charset with
              | None -> Some (Eliom_config.get_config_default_charset ())
              | _ -> charset);
            res_content_type = (match content_type with
	      | None -> r.Ocsigen_http_frame.res_content_type
	      | _ -> content_type
            );
            res_headers = (match headers with
              | None -> r.Ocsigen_http_frame.res_headers
              | Some headers ->
                Http_headers.with_defaults
		  headers r.Ocsigen_http_frame.res_headers
            );
          }

    end

    include Eliom_mkreg.MakeRegister(Cont_reg_base)

  end

module Blocks = Make_TypedXML_Registration(XML)(XHTML.M)(struct
  type content = XHTML_types.body_content
end)

module Blocks5 = Make_TypedXML_Registration(XML)(HTML5.M)(struct
  type content = HTML5_types.body_content
end)

(****************************************************************************)
(****************************************************************************)

module Text_reg_base = struct

  type page = (string * string)

  type options = unit

  type return = http_service

  type result = (unknown_content, http_service) kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code ?content_type ?headers content =
    lwt r = Ocsigen_senders.Text_content.result_of_content content in
    Lwt.return
      { r with
        Ocsigen_http_frame.
        res_code    = code_of_code_option code;
        res_charset = (match charset with
          | None ->  Some (Eliom_config.get_config_default_charset ())
          | _ -> charset);
        res_content_type = (match content_type with
          | None -> r.Ocsigen_http_frame.res_content_type
          | _ -> content_type
        );
        res_headers = (match headers with
          | None -> r.Ocsigen_http_frame.res_headers
          | Some headers ->
            Http_headers.with_defaults headers r.Ocsigen_http_frame.res_headers
        );
      }

end

module Text = Eliom_mkreg.MakeRegister(Text_reg_base)

(****************************************************************************)
(****************************************************************************)

module CssText_reg_base = struct

  type page = string

  type options = unit

  type return = http_service

  type result = (browser_content, http_service) kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code ?content_type ?headers content =
    lwt r =
      Ocsigen_senders.Text_content.result_of_content (content, "text/css") in
    Lwt.return
      { r with
        Ocsigen_http_frame.
        res_code    = code_of_code_option code;
        res_charset = (match charset with
          | None -> Some (Eliom_config.get_config_default_charset ())
          | _ -> charset);
        res_content_type = (match content_type with
          | None -> r.Ocsigen_http_frame.res_content_type
          | _ -> content_type
        );
        res_headers = (match headers with
          | None -> r.Ocsigen_http_frame.res_headers
          | Some headers ->
            Http_headers.with_defaults headers r.Ocsigen_http_frame.res_headers
        );
      }

end

module CssText = Eliom_mkreg.MakeRegister(CssText_reg_base)

(****************************************************************************)
(****************************************************************************)

module HtmlText_reg_base = struct

  type page = string

  type options = unit

  type return = http_service

  type result = (browser_content, http_service) kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code ?content_type ?headers content =
    lwt r =
      Ocsigen_senders.Text_content.result_of_content (content, "text/html") in
    Lwt.return
      { r with
        Ocsigen_http_frame.
        res_code    = code_of_code_option code;
        res_charset = (match charset with
          | None -> Some (Eliom_config.get_config_default_charset ())
          | _ -> charset);
        res_content_type = (match content_type with
          | None -> r.Ocsigen_http_frame.res_content_type
          | _ -> content_type
        );
        res_headers = (match headers with
          | None -> r.Ocsigen_http_frame.res_headers
          | Some headers ->
            Http_headers.with_defaults headers r.Ocsigen_http_frame.res_headers
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

  let make_a ?(a="") ?href l : 'a a_elt =
    let a = match href with
      | None -> a
      | Some v -> " href=\""^Eliom_lazy.force v^"\" "^a
    in
    "<a "^a^">"^(* List.fold_left (^) "" l *) l^"</a>"

  let make_get_form ?(a="") ~action elts : form_elt =
    "<form method=\"get\" action=\""^ Eliom_lazy.force action ^"\""^a^">"^
    Eliom_lazy.force elts^"</form>"

  let make_post_form ?(a="") ~action ?id ?(inline = false) elts
      : form_elt =
    let aa = "enctype=\"multipart/form-data\" "
        (* Always Multipart!!! How to test if there is a file?? *)
      ^(match id with
        None -> a
      | Some i -> " id="^i^" "^a)
    in
    "<form method=\"post\" action=\""^ Eliom_lazy.force action ^"\""^
    (if inline then "style=\"display: inline\"" else "")^aa^">"^
    Eliom_lazy.force elts^"</form>"

  let make_hidden_field content =
    let content = match content with
      | None -> ""
      | Some c -> c
    in
    "<div style=\"display: none\""^content^"</div>"

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

end

module HtmlText_registration = Eliom_mkreg.MakeRegister(HtmlText_reg_base)
module HtmlText_forms = Eliom_mkforms.MakeForms(HtmlText_forms_base)

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

  type return = http_service

  type result = (browser_content, http_service) kind

  let result_of_http_result = Result_types.cast_result

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
      match (si.Eliom_common.si_nonatt_info,
		       si.Eliom_common.si_state_info,
		       ri.request_info.ri_method) with
        | (Eliom_common.RNa_no,
           (Eliom_common.RAtt_no, Eliom_common.RAtt_no),
           Ocsigen_http_frame.Http_header.GET) ->
          let empty_result = Ocsigen_http_frame.empty_result () in
          Lwt.return empty_result
        | _ ->
          let all_cookie_info = sp.Eliom_common.sp_cookie_info in
          lwt ric = Eliommod_cookies.compute_new_ri_cookies
            (Unix.time ())
            ri.request_info.ri_sub_path
            (Lazy.force ri.request_info.ri_cookies)
            all_cookie_info
            user_cookies
	  in
	  lwt all_new_cookies =
	    Eliommod_cookies.compute_cookies_to_send
	      sitedata
              all_cookie_info
              user_cookies in

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
	  match (si.Eliom_common.si_nonatt_info,
		 si.Eliom_common.si_state_info,
		 ri.request_info.ri_method) with
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
		 si.Eliom_common.si_all_get_but_nl);
		(*VVV Also put all_cookie_info in this,
		  to avoid update_cookie_table and get_cookie_info (?)
		*)
		let ri =
		  { ri.request_info with
                    ri_cookies= lazy ric;
                    ri_get_params =
                      lazy si.Eliom_common.si_other_get_params;
		  (* Here we modify ri,
                     thus the request can be taken by other extensions,
                     with its new parameters *)
		  }
		in
		lwt () = Eliommod_pagegen.update_cookie_table sitedata all_cookie_info in
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
		 si.Eliom_common.si_all_get_but_nl);
              let ri =
		{ ri.request_info with
		  ri_method = Ocsigen_http_frame.Http_header.GET;
		  ri_cookies= lazy ric;
		  ri_get_params =
                    lazy si.Eliom_common.si_other_get_params;
		  ri_post_params = Some (fun _ -> Lwt.return []);
		  ri_files = Some (fun _ -> Lwt.return []);
		}
              in
              lwt () = Eliommod_pagegen.update_cookie_table sitedata all_cookie_info in
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
		 si.Eliom_common.si_all_get_but_nl);
              let ri =
		{ ri.request_info with
		  ri_method = Ocsigen_http_frame.Http_header.GET;
		  ri_cookies= lazy ric;
		  ri_get_params =
                    lazy si.Eliom_common.si_other_get_params;
		  ri_post_params = Some (fun _ -> Lwt.return []);
		  ri_files = Some (fun _ -> Lwt.return []);
		}
              in
              lwt () =
		Eliommod_pagegen.update_cookie_table sitedata all_cookie_info in
              send_directly ri (Ocsigen_extensions.compute_result
				  ~previous_cookies:all_new_cookies ri)

end

module Action = Eliom_mkreg.MakeRegister(Action_reg_base)

(** Unit services are like services, do not generate any page, and do not
    reload the page. To be used carefully. Probably not usefull at all.
    (Same as {!Action} with [`NoReload] option).
 *)

module Unit_reg_base = struct

  type page = unit
  type options = unit
  type return = http_service
  type result = (browser_content, http_service) kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?(code = 204)
      ?content_type ?headers content =
    let open Ocsigen_http_frame in
    let empty_result = Ocsigen_http_frame.empty_result () in
    Lwt.return
      {empty_result with
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

(*****************************************************************************)
(*****************************************************************************)

(* Any is a module allowing to register services that decide themselves
   what they want to send.
 *)
module Any_reg_base = struct

  type ('a, 'b) page = ('a, 'b) kind
  type options = unit
  type 'a return = 'a
  type ('a, 'b) result = ('a, 'b) kind

  let result_of_http_result = Result_types.cast_result

(*  let send_appl_content = Eliom_services.XNever *)
  let send_appl_content = Eliom_services.XAlways

  let send ?options ?charset ?code
      ?content_type ?headers (res:('a, 'b) kind) =
    let res = Result_types.cast_kind res in
    let open Ocsigen_http_frame in
    Lwt.return
      {res with
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

module Any = Eliom_mkreg.MakeRegister_AlphaReturn(Any_reg_base)

type 'a application_name = string

let appl_self_redirect send page =
  let open Ocsigen_http_frame in
      if Eliom_request_info.expecting_process_page ()
      then
        let url = Eliom_request_info.get_full_url () in
        let empty_result = empty_result () in
        Lwt.return
          (Result_types.cast_result {empty_result with
            res_headers=
              Http_headers.add
                (Http_headers.name Eliom_common.half_xhr_redir_header) url
                empty_result.res_headers})
      else
        lwt r = (Result_types.cast_function_http send) page in
        Lwt.return (Result_types.cast_result
		{r with res_headers = Http_headers.with_defaults
		    Http_headers.dyn_headers r.res_headers})

let http_redirect = appl_self_redirect

(*****************************************************************************)
(*****************************************************************************)

(* Files is a module allowing to register services that send files *)
module Files_reg_base = struct

  type page = string
  type options = unit
  type return = http_service
  type result = (browser_content, http_service) kind

  let result_of_http_result = Result_types.cast_result

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

module Files =
struct
  include Eliom_mkreg.MakeRegister(Files_reg_base)
  let check_file filename =
    let sp = Eliom_common.get_sp () in
    let request = Eliom_request_info.get_request_sp sp in
    try
      ignore (Ocsigen_local_files.resolve request filename:Ocsigen_local_files.resolved);
      true
    with
      | Ocsigen_local_files.Failed_403
      | Ocsigen_local_files.Failed_404
      | Ocsigen_local_files.NotReadableDirectory ->
        false
end

(****************************************************************************)
(****************************************************************************)

module Streamlist_reg_base = struct

  type page = (((unit -> (string Ocsigen_stream.t) Lwt.t) list) * string)
  type options = unit
  type return = http_service
  type result = (unknown_content, http_service) kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers content =
    let open Ocsigen_http_frame in
    Ocsigen_senders.Streamlist_content.result_of_content content >>= fun r ->
    Lwt.return
      {r with
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
  (B : sig type options type return type page type result end)
  (R : "sigs/eliom_reg.mli" subst type options := B.options
			      and type return  := B.return
			      and type page    := B.page
                              and type result  := B.result)
  (T : sig type page val translate : page -> B.page Lwt.t end) = struct

    type page = T.page
    type return = B.return
    type options = B.options
    type result = B.result

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
      ?secure_session
      ?name
      ?csrf_safe
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
      ?secure_session
      ?name
      ?csrf_safe
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
      ?secure_session
      ?name
      ?csrf_safe
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
      ?secure_session
      ?name
      ?csrf_safe
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
      ?secure_session
      ?name
      ?csrf_safe
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
      ?secure_session
      ?name
      ?csrf_safe
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
      ?secure_session
      ?name
      ?csrf_safe
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
      ?secure_session
      ?name
      ?csrf_safe
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
  type return = http_service
  type result = Ocsigen_http_frame.result

  let result_of_http_result x = x

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers content =
    Result_types.cast_kind_lwt
      (Text.send ?options ?charset ?code
	 ?content_type ?headers
	 (content,
	  Eliom_services.eliom_appl_answer_content_type))

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

  let send ?options ?charset ?code ?content_type ?headers content =
    Result_types.cast_result_lwt
      (M.send ?options ?charset ?code
	 ?content_type ?headers (Eliom_types.encode_eliom_data content))

  let register
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
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
      ?secure_session
      ?name
      ?csrf_safe
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
                                      ?secure_session
                                      ?name
                                      ?csrf_safe
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
      ?secure_session
      ?name
      ?csrf_safe
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
                                      ?secure_session
                                      ?name
                                      ?csrf_safe
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
      ?secure_session
      ?name
      ?csrf_safe
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
                                      ?secure_session
                                      ?name
                                      ?csrf_safe
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
      ?secure_session
      ?name
      ?csrf_safe
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
                                      ?secure_session
                                      ?name
                                      ?csrf_safe
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

type appl_service_options =
    {
      do_not_launch : bool; (** Do not launch the client side program
                                       if it is not already launched.
                                       Default: [false]. *)
    }

let default_appl_service_options = {do_not_launch = false; }

module type APPL_PARAMS = sig
     val application_name : string
end

let comet_service_key = Polytables.make_key ()

let redirection_script =
  (* This will do a redirection if there is a #! in the URL *)
  let script =
    "// Redirect if the URL contains #! while loading the page\n"
    ^ "function redir () {\n"
    ^ "  var str_url = window.location.toString() ;\n"
    ^ "  try{\n"
    ^ "    var match = str_url.match(\"((https?://[^/]*).*)/[^#/?]*(\\\\?.*)?#!((https?://[^/]*/)?(/)?(.*))$\");\n"
    ^ "          //but what if there's a # the search ?\n"
    ^ "    if(match) {\n"
    ^ "      if(match[5]) { // full absolute\n"
    ^ "        window.location = match[4];\n"
    ^ "      }\n"
    ^ "      else\n"
    ^ "      if(match[6]) { // absolute path\n"
    ^ "        window.location = match[2] + match[4];\n"
    ^ "      }\n"
    ^ "      else { // relative\n"
    ^ "        window.location = match[1] + \"/\" + match[4] ;\n"
    ^ "      }\n"
    ^ "    }\n"
    ^ "  } catch(e) {} ;\n"
    ^ "};\n"
    ^ "redir ();" in
  HTML5.M.unique (HTML5.M.script (HTML5.M.cdata_script script))

module Eliom_appl_reg_make_param
  (Html5_content
     : Ocsigen_http_frame.HTTP_CONTENT with type t = [ `Html ] HTML5.M.elt)
  (Appl_params : APPL_PARAMS) = struct

  open HTML5.M
  open HTML5_types

  type appl

  type page = html elt
  type options = appl_service_options
  type return = appl_service
  type result = (appl application_content, appl_service) kind

  let result_of_http_result = Result_types.cast_result

  let eliom_appl_script =
    HTML5.M.unique (HTML5.M.script (HTML5.M.pcdata ""))
  let application_script () =
    HTML5.M.unique
      ~copy:eliom_appl_script
      ( HTML5.M.script
	  ~a:[HTML5.M.a_src (Xhtml.make_uri
			       ~service:(Eliom_services.static_dir ())
			       [Appl_params.application_name ^ ".js"])]
	  (HTML5.M.pcdata "") )
  let is_eliom_appl_script elt =
    XML.get_unique_id (HTML5.M.toelt elt)
    =
    XML.get_unique_id (HTML5.M.toelt eliom_appl_script)

  let eliom_fake_appl_data_script =
    HTML5.M.unique (HTML5.M.script (HTML5.M.pcdata ""))

  let make_eliom_appl_data_script ~sp =

    let script =
      Printf.sprintf
	("var eliom_appl_sitedata = \'%s\';\n"
 	 ^^ "var eliom_appl_process_info = \'%s\'\n"
	 ^^ "var eliom_request_data;\n"
	 ^^ "var eliom_request_cookies;\n"
	 ^^ "var eliom_request_url;\n")
	(Eliom_types.jsmarshal (Eliommod_cli.client_sitedata sp))
	(Eliom_types.jsmarshal (sp.Eliom_common.sp_client_process_info))
    in

    Lwt.return
      (HTML5.M.unique ~copy:eliom_fake_appl_data_script
	 (HTML5.M.script (cdata_script script)))

  let eliom_fake_request_data_script =
    HTML5.M.unique (HTML5.M.script (HTML5.M.pcdata ""))

  let make_eliom_data_script ~sp page =

    let rc = Eliom_request_info.get_request_cache_sp sp in
    let url_to_display =
      "/"
      ^ try Polytables.get ~table:rc ~key:Eliom_mkreg.suffix_redir_uri_key
	(* If it is a suffix service with redirection, the uri has already been
           computed in rc *)
	with Not_found -> Eliom_request_info.get_full_url_sp sp
      (* Otherwise, the full url has already been recomputed
         without internal form info and taking "to_be_considered_as_get"
         into account *)
    in

    (* wrapping of values could create eliom references that may
       create cookies that needs to be sent along the page. Hence,
       cookies should be calculated after wrapping. *)
    let eliom_data =
      Eliom_wrap.wrap
	{ Eliom_types.
	  ejs_ref_tree    = XML.make_ref_tree (HTML5.M.toelt page);
	  ejs_onload      = Eliom_services.get_onload sp;
	  ejs_onunload    = Eliom_services.get_onunload sp;
	  ejs_sess_info   = Eliommod_cli.client_si sp.Eliom_common.sp_si;
	} in

    lwt tab_cookies =
      Eliommod_cookies.compute_cookies_to_send
	sp.Eliom_common.sp_sitedata
	sp.Eliom_common.sp_tab_cookie_info
	sp.Eliom_common.sp_user_tab_cookies
    in

    let script =
      Printf.sprintf
	("eliom_request_data = \'%s\';\n"
	 ^^ "eliom_request_cookies = \'%s\';\n"
	 ^^ "eliom_request_url = \'%s\';\n")
	(Eliom_types.jsmarshal eliom_data)
	(Eliom_types.jsmarshal tab_cookies)
	(Eliom_types.jsmarshal url_to_display)
    in

    Lwt.return
      (HTML5.M.unique ~copy:eliom_fake_request_data_script
	 (HTML5.M.script (cdata_script script)))

  let split_page page :
      ( HTML5_types.html_attrib HTML5.M.attrib list
	* ( HTML5_types.head_attrib HTML5.M.attrib list
	    * [ HTML5_types.title ] HTML5.M.elt
	    * HTML5_types.head_content_fun HTML5.M.elt list )
	* HTML5_types.body HTML5.M.elt ) =
    match XML.content page with
      | XML.Node (_, html_attribs, [head; body]) ->
	begin match XML.content head with
	  | XML.Node (_, head_attribs, head_elts) ->
	    ( List.map HTML5.M.to_attrib html_attribs,
	      ( List.map HTML5.M.to_attrib head_attribs,
		HTML5.M.tot (List.hd head_elts),
		HTML5.M.totl (List.tl head_elts) ),
	      HTML5.M.tot body )
	  | _ -> assert false
	end
      | _ -> assert false

  let add_eliom_scripts ~sp page =

    lwt appl_data_script = make_eliom_appl_data_script ~sp in

    (* First we build a fake page to build the ref_tree... *)
    let	( html_attribs, (head_attribs, title, head_elts), body ) =
      split_page (HTML5.M.toelt page) in
    let head_elts =
      appl_data_script
      :: eliom_fake_request_data_script
      :: redirection_script
      :: ( if List.exists is_eliom_appl_script head_elts
           then head_elts
	   else ( head_elts
		  @ [application_script ()] ) )
    in
    let fake_page =
      HTML5.M.html ~a:html_attribs
	(HTML5.M.head ~a:head_attribs title head_elts)
	body in
    lwt data_script = make_eliom_data_script ~sp fake_page in

    (* Then we replace the faked data_script *)
    let head_elts =
      List.hd head_elts :: data_script :: List.tl (List.tl head_elts) in
    Lwt.return
      (HTML5.M.html ~a:html_attribs
	 (HTML5.M.head ~a:head_attribs title head_elts)
	 body )

  let remove_eliom_scripts page =
    let ( html_attribs, (head_attribs, title, head_elts), body ) =
      split_page (HTML5.M.toelt page) in
    let head_elts = List.filter (fun x -> not (is_eliom_appl_script x)) head_elts in
    Lwt.return
      (HTML5.M.html ~a:html_attribs
         (HTML5.M.head ~a:head_attribs title head_elts)
         body )

  let send_appl_content = Eliom_services.XSame_appl Appl_params.application_name

  let send ?(options = default_appl_service_options) ?charset ?code
      ?content_type ?headers content =

    let sp = Eliom_common.get_sp () in

    (* GRGR FIXME et si le nom de l'application diffère ?? Il faut
       renvoyer un full_redirect... TODO *)
    if sp.Eliom_common.sp_client_appl_name <> Some Appl_params.application_name then

      Eliom_state.set_cookie
        ~cookie_scope:`Client_process
        ~name:Eliom_common.appl_name_cookie_name
        ~value:Appl_params.application_name ();

    lwt page =
      match sp.Eliom_common.sp_client_appl_name, options.do_not_launch with
	| None, true -> remove_eliom_scripts content
	| _ -> add_eliom_scripts ~sp content in

    lwt r = Html5_content.result_of_content page in

    let headers =
      match headers with
        | None -> r.Ocsigen_http_frame.res_headers
        | Some headers ->
          Http_headers.with_defaults headers r.Ocsigen_http_frame.res_headers
    in
    let headers = Http_headers.replace
      (Http_headers.name Eliom_common_base.appl_name_header_name)
      Appl_params.application_name
      headers
    in

    let content_type =
      match content_type with
	| None ->
	  let header = Lazy.force sp.Eliom_common.sp_request.
	    Ocsigen_extensions.request_info.Ocsigen_extensions.ri_accept in
	  if List.exists
	    (function
	      | ((Some "text", Some "xml"),_,_) -> true
	      | _ -> false) header
	  then Some "text/xml"
	  else None
	| _ -> content_type
    in

    Lwt.return
      { r with
        Ocsigen_http_frame.
        res_code    = code_of_code_option code;
        res_charset = (match charset with
          | None -> Some (Eliom_config.get_config_default_charset ())
          | _ -> charset
	);
        res_content_type= content_type;
        res_headers = headers;
      }

  end

module type Eliom_appl = sig

  (** unique type *)
  type appl

  include "sigs/eliom_reg.mli"
    subst type page    := HTML5_types.html HTML5.M.elt
      and type options := appl_service_options
      and type return  := appl_service
      and type result  := (appl application_content, appl_service) kind

  (** Unique identifier for this application.
      It is the application name.
      Warning: do not mix up with the "application instance id",
      that is unique for each instance of the application.
  *)
  val application_name : string
  val typed_name : appl application_name

  val application_script : unit -> [> `Script ] HTML5.M.elt

end

module Eliom_appl (Appl_params : APPL_PARAMS) : Eliom_appl = struct

  module Eliom_appl_reg_param =
    Eliom_appl_reg_make_param
      (Ocsigen_senders.Make_XML_Content(XML)(HTML5.M))
      (Appl_params)

  type appl = Eliom_appl_reg_param.appl

  module Eliom_appl_registration = Eliom_mkreg.MakeRegister(Eliom_appl_reg_param)

  include Eliom_appl_registration

  (** Unique identifier for this application.
      It is the application name.
      Warning: do not mix up with the "application instance id",
      that is unique for each instance of the application.
  *)
  let application_name = Appl_params.application_name
  let typed_name = Appl_params.application_name

  let application_script = Eliom_appl_reg_param.application_script

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
  type return = http_service
  type result = (browser_content, http_service) kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_services.XAlways
  (* actually, the service will decide itself *)

  let send ?(options = `Permanent) ?charset ?code
      ?content_type ?headers content =
    let uri = Uri.string_of_uri content in
    let empty_result = Ocsigen_http_frame.empty_result () in
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
       If the request is an xhr done by a client side Eliom program
       expecting a process page,
       we do not send an HTTP redirection.
       In that case, we send a full xhr redirection.
       If the application to which belongs the destination service is the same,
       then it is ok, otherwise, there will be another redirection ...
    *)
    if not (Eliom_request_info.expecting_process_page ())
    then (* the browser did not ask application eliom data,
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
          Ocsigen_http_frame.res_code= code;
          res_location = Some uri;
          res_content_type= content_type;
          res_headers= headers;
        }
    else
      Lwt.return
        {empty_result with
          Ocsigen_http_frame.res_content_type= content_type;
          res_headers=
            Http_headers.add
              (Http_headers.name Eliom_common.full_xhr_redir_header)
              uri headers
        }


end

module String_redirection = Eliom_mkreg.MakeRegister(String_redir_reg_base)

module Redir_reg_base = struct

  type ('a, 'b) page =
      (unit, unit, Eliom_services.get_service_kind,
       [ `WithoutSuffix ],
       unit, unit, Eliom_services.registrable, 'b)
        Eliom_services.service

  type options = [ `Temporary | `Permanent ]

  type 'a return = 'a

  type ('a, 'b) result = ('a, 'b) kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_services.XAlways
  (* actually, the service will decide itself *)

  let send ?(options = `Permanent) ?charset ?code
      ?content_type ?headers service =
    let uri = lazy (Xhtml.make_string_uri ~absolute:true ~service ()) in
    let empty_result = Ocsigen_http_frame.empty_result () in
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
       If the request is an xhr done by a client side Eliom program
       expecting a process page,
       we do not send an HTTP redirection.
       In that case, we send:
       - a full xhr redirection if the application to which belongs
       the destination service is the same (thus it will send back tab cookies)
       - a half xhr redirection otherwise
    *)
    match Eliom_request_info.expecting_process_page (),
      Eliom_request_info.get_sp_client_appl_name () with
      (* the appl name as sent by browser *)
      | true, None (* should not happen *)
      | false, _ -> (* the browser did not ask for process data,
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
            Ocsigen_http_frame.res_code= code;
            res_location = Some (Lazy.force uri);
            res_content_type= content_type;
            res_headers= headers; }

      | true, Some anr ->
        (* the browser asked application eliom data
           for the application called anr *)
        (* If it comes from an xhr, we use answer with a special header field *)
          match Eliom_services.get_send_appl_content service with
          (* the appl name of the destination service *)
            | Eliom_services.XSame_appl an when (an = anr) ->
            (* Same appl, we do a full xhr redirection
               (not an http redirection, because we want to
               send back tab cookies) *)
              Lwt.return
                {empty_result with
                  Ocsigen_http_frame.res_content_type= content_type;
                  res_headers=
                    Http_headers.add
                      (Http_headers.name Eliom_common.full_xhr_redir_header)
                      (Lazy.force uri) headers
                }

            | Eliom_services.XAlways ->
            (* It is probably an action, or a void coservice. Full xhr again *)
              Lwt.return
                {empty_result with
                  Ocsigen_http_frame.res_content_type= content_type;
                  res_headers=
                    Http_headers.add
                      (Http_headers.name Eliom_common.full_xhr_redir_header)
                      (Lazy.force uri) headers
                }

            | _ -> (* No application, or another application.
                      We ask the browser to do an HTTP redirection. *)
              Lwt.return
                {empty_result with
                  Ocsigen_http_frame.res_content_type= content_type;
                  res_headers=
                    Http_headers.add
                      (Http_headers.name Eliom_common.half_xhr_redir_header)
                      (Lazy.force uri) headers
                }


end


module Redirection = Eliom_mkreg.MakeRegister_AlphaReturn(Redir_reg_base)

(*****************************************************************************)

let set_exn_handler h =
  let sitedata = Eliom_request_info.find_sitedata "set_exn_handler" in
  Eliom_request_info.set_site_handler sitedata (Result_types.cast_function_http h)
