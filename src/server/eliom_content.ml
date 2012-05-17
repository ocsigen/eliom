
open Eliom_lib
open Eliom_content_core

module type Forms = "sigs/eliom_forms.mli"

module Xml = Xml

module Svg = struct

  module F = Svg.F
  module D = Svg.D

  module Id = Svg.Id

  type +'a elt = 'a F.elt
  type +'a attrib = 'a F.attrib
  type uri = F.uri

  module Printer = Svg.Printer

end

module Html5 = struct

  module F = struct
    include Html5.F
    let raw_a = a
    let raw_input = input
    include Eliom_output_base.Html5_forms.F
  end

  module D = struct
    include Html5.D
    let raw_a = a
    let raw_input = input
    include Eliom_output_base.Html5_forms.D
  end

  module Id = Html5.Id

  module Printer = Html5.Printer

  type +'a elt = 'a F.elt
  type +'a attrib = 'a F.attrib
  type uri = F.uri
end

module Xhtml = struct

  module Xhtml_forms = struct
    module Forms_base = struct

      type uri = Xhtml.F.uri
      type pcdata_elt = Xhtml_types.pcdata Xhtml.F.elt

      type form_elt = Xhtml_types.form Xhtml.F.elt
      type form_content_elt = Xhtml_types.form_content Xhtml.F.elt
      type form_content_elt_list = Xhtml_types.form_content Xhtml.F.elt list
      type form_attrib_t = Xhtml_types.form_attrib Xhtml.F.attrib list

      type 'a a_elt = Xhtml_types.a Xhtml.F.elt
      type 'a a_elt_list = Xhtml_types.a Xhtml.F.elt list
      type 'a a_content_elt = Xhtml_types.a_content Xhtml.F.elt
      type 'a a_content_elt_list = Xhtml_types.a_content Xhtml.F.elt list
      type a_attrib_t = Xhtml_types.a_attrib Xhtml.F.attrib list

      type link_elt = Xhtml_types.link Xhtml.F.elt
      type link_attrib_t = Xhtml_types.link_attrib Xhtml.F.attrib list

      type script_elt = Xhtml_types.script Xhtml.F.elt
      type script_attrib_t = Xhtml_types.script_attrib Xhtml.F.attrib list

      type textarea_elt = Xhtml_types.textarea Xhtml.F.elt
      type textarea_attrib_t = Xhtml_types.textarea_attrib Xhtml.F.attrib list

      type input_elt = Xhtml_types.input Xhtml.F.elt
      type input_attrib_t = Xhtml_types.input_attrib Xhtml.F.attrib list

      type select_elt = Xhtml_types.select Xhtml.F.elt
      type select_content_elt = Xhtml_types.select_content Xhtml.F.elt
      type select_content_elt_list = Xhtml_types.select_content Xhtml.F.elt list
      type select_attrib_t = Xhtml_types.select_attrib Xhtml.F.attrib list

      type button_elt = Xhtml_types.button Xhtml.F.elt
      type button_content_elt = Xhtml_types.button_content Xhtml.F.elt
      type button_content_elt_list = Xhtml_types.button_content Xhtml.F.elt list
      type button_attrib_t = Xhtml_types.button_attrib Xhtml.F.attrib list

      type option_elt = Xhtml_types.selectoption Xhtml.F.elt
      type option_elt_list = Xhtml_types.selectoption Xhtml.F.elt list
      type optgroup_attrib_t = [ Xhtml_types.common | `Disabled ] Xhtml.F.attrib list
      type option_attrib_t = Xhtml_types.option_attrib Xhtml.F.attrib list

      open Xhtml.F
      open Xhtml_types
      open Eliom_output_base

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

      let uri_of_string = Eliom_content_core.Xml.uri_of_fun

      let map_option = List.map
      let map_optgroup f a l = ((f a), List.map f l)
      let select_content_of_option a = (a :> select_content_elt)

      let make_pcdata s = pcdata s

      let make_a ?(a=[]) ?href l : 'a a_elt =
        let a = match href with
          | None -> a
          | Some v -> a_href v :: a
        in
        Xhtml.F.a ~a l

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
          lazy_form ~a:((Xhtml.F.a_enctype "multipart/form-data")::
                 (* Always Multipart!!! How to test if there is a file?? *)
                      (a_method `Post)::
                      (if inline then (a_class ["inline"])::aa else aa))
            ~action:action elt1 elts
        in
        r

      let empty_seq = []
      let cons_hidden_fieldset fields content =
        let fieldset =
          Xhtml.F.fieldset
            ~a:[a_style "display: none;"]
            fields in
        (fieldset :: content :> form_content_elt_list)

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

      let make_textarea ?(a=[]) ~name ?(value="") () =
        let a3 = (a_name name)::a in
        textarea ~a:a3 ~rows:10 ~cols:50 (pcdata value)

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

      type for_attrib = [`For] Xhtml.F.attrib
      let make_for_attrib = a_for

    end

    module F = struct

      open Xhtml.F
      open Xhtml_types
      open Eliom_services
      open Eliom_parameters

      module Xhtml_forms_closed = Eliom_mkforms.MakeForms(Forms_base)

      (* As we want -> [> a ] elt and not -> [ a ] elt (etc.), as found in
         Xhtmlforms_closed, we introduce explicit coercion.  *)

      open Xhtml_forms_closed
      (*BB I do not include [Xhtml_forms_closed], because we must not include
         [type url] for later inclusion along Xhtml.F *)
      type 'a soption = 'a Xhtml_forms_closed.soption
      type 'a select_opt = 'a Xhtml_forms_closed.select_opt =
        | Optgroup of
            Forms_base.optgroup_attrib_t
            * string (* label *)
            * 'a soption
            * 'a soption list
        | Option of 'a soption
      let make_proto_prefix = make_proto_prefix
      let make_post_uri_components = make_post_uri_components
      let make_uri_components = make_uri_components
      let make_string_uri = make_string_uri
      let uri_of_string = uri_of_string

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
                ?xhr:bool ->
                a_content elt list -> 'get ->
                a Xhtml.F.elt :>
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
                ?xhr:bool ->
                a_content elt list -> 'get ->
                [> a] Xhtml.F.elt)

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
                       ?xhr:bool ->
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
                       ?xhr:bool ->
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
                           ?xhr:bool ->
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
                           ?xhr:bool ->
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
                        ?xhr:bool ->
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
                        ?xhr:bool ->
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
                            ?xhr:bool ->
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
                            ?xhr:bool ->
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
                             ?a:(input_attrib attrib list) -> ?checked:bool ->
                            name:'a -> unit -> input elt :>
                            ?a:(input_attrib attrib list) -> ?checked:bool ->
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
                            ?a:(input_attrib attrib list) -> ?checked:bool ->
                           name:'a -> value:string -> unit -> input elt :>
                           ?a:(input_attrib attrib list) -> ?checked:bool ->
                           name:'a -> value:string -> unit -> [> input ] elt)

      let int_radio = (int_radio :
                         ?a:(input_attrib attrib list) -> ?checked:bool ->
                        name:'a -> value:int -> unit -> input elt :>
                        ?a:(input_attrib attrib list) -> ?checked:bool ->
                        name:'a -> value:int -> unit -> [> input ] elt)

      let int32_radio = (int32_radio :
                           ?a:(input_attrib attrib list) -> ?checked:bool ->
                          name:'a -> value:int32 -> unit -> input elt :>
                          ?a:(input_attrib attrib list) -> ?checked:bool ->
                          name:'a -> value:int32 -> unit -> [> input ] elt)

      let int64_radio = (int64_radio :
                           ?a:(input_attrib attrib list) -> ?checked:bool ->
                          name:'a -> value:int64 -> unit -> input elt :>
                          ?a:(input_attrib attrib list) -> ?checked:bool ->
                          name:'a -> value:int64 -> unit -> [> input ] elt)

      let float_radio = (float_radio :
                           ?a:(input_attrib attrib list) -> ?checked:bool ->
                          name:'a -> value:float -> unit -> input elt :>
                          ?a:(input_attrib attrib list) -> ?checked:bool ->
                          name:'a -> value:float -> unit -> [> input ] elt)

      let user_type_radio = (user_type_radio :
                               ('a -> string) ->
                              ?a:(input_attrib attrib list) -> ?checked:bool ->
                              name:'b -> value:'a -> unit -> input elt :>
                              ('a -> string) ->

                              ?a:(input_attrib attrib list) -> ?checked:bool ->
                              name:'b -> value:'a -> unit -> [> input ] elt)

      let raw_radio = (raw_radio :
                         ?a:(input_attrib attrib list) -> ?checked:bool ->
                        name:string -> value:string -> unit -> input elt :>
                        ?a:(input_attrib attrib list) -> ?checked:bool ->
                        name:string -> value:string -> unit -> [> input ] elt)

      let textarea = (textarea :
                        ?a:textarea_attrib attrib list ->
                       name:'a -> ?value:string ->
                       unit -> textarea elt :>
                       ?a:textarea_attrib attrib list ->
                       name:'a -> ?value:string ->
                       unit -> [> textarea ] elt)

      let raw_textarea = (raw_textarea :
                            ?a:textarea_attrib attrib list ->
                           name:string -> ?value:string ->
                           unit -> textarea elt :>
                           ?a:textarea_attrib attrib list ->
                           name:string -> ?value:string ->
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

      let a_for = (a_for: _ -> [ `For ] attrib :> _ -> [> `For ] attrib)

    end
  end

  module F = struct
    include Xhtml.F
    let raw_a = a
    let raw_input = input
    include Xhtml_forms.F
  end
  module F_01_00 = struct
    include Xhtml.F_01_00
    let raw_a = a
    let raw_input = input
    include Xhtml_forms.F
  end
  module F_01_01 = struct
    include Xhtml.F_01_01
    let raw_a = a
    let raw_input = input
    include Xhtml_forms.F
  end
  module Printer = Xhtml.Printer
  module Printer_01_00 = Xhtml.Printer_01_00
  module Printer_01_01 = Xhtml.Printer_01_01

end

module Html_text = struct
  module Forms_base = struct

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
