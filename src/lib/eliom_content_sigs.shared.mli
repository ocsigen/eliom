module type FORMS = sig

  include Eliom_form_sigs.S

  type (_, _, _) star

  type ('a, 'b, 'c) lazy_star =
    ?a: (('a attrib) list) ->
    ('b elt) list Eliom_lazy.request ->
    'c elt

  val lazy_form:
    ([< Html5_types.form_attrib ],
     [< Html5_types.form_content_fun ],
     [> Html5_types.form ]) lazy_star

  (** Creates an untyped form. *)
  val raw_form :
    ([< Html5_types.form_attrib ],
     [< Html5_types.form_content_fun ],
     [> Html5_types.form ]) star

  (** This is an alias to {% <<a_api|val
      Eliom_content.Html5.D.get_form>> %} to avoid the untyped
      [Eliom_content.Html5.D.form]. *)
  val form :
    ?absolute:bool -> ?absolute_path:bool -> ?https:bool ->
    ?a:Html5_types.form_attrib attrib list ->
    service:
      ('get, unit,
       [< Eliom_service.get_service_kind],
       _, _,
       [< Eliom_service.suff],
       'gn, 'pn,
       [< Eliom_service.registrable],
       [< Eliom_service.non_ocaml_service ]) Eliom_service.service ->
    ?hostname:string ->
    ?port:int ->
    ?fragment:string ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?nl_params: Eliom_parameter.nl_params_set ->
    ?xhr:bool ->
    ('gn -> Html5_types.form_content elt list) ->
    [> Html5_types.form ] elt

  (** This is an alias to {% <<a_api|val
      Eliom_content.Html5.D.string_input>> %} to avoid the untyped
      [Eliom_content.Html5.D.input]. *)
  val input :
    ?a:Html5_types.input_attrib attrib list ->
    input_type:[< Eliom_form_sigs.input_type] ->
    ?name:
      [< string Eliom_parameter.setoneradio]
      Eliom_parameter.param_name ->
    ?value:string ->
    unit ->
    [> Html5_types.input ] elt

  (** This is an alias to {% <<a_api|val
      Eliom_content.Html5.D.string_select>> %} to avoid the untyped
      [Eliom_content.Html5.D.select]. *)
  val select :
    ?a:Html5_types.select_attrib attrib list ->
    name:[ `One of string ] Eliom_parameter.param_name ->
    string select_opt ->
    string select_opt list ->
    [> Html5_types.select ] elt

end
