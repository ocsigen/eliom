
type uri
type pcdata_elt

type form_elt
type form_content_elt
type form_content_elt_list
type form_attrib_t

type 'a a_elt
type 'a a_elt_list
type 'a a_content_elt
type 'a a_content_elt_list
type a_attrib_t

type link_elt
type link_attrib_t

type script_elt
type script_attrib_t

type textarea_elt
type textarea_attrib_t

type input_elt
type input_attrib_t
val a_input_required : [`Required] -> input_attrib_t
val input_attrib_append : input_attrib_t -> input_attrib_t -> input_attrib_t


type select_elt
type select_content_elt
type select_content_elt_list
val select_content_cons : select_content_elt -> select_content_elt_list -> select_content_elt_list
type select_attrib_t
val a_select_required : [`Required] -> select_attrib_t
val select_attrib_append : select_attrib_t -> select_attrib_t -> select_attrib_t

type button_elt
type button_content_elt
type button_content_elt_list
type button_attrib_t

type option_elt
type option_elt_list
type optgroup_attrib_t
type option_attrib_t

type input_type_t
type button_type_t

type for_attrib

val hidden : input_type_t
val checkbox : input_type_t
val radio : input_type_t
val submit : input_type_t
val file : input_type_t
val image : input_type_t

val buttonsubmit : button_type_t

val empty_seq : form_content_elt_list
val map_option :
  ('a -> option_elt) -> 'a list ->
  option_elt_list
val map_optgroup :
  ('a -> select_content_elt) -> 'a -> 'a list ->
  (select_content_elt * select_content_elt_list)
val select_content_of_option : option_elt -> select_content_elt

val make_pcdata : string -> pcdata_elt
val make_a : ?a:a_attrib_t -> ?href:uri -> 'a a_content_elt_list -> 'a a_elt
val make_get_form :
  ?a:form_attrib_t -> action:uri ->
  form_content_elt_list Eliom_lazy.request -> form_elt
val make_post_form : ?a:form_attrib_t ->
  action:uri -> ?id:string -> ?inline:bool ->
  form_content_elt_list Eliom_lazy.request -> form_elt
val cons_hidden_fieldset : input_elt list -> form_content_elt_list -> form_content_elt_list
val make_input : ?a:input_attrib_t -> ?checked:bool ->
  typ:input_type_t -> ?name:string -> ?src:uri ->
  ?value:string -> unit -> input_elt
val make_button : ?a:button_attrib_t -> button_type:button_type_t ->
  ?name:string -> ?value:string ->
  button_content_elt_list -> button_elt
val make_textarea :
  ?a:textarea_attrib_t ->
  name:string -> ?value:string ->
  unit -> textarea_elt
val make_select :
  ?a:select_attrib_t ->
  multiple:bool ->
  name:string ->
  select_content_elt ->
  select_content_elt_list ->
  select_elt
val make_option :
  ?a:option_attrib_t ->
  selected:bool ->
  ?value:string ->
  pcdata_elt ->
  option_elt
val make_optgroup :
  ?a:optgroup_attrib_t ->
  label:string ->
  option_elt ->
  option_elt_list ->
  select_content_elt
val uri_of_string : (unit -> string) -> uri

val make_css_link : ?a:link_attrib_t -> uri:uri -> unit -> link_elt

val make_js_script : ?a:script_attrib_t -> uri:uri -> unit -> script_elt

val make_for_attrib : string -> for_attrib
