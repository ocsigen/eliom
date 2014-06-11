
(* When modifying this interface, please ensure that the ocamldoc is
   coherent with the ocamldoc from Eliom_uri. *)

open Eliom_lib

open Eliom_parameter
open Eliom_service

type uri
type pcdata_elt

type form_elt
type form_content_elt
type form_content_elt_list
type form_attrib_t

type +'a a_elt
type +'a a_content_elt
type +'a a_content_elt_list
type a_attrib_t

type link_elt
type link_attrib_t

type script_elt
type script_attrib_t

type textarea_elt
type textarea_attrib_t

type input_elt
type input_attrib_t

type select_elt
type select_attrib_t

type button_elt
type button_content_elt
type button_content_elt_list
type button_attrib_t

type optgroup_attrib_t
type option_attrib_t

type input_type_t
type button_type_t

type for_attrib

(** {2 Forms creation } *)

(** {3 Links and forms} *)

(** The function [make_uri service get_params] returns the URL of the
    service [service] applied to the GET parameters [get_params]. By
    default the returned URL is relative to the current request URL
    but it is absolute when one of the following conditions is met:

    - the optional parameter [~absolute_path] is [true].
    - the optional parameter [~absolute] is [true].
    - the optional parameter [~https] is [true] (resp. [false])
    and the current request protocol is [http] (resp. [https]).
    - the optional parameter [~https] is [true] and the
    function is used outside of a service handler
    - the [service] has been created with [~https:true] and the
    current request protocol is [http].
    - the [service] has been created with [~https:true] and the
    function is used outside of a service handler.

    When only the first condition is met ([~absolute_path] is [true])
    the returned URL is just the absolute path, but when any other
    condition is satisfied the returned URL is prefixed with
    [protocol://hostname\[:port\]], where:

    - [protocol] is:
    {ul {- [https] if the [service] has been created with [~https:true]
    or the optional parameter [~https] is [true];}
    {- [http] if  the optional parameter [~https] is [false];}
    {- the current request protocol if available;}
    {- [http] in any other case.}}
    - [hostname] is:
    {ul {- the optional parameter [~hostname] if given;}
    {- the attribute [defaulthostname] of [<host>] tag in
    configuration file or the machine hostname
    if the option [<usedefaulthostname/>] is set;}
    {- the [Host] http header of the current request if available;}
    {- the attribute [defaulthostname] of [<host>] tag in
    configuration file or the machine hostname in any other case.}}
    - [port] is:
    {ul {- the optional parameter [~port] if given;}
    {- the attribute [defaulthttpsport] (resp. [defaulthttpport]) of [<host>] tag
    in configuration file or [443] (resp. 80) if [protocol] is [https] (resp. [http]) and
    the current request protocol is [http] (resp. [https]);}
    {- the attribute [defaulthttpsport] (resp. [defaulthttpsport]) of [<host>] tag
    in configuration file or [443] (resp. 80) if the option [<usedefaulthostname/>]
    is set and [protocol] is [https] (resp. [http]);}
    {- the port associated to the [Host] http header of the current
    request if available;}
    {- the incoming port of the current request if available;}
    {- the attribute [defaulthttpport] (resp. [defaulthttpsport]) of [<host>] tag
    in configuration file or [80] (resp. [443]) in any other case.}}

    If given the optional parameter [~fragment] is prefixed by [#]
    and appended to the URL.

    The optional parameter [keep_nl_params] allows one to override the
    [keep_nl_params] parameter used when creating the [service], see
    {!Eliom_service.Http.service} for a detailled description.

    The optional parameter [nl_params] allows one to add non localized
    GET parameter to the URL.  See the eliom manual for more
    information about {% <<a_manual chapter="server-params"
    fragment="nonlocalizedparameters"|non localized parameters>>%}.
*)
val make_uri :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('get, unit, [< get_service_kind ], _, _,
           [< suff ], 'gn, unit,
           [< registrable ], 'return) service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?nl_params: Eliom_parameter.nl_params_set ->
  'get ->
  uri

(** The function [make_string_uri service get_params] returns the URL of the
    of the service [service] applied to the GET parameters
    [get_params]. See {!make_uri} for a detailled
    description of optional parameters.

    The function [make_string_uri] is an alias of {!Eliom_uri.make_string_uri}.

    {e Warning: The function [make_string_uri] should not be called outside
    of a service handler, unless one of the following condition is met:}

    - the optional parameter [~absolute_path] is [true].
    - the optional parameter [~absolute] is [true].
    - the optional parameter [~https] is [true].
    - the [service] has been created with [~https:true].
    - the [service] is an external service. *)
val make_string_uri :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('get, unit, [< get_service_kind ], _, _,
           [< suff ], 'gn, unit,
           [< registrable ], 'return) service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?nl_params: Eliom_parameter.nl_params_set ->
  'get ->
  string

(** The function [uri_of_string f] returns a URI whose content is
    equivalent to [f ()].

    For XML tree build with TyXML, like {!Html5} or
    {!Svg.F}, the function [f] is applied each time the XML tree is
    sent to the client (either as page content or as a marshalled
    OCaml value). Hence, the function is always evaluated in the
    context of a service handler.

    For other module, the function [f] is immediatly applied. *)
val uri_of_string : (unit -> string) -> uri

(** The function [make_uri_components service get_params] returns the
    a triplet [(path, get_params, fragment)] that is a decomposition
    of the URL for the service [service] applied to the GET parameters
    [get_params]. By default the returned [path] is relative to the
    current request URL but it could be absolute URL in some
    situation, see {!make_uri} for more information and a description
    of optional parameters.

    The function [make_uri_components] is an alias for
    {!Eliom_uri.make_uri_components}.

    {e Warning: depending on the optional parameters, the function
    [make_uri_components] may not be used outside of a service
    handler. See {!make_string_uri} for a detailled description.}
*)
val make_uri_components :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('get, unit, [< get_service_kind ], _, _,
           [< suff ], 'gn, unit,
           [< registrable ], 'return) service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?nl_params: Eliom_parameter.nl_params_set ->
  'get ->
  string * (string * Eliommod_parameters.param) list * string option

(** Same a {!make_uri_components}, but also returns a list of post
    parameters. *)
val make_post_uri_components :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('get, 'post, [< post_service_kind ], _, _,
           [< suff ], 'gn, 'pn,
           [< registrable ], 'return) service ->
(*  service:('get, 'post,
           [< `Attached of ([> `External ], 'c) Eliom_service.a_s
           | `Nonattached of [< `Post ] Eliom_service.na_s ],
           [< Eliom_service.suff ], 'gn, 'pn,
           [< Eliom_service.registrable ], 'return)
    Eliom_service.service -> *)
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?nl_params: Eliom_parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  'get ->
  'post ->
  string * (string * Eliommod_parameters.param) list * string option *
    (string * Eliommod_parameters.param) list

(**/**)
(** Creates the string corresponding to the beginning of the URL,
    containing the scheme (protocol), server and port number (if necessary).
*)
val make_proto_prefix :
  ?hostname:string ->
  ?port:int ->
  bool ->
  string
(**/**)

(** The function [a service a_content get_params] creates a [<a>]
    node that link to [service] applied to GET parameters [get_params]
    and whose content is [a_content]. By default, the [href] attribute
    is a relative URL recomputed at each request with {!make_uri}.

    By default, the link is realized such that the client-side Eliom application
    keeps running irrespectable of the usage of the link (cf. {% <<a_api
    project="eliom" subproject="client" | val Eliom_client.change_page>> %}).

    By contrast, if the optional parameter [~xhr:false] is given, the link is
    realized as a standard HTML link and clicking it discontinues the Eliom
    application.
    The [~xhr] parameter has no effect outside an Eliom application.

    NB that the default value of [~xhr] is configurable through <<a_api
    project="eliom" subproject="server" | val Eliom_config.set_default_links_xhr >>

    The optional parameter [~a] allows one to add extra HTML attributes to
    the generated node.

    See {!make_uri} for description of other optional parameters.
*)
val a :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  ?a:a_attrib_t ->
  service:('get, unit, [< get_service_kind ], _, _,
           [< suff ], 'd, unit,
           [< registrable ], [< non_ocaml_service])
    service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?nl_params: Eliom_parameter.nl_params_set ->
  ?xhr:bool ->
  'a a_content_elt_list ->
  'get ->
  'a a_elt

(** The function [css_link ~uri ()] creates a [<link>] node that
    reference a Cascading StyleSheet (CSS).

    If the CSS is generated by an Eliom service, use {!make_uri} to
    calculate the service URI. If the CSS is a static file, you may
    also use {!Eliom_service.static_dir} or
    {!Eliom_service.Http.external_service} to abstract the file with a
    service.

    The optional parameter [~a] allows one to add extra HTML attributes to
    the generated node.
*)
val css_link : ?a:link_attrib_t -> uri:uri -> unit -> link_elt

(** The function [js_script ~uri ()] creates a [<script>] node that
    reference a javascript file.

    If the script content is generated by an Eliom service, use
    {!make_uri} to calculate the service URI. If it is a static file,
    you may also use {!Eliom_service.static_dir} or
    {!Eliom_service.Http.external_service} to abstract the file with a
    service.

    The optional parameter [~a] allows one to add extra HTML attributes to
    the generated node.
*)
val js_script :
  ?a:script_attrib_t -> uri:uri -> unit -> script_elt

(** The function [post_form service formgen] creates a GET [<form>] to
    [service]. The content of the [<form>] is generated by the
    function [formgen], that takes the names of the service parameters
    as parameters. By default, the [action] attribute is a relative
    URL recomputed at each request with {!make_uri}.

    By default, the form is realized such that the client-side Eliom application
    keeps running irrespectable of the usage of the form (cf. {% <<a_api
    project="eliom" subproject="client" | val Eliom_client.change_page>> %}).

    By contrast, if the optional parameter [~xhr:false] is given, the form is
    realized as a standard HTML form and submitting it discontinues the Eliom
    application.
    The [~xhr] parameter has no effect outside an Eliom application.

    NB that the default value of [~xhr] is configurable through <<a_api
    project="eliom" subproject="server" | val Eliom_config.set_default_links_xhr >>

    The optional parameter [~a] allows one to add extra HTML attributes to
    the generated node.

    See {!make_uri} for description of other optional parameters.
*)
val get_form :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  ?a:form_attrib_t ->
  service:('get, unit, [< get_service_kind ], _, _,
           [<suff ], 'gn, 'pn,
           [< registrable ], [< non_ocaml_service]) service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?nl_params: Eliom_parameter.nl_params_set ->
  ?xhr:bool ->
  ('gn -> form_content_elt_list) ->
  form_elt

(** Same as {!get_form} but taking a cooperative function for
    [<form>] content generation. *)
val lwt_get_form :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  ?a:form_attrib_t ->
  service:('get, unit, [< get_service_kind ], _, _,
           [<suff ], 'gn, 'pn,
           [< registrable ], [< non_ocaml_service]) service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?nl_params: Eliom_parameter.nl_params_set ->
  ?xhr:bool ->
  ('gn -> form_content_elt_list Lwt.t) ->
  form_elt Lwt.t


(** The function [post_form service formgen get_params] creates a POST
    [<form>] to [service] preapplied to the GET parameters
    [get_params]. The content of the [<form>] is generated by the
    function [formgen], that takes the names of the service parameters
    as parameters. By default, the [action] attribute is a relative
    URL recomputed at each request with {!make_uri}.

    The optional parameter [~a] allows one to add HTML attributes to the
    generated node.

    See {!Eliom_service.Http.post_coservice'} for a description of the
    [~keep_get_na_params] optional parameter ; see {!get_form} for
    [~xhr] and see {!make_uri} for other optional parameters.
*)
val post_form :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  ?a:form_attrib_t ->
  service:('get, 'post, [< post_service_kind ], _, _,
           [< suff ], 'gn, 'pn,
           [< registrable ], [< non_ocaml_service]) service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?keep_get_na_params:bool ->
  ?nl_params: Eliom_parameter.nl_params_set ->
  ?xhr:bool ->
  ('pn -> form_content_elt_list) ->
  'get ->
  form_elt

(** Same as {!post_form} but taking a cooperative function for
    [<form>] content generation. *)
val lwt_post_form :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  ?a:form_attrib_t ->
  service:('get, 'post, [< post_service_kind ], _, _,
           [< suff ], 'gn, 'pn,
           [< registrable ], [< non_ocaml_service]) service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?keep_get_na_params:bool ->
  ?nl_params: Eliom_parameter.nl_params_set ->
  ?xhr:bool ->
  ('pn -> form_content_elt_list Lwt.t) ->
  'get ->
  form_elt Lwt.t





(** {2:form_widgets Form widgets } *)

(** Creates an [<input>] tag for an integer *)
val int_input :
  ?a:input_attrib_t -> input_type:input_type_t ->
  ?name:[< int setoneradio ] param_name ->
  ?value:int -> unit -> input_elt

(** Creates an [<input>] tag for an integer *)
val int32_input :
  ?a:input_attrib_t -> input_type:input_type_t ->
  ?name:[< int32 setoneradio ] param_name ->
  ?value:int32 -> unit -> input_elt

(** Creates an [<input>] tag for an integer *)
val int64_input :
  ?a:input_attrib_t -> input_type:input_type_t ->
  ?name:[< int64 setoneradio ] param_name ->
  ?value:int64 -> unit -> input_elt

(** Creates an [<input>] tag for a float *)
val float_input :
  ?a:input_attrib_t -> input_type:input_type_t ->
  ?name:[< float setoneradio ] param_name ->
  ?value:float -> unit -> input_elt

(** Creates an [<input>] tag for a string *)
val string_input :
  ?a:input_attrib_t -> input_type:input_type_t ->
  ?name:[< string setoneradio ] param_name ->
  ?value:string -> unit -> input_elt

(** Creates an [<input>] tag for a user type *)
val user_type_input : ('a -> string) ->
  ?a:input_attrib_t -> input_type:input_type_t ->
  ?name:[< 'a setoneradio ] param_name ->
  ?value:'a -> unit -> input_elt

(** Creates an untyped [<input>] tag. You may use the name you want
    (for example to use with {!Eliom_parameter.any}). *)
val raw_input :
  ?a:input_attrib_t -> input_type:input_type_t ->
  ?name:string -> ?value:string -> unit -> input_elt

(** Creates an [<input>] tag for sending a file *)
val file_input :
  ?a:input_attrib_t ->
  name:[< file_info setoneradio ] param_name ->
  unit -> input_elt

(** Creates an [<input type="image" name="...">] tag that sends the coordinates
    the user clicked on *)
val image_input :
  ?a:input_attrib_t ->
  name:[< coordinates oneradio ] param_name ->
  ?src:uri -> unit -> input_elt

(** Creates an [<input type="image" name="..." value="...">] tag that sends
    the coordinates the user clicked on and a value of type int *)
val int_image_input :
  ?a:input_attrib_t ->
  name:[< (int * coordinates) oneradio ] param_name -> value:int ->
  ?src:uri -> unit -> input_elt

(** Creates an [<input type="image" name="..." value="...">] tag that sends
    the coordinates the user clicked on and a value of type int32 *)
val int32_image_input :
  ?a:input_attrib_t ->
  name:[< (int32 * coordinates) oneradio ] param_name -> value:int32 ->
  ?src:uri -> unit -> input_elt

(** Creates an [<input type="image" name="..." value="...">] tag that sends
    the coordinates the user clicked on and a value of type int64 *)
val int64_image_input :
  ?a:input_attrib_t ->
  name:[< (int64 * coordinates) oneradio ] param_name -> value:int64 ->
  ?src:uri -> unit -> input_elt

(** Creates an [<input type="image" name="..." value="...">] tag that sends
    the coordinates the user clicked on and a value of type float *)
val float_image_input :
  ?a:input_attrib_t ->
  name:[< (float * coordinates) oneradio ] param_name -> value:float ->
  ?src:uri -> unit -> input_elt

(** Creates an [<input type="image" name="..." value="...">] tag that sends
    the coordinates the user clicked on and a value of type string *)
val string_image_input :
  ?a:input_attrib_t ->
  name:[< (string * coordinates) oneradio ] param_name -> value:string ->
  ?src:uri -> unit -> input_elt

(** Creates an [<input type="image" name="..." value="...">] tag that sends
    the coordinates the user clicked on and a value of user defined type *)
val user_type_image_input : ('a -> string) ->
  ?a:input_attrib_t ->
  name:[< ('a * coordinates) oneradio ] param_name -> value:'a ->
  ?src:uri -> unit -> input_elt

(** Creates an [<input type="image" name="..." value="...">] tag that sends
    the coordinates the user clicked on and an untyped value *)
val raw_image_input :
  ?a:input_attrib_t ->
  name:string -> value:string -> ?src:uri -> unit -> input_elt

(** Creates a checkbox [<input>] tag that will have a boolean value.
    The service must declare a [bool] parameter. *)
val bool_checkbox :
  ?a:input_attrib_t -> ?checked:bool ->
  name:[ `One of bool ] param_name -> unit -> input_elt


(** Creates a checkbox [<input>] tag that will have an int value.
    Thus you can do several checkboxes with the same name
    (and different values).
    The service must declare a parameter of type [set]. *)
val int_checkbox :
  ?a:input_attrib_t -> ?checked:bool ->
  name:[ `Set of int ] param_name -> value:int -> unit -> input_elt

(** Creates a checkbox [<input>] tag that will have an int32 value.
    Thus you can do several checkboxes with the same name
    (and different values).
    The service must declare a parameter of type [set]. *)
val int32_checkbox :
  ?a:input_attrib_t -> ?checked:bool ->
  name:[ `Set of int32 ] param_name -> value:int32 -> unit -> input_elt

(** Creates a checkbox [<input>] tag that will have an int64 value.
    Thus you can do several checkboxes with the same name
    (and different values).
    The service must declare a parameter of type [set]. *)
val int64_checkbox :
  ?a:input_attrib_t -> ?checked:bool ->
  name:[ `Set of int64 ] param_name -> value:int64 -> unit -> input_elt

(** Creates a checkbox [<input>] tag that will have a float value.
    Thus you can do several checkboxes with the same name
    (and different values).
    The service must declare a parameter of type [set]. *)
val float_checkbox :
  ?a:input_attrib_t -> ?checked:bool ->
  name:[ `Set of float ] param_name -> value:float -> unit -> input_elt

(** Creates a checkbox [<input>] tag that will have a string value.
    Thus you can do several checkboxes with the same name
    (and different values).
    The service must declare a parameter of type [set]. *)
val string_checkbox :
  ?a:input_attrib_t -> ?checked:bool ->
  name:[ `Set of string ] param_name -> value:string ->
  unit -> input_elt

(** Creates a checkbox [<input>] tag that will have a "user type" value.
    Thus you can do several checkboxes with the same name
    (and different values).
    The service must declare a parameter of type [set]. *)
val user_type_checkbox : ('a -> string) ->
  ?a:input_attrib_t -> ?checked:bool ->
  name:[ `Set of 'a ] param_name -> value:'a ->
  unit -> input_elt

(** Creates a checkbox [<input>] tag with untyped content.
    Thus you can do several checkboxes with the same name
    (and different values).
    The service must declare a parameter of type [any]. *)
val raw_checkbox :
  ?a:input_attrib_t -> ?checked:bool ->
  name:string -> value:string -> unit -> input_elt

(** Creates a radio [<input>] tag with string content *)
val string_radio :
  ?a:input_attrib_t -> ?checked:bool ->
  name:[ `Radio of string ] param_name ->
  value:string -> unit -> input_elt

val string_radio_required :
  ?a:input_attrib_t -> ?checked:bool ->
  name:[ `One of string ] param_name ->
  value:string -> unit -> input_elt

(** Creates a radio [<input>] tag with int content *)
val int_radio :
  ?a:input_attrib_t -> ?checked:bool ->
  name:[ `Radio of int ] param_name ->
  value:int -> unit -> input_elt

(** Creates a radio [<input>] tag with int32 content *)
val int32_radio :
  ?a:input_attrib_t -> ?checked:bool ->
  name:[ `Radio of int32 ] param_name ->
  value:int32 -> unit -> input_elt

(** Creates a radio [<input>] tag with int64 content *)
val int64_radio :
  ?a:input_attrib_t -> ?checked:bool ->
  name:[ `Radio of int64 ] param_name ->
  value:int64 -> unit -> input_elt

(** Creates a radio [<input>] tag with float content *)
val float_radio :
  ?a:input_attrib_t -> ?checked:bool ->
  name:[ `Radio of float ] param_name ->
  value:float -> unit -> input_elt

(** Creates a radio [<input>] tag with user_type content *)
val user_type_radio : ('a -> string) ->
  ?a:input_attrib_t -> ?checked:bool ->
  name:[ `Radio of 'a ] param_name ->
  value:'a -> unit -> input_elt

(** Creates a radio [<input>] tag with untyped string content (low level) *)
val raw_radio :
  ?a:input_attrib_t -> ?checked:bool ->
  name:string -> value:string -> unit -> input_elt

(** Creates a [<button>] tag with string content *)
val string_button :
  ?a:button_attrib_t ->
  name:[< string setone ] param_name -> value:string ->
  button_content_elt_list -> button_elt

(** Creates a [<button>] tag with int content *)
val int_button :
  ?a:button_attrib_t ->
  name:[< int setone ] param_name -> value:int ->
  button_content_elt_list -> button_elt


(** Creates a [<button>] tag with int32 content *)
val int32_button :
  ?a:button_attrib_t ->
  name:[< int32 setone ] param_name -> value:int32 ->
  button_content_elt_list -> button_elt


(** Creates a [<button>] tag with int64 content *)
val int64_button :
  ?a:button_attrib_t ->
  name:[< int64 setone ] param_name -> value:int64 ->
  button_content_elt_list -> button_elt


(** Creates a [<button>] tag with float content *)
val float_button :
  ?a:button_attrib_t ->
  name:[< float setone ] param_name -> value:float ->
  button_content_elt_list -> button_elt


(** Creates a [<button>] tag with user_type content *)
val user_type_button : ('a -> string) ->
  ?a:button_attrib_t ->
  name:[< 'a setone ] param_name -> value:'a ->
  button_content_elt_list -> button_elt


(** Creates a [<button>] tag with untyped string content *)
val raw_button :
  ?a:button_attrib_t ->
  button_type:button_type_t ->
  name:string -> value:string ->
  button_content_elt_list -> button_elt


(** Creates a [<button>] tag with no value. No value is sent. *)
val button :
  ?a:button_attrib_t ->
  button_type:button_type_t ->
  button_content_elt_list -> button_elt


(** Creates a [<textarea>] tag *)
val textarea :
  ?a:textarea_attrib_t ->
  name:[< string setoneradio ] param_name -> ?value:string ->
  unit -> textarea_elt


(** Creates a [<textarea>] tag for untyped form *)
val raw_textarea :
  ?a:textarea_attrib_t ->
  name:string -> ?value:string -> unit -> textarea_elt


type 'a soption =
    option_attrib_t
    * 'a (* Content (or value if the following is present) *)
    * pcdata_elt option (* if content different from value *)
    * bool (* selected *)

(** The type for [<select>] options and groups of options.
    - The field of type 'a in [soption] is the value that will be sent
    by the form.
    - If the [pcdata_elt option] is not present it is also the
    value displayed.
    - The string in [select_opt] is the label
*)
type 'a select_opt =
  | Optgroup of
      optgroup_attrib_t
    * string (* label *)
    * 'a soption
    * 'a soption list
  | Option of 'a soption


(** Creates a [<select>] tag for int values. *)
val int_select :
  ?a:select_attrib_t ->
  ?required:pcdata_elt ->
  name:[ `One of int ] param_name ->
  int select_opt ->
  int select_opt list ->
  select_elt


(** Creates a [<select>] tag for int32 values. *)
val int32_select :
  ?a:select_attrib_t ->
  ?required:pcdata_elt ->
  name:[ `One of int32 ] param_name ->
  int32 select_opt ->
  int32 select_opt list ->
  select_elt


(** Creates a [<select>] tag for int64 values. *)
val int64_select :
  ?a:select_attrib_t ->
  ?required:pcdata_elt ->
  name:[ `One of int64 ] param_name ->
  int64 select_opt ->
  int64 select_opt list ->
  select_elt


(** Creates a [<select>] tag for float values. *)
val float_select :
  ?a:select_attrib_t ->
  ?required:pcdata_elt ->
  name:[ `One of float ] param_name ->
  float select_opt ->
  float select_opt list ->
  select_elt


(** Creates a [<select>] tag for string values. *)
val string_select :
  ?a:select_attrib_t ->
  ?required:pcdata_elt ->
  name:[ `One of string ] param_name ->
  string select_opt ->
  string select_opt list ->
  select_elt


(** Creates a [<select>] tag for user type values. *)
val user_type_select : ('a -> string) ->
  ?a:select_attrib_t ->
  ?required:pcdata_elt ->
  name:[ `One of 'a ] param_name ->
  'a select_opt ->
  'a select_opt list ->
  select_elt


(** Creates a [<select>] tag for any (untyped) value. *)
val raw_select :
  ?a:select_attrib_t ->
  ?required:pcdata_elt ->
  name:string ->
  string select_opt ->
  string select_opt list ->
  select_elt


(** Creates a [<select>] tag for int values. *)
val int_multiple_select :
  ?a:select_attrib_t ->
  ?required:pcdata_elt ->
  name:[ `Set of int ] param_name ->
  int select_opt ->
  int select_opt list ->
  select_elt


(** Creates a [<select>] tag for int32 values. *)
val int32_multiple_select :
  ?a:select_attrib_t ->
  ?required:pcdata_elt ->
  name:[ `Set of int32 ] param_name ->
  int32 select_opt ->
  int32 select_opt list ->
  select_elt


(** Creates a [<select>] tag for int64 values. *)
val int64_multiple_select :
  ?a:select_attrib_t ->
  ?required:pcdata_elt ->
  name:[ `Set of int64 ] param_name ->
  int64 select_opt ->
  int64 select_opt list ->
  select_elt


(** Creates a [<select>] tag for float values. *)
val float_multiple_select :
  ?a:select_attrib_t ->
  ?required:pcdata_elt ->
  name:[ `Set of float ] param_name ->
  float select_opt ->
  float select_opt list ->
  select_elt


(** Creates a [<select>] tag for string values. *)
val string_multiple_select :
  ?a:select_attrib_t ->
  ?required:pcdata_elt ->
  name:[ `Set of string ] param_name ->
  string select_opt ->
  string select_opt list ->
  select_elt


(** Creates a [<select>] tag for user type values. *)
val user_type_multiple_select :
  ('a -> string) ->
  ?a:select_attrib_t ->
  ?required:pcdata_elt ->
  name:[ `Set of 'a ] param_name ->
  'a select_opt ->
  'a select_opt list ->
  select_elt


(** Creates a [<select>] tag for any (untyped) value. *)
val raw_multiple_select :
  ?a:select_attrib_t ->
  ?required:pcdata_elt ->
  name:string ->
  string select_opt ->
  string select_opt list ->
  select_elt

val a_for: 'a Eliom_parameter.param_name -> for_attrib
