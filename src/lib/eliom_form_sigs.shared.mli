(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015 Vasilis Papavasileiou
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

type button_type = [ `Button | `Reset | `Submit ]

module type LINKS = sig

  type +'a elt
  type +'a attrib
  type uri

  (** The function [make_uri service get_params] returns the URL of
      the service [service] applied to the GET parameters
      [get_params]. By default the returned URL is relative to the
      current request URL but it is absolute when one of the following
      conditions is met:

      - the optional parameter [~absolute_path] is [true].
      - the optional parameter [~absolute] is [true].
      - the optional parameter [~https] is [true] (resp. [false]) and
        the current request protocol is [http] (resp. [https]).
      - the optional parameter [~https] is [true] and the function is
        used outside of a service handler
      - the [service] has been created with [~https:true] and the
        current request protocol is [http].
      - the [service] has been created with [~https:true] and the
        function is used outside of a service handler.

      When only the first condition is met ([~absolute_path] is
      [true]) the returned URL is just the absolute path, but when any
      other condition is satisfied the returned URL is prefixed with
      [protocol://hostname\[:port\]], where:

      - [protocol] is:
        {ul {- [https] if the [service] has been created with
             [~https:true] or the optional parameter [~https] is
             [true];}
            {- [http] if the optional parameter [~https] is [false];}
            {- the current request protocol if available;}
            {- [http] in any other case.}}
      - [hostname] is:
        {ul {- the optional parameter [~hostname] if given;}
            {- the attribute [defaulthostname] of [<host>] tag in
               configuration file or the machine hostname if the
               option [<usedefaulthostname/>] is set;}
            {- the [Host] http header of the current request if
               available;}
            {- the attribute [defaulthostname] of [<host>] tag in
               configuration file or the machine hostname in any other
               case.}}
      - [port] is:
        {ul {- the optional parameter [~port] if given;}
            {- the attribute [defaulthttpsport]
               (resp. [defaulthttpport]) of [<host>] tag in
               configuration file or [443] (resp. 80) if [protocol] is
               [https] (resp. [http]) and the current request protocol
               is [http] (resp. [https]);}
            {- the attribute [defaulthttpsport]
               (resp. [defaulthttpsport]) of [<host>] tag in
               configuration file or [443] (resp. 80) if the option
               [<usedefaulthostname/>] is set and [protocol] is
               [https] (resp. [http]);}
            {- the port associated to the [Host] http header of the
               current request if available;}
            {- the incoming port of the current request if available;}
            {- the attribute [defaulthttpport]
               (resp. [defaulthttpsport]) of [<host>] tag in
               configuration file or [80] (resp. [443]) in any other
               case.}}

      If given the optional parameter [~fragment] is prefixed by [#]
      and appended to the URL.

      The optional parameter [keep_nl_params] allows one to override
      the [keep_nl_params] parameter used when creating the [service],
      see {!Eliom_service.Http.service} for a detailled description.

      The optional parameter [nl_params] allows one to add non
      localized GET parameter to the URL.  See the eliom manual for
      more information about {% <<a_manual chapter="server-params"
      fragment="nonlocalizedparameters"|non localized
      parameters>>%}.  *)
  val make_uri :
    ?absolute:bool ->
    ?absolute_path:bool ->
    ?https:bool ->
    service:('get, unit, [< Eliom_service.get_service_kind ], _, _,
             [< Eliom_service.suff ], 'gn, unit,
             [< Eliom_service.registrable ],
             'return) Eliom_service.service ->
    ?hostname:string ->
    ?port:int ->
    ?fragment:string ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?nl_params: Eliom_parameter.nl_params_set ->
    'get ->
    uri

  (** The function [uri_of_string f] returns a URI whose content is
      equivalent to [f ()].

      For XML tree build with TyXML, like {!Html5} or {!Svg.F}, the
      function [f] is applied each time the XML tree is sent to the
      client (either as page content or as a marshalled OCaml
      value). Hence, the function is always evaluated in the context
      of a service handler.

      For other module, the function [f] is immediatly applied. *)
  val uri_of_string : (unit -> string) -> uri

  (** The function [css_link ~uri ()] creates a [<link>] node that
      reference a Cascading StyleSheet (CSS).

      If the CSS is generated by an Eliom service, use {!make_uri} to
      calculate the service URI. If the CSS is a static file, you may
      also use {!Eliom_service.static_dir} or
      {!Eliom_service.Http.external_service} to abstract the file with
      a service.

      The optional parameter [~a] allows one to add extra HTML
      attributes to the generated node.  *)
  val css_link :
    ?a:Html5_types.link_attrib attrib list ->
    uri:uri -> unit -> [> Html5_types.link] elt

  (** The function [js_script ~uri ()] creates a [<script>] node that
      reference a javascript file.

      If the script content is generated by an Eliom service, use
      {!make_uri} to calculate the service URI. If it is a static
      file, you may also use {!Eliom_service.static_dir} or
      {!Eliom_service.Http.external_service} to abstract the file with
      a service.

      The optional parameter [~a] allows one to add extra HTML
      attributes to the generated node.  *)
  val js_script :
    ?a:Html5_types.script_attrib attrib list -> uri:uri -> unit ->
    [> Html5_types.script] elt

  (** The function [a service a_content get_params] creates a [<a>]
      node that link to [service] applied to GET parameters
      [get_params] and whose content is [a_content]. By default, the
      [href] attribute is a relative URL recomputed at each request
      with {!make_uri}.

      By default, the link is implemented in a way that allows the
      client-side Eliom application to keep running, irrespectable of
      the usage of the link (cf. {% <<a_api project="eliom"
      subproject="client" | val Eliom_client.change_page>> %}).

      By contrast, if the optional parameter [~xhr:false] is given,
      the link is realized as a standard HTML link and clicking it
      discontinues the Eliom application.  The [~xhr] parameter has no
      effect outside an Eliom application.

      NB that the default value of [~xhr] is configurable through
      <<a_api project="eliom" subproject="server" | val
      Eliom_config.set_default_links_xhr >>

      The optional parameter [~a] allows one to add extra HTML
      attributes to the generated node.

      See {!make_uri} for description of other optional
      parameters.  *)
  val a :
    ?absolute:bool ->
    ?absolute_path:bool ->
    ?https:bool ->
    ?a:Html5_types.a_attrib attrib list ->
    service:('get, unit, [< Eliom_service.get_service_kind ], _, _,
             [< Eliom_service.suff ], 'd, unit,
             [< Eliom_service.registrable ],
             [< Eliom_service.non_ocaml_service])
        Eliom_service.service ->
    ?hostname:string ->
    ?port:int ->
    ?fragment:string ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?nl_params: Eliom_parameter.nl_params_set ->
    ?xhr:bool ->
    'a elt list ->
    'get ->
    [> 'a Html5_types.a] elt

end

module type S = sig

  (* When modifying this interface, please ensure that the ocamldoc is
   coherent with the ocamldoc from Eliom_uri. *)

  open Eliom_lib

  open Eliom_parameter
  open Eliom_service

  type 'a param

  type +'a elt
  type +'a attrib

  type uri

  val float : float param
  val int : int param
  val int32 : int32 param
  val int64: int64 param
  val nativeint : nativeint param
  val bool : bool param
  val string : string param

  (** Same as {!LINK.make_uri_components}, but also returns a list of
      post parameters. *)
  val make_post_uri_components :
    ?absolute:bool ->
    ?absolute_path:bool ->
    ?https:bool ->
    service:('get, 'post, [< post_service_kind ], _, _,
             [< suff ], 'gn, 'pn,
             [< registrable ], 'return) service ->
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

  (** The function [post_form service formgen] creates a GET [<form>]
      to [service]. The content of the [<form>] is generated by the
      function [formgen], that takes the names of the service
      parameters as parameters. By default, the [action] attribute is
      a relative URL recomputed at each request with {!make_uri}.

      By default, the form is realized such that the client-side Eliom
      application keeps running irrespectable of the usage of the form
      (cf. {% <<a_api project="eliom" subproject="client" | val
      Eliom_client.change_page>> %}).

      By contrast, if the optional parameter [~xhr:false] is given,
      the form is realized as a standard HTML form and submitting it
      discontinues the Eliom application.  The [~xhr] parameter has no
      effect outside an Eliom application.

      NB that the default value of [~xhr] is configurable through
      <<a_api project="eliom" subproject="server" | val
      Eliom_config.set_default_links_xhr >>

      The optional parameter [~a] allows one to add extra HTML
      attributes to the generated node.

      See {!make_uri} for description of other optional
      parameters.  *)
  val get_form :
    ?absolute:bool ->
    ?absolute_path:bool ->
    ?https:bool ->
    ?a:Html5_types.form_attrib attrib list ->
    service:('get, unit, [< get_service_kind ], _, _,
             [<suff ], 'gn, 'pn,
             [< registrable ], [< non_ocaml_service]) service ->
    ?hostname:string ->
    ?port:int ->
    ?fragment:string ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?nl_params: Eliom_parameter.nl_params_set ->
    ?xhr:bool ->
    ('gn -> Html5_types.form_content elt list) ->
    [> Html5_types.form ] elt

  (** Same as {!get_form} but taking a cooperative function for
      [<form>] content generation. *)
  val lwt_get_form :
    ?absolute:bool ->
    ?absolute_path:bool ->
    ?https:bool ->
    ?a:Html5_types.form_attrib attrib list ->
    service:('get, unit, [< get_service_kind ], _, _,
             [<suff ], 'gn, 'pn,
             [< registrable ], [< non_ocaml_service]) service ->
    ?hostname:string ->
    ?port:int ->
    ?fragment:string ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?nl_params: Eliom_parameter.nl_params_set ->
    ?xhr:bool ->
    ('gn -> Html5_types.form_content elt list Lwt.t) ->
    [> Html5_types.form ] elt Lwt.t

  (** The function [post_form service formgen get_params] creates a
      POST [<form>] to [service] preapplied to the GET parameters
      [get_params]. The content of the [<form>] is generated by the
      function [formgen], that takes the names of the service
      parameters as parameters. By default, the [action] attribute is
      a relative URL recomputed at each request with {!make_uri}.

      The optional parameter [~a] allows one to add HTML attributes to
      the generated node.

      See {!Eliom_service.Http.post_coservice'} for a description of
      the [~keep_get_na_params] optional parameter ; see {!get_form}
      for [~xhr] and see {!make_uri} for other optional
      parameters.  *)
  val post_form :
    ?absolute:bool ->
    ?absolute_path:bool ->
    ?https:bool ->
    ?a:Html5_types.form_attrib attrib list ->
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
    ('pn -> Html5_types.form_content elt list) ->
    'get ->
    [> Html5_types.form ] elt

  (** Same as {!post_form} but taking a cooperative function for
      [<form>] content generation. *)
  val lwt_post_form :
    ?absolute:bool ->
    ?absolute_path:bool ->
    ?https:bool ->
    ?a:Html5_types.form_attrib attrib list ->
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
    ('pn -> Html5_types.form_content elt list Lwt.t) ->
    'get ->
    [> Html5_types.form ] elt Lwt.t

  (** Creates an [<input>] tag. *)
  val input :
    ?a:Html5_types.input_attrib attrib list ->
    input_type:[< Html5_types.input_type] ->
    ?name:[< 'a setoneradio] param_name ->
    ?value:'a ->
    'a param ->
    [> Html5_types.input] elt

  (** Creates an [<input>] tag for a user type *)
  val user_type_input : ('a -> string) ->
    ?a:Html5_types.input_attrib attrib list ->
    input_type:[< Html5_types.input_type] ->
    ?name:[< 'a setoneradio ] param_name ->
    ?value:'a -> unit ->
    [> Html5_types.input] elt

  (** Creates an untyped [<input>] tag. You may use the name you want
      (for example to use with {!Eliom_parameter.any}). *)
  val raw_input :
    ?a:Html5_types.input_attrib attrib list ->
    input_type:[< Html5_types.input_type] ->
    ?name:string -> ?value:string -> unit ->
    [> Html5_types.input] elt

  (** Creates an [<input>] tag for sending a file *)
  val file_input :
    ?a:Html5_types.input_attrib attrib list ->
    name:[< file_info setoneradio ] param_name ->
    unit ->
    [> Html5_types.input] elt

  (** Creates an [<input type="image" name="...">] tag. *)
  val image_input :
    ?a:Html5_types.input_attrib attrib list ->
    name:[< 'a oneradio ] param_name ->
    value:'a ->
    ?src:uri ->
    'a param ->
    [> Html5_types.input] elt

  (** Creates an [<input type="image" name="..." value="...">] tag
      that sends the coordinates the user clicked on and a value of
      user defined type *)
  val user_type_image_input : ('a -> string) ->
    ?a:Html5_types.input_attrib attrib list ->
    name:[< ('a * coordinates) oneradio ] param_name -> value:'a ->
    ?src:uri -> unit ->
    [> Html5_types.input] elt

  (** Creates an [<input type="image" name="..." value="...">] tag
      that sends the coordinates the user clicked on and an untyped
      value *)
  val raw_image_input :
    ?a:Html5_types.input_attrib attrib list ->
    name:string -> value:string -> ?src:uri -> unit ->
    [> Html5_types.input] elt

  (** Creates a checkbox [<input>] tag. *)
  val checkbox :
    ?a:Html5_types.input_attrib attrib list -> ?checked:bool ->
    name:[ `One of 'a ] Eliom_parameter.param_name -> value:'a ->
    'a param ->
    [> Html5_types.input] elt

  (** Creates a checkbox [<input>] tag that will have a "user type"
      value.  Thus you can do several checkboxes with the same name
      (and different values).  The service must declare a parameter of
      type [set]. *)
  val user_type_checkbox : ('a -> string) ->
    ?a:Html5_types.input_attrib attrib list -> ?checked:bool ->
    name:[ `Set of 'a ] param_name -> value:'a -> unit ->
    [> Html5_types.input] elt

  (** Creates a checkbox [<input>] tag with untyped content.  Thus you
      can do several checkboxes with the same name (and different
      values).  The service must declare a parameter of type [any]. *)
  val raw_checkbox :
    ?a:Html5_types.input_attrib attrib list -> ?checked:bool ->
    name:string -> value:string -> unit -> [> Html5_types.input] elt

  (** Creates a radio [<input>] tag. *)
  val radio :
    ?a:Html5_types.input_attrib attrib list -> ?checked:bool ->
    name:[ `Radio of 'a ] param_name ->
    value:'a ->
    'a param ->
    [> Html5_types.input] elt

  val string_radio_required :
    ?a:Html5_types.input_attrib attrib list -> ?checked:bool ->
    name:[ `One of string ] param_name ->
    value:string -> unit ->
    [> Html5_types.input] elt

  (** Creates a radio [<input>] tag with user_type content *)
  val user_type_radio : ('a -> string) ->
    ?a:Html5_types.input_attrib attrib list -> ?checked:bool ->
    name:[ `Radio of 'a ] param_name ->
    value:'a -> unit ->
    [> Html5_types.input] elt

  (** Creates a radio [<input>] tag with untyped string content (low
      level) *)
  val raw_radio :
    ?a:Html5_types.input_attrib attrib list -> ?checked:bool ->
    name:string -> value:string -> unit ->
    [> Html5_types.input] elt

  (** Creates a [<button>] tag. *)
  val button :
    ?a:Html5_types.button_attrib attrib list ->
    name:[< 'a setone ] param_name ->
    value:'a ->
    'a param ->
    Html5_types.button_content elt list ->
    [> Html5_types.button] elt

  (** Creates a [<button>] tag with user_type content *)
  val user_type_button : ('a -> string) ->
    ?a:Html5_types.button_attrib attrib list ->
    name:[< 'a setone ] param_name -> value:'a ->
    Html5_types.button_content elt list ->
    [> Html5_types.button] elt

  (** Creates a [<button>] tag with untyped string content *)
  val raw_button :
    ?a:Html5_types.button_attrib attrib list ->
    button_type:[< button_type] ->
    name:string -> value:string ->
    Html5_types.button_content elt list ->
    [> Html5_types.button] elt

  (** Creates a [<button>] tag with no value. No value is sent. *)
  val button_no_value :
    ?a:Html5_types.button_attrib attrib list ->
    button_type:[< button_type] ->
    Html5_types.button_content elt list ->
    [> Html5_types.button] elt

  (** Creates a [<textarea>] tag *)
  val textarea :
    ?a:Html5_types.textarea_attrib attrib list ->
    name:[< string setoneradio ] param_name -> ?value:string ->
    unit -> [> Html5_types.textarea] elt

  (** Creates a [<textarea>] tag for untyped form *)
  val raw_textarea :
    ?a:Html5_types.textarea_attrib attrib list ->
    name:string -> ?value:string -> unit ->
    [> Html5_types.textarea] elt

  type 'a soption =
    Html5_types.option_attrib attrib list
    * 'a (* Content (or value if the following is present) *)
    * Html5_types.pcdata elt option (* if content different from value *)
    * bool (* selected *)

  (** The type for [<select>] options and groups of options.

      - The field of type 'a in [soption] is the value that will be
        sent by the form.
      - If the [Html5_types.pcdata elt option] is not present it is
        also the value displayed.
      - The string in [select_opt] is the label *)
  type 'a select_opt =
    | Optgroup of
        [ Html5_types.common | `Disabled ] attrib list
        * string (* label *)
        * 'a soption
        * 'a soption list
    | Option of 'a soption

  (** Creates a [<select>] tag. *)
  val select :
    ?a:Html5_types.select_attrib attrib list ->
    ?required:Html5_types.pcdata elt ->
    name:[ `One of 'a ] param_name ->
    'a param ->
    'a select_opt ->
    'a select_opt list ->
    [> Html5_types.select] elt

  (** Creates a [<select>] tag for user type values. *)
  val user_type_select : ('a -> string) ->
    ?a:Html5_types.select_attrib attrib list ->
    ?required:Html5_types.pcdata elt ->
    name:[ `One of 'a ] param_name ->
    'a select_opt ->
    'a select_opt list ->
    [> Html5_types.select] elt

  (** Creates a [<select>] tag for any (untyped) value. *)
  val raw_select :
    ?a:Html5_types.select_attrib attrib list ->
    ?required:Html5_types.pcdata elt ->
    name:string ->
    string select_opt ->
    string select_opt list ->
    [> Html5_types.select] elt

  (** Creates a multiple-selection [<select>] tag. *)
  val multiple_select :
    ?a:Html5_types.select_attrib attrib list ->
    ?required:Html5_types.pcdata elt ->
    name:[ `Set of 'a ] param_name ->
    'a param ->
    'a select_opt ->
    'a select_opt list ->
    [> Html5_types.select] elt

  (** Creates a [<select>] tag for user type values. *)
  val user_type_multiple_select :
    ('a -> string) ->
    ?a:Html5_types.select_attrib attrib list ->
    ?required:Html5_types.pcdata elt ->
    name:[ `Set of 'a ] param_name ->
    'a select_opt ->
    'a select_opt list ->
    [> Html5_types.select] elt

  (** Creates a [<select>] tag for any (untyped) value. *)
  val raw_multiple_select :
    ?a:Html5_types.select_attrib attrib list ->
    ?required:Html5_types.pcdata elt ->
    name:string ->
    string select_opt ->
    string select_opt list ->
    [> Html5_types.select] elt

end
