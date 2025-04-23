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

  val make_uri :
    ?absolute:bool ->
    ?absolute_path:bool ->
    ?https:bool ->
    service:
      ('get, unit, Eliom_service.get, _, _, _, _, _, _, unit, _) Eliom_service.t ->
    ?hostname:string ->
    ?port:int ->
    ?fragment:string ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?nl_params:Eliom_parameter.nl_params_set ->
    'get ->
    uri
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
      see {!Eliom_service.create} for a detailed description.

      The optional parameter [nl_params] allows one to add non
      localized GET parameter to the URL.  See the eliom manual for
      more information about {% <<a_manual chapter="server-params"
      fragment="nonlocalizedparameters"|non localized
      parameters>>%}.  *)

  val uri_of_string : (unit -> string) -> uri
  (** The function [uri_of_string f] returns a URI whose content is
      equivalent to [f ()].

      For XML tree build with TyXML, like {!Html} or {!Svg.F}, the
      function [f] is applied each time the XML tree is sent to the
      client (either as page content or as a marshalled OCaml
      value). Hence, the function is always evaluated in the context
      of a service handler.

      For other module, the function [f] is immediately applied. *)

  val css_link :
    ?a:[< Html_types.link_attrib ] attrib list ->
    uri:uri ->
    unit ->
    [> Html_types.link ] elt
  (** The function [css_link ~uri ()] creates a [<link>] node that
      reference a Cascading StyleSheet (CSS).

      If the CSS is generated by an Eliom service, use {!make_uri} to
      calculate the service URI. If the CSS is a static file, you may
      also use {!Eliom_service.static_dir} or {!Eliom_service.extern}
      to abstract the file with a service.

      The optional parameter [~a] allows one to add extra HTML
      attributes to the generated node.  *)

  val js_script :
    ?a:[< Html_types.script_attrib ] attrib list ->
    uri:uri ->
    unit ->
    [> Html_types.script ] elt
  (** The function [js_script ~uri ()] creates a [<script>] node that
      reference a javascript file.

      If the script content is generated by an Eliom service, use
      {!make_uri} to calculate the service URI. If it is a static
      file, you may also use {!Eliom_service.static_dir} or
      {!Eliom_service.extern} to abstract the file with a service.

      The optional parameter [~a] allows one to add extra HTML
      attributes to the generated node.  *)

  val a :
    ?absolute:bool ->
    ?absolute_path:bool ->
    ?https:bool ->
    ?a:[< Html_types.a_attrib ] attrib list ->
    service:
      ( 'get,
        unit,
        Eliom_service.get,
        _,
        _,
        _,
        _,
        _,
        _,
        unit,
        Eliom_service.non_ocaml )
      Eliom_service.t ->
    ?hostname:string ->
    ?port:int ->
    ?fragment:string ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?nl_params:Eliom_parameter.nl_params_set ->
    ?xhr:bool ->
    'a elt list ->
    'get ->
    [> 'a Html_types.a ] elt
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
      The default value of [~xhr] is configurable through
      {% <<a_api project="eliom" subproject="server" | val
      Eliom_config.set_default_links_xhr >> %}.

      The optional parameter [~a] allows one to add extra HTML
      attributes to the generated node.

      See {!make_uri} for description of other optional
      parameters.

      To create a [<a>] node that is not associated to a service, use
      [Raw.a].
  *)
end

module type S = sig
  (* When modifying this interface, please ensure that the ocamldoc is
   coherent with the ocamldoc from Eliom_uri. *)

  open Eliom_lib
  open Eliom_parameter

  type 'a param
  type +'a elt
  type +'a attrib
  type uri

  val float : float param
  val int : int param
  val int32 : int32 param
  val int64 : int64 param
  val nativeint : nativeint param
  val bool : bool param
  val string : string param
  val user : ('a -> string) -> 'a param

  val make_post_uri_components :
    ?absolute:bool ->
    ?absolute_path:bool ->
    ?https:bool ->
    service:
      ('get, 'post, Eliom_service.post, _, _, _, _, _, _, _, _) Eliom_service.t ->
    ?hostname:string ->
    ?port:int ->
    ?fragment:string ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?nl_params:Eliom_parameter.nl_params_set ->
    ?keep_get_na_params:bool ->
    'get ->
    'post ->
    string
    * (string * Eliommod_parameters.param) list
    * string option
    * (string * Eliommod_parameters.param) list
  (** Same as {!LINK.make_uri_components}, but also returns a list of
      post parameters. *)

  val get_form :
    ?absolute:bool ->
    ?absolute_path:bool ->
    ?https:bool ->
    ?a:[< Html_types.form_attrib ] attrib list ->
    service:
      ( _,
        unit,
        Eliom_service.get,
        _,
        _,
        _,
        _,
        _,
        'gn,
        _,
        Eliom_service.non_ocaml )
      Eliom_service.t ->
    ?hostname:string ->
    ?port:int ->
    ?fragment:string ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?nl_params:Eliom_parameter.nl_params_set ->
    ?xhr:bool ->
    ('gn -> [< Html_types.form_content ] elt list) ->
    [> Html_types.form ] elt
  (** The function [get_form service formgen] creates a GET [<form>]
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
      {% <<a_api project="eliom" subproject="server" | val
      Eliom_config.set_default_links_xhr >> %}.

      The optional parameter [~a] allows one to add extra HTML
      attributes to the generated node.

      See {!make_uri} for description of other optional
      parameters.  *)

  val lwt_get_form :
    ?absolute:bool ->
    ?absolute_path:bool ->
    ?https:bool ->
    ?a:[< Html_types.form_attrib ] attrib list ->
    service:
      ( _,
        unit,
        Eliom_service.get,
        _,
        _,
        _,
        _,
        _,
        'gn,
        _,
        Eliom_service.non_ocaml )
      Eliom_service.t ->
    ?hostname:string ->
    ?port:int ->
    ?fragment:string ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?nl_params:Eliom_parameter.nl_params_set ->
    ?xhr:bool ->
    ('gn -> [< Html_types.form_content ] elt list Lwt.t) ->
    [> Html_types.form ] elt Lwt.t
  (** Same as {!get_form} but taking a cooperative function for
      [<form>] content generation. *)

  val post_form :
    ?absolute:bool ->
    ?absolute_path:bool ->
    ?https:bool ->
    ?a:[< Html_types.form_attrib ] attrib list ->
    service:
      ( 'get,
        _,
        Eliom_service.post,
        _,
        _,
        _,
        _,
        _,
        _,
        'pn,
        Eliom_service.non_ocaml )
      Eliom_service.t ->
    ?hostname:string ->
    ?port:int ->
    ?fragment:string ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?keep_get_na_params:bool ->
    ?nl_params:Eliom_parameter.nl_params_set ->
    ?xhr:bool ->
    ('pn -> [< Html_types.form_content ] elt list) ->
    'get ->
    [> Html_types.form ] elt
  (** The function [post_form service formgen post_params] creates a
      POST [<form>] to [service] preapplied to the POST parameters
      [post_params]. The content of the [<form>] is generated by the
      function [formgen], that takes the names of the service
      parameters as parameters. By default, the [action] attribute is
      a relative URL recomputed at each request with {!make_uri}.

      The optional parameter [~a] allows one to add HTML attributes to
      the generated node.

      See {!Eliom_service.make} for a description of the
      [~keep_get_na_params] optional parameter ; see {!get_form} for
      [~xhr] and see {!make_uri} for other optional parameters.  *)

  val lwt_post_form :
    ?absolute:bool ->
    ?absolute_path:bool ->
    ?https:bool ->
    ?a:[< Html_types.form_attrib ] attrib list ->
    service:
      ( 'get,
        _,
        Eliom_service.post,
        _,
        _,
        _,
        _,
        _,
        _,
        'pn,
        Eliom_service.non_ocaml )
      Eliom_service.t ->
    ?hostname:string ->
    ?port:int ->
    ?fragment:string ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?keep_get_na_params:bool ->
    ?nl_params:Eliom_parameter.nl_params_set ->
    ?xhr:bool ->
    ('pn -> [< Html_types.form_content ] elt list Lwt.t) ->
    'get ->
    [> Html_types.form ] elt Lwt.t
  (** Same as {!post_form} but taking a cooperative function for
      [<form>] content generation. *)

  val input :
    ?a:[< Html_types.input_attrib ] attrib list ->
    input_type:[< Html_types.input_type ] ->
    ?name:[< 'a setoneradio ] param_name ->
    ?value:'a ->
    'a param ->
    [> Html_types.input ] elt
  (** Creates an [<input>] tag. *)

  val file_input :
    ?a:[< Html_types.input_attrib ] attrib list ->
    name:[< file_info setoneradio ] param_name ->
    unit ->
    [> Html_types.input ] elt
  (** Creates an [<input>] tag for sending a file *)

  val image_input :
    ?a:[< Html_types.input_attrib ] attrib list ->
    name:[< coordinates oneradio ] param_name ->
    ?src:uri ->
    unit ->
    [> Html_types.input ] elt
  (** Creates an [<input type="image" name="...">] tag. The server
      receives the coordinates that the user clicked on. *)

  val checkbox :
    ?a:[< Html_types.input_attrib ] attrib list ->
    ?checked:bool ->
    name:[ `Set of 'a ] Eliom_parameter.param_name ->
    value:'a ->
    'a param ->
    [> Html_types.input ] elt
  (** Creates a checkbox [<input>] tag. You can produce several
      checkboxes with the same name (and different values). The
      service must declare a parameter of type [set]. *)

  val bool_checkbox_one :
    ?a:[< Html_types.input_attrib ] attrib list ->
    ?checked:bool ->
    name:[ `One of bool ] Eliom_parameter.param_name ->
    unit ->
    [> Html_types.input ] elt
  (** Creates a checkbox [<input>] tag of type bool. Only one checkbox
      with the same [name] is allowed. *)

  val radio :
    ?a:[< Html_types.input_attrib ] attrib list ->
    ?checked:bool ->
    name:[ `Radio of 'a ] param_name ->
    value:'a ->
    'a param ->
    [> Html_types.input ] elt
  (** Creates a radio [<input>] tag. *)

  val string_radio_required :
    ?a:[< Html_types.input_attrib ] attrib list ->
    ?checked:bool ->
    name:[ `One of string ] param_name ->
    value:string ->
    unit ->
    [> Html_types.input ] elt

  val button :
    ?a:[< Html_types.button_attrib ] attrib list ->
    button_type:[< button_type ] ->
    name:[< 'a setone ] param_name ->
    value:'a ->
    'a param ->
    Html_types.button_content elt list ->
    [> Html_types.button ] elt
  (** Creates a [<button>] tag. *)

  val button_no_value :
    ?a:[< Html_types.button_attrib ] attrib list ->
    button_type:[< button_type ] ->
    Html_types.button_content elt list ->
    [> Html_types.button ] elt
  (** Creates a [<button>] tag with no value. No value is sent. *)

  val textarea :
    ?a:[< Html_types.textarea_attrib ] attrib list ->
    name:[< string setoneradio ] param_name ->
    ?value:string ->
    unit ->
    [> Html_types.textarea ] elt
  (** Creates a [<textarea>] tag *)

  type 'a soption =
    Html_types.option_attrib attrib list
    * 'a
    (* Content (or value if the following is present) *)
    * Html_types.pcdata elt option
    (* if content different from value *)
    * bool
  (* selected *)

  (** The type for [<select>] options and groups of options.

      - The field of type 'a in [soption] is the value that will be
        sent by the form.
      - If the [Html_types.pcdata elt option] is not present it is
        also the value displayed.
      - The string in [select_opt] is the label *)
  type 'a select_opt =
    | Optgroup of
        [ Html_types.common | `Disabled ] attrib list
        * string (* label *)
        * 'a soption
        * 'a soption list
    | Option of 'a soption

  val select :
    ?a:[< Html_types.select_attrib ] attrib list ->
    ?required:Html_types.pcdata elt ->
    name:[ `One of 'a ] param_name ->
    'a param ->
    'a select_opt ->
    'a select_opt list ->
    [> Html_types.select ] elt
  (** Creates a [<select>] tag. *)

  val multiple_select :
    ?a:[< Html_types.select_attrib ] attrib list ->
    ?required:Html_types.pcdata elt ->
    name:[ `Set of 'a ] param_name ->
    'a param ->
    'a select_opt ->
    'a select_opt list ->
    [> Html_types.select ] elt
  (** Creates a multiple-selection [<select>] tag. *)
end
