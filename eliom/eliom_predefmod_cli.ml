(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_predefmod
 * Copyright (C) 2007 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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




open Eliom_mkforms
open Eliom_services
open Eliom_parameters
open Eliom_sessions


module Xhtmlforms_ = struct
  open XHTML.M
  open Xhtmltypes

  type form_content_elt = form_content elt
  type form_content_elt_list = form_content elt list
  type uri = XHTML.M.uri

  type a_content_elt = a_content elt
  type a_content_elt_list = a_content elt list

  type div_content_elt = div_content elt
  type div_content_elt_list = div_content elt list

  type a_elt = a elt
  type a_elt_list = a elt list
  type form_elt = form elt

  type textarea_elt = textarea elt
  type input_elt = input elt

  type link_elt = link elt
  type script_elt = script elt

  type pcdata_elt = pcdata elt

  type select_elt = select elt
  type select_content_elt = select_content elt
  type select_content_elt_list = select_content elt list
  type option_elt = selectoption elt
  type option_elt_list = selectoption elt list

  type button_elt = button elt
  type button_content_elt = button_content elt
  type button_content_elt_list = button_content elt list

  type a_attrib_t = Xhtmltypes.a_attrib XHTML.M.attrib list
  type form_attrib_t = Xhtmltypes.form_attrib XHTML.M.attrib list
  type input_attrib_t = Xhtmltypes.input_attrib XHTML.M.attrib list
  type textarea_attrib_t = Xhtmltypes.textarea_attrib XHTML.M.attrib list
  type select_attrib_t = Xhtmltypes.select_attrib XHTML.M.attrib list
  type link_attrib_t = Xhtmltypes.link_attrib XHTML.M.attrib list
  type script_attrib_t = Xhtmltypes.script_attrib XHTML.M.attrib list
  type optgroup_attrib_t = [ common | `Disabled ] XHTML.M.attrib list
  type option_attrib_t = Xhtmltypes.option_attrib XHTML.M.attrib list
  type button_attrib_t = Xhtmltypes.button_attrib XHTML.M.attrib list

  type input_type_t =
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

  type button_type_t =
      [ `Button | `Reset | `Submit ]

  let hidden = `Hidden
  let checkbox = `Checkbox
  let radio = `Radio
  let submit = `Submit
  let file = `File
  let image = `Image

  let buttonsubmit = `Submit

  let uri_of_string = XHTML.M.uri_of_string

  let empty_seq = []
  let cons_form a l = a::l

  let map_option = List.map
  let map_optgroup f a l = ((f a), List.map f l)
  let select_content_of_option a = (a :> select_content elt)

  let make_pcdata s = pcdata s

  let make_a ?(a=[]) ?href ?onclick l : a_elt =
    let a = match href with
      | None -> a
      | Some v -> (a_href (uri_of_string v))::a
    in
    let a = match onclick with
      | None -> a
      | Some v -> (a_onclick v)::a
    in
    XHTML.M.a ~a l

  let make_get_form ?(a=[]) ~action elt1 elts : form_elt =
    form ~a:((a_method `Get)::a)
      ~action:(uri_of_string action) elt1 elts

  let make_post_form ?(a=[]) ~action ?id ?(inline = false) elt1 elts
      : form_elt =
    let aa = (match id with
    | None -> a
    | Some i -> (a_id i)::a)
    in
    form ~a:((XHTML.M.a_enctype "multipart/form-data")::
             (* Always Multipart!!! How to test if there is a file?? *)
             (a_method `Post)::
             (if inline then (a_class ["inline"])::aa else aa))
      ~action:(uri_of_string action) elt1 elts

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

  let register_event node = XML.register_event (XHTML.M.toelt node)

end



(*****************************************************************************)
(*****************************************************************************)

module Xhtmlforms' = MakeForms(Xhtmlforms_)

module type XHTMLFORMSSIG = sig
(* Pasted from mli *)




  open XHTML.M
  open Xhtmltypes

(** {2 Links and forms} *)




    val make_string_uri :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      service:('get, unit, [< get_service_kind ],
               [< suff ], 'gn, unit,
               [< registrable ], 'return) service ->
      ?sp:Eliom_sessions.server_params ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      'get -> 
      string
(** Creates the string corresponding to the relative URL of a service applied to
    its GET parameters.

    If [absolute] is set to [true], or if there is a protocol change,
    the URL will be absolute.
    
    If [absolute_path] is set to [true], and [absolute] is [false],
    the URL will be absolute, but without [protocol://server:port].
    
    Default hostname is determined from the [Host] http header of the request
    (or the attribute of <host> tag in
    configuration file if the option [<usedefaulthostname/>] is set).
    Default port is the current port (or another port of the server if
    you are switching from or to https).
    But you can choose the hostname or port you want by setting 
    the optional [?hostname] and [?port] parameters here.

 *)

    val make_uri :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      service:('get, unit, [< get_service_kind ],
               [< suff ], 'gn, unit,
               [< registrable ], 'return) service ->
      ?sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string -> 
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      'get -> 
      uri
(** Creates the URL for a service.
    Like the [a] function, it may take extra parameters. *)

    val make_uri_components :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      service:('get, unit, [< get_service_kind ],
               [< suff ], 'gn, unit,
               [< registrable ], 'return) service ->
      ?sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string -> 
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      'get -> 
      string * (string * string) list * string option
(** Creates the URL for a service.
    Returns the path (as a string, encoded),
    the association list of get parameters (not encoded),
    and the fragment (not encoded, if any).
    Like the [a] function, it may take extra parameters. *)

    val make_post_uri_components :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      service:('get, 'post, [< post_service_kind ],
               [< suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string -> 
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ?keep_get_na_params:bool ->
      'get -> 
      'post ->
      string * (string * string) list * string option * (string * string) list
(** Like [make_uri_components], but also creates a table of post parameters. *)

    val make_proto_prefix :
      ?sp:Eliom_sessions.server_params ->
      ?hostname:string ->
      ?port:int ->
      bool ->
      string
(** Creates the string corresponding to the beginning of the URL,
    containing the scheme (protocol), server and port number (if necessary).
 *)

    val a :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:a_attrib attrib list ->
      service:('get, unit, [< get_service_kind ],
               [< suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ?use_href:bool ->
      a_content elt list -> 
      'get -> 
    [> a] XHTML.M.elt
(** [a service sp cont ()] creates a link to [service].
   The text of
   the link is [cont]. For example [cont] may be something like
   [\[pcdata "click here"\]].

   The last  parameter is for GET parameters.
   For example [a service sp cont (42,"hello")]

   The [~a] optional parameter is used for extra attributes.

   The [~fragment] optional parameter is used for the "fragment" part
   of the URL, that is, the part after character "#".

    When possible, all links generated by Eliom are relative, for example
    to make easier the use with reverse proxies.
    But in case of protocol change (if you want to switch to https using
    [~https:true] for example, or if the service imposes https),
    absolute links will be generated. 
    In that case,
    default hostname is determined from the [Host] http header of the request
    (or the attribute of <host> tag in
    configuration file if the option [<usedefaulthostname/>] is set).
    Default port is the current port (or another port of the server if
    you are switching from or to https).
    But you can choose the hostname or port you want by setting 
    the optional [?hostname] and [?port] parameters here.
    These options have no effect for relative links.

    You can add non-localized parameters using the optional parameter
    [nl_params]. See {!Eliom_parameters.nl_params_set}.

    If [~keep_nl_params] is [`Persistent] (resp. [`All]),
    persistent (resp all) non localized GET parameters
    will be kept in the URL (default is the default for the service).

    If a client side application is running, and unless
    [~use_href:true] is specified, it will use [<a onclick=...>]
    instead of [<a href=...>] in case of link inside a same Eliom application.
    Thus, the client side application will not be stopped when the link
    is clicked.

*)

    val css_link : ?a:link_attrib attrib list -> uri:uri -> unit -> [>link] elt
(** Creates a [<link>] tag for a Cascading StyleSheet (CSS). *)

    val js_script :
        ?a:script_attrib attrib list -> uri:uri -> unit -> [>script] elt
(** Creates a [<script>] tag to add a javascript file *)


    val get_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib attrib list ->
      service:('get, unit, [< get_service_kind ],
               [<suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ('gn -> form_content elt list) -> 
      [>form] elt
(** [get_form service sp formgen] creates a GET form to [service].
   The content of
   the form is generated by the function [formgen], that takes the names
   of the service parameters as parameters. *)

    val lwt_get_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib attrib list ->
      service:('get, unit, [< get_service_kind ],
               [<suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ('gn -> form_content elt list Lwt.t) -> 
      [>form] elt Lwt.t
(** The same but taking a cooperative function. *)


    val post_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib attrib list ->
      service:('get, 'post, [< post_service_kind ],
               [< suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?keep_get_na_params:bool ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ('pn -> form_content elt list) -> 
      'get -> 
      [>form] elt
(** [post_form service sp formgen] creates a POST form to [service].
    The last parameter is for GET parameters (as in the function [a]).

    If [~keep_nl_params] is [`Persistent] (resp. [`All]),
    persistent (resp all) non localized GET parameters
    will be kept in the URL (default is the default for the service).

 *)

    val lwt_post_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib attrib list ->
      service:('get, 'post, [< post_service_kind ],
               [< suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?keep_get_na_params:bool ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ('pn -> form_content elt list Lwt.t) -> 
      'get -> 
      [>form] elt Lwt.t
(** The same but taking a cooperative function. *)








(** {2 Form widgets} *)

  type basic_input_type =
      [
    | `Hidden
    | `Password
    | `Submit
    | `Text ]

  val int_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< int setoneradio ] param_name ->
          ?value:int -> unit -> [> input ] elt
(** Creates an [<input>] tag for an integer *)

  val int32_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< int32 setoneradio ] param_name ->
          ?value:int32 -> unit -> [> input ] elt
(** Creates an [<input>] tag for a 32 bits integer *)

  val int64_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< int64 setoneradio ] param_name ->
          ?value:int64 -> unit -> [> input ] elt
(** Creates an [<input>] tag for a 64 bits integer *)

  val float_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< float setoneradio ] param_name ->
          ?value:float -> unit -> [> input ] elt
(** Creates an [<input>] tag for a float *)

  val string_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< string setoneradio ] param_name ->
          ?value:string -> unit -> [> input ] elt
(** Creates an [<input>] tag for a string *)

  val user_type_input :
    ('a -> string) -> 
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< 'a setoneradio ] param_name ->
          ?value:'a -> unit -> [> input ] elt
(** Creates an [<input>] tag for a user type *)

  val raw_input :
      ?a:input_attrib attrib list ->
        input_type:[< basic_input_type | `Reset | `Button ] ->
        ?name:string -> ?value:string -> unit -> [> input ] elt
(** Creates an untyped [<input>] tag. You may use the name you want
   (for example to use with {!Eliom_parameters.any}).
 *)

  val file_input :
      ?a:input_attrib attrib list ->
        name:[< Ocsigen_lib.file_info setoneradio ] param_name ->
          unit -> [> input ] elt
(** Creates an [<input>] tag for sending a file *)

  val image_input :
      ?a:input_attrib attrib list ->
        name:[< coordinates oneradio ] param_name ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="...">] tag that sends the coordinates
   the user clicked on *)

  val int_image_input :
      ?a:input_attrib attrib list ->
        name:[< (int * coordinates) oneradio ] param_name -> value:int ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int *)

  val int32_image_input :
      ?a:input_attrib attrib list ->
        name:[< (int32 * coordinates) oneradio ] param_name -> value:int32 ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int32 *)

  val int64_image_input :
      ?a:input_attrib attrib list ->
        name:[< (int64 * coordinates) oneradio ] param_name -> value:int64 ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int64 *)

  val float_image_input :
      ?a:input_attrib attrib list ->
        name:[< (float * coordinates) oneradio ] param_name -> value:float ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
    the coordinates the user clicked on and a value of type float *)

  val string_image_input :
      ?a:input_attrib attrib list ->
        name:[< (string * coordinates) oneradio ] param_name -> value:string ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type string *)

  val user_type_image_input :
    ('a -> string) -> 
      ?a:input_attrib attrib list ->
        name:[< ('a * coordinates) oneradio ] param_name -> value:'a ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of user defined type *)

  val raw_image_input :
      ?a:input_attrib attrib list ->
        name:string -> value:string -> ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and an untyped value *)


  val bool_checkbox :
      ?a:input_attrib attrib list -> ?checked:bool ->
        name:[ `One of bool ] param_name -> unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have a boolean value.
   The service must declare a [bool] parameter.
 *)

    val int_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of int ] param_name -> value:int ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have an int value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val int32_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of int32 ] param_name -> value:int32 ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have an int32 value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val int64_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of int64 ] param_name -> value:int64 ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have an int64 value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val float_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of float ] param_name -> value:float ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have a float value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)


    val string_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of string ] param_name -> value:string ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have a string value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)


    val user_type_checkbox :
      ('a -> string) -> 
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of 'a ] param_name -> value:'a -> unit -> 
            [> input ] elt
(** Creates a checkbox [<input>] tag that will have a "user type" value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)


    val raw_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:string -> value:string -> unit -> [> input ] elt
(** Creates a checkbox [<input>] tag with untyped content.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [any].
 *)




  val string_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
    name:[ `Radio of string ] param_name -> value:string -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with string content *)

  val int_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
     name:[ `Radio of int ] param_name -> value:int -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with int content *)

  val int32_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
     name:[ `Radio of int32 ] param_name -> value:int32 -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with int32 content *)

  val int64_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
     name:[ `Radio of int64 ] param_name -> value:int64 -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with int64 content *)

  val float_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
     name:[ `Radio of float ] param_name -> value:float -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with float content *)

  val user_type_radio : 
    ('a -> string) -> ?a:(input_attrib attrib list ) -> ?checked:bool ->
    name:[ `Radio of 'a ] param_name -> value:'a -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with user_type content *)

  val raw_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
    name:string -> value:string -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with untyped string content (low level) *)


  type button_type =
      [ `Button | `Reset | `Submit ]

  val string_button : ?a:button_attrib attrib list ->
    name:[< string setone ] param_name -> value:string ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with string content *)

  val int_button : ?a:button_attrib attrib list ->
    name:[< int setone ] param_name -> value:int ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with int content *)

  val int32_button : ?a:button_attrib attrib list ->
    name:[< int32 setone ] param_name -> value:int32 ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with int32 content *)

  val int64_button : ?a:button_attrib attrib list ->
    name:[< int64 setone ] param_name -> value:int64 ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with int64 content *)

  val float_button : ?a:button_attrib attrib list ->
    name:[< float setone ] param_name -> value:float ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with float content *)

  val user_type_button : ('a -> string) -> ?a:button_attrib attrib list ->
    name:[< 'a setone ] param_name -> value:'a ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with user_type content *)

  val raw_button : ?a:button_attrib attrib list ->
    button_type:[< button_type ] ->
      name:string -> value:string ->
        button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with untyped string content *)

  val button : ?a:button_attrib attrib list ->
    button_type:[< button_type ] ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with no value. No value is sent. *)



  val textarea :
      ?a:textarea_attrib attrib list ->
        name:[< string setoneradio ] param_name ->
          ?value:string ->
            rows:int -> cols:int ->
              unit -> [> textarea ] elt
(** Creates a [<textarea>] tag *)

  val raw_textarea :
      ?a:textarea_attrib attrib list ->
        name:string ->
          ?value:string ->
            rows:int -> cols:int ->
              unit -> [> textarea ] elt
(** Creates a [<textarea>] tag for untyped form *)

  type 'a soption =
      Xhtmltypes.option_attrib XHTML.M.attrib list
        * 'a (* Value to send *)
        * pcdata elt option (* Text to display (if different from the latter) *)
        * bool (* selected *)

  type 'a select_opt =
    | Optgroup of
        [ common | `Disabled ] XHTML.M.attrib list
          * string (* label *)
          * 'a soption
          * 'a soption list
    | Option of 'a soption

  (** The type for [<select>] options and groups of options.
     - The field of type 'a in [soption] is the value that will be sent
     by the form.
     - If the [pcdata elt option] is not present it is also the
     value displayed.
     - The string in [select_opt] is the label
   *)

  val int_select :
      ?a:select_attrib attrib list ->
        name:[< `One of int ] param_name ->
          int select_opt ->
            int select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int values. *)

  val int32_select :
      ?a:select_attrib attrib list ->
        name:[< `One of int32 ] param_name ->
          int32 select_opt ->
            int32 select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int32 values. *)

  val int64_select :
      ?a:select_attrib attrib list ->
        name:[< `One of int64 ] param_name ->
          int64 select_opt ->
            int64 select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int64 values. *)

  val float_select :
      ?a:select_attrib attrib list ->
        name:[< `One of float ] param_name ->
          float select_opt ->
            float select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for float values. *)

  val string_select :
      ?a:select_attrib attrib list ->
        name:[< `One of string ] param_name ->
          string select_opt ->
            string select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for string values. *)

  val user_type_select :
    ('a -> string) -> 
      ?a:select_attrib attrib list ->
        name:[< `One of 'a ] param_name ->
          'a select_opt ->
            'a select_opt list ->
                [> select ] elt
(** Creates a [<select>] tag for user type values. *)

  val raw_select :
      ?a:select_attrib attrib list ->
        name:string ->
          string select_opt ->
            string select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for any (untyped) value. *)


  val int_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of int ] param_name ->
          int select_opt ->
            int select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int values. *)

  val int32_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of int32 ] param_name ->
          int32 select_opt ->
            int32 select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int32 values. *)

  val int64_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of int64 ] param_name ->
          int64 select_opt ->
            int64 select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int64 values. *)

  val float_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of float ] param_name ->
          float select_opt ->
            float select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for float values. *)

  val string_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of string ] param_name ->
          string select_opt ->
            string select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for string values. *)

  val user_type_multiple_select :
    ('a -> string) -> 
      ?a:select_attrib attrib list ->
        name:[< `Set of 'a ] param_name ->
          'a select_opt ->
            'a select_opt list ->
                [> select ] elt
(** Creates a [<select>] tag for user type values. *)

  val raw_multiple_select :
      ?a:select_attrib attrib list ->
        name:string ->
          string select_opt ->
            string select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for any (untyped) value. *)


end


module Xhtmlforms : XHTMLFORMSSIG = struct

  open XHTML.M
  open Xhtmltypes
  include Xhtmlforms'

(* As we want -> [> a ] elt and not -> [ a ] elt (etc.),
   we define a new module: *)
  let a = (a :
             ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:a_attrib attrib list ->
        service:('get, unit, [< get_service_kind ],
         [< suff ], 'gn, 'pn,
         [< registrable ], 'return) service ->
           sp:server_params -> 
            ?hostname:string ->
            ?port:int ->
            ?fragment:string ->
            ?keep_nl_params:[ `All | `Persistent | `None ] ->
            ?nl_params: Eliom_parameters.nl_params_set ->
            ?use_href:bool ->
             a_content elt list -> 'get ->
             a XHTML.M.elt :>
             ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:a_attrib attrib list ->
        service:('get, unit, [< get_service_kind ],
         [< suff ], 'gn, 'pn,
         [< registrable ], 'return) service ->
           sp:server_params ->
            ?hostname:string ->
            ?port:int ->
            ?fragment:string ->
            ?keep_nl_params:[ `All | `Persistent | `None ] ->
            ?nl_params: Eliom_parameters.nl_params_set ->
            ?use_href:bool ->
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
                   ?sp:server_params ->
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
                   sp:server_params ->
                   ?hostname:string ->
                   ?port:int ->
                   ?fragment:string ->
                   ?keep_nl_params:[ `All | `Persistent | `None ] ->
                   ?nl_params: Eliom_parameters.nl_params_set ->
             ('gn -> form_content elt list) -> form elt :>
                   ?absolute:bool ->
                   ?absolute_path:bool ->
                   ?https:bool ->
                   ?a:form_attrib attrib list ->
                   service:('get, unit, [< get_service_kind ],
                            [<suff ], 'gn, 'pn,
                            [< registrable ], 'return) service ->
                   sp:server_params ->
                   ?hostname:string ->
                   ?port:int ->
                   ?fragment:string ->
                   ?keep_nl_params:[ `All | `Persistent | `None ] ->
                   ?nl_params: Eliom_parameters.nl_params_set ->
                   ('gn -> form_content elt list) -> [> form ] elt)


  let lwt_get_form = (lwt_get_form :
                        ?absolute:bool ->
                       ?absolute_path:bool ->
                       ?https:bool ->
                       ?a:form_attrib attrib list ->
                       service:('get, unit, [< get_service_kind ],
                                [<suff ], 'gn, 'pn,
                                [< registrable ], 'return) service ->
                       sp:server_params ->
                       ?hostname:string ->
                       ?port:int ->
                       ?fragment:string ->
                       ?keep_nl_params:[ `All | `Persistent | `None ] ->
                       ?nl_params: Eliom_parameters.nl_params_set ->
                       ('gn -> form_content elt list Lwt.t) -> form elt Lwt.t :>
                       ?absolute:bool ->
                       ?absolute_path:bool ->
                       ?https:bool ->
                       ?a:form_attrib attrib list ->
                       service:('get, unit, [< get_service_kind ],
                                [<suff ], 'gn, 'pn,
                                [< registrable ], 'return) service ->
                       sp:server_params ->
                       ?hostname:string ->
                       ?port:int ->
                       ?fragment:string ->
                       ?keep_nl_params:[ `All | `Persistent | `None ] ->
                       ?nl_params: Eliom_parameters.nl_params_set ->
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
                    sp:server_params ->
                    ?hostname:string ->
                    ?port:int ->
                    ?fragment:string ->
                    ?keep_nl_params:[ `All | `Persistent | `None ] ->
                    ?keep_get_na_params:bool ->
                    ?nl_params: Eliom_parameters.nl_params_set ->
                    ('pn -> form_content elt list) -> 'get -> form elt :>
                    ?absolute:bool ->
                    ?absolute_path:bool ->
                    ?https:bool ->
                    ?a:form_attrib attrib list ->
                    service:('get, 'post, [< post_service_kind ],
                             [< suff ], 'gn, 'pn,
                             [< registrable ], 'return) service ->
                    sp:server_params ->
                    ?hostname:string ->
                    ?port:int ->
                    ?fragment:string ->
                    ?keep_nl_params:[ `All | `Persistent | `None ] ->
                    ?keep_get_na_params:bool ->
                    ?nl_params: Eliom_parameters.nl_params_set ->
                    ('pn -> form_content elt list) -> 'get -> [> form ] elt)

  let lwt_post_form = (lwt_post_form :
                         ?absolute:bool ->
                        ?absolute_path:bool ->
                        ?https:bool ->
                        ?a:form_attrib attrib list ->
                        service:('get, 'post, [< post_service_kind ],
                                 [< suff ], 'gn, 'pn,
                                 [< registrable ], 'return) service ->
                        sp:server_params ->
                        ?hostname:string ->
                        ?port:int ->
                        ?fragment:string ->
                        ?keep_nl_params:[ `All | `Persistent | `None ] ->
                        ?keep_get_na_params:bool ->
                        ?nl_params: Eliom_parameters.nl_params_set ->
                        ('pn -> form_content elt list Lwt.t) -> 
                        'get -> form elt Lwt.t :>
                        ?absolute:bool ->
                        ?absolute_path:bool ->
                        ?https:bool ->
                        ?a:form_attrib attrib list ->
                        service:('get, 'post, [< post_service_kind ],
                                 [< suff ], 'gn, 'pn,
                                 [< registrable ], 'return) service ->
                        sp:server_params ->
                        ?hostname:string ->
                        ?port:int ->
                        ?fragment:string ->
                        ?keep_nl_params:[ `All | `Persistent | `None ] ->
                        ?keep_get_na_params:bool ->
                        ?nl_params: Eliom_parameters.nl_params_set ->
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
(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)


module Xhtml5forms_ = struct
  open XHTML5.M
  open Xhtml5types

  type form_content_elt = form_content elt
  type form_content_elt_list = form_content elt list
  type uri = XHTML5.M.uri

  type a_content_elt = a_content elt
  type a_content_elt_list = a_content elt list

  type div_content_elt = div_content elt
  type div_content_elt_list = div_content elt list

  type a_elt = a elt
  type a_elt_list = a elt list
  type form_elt = form elt

  type textarea_elt = textarea elt
  type input_elt = input elt

  type link_elt = link elt
  type script_elt = script elt

  type pcdata_elt = pcdata elt

  type select_elt = select elt
  type select_content_elt = select_content elt
  type select_content_elt_list = select_content elt list
  type option_elt = selectoption elt
  type option_elt_list = selectoption elt list

  type button_elt = button elt
  type button_content_elt = button_content elt
  type button_content_elt_list = button_content elt list

  type a_attrib_t = Xhtml5types.a_attrib XHTML5.M.attrib list
  type form_attrib_t = Xhtml5types.form_attrib XHTML5.M.attrib list
  type input_attrib_t = Xhtml5types.input_attrib XHTML5.M.attrib list
  type textarea_attrib_t = Xhtml5types.textarea_attrib XHTML5.M.attrib list
  type select_attrib_t = Xhtml5types.select_attrib XHTML5.M.attrib list
  type link_attrib_t = Xhtml5types.link_attrib XHTML5.M.attrib list
  type script_attrib_t = Xhtml5types.script_attrib XHTML5.M.attrib list
  type optgroup_attrib_t = [ common | `Disabled ] XHTML5.M.attrib list
  type option_attrib_t = Xhtml5types.option_attrib XHTML5.M.attrib list
  type button_attrib_t = Xhtml5types.button_attrib XHTML5.M.attrib list

  type input_type_t =
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

  type button_type_t =
      [ `Button | `Reset | `Submit ]

  let hidden = `Hidden
  let checkbox = `Checkbox
  let radio = `Radio
  let submit = `Submit
  let file = `File
  let image = `Image

  let buttonsubmit = `Submit

  let uri_of_string = XHTML5.M.uri_of_string

  let empty_seq = []
  let cons_form a l = a::l

  let map_option = List.map
  let map_optgroup f a l = ((f a), List.map f l)
  let select_content_of_option a = (a :> select_content elt)

  let make_pcdata s = pcdata s

  let make_a ?(a=[]) ?href ?onclick l : a_elt =
    let a = match href with
      | None -> a
      | Some v -> (a_href (uri_of_string v))::a
    in
    let a = match onclick with
      | None -> a
      | Some v -> (a_onclick v)::a
    in
    XHTML5.M.a ~a l

  let make_get_form ?(a=[]) ~action elt1 elts : form_elt =
    XHTML5.M.form ~a:((a_method `Get)::(a_action (uri_of_string action))::a)
      elt1 elts

  let make_post_form ?(a=[]) ~action ?id ?(inline = false) elt1 elts
      : form_elt =
    let aa = (match id with
    | None -> a
    | Some i -> (a_id i)::a)
    in
    form ~a:((XHTML5.M.a_enctype "multipart/form-data")::
                (* Always Multipart!!! How to test if there is a file?? *)
                (a_action (uri_of_string action))::
                (a_method `Post)::
                (if inline then (a_class ["inline"])::aa else aa))
       elt1 elts

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
    | Some v -> (a_text_value v)::a
    in
    let a = match name with
    | None -> a
    | Some v -> (a_name v)::a
    in
    button ~a:((a_button_type button_type)::a) c

  let make_textarea ?(a=[]) ~name ?(value="") ~rows ~cols () =
    let a3 = (a_name name)::a in
    textarea ~a:((a_rows rows)::(a_cols cols)::a3) (pcdata value)

  let make_select ?(a=[]) ~multiple ~name elt elts =
    let a = if multiple then (a_multiple `Multiple)::a else a in
    select ~a:((a_name name)::a) (elt::elts)

  let make_option ?(a=[]) ~selected ?value c =
    let a = match value with
    | None -> a
    | Some v -> (a_text_value v)::a
    in
    let a = if selected then (a_selected `Selected)::a else a in
    option ~a c

  let make_optgroup ?(a=[]) ~label elt elts =
    optgroup ~label ~a (elt::elts)

  let make_css_link ?(a=[]) ~uri () =
    link ~href:uri ~rel:[`Stylesheet]  ~a:((a_mime_type "text/css")::a) ()

  let make_js_script ?(a=[]) ~uri () =
    script ~a:((a_src uri)::a) ~contenttype:"text/javascript" (pcdata "")

  let register_event node = XML.register_event (XHTML5.M.toelt node)

end



(*****************************************************************************)
(*****************************************************************************)

module Xhtml5forms' = MakeForms(Xhtml5forms_)

module type XHTML5FORMSSIG = sig
(* Pasted from mli *)




  open XHTML5.M
  open Xhtml5types

(** {2 Links and forms} *)




    val make_string_uri :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      service:('get, unit, [< get_service_kind ],
               [< suff ], 'gn, unit,
               [< registrable ], 'return) service ->
      ?sp:Eliom_sessions.server_params ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      'get -> 
      string
(** Creates the string corresponding to the relative URL of a service applied to
    its GET parameters.

    If [absolute] is set to [true], or if there is a protocol change,
    the URL will be absolute.
    
    If [absolute_path] is set to [true], and [absolute] is [false],
    the URL will be absolute, but without [protocol://server:port].
    
    Default hostname is determined from the [Host] http header of the request
    (or the attribute of <host> tag in
    configuration file if the option [<usedefaulthostname/>] is set).
    Default port is the current port (or another port of the server if
    you are switching from or to https).
    But you can choose the hostname or port you want by setting 
    the optional [?hostname] and [?port] parameters here.

 *)

    val make_uri :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      service:('get, unit, [< get_service_kind ],
               [< suff ], 'gn, unit,
               [< registrable ], 'return) service ->
      ?sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string -> 
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      'get -> 
      uri
(** Creates the URL for a service.
    Like the [a] function, it may take extra parameters. *)

    val make_uri_components :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      service:('get, unit, [< get_service_kind ],
               [< suff ], 'gn, unit,
               [< registrable ], 'return) service ->
      ?sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string -> 
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      'get -> 
      string * (string * string) list * string option
(** Creates the URL for a service.
    Returns the path (as a string, encoded),
    the association list of get parameters (not encoded),
    and the fragment (not encoded, if any).
    Like the [a] function, it may take extra parameters. *)

    val make_post_uri_components :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      service:('get, 'post, [< post_service_kind ],
               [< suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string -> 
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ?keep_get_na_params:bool ->
      'get -> 
      'post ->
      string * (string * string) list * string option * (string * string) list
(** Like [make_uri_components], but also creates a table of post parameters. *)

    val make_proto_prefix :
      ?sp:Eliom_sessions.server_params ->
      ?hostname:string ->
      ?port:int ->
      bool ->
      string
(** Creates the string corresponding to the beginning of the URL,
    containing the scheme (protocol), server and port number (if necessary).
 *)

    val a :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:a_attrib attrib list ->
      service:('get, unit, [< get_service_kind ],
               [< suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ?use_href:bool ->
      a_content elt list -> 
      'get -> 
    [> a] XHTML5.M.elt
(** [a service sp cont ()] creates a link to [service].
   The text of
   the link is [cont]. For example [cont] may be something like
   [\[pcdata "click here"\]].

   The last  parameter is for GET parameters.
   For example [a service sp cont (42,"hello")]

   The [~a] optional parameter is used for extra attributes.

   The [~fragment] optional parameter is used for the "fragment" part
   of the URL, that is, the part after character "#".

    When possible, all links generated by Eliom are relative, for example
    to make easier the use with reverse proxies.
    But in case of protocol change (if you want to switch to https using
    [~https:true] for example, or if the service imposes https),
    absolute links will be generated. 
    In that case,
    default hostname is determined from the [Host] http header of the request
    (or the attribute of <host> tag in
    configuration file if the option [<usedefaulthostname/>] is set).
    Default port is the current port (or another port of the server if
    you are switching from or to https).
    But you can choose the hostname or port you want by setting 
    the optional [?hostname] and [?port] parameters here.
    These options have no effect for relative links.

    You can add non-localized parameters using the optional parameter
    [nl_params]. See {!Eliom_parameters.nl_params_set}.

    If [~keep_nl_params] is [`Persistent] (resp. [`All]),
    persistent (resp all) non localized GET parameters
    will be kept in the URL (default is the default for the service).

    If a client side application is running, and unless
    [~use_href:true] is specified, it will use [<a onclick=...>]
    instead of [<a href=...>] in case of link inside a same Eliom application.
    Thus, the client side application will not be stopped when the link
    is clicked.

*)

    val css_link : ?a:link_attrib attrib list -> uri:uri -> unit -> [>link] elt
(** Creates a [<link>] tag for a Cascading StyleSheet (CSS). *)

    val js_script :
        ?a:script_attrib attrib list -> uri:uri -> unit -> [>script] elt
(** Creates a [<script>] tag to add a javascript file *)


    val get_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib attrib list ->
      service:('get, unit, [< get_service_kind ],
               [<suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ('gn -> form_content elt list) -> 
      [>form] elt
(** [get_form service sp formgen] creates a GET form to [service].
   The content of
   the form is generated by the function [formgen], that takes the names
   of the service parameters as parameters. *)

    val lwt_get_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib attrib list ->
      service:('get, unit, [< get_service_kind ],
               [<suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ('gn -> form_content elt list Lwt.t) -> 
      [>form] elt Lwt.t
(** The same but taking a cooperative function. *)


    val post_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib attrib list ->
      service:('get, 'post, [< post_service_kind ],
               [< suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?keep_get_na_params:bool ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ('pn -> form_content elt list) -> 
      'get -> 
      [>form] elt
(** [post_form service sp formgen] creates a POST form to [service].
    The last parameter is for GET parameters (as in the function [a]).

    If [~keep_nl_params] is [`Persistent] (resp. [`All]),
    persistent (resp all) non localized GET parameters
    will be kept in the URL (default is the default for the service).

 *)

    val lwt_post_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib attrib list ->
      service:('get, 'post, [< post_service_kind ],
               [< suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?keep_get_na_params:bool ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ('pn -> form_content elt list Lwt.t) -> 
      'get -> 
      [>form] elt Lwt.t
(** The same but taking a cooperative function. *)








(** {2 Form widgets} *)

  type basic_input_type =
      [
    | `Hidden
    | `Password
    | `Submit
    | `Text ]

  val int_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< int setoneradio ] param_name ->
          ?value:int -> unit -> [> input ] elt
(** Creates an [<input>] tag for an integer *)

  val int32_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< int32 setoneradio ] param_name ->
          ?value:int32 -> unit -> [> input ] elt
(** Creates an [<input>] tag for a 32 bits integer *)

  val int64_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< int64 setoneradio ] param_name ->
          ?value:int64 -> unit -> [> input ] elt
(** Creates an [<input>] tag for a 64 bits integer *)

  val float_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< float setoneradio ] param_name ->
          ?value:float -> unit -> [> input ] elt
(** Creates an [<input>] tag for a float *)

  val string_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< string setoneradio ] param_name ->
          ?value:string -> unit -> [> input ] elt
(** Creates an [<input>] tag for a string *)

  val user_type_input :
    ('a -> string) -> 
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< 'a setoneradio ] param_name ->
          ?value:'a -> unit -> [> input ] elt
(** Creates an [<input>] tag for a user type *)

  val raw_input :
      ?a:input_attrib attrib list ->
        input_type:[< basic_input_type | `Reset | `Button ] ->
        ?name:string -> ?value:string -> unit -> [> input ] elt
(** Creates an untyped [<input>] tag. You may use the name you want
   (for example to use with {!Eliom_parameters.any}).
 *)

  val file_input :
      ?a:input_attrib attrib list ->
        name:[< Ocsigen_lib.file_info setoneradio ] param_name ->
          unit -> [> input ] elt
(** Creates an [<input>] tag for sending a file *)

  val image_input :
      ?a:input_attrib attrib list ->
        name:[< coordinates oneradio ] param_name ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="...">] tag that sends the coordinates
   the user clicked on *)

  val int_image_input :
      ?a:input_attrib attrib list ->
        name:[< (int * coordinates) oneradio ] param_name -> value:int ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int *)

  val int32_image_input :
      ?a:input_attrib attrib list ->
        name:[< (int32 * coordinates) oneradio ] param_name -> value:int32 ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int32 *)

  val int64_image_input :
      ?a:input_attrib attrib list ->
        name:[< (int64 * coordinates) oneradio ] param_name -> value:int64 ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int64 *)

  val float_image_input :
      ?a:input_attrib attrib list ->
        name:[< (float * coordinates) oneradio ] param_name -> value:float ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
    the coordinates the user clicked on and a value of type float *)

  val string_image_input :
      ?a:input_attrib attrib list ->
        name:[< (string * coordinates) oneradio ] param_name -> value:string ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type string *)

  val user_type_image_input :
    ('a -> string) -> 
      ?a:input_attrib attrib list ->
        name:[< ('a * coordinates) oneradio ] param_name -> value:'a ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of user defined type *)

  val raw_image_input :
      ?a:input_attrib attrib list ->
        name:string -> value:string -> ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and an untyped value *)


  val bool_checkbox :
      ?a:input_attrib attrib list -> ?checked:bool ->
        name:[ `One of bool ] param_name -> unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have a boolean value.
   The service must declare a [bool] parameter.
 *)

    val int_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of int ] param_name -> value:int ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have an int value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val int32_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of int32 ] param_name -> value:int32 ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have an int32 value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val int64_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of int64 ] param_name -> value:int64 ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have an int64 value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val float_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of float ] param_name -> value:float ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have a float value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)


    val string_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of string ] param_name -> value:string ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have a string value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)


    val user_type_checkbox :
      ('a -> string) -> 
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of 'a ] param_name -> value:'a -> unit -> 
            [> input ] elt
(** Creates a checkbox [<input>] tag that will have a "user type" value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)


    val raw_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:string -> value:string -> unit -> [> input ] elt
(** Creates a checkbox [<input>] tag with untyped content.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [any].
 *)




  val string_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
    name:[ `Radio of string ] param_name -> value:string -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with string content *)

  val int_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
     name:[ `Radio of int ] param_name -> value:int -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with int content *)

  val int32_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
     name:[ `Radio of int32 ] param_name -> value:int32 -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with int32 content *)

  val int64_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
     name:[ `Radio of int64 ] param_name -> value:int64 -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with int64 content *)

  val float_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
     name:[ `Radio of float ] param_name -> value:float -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with float content *)

  val user_type_radio : 
    ('a -> string) -> ?a:(input_attrib attrib list ) -> ?checked:bool ->
    name:[ `Radio of 'a ] param_name -> value:'a -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with user_type content *)

  val raw_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
    name:string -> value:string -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with untyped string content (low level) *)


  type button_type =
      [ `Button | `Reset | `Submit ]

  val string_button : ?a:button_attrib attrib list ->
    name:[< string setone ] param_name -> value:string ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with string content *)

  val int_button : ?a:button_attrib attrib list ->
    name:[< int setone ] param_name -> value:int ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with int content *)

  val int32_button : ?a:button_attrib attrib list ->
    name:[< int32 setone ] param_name -> value:int32 ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with int32 content *)

  val int64_button : ?a:button_attrib attrib list ->
    name:[< int64 setone ] param_name -> value:int64 ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with int64 content *)

  val float_button : ?a:button_attrib attrib list ->
    name:[< float setone ] param_name -> value:float ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with float content *)

  val user_type_button : ('a -> string) -> ?a:button_attrib attrib list ->
    name:[< 'a setone ] param_name -> value:'a ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with user_type content *)

  val raw_button : ?a:button_attrib attrib list ->
    button_type:[< button_type ] ->
      name:string -> value:string ->
        button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with untyped string content *)

  val button : ?a:button_attrib attrib list ->
    button_type:[< button_type ] ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with no value. No value is sent. *)



  val textarea :
      ?a:textarea_attrib attrib list ->
        name:[< string setoneradio ] param_name ->
          ?value:string ->
            rows:int -> cols:int ->
              unit -> [> textarea ] elt
(** Creates a [<textarea>] tag *)

  val raw_textarea :
      ?a:textarea_attrib attrib list ->
        name:string ->
          ?value:string ->
            rows:int -> cols:int ->
              unit -> [> textarea ] elt
(** Creates a [<textarea>] tag for untyped form *)

  type 'a soption =
      Xhtml5types.option_attrib XHTML5.M.attrib list
        * 'a (* Value to send *)
        * pcdata elt option (* Text to display (if different from the latter) *)
        * bool (* selected *)

  type 'a select_opt =
    | Optgroup of
        [ common | `Disabled ] XHTML5.M.attrib list
          * string (* label *)
          * 'a soption
          * 'a soption list
    | Option of 'a soption

  (** The type for [<select>] options and groups of options.
     - The field of type 'a in [soption] is the value that will be sent
     by the form.
     - If the [pcdata elt option] is not present it is also the
     value displayed.
     - The string in [select_opt] is the label
   *)

  val int_select :
      ?a:select_attrib attrib list ->
        name:[< `One of int ] param_name ->
          int select_opt ->
            int select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int values. *)

  val int32_select :
      ?a:select_attrib attrib list ->
        name:[< `One of int32 ] param_name ->
          int32 select_opt ->
            int32 select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int32 values. *)

  val int64_select :
      ?a:select_attrib attrib list ->
        name:[< `One of int64 ] param_name ->
          int64 select_opt ->
            int64 select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int64 values. *)

  val float_select :
      ?a:select_attrib attrib list ->
        name:[< `One of float ] param_name ->
          float select_opt ->
            float select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for float values. *)

  val string_select :
      ?a:select_attrib attrib list ->
        name:[< `One of string ] param_name ->
          string select_opt ->
            string select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for string values. *)

  val user_type_select :
    ('a -> string) -> 
      ?a:select_attrib attrib list ->
        name:[< `One of 'a ] param_name ->
          'a select_opt ->
            'a select_opt list ->
                [> select ] elt
(** Creates a [<select>] tag for user type values. *)

  val raw_select :
      ?a:select_attrib attrib list ->
        name:string ->
          string select_opt ->
            string select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for any (untyped) value. *)


  val int_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of int ] param_name ->
          int select_opt ->
            int select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int values. *)

  val int32_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of int32 ] param_name ->
          int32 select_opt ->
            int32 select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int32 values. *)

  val int64_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of int64 ] param_name ->
          int64 select_opt ->
            int64 select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int64 values. *)

  val float_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of float ] param_name ->
          float select_opt ->
            float select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for float values. *)

  val string_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of string ] param_name ->
          string select_opt ->
            string select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for string values. *)

  val user_type_multiple_select :
    ('a -> string) -> 
      ?a:select_attrib attrib list ->
        name:[< `Set of 'a ] param_name ->
          'a select_opt ->
            'a select_opt list ->
                [> select ] elt
(** Creates a [<select>] tag for user type values. *)

  val raw_multiple_select :
      ?a:select_attrib attrib list ->
        name:string ->
          string select_opt ->
            string select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for any (untyped) value. *)


end


module Xhtml5forms : XHTML5FORMSSIG = struct

  open XHTML5.M
  open Xhtml5types
  include Xhtml5forms'

(* As we want -> [> a ] elt and not -> [ a ] elt (etc.),
   we define a new module: *)
  let a = (a :
             ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:a_attrib attrib list ->
        service:('get, unit, [< get_service_kind ],
         [< suff ], 'gn, 'pn,
         [< registrable ], 'return) service ->
           sp:server_params -> 
            ?hostname:string ->
            ?port:int ->
            ?fragment:string ->
            ?keep_nl_params:[ `All | `Persistent | `None ] ->
            ?nl_params: Eliom_parameters.nl_params_set ->
            ?use_href:bool ->
             a_content elt list -> 'get ->
             a XHTML5.M.elt :>
             ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:a_attrib attrib list ->
        service:('get, unit, [< get_service_kind ],
         [< suff ], 'gn, 'pn,
         [< registrable ], 'return) service ->
           sp:server_params ->
            ?hostname:string ->
            ?port:int ->
            ?fragment:string ->
            ?keep_nl_params:[ `All | `Persistent | `None ] ->
            ?nl_params: Eliom_parameters.nl_params_set ->
            ?use_href:bool ->
            a_content elt list -> 'get ->
             [> a] XHTML5.M.elt)

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
                   ?sp:server_params ->
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
                   sp:server_params ->
                   ?hostname:string ->
                   ?port:int ->
                   ?fragment:string ->
                   ?keep_nl_params:[ `All | `Persistent | `None ] ->
                   ?nl_params: Eliom_parameters.nl_params_set ->
             ('gn -> form_content elt list) -> form elt :>
                   ?absolute:bool ->
                   ?absolute_path:bool ->
                   ?https:bool ->
                   ?a:form_attrib attrib list ->
                   service:('get, unit, [< get_service_kind ],
                            [<suff ], 'gn, 'pn,
                            [< registrable ], 'return) service ->
                   sp:server_params ->
                   ?hostname:string ->
                   ?port:int ->
                   ?fragment:string ->
                   ?keep_nl_params:[ `All | `Persistent | `None ] ->
                   ?nl_params: Eliom_parameters.nl_params_set ->
                   ('gn -> form_content elt list) -> [> form ] elt)


  let lwt_get_form = (lwt_get_form :
                        ?absolute:bool ->
                       ?absolute_path:bool ->
                       ?https:bool ->
                       ?a:form_attrib attrib list ->
                       service:('get, unit, [< get_service_kind ],
                                [<suff ], 'gn, 'pn,
                                [< registrable ], 'return) service ->
                       sp:server_params ->
                       ?hostname:string ->
                       ?port:int ->
                       ?fragment:string ->
                       ?keep_nl_params:[ `All | `Persistent | `None ] ->
                       ?nl_params: Eliom_parameters.nl_params_set ->
                       ('gn -> form_content elt list Lwt.t) -> form elt Lwt.t :>
                       ?absolute:bool ->
                       ?absolute_path:bool ->
                       ?https:bool ->
                       ?a:form_attrib attrib list ->
                       service:('get, unit, [< get_service_kind ],
                                [<suff ], 'gn, 'pn,
                                [< registrable ], 'return) service ->
                       sp:server_params ->
                       ?hostname:string ->
                       ?port:int ->
                       ?fragment:string ->
                       ?keep_nl_params:[ `All | `Persistent | `None ] ->
                       ?nl_params: Eliom_parameters.nl_params_set ->
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
                    sp:server_params ->
                    ?hostname:string ->
                    ?port:int ->
                    ?fragment:string ->
                    ?keep_nl_params:[ `All | `Persistent | `None ] ->
                    ?keep_get_na_params:bool ->
                    ?nl_params: Eliom_parameters.nl_params_set ->
                    ('pn -> form_content elt list) -> 'get -> form elt :>
                    ?absolute:bool ->
                    ?absolute_path:bool ->
                    ?https:bool ->
                    ?a:form_attrib attrib list ->
                    service:('get, 'post, [< post_service_kind ],
                             [< suff ], 'gn, 'pn,
                             [< registrable ], 'return) service ->
                    sp:server_params ->
                    ?hostname:string ->
                    ?port:int ->
                    ?fragment:string ->
                    ?keep_nl_params:[ `All | `Persistent | `None ] ->
                    ?keep_get_na_params:bool ->
                    ?nl_params: Eliom_parameters.nl_params_set ->
                    ('pn -> form_content elt list) -> 'get -> [> form ] elt)

  let lwt_post_form = (lwt_post_form :
                         ?absolute:bool ->
                        ?absolute_path:bool ->
                        ?https:bool ->
                        ?a:form_attrib attrib list ->
                        service:('get, 'post, [< post_service_kind ],
                                 [< suff ], 'gn, 'pn,
                                 [< registrable ], 'return) service ->
                        sp:server_params ->
                        ?hostname:string ->
                        ?port:int ->
                        ?fragment:string ->
                        ?keep_nl_params:[ `All | `Persistent | `None ] ->
                        ?keep_get_na_params:bool ->
                        ?nl_params: Eliom_parameters.nl_params_set ->
                        ('pn -> form_content elt list Lwt.t) -> 
                        'get -> form elt Lwt.t :>
                        ?absolute:bool ->
                        ?absolute_path:bool ->
                        ?https:bool ->
                        ?a:form_attrib attrib list ->
                        service:('get, 'post, [< post_service_kind ],
                                 [< suff ], 'gn, 'pn,
                                 [< registrable ], 'return) service ->
                        sp:server_params ->
                        ?hostname:string ->
                        ?port:int ->
                        ?fragment:string ->
                        ?keep_nl_params:[ `All | `Persistent | `None ] ->
                        ?keep_get_na_params:bool ->
                        ?nl_params: Eliom_parameters.nl_params_set ->
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
