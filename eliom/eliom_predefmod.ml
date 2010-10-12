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

open Lwt
open Ocsigen_lib
open XHTML.M
open Xhtmltypes
open Ocsigen_extensions
open Eliom_sessions
open Eliom_services
open Eliom_parameters
open Eliom_mkforms
open Eliom_mkreg

open Ocsigen_http_frame
open Ocsigen_http_com

include Eliom_predefmod_cli

module type ELIOMSIG = sig
  include Eliom_mkreg.ELIOMREGSIG
  include Eliom_mkforms.ELIOMFORMSIG
end

let code_of_code_option = function
  | None -> 200
  | Some c -> c





(*****************************************************************************)
module Xhtmlforms_ = struct
  open XHTML.M
  open Xhtmltypes

  type form_content_elt = form_content elt
  type form_content_elt_list = form_content elt list
  type uri = Xhtmltypes.uri

  type 'a a_content_elt = a_content elt
  type 'a a_content_elt_list = a_content elt list

  type div_content_elt = div_content elt
  type div_content_elt_list = div_content elt list

  type 'a a_elt = a elt
  type 'a a_elt_list = a elt list
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

  let uri_of_string = uri_of_string

  let empty_seq = []
  let cons_form a l = a::l

  let map_option = List.map
  let map_optgroup f a l = ((f a), List.map f l)
  let select_content_of_option a = (a :> select_content elt)

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

  let make_get_form ?(a=[]) ~action ?onsubmit elt1 elts : form_elt =
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
      : form_elt =
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

  let register_event_a node = XML.register_event (XHTML.M.toelt node)
  let register_event_form node = XML.register_event (XHTML.M.toelt node)

  let add_tab_cookies_to_get_form _ () = 
    failwith "add_tab_cookies_to_get_form not implemented for xhtml1"

  let add_tab_cookies_to_post_form _ () = 
    failwith "add_tab_cookies_to_post_form not implemented for xhtml1"

  let add_tab_cookies_to_get_form_id_string = "not implemented for xhtml1"
   
  let add_tab_cookies_to_post_form_id_string =
    add_tab_cookies_to_get_form_id_string

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
      ?no_appl:bool ->
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
    [~no_appl:true] is specified, it will use [<a onclick=...>]
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
      ?no_appl:bool ->
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
      ?no_appl:bool ->
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
      ?no_appl:bool ->
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
      ?no_appl:bool ->
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
           sp:server_params ->
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
                  ?no_appl:bool ->
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
                       sp:server_params ->
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
                       sp:server_params ->
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
                    sp:server_params ->
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
                    sp:server_params ->
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
                        sp:server_params ->
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
                        sp:server_params ->
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




module Xhtmlreg_(Xhtml_content : Ocsigen_http_frame.HTTP_CONTENT
                         with type t = [ `Html ] XHTML.M.elt
                   and type options = XHTML.M.doctypes
                ) = struct
  open XHTML.M
  open Xhtmltypes

  type page = xhtml elt

  type options = XHTML.M.doctypes

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

  let pre_service ?options ~sp = Lwt.return ()

  let do_appl_xhr = Eliom_services.XNever

  let send ?(options = `XHTML_01_01) ?charset ?code
      ?content_type ?headers ~sp content =
    Xhtml_content.result_of_content ~options content >>= fun r ->
    Lwt.return
      {r with
         res_cookies= (Eliom_sessions.get_user_cookies ~sp);
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None -> Some (get_config_default_charset sp)
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

module Xhtmlreg = MakeRegister(Xhtmlreg_(Ocsigen_senders.Xhtml_content))
module Xhtmlcompactreg =
  MakeRegister(Xhtmlreg_(Ocsigen_senders.Xhtmlcompact_content))

module Xhtmlprettyreg =
  MakeRegister(Xhtmlreg_(Ocsigen_senders.Xhtmlpretty_content))


module Xhtml = struct
  include Xhtmlforms
  include Xhtmlreg
end

module Xhtmlpretty = struct
  include Xhtmlforms
  include Xhtmlprettyreg
end

module Xhtmlcompact' = Xhtmlcompact
module Xhtmlcompact = struct
  include Xhtmlforms
  include Xhtmlcompactreg
end


(****************************************************************************)
(****************************************************************************)
module SubXhtml(Format : sig 
      type doctypes
      type content
      type 'a elt
      val xhtml_list_stream : ?version:doctypes -> ?width: int -> ?encode:(string->string) 
        -> ?html_compat : bool -> content elt list -> string Ocsigen_stream.t end) = 
  (struct
    let result_of_content_subxhtml get_etag c =
      let x = Format.xhtml_list_stream c in
      let default_result = default_result () in
      Lwt.return
        {default_result with
          res_content_length = None;
          res_content_type = Some "text/html";
          res_etag = get_etag c;
          res_headers= Http_headers.dyn_headers;
          res_stream = (x, None)
        }
    module Cont_content =
      (* Pasted from ocsigen_senders.ml and modified *)
      struct
        type t = Format.content Format.elt list

        let get_etag_aux x = None

        let get_etag ?options c = None

        let result_of_content c = result_of_content_subxhtml get_etag c

      end

    module Contreg_ = struct
      open XHTML.M
      open Xhtmltypes

      type page = Format.content Format.elt list

      type options = unit

      type return = Eliom_services.http

      let pre_service ?options ~sp = Lwt.return ()

      let do_appl_xhr = Eliom_services.XNever

      let send ?options ?charset ?code 
          ?content_type ?headers ~sp content =
        Cont_content.result_of_content content >>= fun r ->
        Lwt.return
          {r with
             res_cookies= (Eliom_sessions.get_user_cookies ~sp);
             res_code= code_of_code_option code;
             res_charset= (match charset with
                             | None -> Some (get_config_default_charset sp)
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

    module Contreg = MakeRegister(Contreg_)

    include Xhtmlforms
    include Contreg

  end : sig

    include ELIOMREGSIG with type page = Format.content Format.elt list
                        and type options = unit
                        and type return = Eliom_services.http
    include XHTMLFORMSSIG

  end)

module Blocks = SubXhtml(struct
  type content = Xhtmltypes.body_content
  include Xhtml_format.XhtmlInfo
  include Xhtmlpretty_streams
end)

module Blocks5 = SubXhtml(struct
  type content = Xhtml5types.body_content
  include Xhtml_format.Xhtml5Info
  include Xhtml5pretty_streams
end)


(****************************************************************************)
(****************************************************************************)

module Textreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = (string * string)

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let do_appl_xhr = Eliom_services.XNever

  let send ?options ?charset ?code 
      ?content_type ?headers ~sp content =
    Ocsigen_senders.Text_content.result_of_content content >>= fun r ->
    Lwt.return
      {r with
         res_cookies= (Eliom_sessions.get_user_cookies ~sp);
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None ->  Some (get_config_default_charset sp)
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

module Text = MakeRegister(Textreg_)

(****************************************************************************)
(****************************************************************************)

module CssTextreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = string

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let do_appl_xhr = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers ~sp content =
    Ocsigen_senders.Text_content.result_of_content (content, "text/css")
    >>= fun r ->
    Lwt.return
      {r with
         res_cookies= (Eliom_sessions.get_user_cookies ~sp);
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None -> Some (get_config_default_charset sp)
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

module CssText = MakeRegister(CssTextreg_)


(****************************************************************************)
(****************************************************************************)

module HtmlTextreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = string

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let do_appl_xhr = Eliom_services.XNever

  let send ?options ?charset ?code 
      ?content_type ?headers ~sp content =
    Ocsigen_senders.Text_content.result_of_content (content, "text/html")
    >>= fun r ->
    Lwt.return
      {r with
         res_cookies= (Eliom_sessions.get_user_cookies ~sp);
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None -> Some (get_config_default_charset sp)
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

module HtmlTextforms_ = struct
  open XHTML.M
  open Xhtmltypes

  type form_content_elt = string
  type form_content_elt_list = string
  type uri = string
  type 'a a_content_elt = string
  type 'a a_content_elt_list = string
  type div_content_elt = string
  type div_content_elt_list = string

  type 'a a_elt = string
  type 'a a_elt_list = string
  type form_elt = string

  type textarea_elt = string
  type input_elt = string
  type select_elt = string
  type select_content_elt = string
  type select_content_elt_list = string
  type option_elt = string
  type option_elt_list = string
  type button_elt = string
  type button_content_elt = string
  type button_content_elt_list = string

  type link_elt = string
  type script_elt = string

  type pcdata_elt = string

  type a_attrib_t = string
  type form_attrib_t = string
  type input_attrib_t = string
  type textarea_attrib_t = string
  type select_attrib_t = string
  type link_attrib_t = string
  type script_attrib_t = string
  type optgroup_attrib_t = string
  type option_attrib_t = string
  type button_attrib_t = string

  type input_type_t = string
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

  let register_event_a elt ev callback v =
    failwith "register_event_a not implemented for text"

  let register_event_form elt ev callback v =
    failwith "register_event_form not implemented for text"

  let add_tab_cookies_to_get_form _ () = 
    failwith "add_tab_cookies_to_get_form not implemented for text"

  let add_tab_cookies_to_post_form _ () = 
    failwith "add_tab_cookies_to_post_form not implemented for text"

  let add_tab_cookies_to_get_form_id_string = "not implemented for text"
   
  let add_tab_cookies_to_post_form_id_string =
    add_tab_cookies_to_get_form_id_string

end



(****************************************************************************)
(****************************************************************************)

module HtmlTextforms = MakeForms(HtmlTextforms_)
module HtmlTextreg = MakeRegister(HtmlTextreg_)

module HtmlText = struct
  include HtmlTextforms
  include HtmlTextreg
end


(****************************************************************************)
(****************************************************************************)

(** Actions are like services, but do not generate any page. The current
   page is reloaded (but if you give the optional parameter
    [~options:`NoReload] to the registration function).
 *)
module Actionreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = unit

  type options = [ `Reload | `NoReload ]

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let do_appl_xhr = Eliom_services.XAlways
  (* The post action service will decide later *)

  let send
      ?(options = `Reload) ?charset ?(code = 204)
      ?content_type ?headers ~sp () =
    let user_cookies = Eliom_sessions.get_user_cookies ~sp in
    if options = `NoReload
    then
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
      let sitedata = Eliom_sessions.get_sitedata ~sp in
      let si = Eliom_sessions.get_si ~sp in
      let ri = Eliom_sessions.get_request ~sp in
      (match (si.Eliom_common.si_nonatt_info,
              si.Eliom_common.si_state_info,
              ri.request_info.ri_method) with
          | (Eliom_common.RNa_no,
             (Eliom_common.RAtt_no, Eliom_common.RAtt_no), 
             Ocsigen_http_frame.Http_header.GET) ->
            let empty_result = Ocsigen_http_frame.empty_result () in
            Lwt.return empty_result 
          | _ ->
            let all_cookie_info = 
              (Eliom_sessions.esp_of_sp sp).Eliom_common.sp_cookie_info 
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
            let rc = Eliom_sessions.get_request_cache ~sp in
            Polytables.set
              ~table:rc ~key:Eliom_common.tab_cookie_action_info_key
              ~value:(Eliom_sessions.get_sp_tab_cookie_info ~sp,
                      Eliom_sessions.get_user_tab_cookies ~sp,
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
                    si.Eliom_common.si_all_post_params, (* is [] *)
                    si.Eliom_common.si_nl_get_params,
                    si.Eliom_common.si_nl_post_params,
                    si.Eliom_common.si_all_get_but_nl)
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
               Ocsigen_extensions.serve_request 
                 ~previous_cookies:all_new_cookies ri
                 

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
                    si.Eliom_common.si_all_get_but_nl)
                 ;
                 let ri =
                   {ri.request_info with
                     ri_method = Ocsigen_http_frame.Http_header.GET;
                     ri_cookies= lazy ric;
                     ri_get_params = 
                       lazy si.Eliom_common.si_other_get_params;
                     ri_post_params = (fun _ -> Lwt.return []);
                     ri_files = (fun _ -> Lwt.return []);
                   }
                 in
                 Eliommod_pagegen.update_cookie_table sitedata all_cookie_info
                 >>= fun () ->
                 Ocsigen_extensions.serve_request
                   ~previous_cookies:all_new_cookies ri

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
                    si.Eliom_common.si_all_get_but_nl)
                 ;
                 let ri = 
                   {ri.request_info with
                     ri_method = Ocsigen_http_frame.Http_header.GET;
                     ri_cookies= lazy ric;
                     ri_get_params = 
                       lazy si.Eliom_common.si_other_get_params;
                     ri_post_params = (fun _ -> Lwt.return []);
                     ri_files = (fun _ -> Lwt.return []);
                   }
                 in
                 Eliommod_pagegen.update_cookie_table sitedata all_cookie_info
                 >>= fun () ->
                 Ocsigen_extensions.serve_request
                   ~previous_cookies:all_new_cookies ri)
      )

end

module Action = MakeRegister(Actionreg_)




(** Unit services are like services, do not generate any page, and do not
    reload the page. To be used carefully. Probably not usefull at all.
    (Same as {!Eliom_predefmod.Action} with [`NoReload] option).
 *)
module Unitreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = unit

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let do_appl_xhr = Eliom_services.XNever

  let send ?options ?charset ?(code = 204)
      ?content_type ?headers ~sp content =
    let empty_result = Ocsigen_http_frame.empty_result () in
    Lwt.return
      {empty_result with
         res_cookies= (Eliom_sessions.get_user_cookies ~sp);
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


module Unit = MakeRegister(Unitreg_)



(* Any is a module allowing to register services that decide themselves
   what they want to send.
 *)
module Anyreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = Ocsigen_http_frame.result

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let do_appl_xhr = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers ~sp res =
    Lwt.return
      {res with
         res_cookies= 
          Ocsigen_cookies.add_cookies
            (Eliom_sessions.get_user_cookies ~sp)
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

module Any = MakeRegister(Anyreg_)


(* Files is a module allowing to register services that send files *)
module Filesreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = string

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let do_appl_xhr = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers ~sp filename =
    let file =
      try Ocsigen_LocalFiles.resolve (Eliom_sessions.get_request sp) filename
      with
        | Ocsigen_LocalFiles.Failed_403 (* XXXBY : maybe we should signal a true 403? *)
        | Ocsigen_LocalFiles.Failed_404
        | Ocsigen_LocalFiles.NotReadableDirectory ->
            raise Eliom_common.Eliom_404
    in
    Ocsigen_LocalFiles.content ~request:(Eliom_sessions.get_request sp) ~file
    >>= fun r ->
    Lwt.return
      { r with
          res_cookies = (Eliom_sessions.get_user_cookies ~sp);
          res_code = code_of_code_option code;
          res_charset = (match charset with
                           | None ->
                               Some (Ocsigen_charset_mime.find_charset
                                       filename(get_config_info sp).charset_assoc)
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

module Files = MakeRegister(Filesreg_)

(****************************************************************************)
(****************************************************************************)

module Streamlistreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = (((unit -> (string Ocsigen_stream.t) Lwt.t) list) *
                 string)

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let do_appl_xhr = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers ~sp content =
    Ocsigen_senders.Streamlist_content.result_of_content content >>= fun r ->
    Lwt.return
      {r with
         res_cookies= (Eliom_sessions.get_user_cookies ~sp);
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None ->  Some (get_config_default_charset sp)
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

module Streamlist = MakeRegister(Streamlistreg_)



(****************************************************************************)
(****************************************************************************)

module Camlreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = string

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let do_appl_xhr = Eliom_services.XNever

  let send ?options ?charset ?code 
      ?content_type ?headers ~sp content =
    Text.send ?options ?charset ?code 
      ?content_type ?headers ~sp
      (content,
       Eliom_client_types.eliom_appl_answer_content_type)

end

module Caml = struct
  module M = MakeRegister(Camlreg_)

  type options = unit

  let make_eh = function
    | None -> None
    | Some eh -> 
        Some (fun sp l -> 
                eh sp l >>= fun r ->
                Lwt.return (Eliom_client_types.encode_eliom_data r))

  let make_service_handler f =
    fun sp g p -> 
      f sp g p >>= fun r -> 
      Lwt.return (Eliom_client_types.encode_eliom_data r)

  let pre_service ?options ~sp = Lwt.return ()

  let do_appl_xhr = Eliom_services.XNever

  let send ?options ?charset ?code 
      ?content_type ?headers ~sp content =
    M.send ?options ?charset ?code 
      ?content_type ?headers ~sp (Eliom_client_types.encode_eliom_data content)

  let register
      ?level
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?session_name
      ?secure_session
      ?sp
      ~(service : ('get, 'post,
                   [< internal_service_kind ],
                   [< suff ], 'gn, 'pn, [ `Registrable ], 
                   'return Eliom_parameters.caml) service)
      ?(error_handler : (Eliom_sessions.server_params ->
                           (string * exn) list -> 'return Lwt.t) option)
      (f : (Eliom_sessions.server_params -> 'get -> 'post -> 'return Lwt.t)) =
    M.register
      ?level
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?session_name
      ?secure_session
      ?sp
      ~service:(Eliom_services.untype_service_ service)
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)


  let register_service 
      ?level
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?session_name
      ?secure_session
      ?sp
      ?https
      ~path
      ~get_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_service 
                                      ?level
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?session_name
                                      ?secure_session
                                      ?sp
                                      ?https
                                      ~path
                                      ~get_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_coservice 
      ?level
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?session_name
      ?secure_session
      ?sp
      ?name
      ?csrf_safe
      ?csrf_session_name
      ?csrf_secure_session
      ?max_use
      ?timeout
      ?https
      ~fallback
      ~get_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_coservice 
                                      ?level
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?session_name
                                      ?secure_session
                                      ?sp
                                      ?name
                                      ?csrf_safe
                                      ?csrf_session_name
                                      ?csrf_secure_session
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~fallback:(Eliom_services.untype_service_ fallback)
                                      ~get_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_coservice'
      ?level
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?session_name
      ?secure_session
      ?sp
      ?name
      ?csrf_safe
      ?csrf_session_name
      ?csrf_secure_session
      ?max_use
      ?timeout
      ?https
      ~get_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_coservice' 
                                      ?level
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?session_name
                                      ?secure_session
                                      ?sp
                                      ?name
                                      ?csrf_safe
                                      ?csrf_session_name
                                      ?csrf_secure_session
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~get_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))


  let register_post_service
      ?level
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?session_name
      ?secure_session
      ?sp
      ?https
      ~fallback
      ~post_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_post_service 
                                      ?level
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?session_name
                                      ?secure_session
                                      ?sp
                                      ?https
                                      ~fallback:(Eliom_services.untype_service_ fallback)
                                      ~post_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_post_coservice
      ?level
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?session_name
      ?secure_session
      ?sp
      ?name
      ?csrf_safe
      ?csrf_session_name
      ?csrf_secure_session
      ?max_use
      ?timeout
      ?https
      ~fallback
      ~post_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_post_coservice 
                                      ?level
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?session_name
                                      ?secure_session
                                      ?sp
                                      ?name
                                      ?csrf_safe
                                      ?csrf_session_name
                                      ?csrf_secure_session
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~fallback:(Eliom_services.untype_service_ fallback)
                                      ~post_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_post_coservice'
      ?level
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?session_name
      ?secure_session
      ?sp
      ?name
      ?csrf_safe
      ?csrf_session_name
      ?csrf_secure_session
      ?max_use
      ?timeout
      ?keep_get_na_params
      ?https
      ~post_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_post_coservice' 
                                      ?level
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?session_name
                                      ?secure_session
                                      ?sp
                                      ?name
                                      ?csrf_safe
                                      ?csrf_session_name
                                      ?csrf_secure_session
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

open XHTML5.M

type appl_service_params =
    {
      ap_doctype: XHTML5.M.doctypes;
      ap_title: string;
      ap_container : 'a.
        ((([< Xhtml5types.common ] as 'a) XHTML5.M.attrib list) option *
           (sp:Eliom_sessions.server_params -> 
            Xhtml5types.body_content elt ->
            Xhtml5types.body_content elt list))
        option;
      ap_body_attributes : 
        'a. (([< Xhtml5types.common ] as 'a) XHTML5.M.attrib list) option;
      ap_headers_before : Xhtml5types.head_content_fun elt list;
      ap_headers_after : Xhtml5types.head_content_fun elt list
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
  { ap_doctype = `XHTML_05_00;
    ap_title = "Eliom application";
    ap_container = None;
    ap_body_attributes = None;
    ap_headers_before = [];
    ap_headers_after = [];
  }


module Eliom_appl_reg_
  (Xhtml_content : Ocsigen_http_frame.HTTP_CONTENT
   with type t = [ `Html ] XHTML5.M.elt
   and type options = XHTML5.M.doctypes
  )
  (Appl_params : APPL_PARAMS) = struct
  open XHTML5.M
  open Xhtml5types

  type page = body_content elt list

  type options = appl_service_options

  type return = Eliom_services.appl_service

  let eliom_appl_session_name = "__eliom_appl_internal"

  let change_page_event_table : ('a -> unit) Eliom_sessions.volatile_table =
    Eliom_sessions.create_volatile_table
      ~session_name:eliom_appl_session_name
      ~level:`Tab
      ()

  let get_tab_cook sp =
    Eliommod_cookies.compute_cookies_to_send
      (Eliom_sessions.esp_of_sp sp).Eliom_common.sp_sitedata
      (Eliom_sessions.esp_of_sp sp).Eliom_common.sp_tab_cookie_info
      (Eliom_sessions.get_user_tab_cookies ~sp)
                        
  let create_page
      ~options ~sp params cookies_to_send
      change_page_event content = 
    let do_not_launch = options.do_not_launch
        (* || 
           (Ocsigen_cookies.length tab_cookies_to_send > 1)
        (* If there are cookies, we launch the application *)
           Actually, no, we trust options ...
           Because we must decide whether to launch
           the application or not before
           creating links and forms.
        *)
    in
    let body, container_node = match params.ap_container with
      | None -> let b = XHTML5.M.body ?a:params.ap_body_attributes content in
                (b, (XHTML5.M.toelt b))
      | Some (a, container) ->
        let d = XHTML5.M.div ?a content in
        (XHTML5.M.body
           ?a:params.ap_body_attributes 
           (container ~sp d),
         (XHTML5.M.toelt d))
    in
    ignore (XML.ref_node container_node); (* The ref must be created 
                                             for container before
                                             calling make_ref_tree! *)
    XHTML5.M.html
      (XHTML5.M.head (XHTML5.M.title (XHTML5.M.pcdata params.ap_title)) 
         (
           params.ap_headers_before@
           XHTML5.M.style
             [
               XHTML5.M.pcdata
                 "\n.eliom_inline {display: inline}\n.eliom_nodisplay {display: none}\n"]::

             (* This will do a redirection if there is a #! in the URL *)
             XHTML5.M.script
             (cdata_script
                ("// Redirect if the URL contains #! while loading the page
function redir () {
  var str_url = window.location.toString() ;
  try{
    var match = str_url.match(\"(.*)/[^#/?]*(\\\\?.*)?#!((https?://)?(.*))$\");
          //but what if there's a # the search ?
    if(match) {
      if(match[4]) { //absolute
        window.location = match[3];
      }
      else { //relative
        window.location = match[1] + \"/\" + match[3] ;
      }
    }
  } catch(e) {} ;
};
redir ();"))::

             if not do_not_launch
             then
               XHTML5.M.script
                 (cdata_script
                    (
                      String.concat
                        ""
                        [
                          "var container_node = \'";
                          (let reqnum = Eliom_sessions.get_request_id ~sp in
                           (Eliom_client_types.jsmarshal
                              (Eliom_client_types.to_data_key_
                                 (reqnum, XML.ref_node container_node))
                           )) ; "\'; \n";

                          "var eliom_data = \'" ;
                          (Eliom_client_types.jsmarshal
                             ((Ocsigen_lib.Left
                                 (XML.make_ref_tree (XHTML5.M.toelt body)),
                            (* Warning: due to right_to_left evaluation,
                               make_ref_tree is called before the previous
                               items. Do not create new node refs in
                               previous items!
                            *)
                               (Eliommod_cli.get_eliom_appl_page_data_ ~sp),
                               cookies_to_send,
                               Eliom_services.get_onload ~sp,
                               Eliom_services.get_onunload ~sp
                              ) :
                                 Eliom_client_types.eliom_data_type
                             )
                          ) ; "\'; \n" ;

                          "var change_page_event = \'" ;
                          (Eliom_client_types.jsmarshal
                             (Eliommod_event.Down.wrap ~sp
                                (Eliommod_event.Down.of_react change_page_event)
                             )
                          ) ; "\'; \n"
                        ]

                    )
                 ) ::
               (* Javascript program: *)
               XHTML5.M.script
                   ~a:[a_src (Xhtml.make_uri 
                                ~service:(Eliom_services.static_dir ~sp)
                                ~sp
                                [Appl_params.application_name ^ ".js"])]
                 (pcdata "")::
                 params.ap_headers_after
             else params.ap_headers_before@params.ap_headers_after

         ))
      body

  let pre_service ?(options = default_appl_service_options) ~sp =
    (* If we launch a new application, we must set the application name.
       Otherwise, we get it from cookie. *)
    (match Eliom_sessions.get_sp_appl_name ~sp (* sent by the browser *) with
      | Some appl_name_cookie ->
        if appl_name_cookie <> Appl_params.application_name
        then begin
          Eliom_sessions.set_sp_appl_name ~sp
            (Some Appl_params.application_name);
          Eliom_sessions.set_sp_content_only ~sp false;
        end
      | None -> (* The application was not launched on client side *)
        if not options.do_not_launch
        (* if do_not_launch is true,
           we do not launch the client side program. *)
        then Eliom_sessions.set_sp_appl_name ~sp
          (Some Appl_params.application_name);
    );
    Lwt.return ()
    
  let do_appl_xhr = Eliom_services.XSame_appl Appl_params.application_name


  let get_eliom_page_content ~options sp content =
    get_tab_cook sp >>= fun tab_cookies_to_send ->
(*VVV Here we do not send a stream *)
    Lwt.return 
      (Eliom_client_types.EAContent
         ((Ocsigen_lib.Right
             (XML.make_ref_tree_list (XHTML5.M.toeltl content)),
           (Eliommod_cli.get_eliom_appl_page_data_ ~sp),
           tab_cookies_to_send,
           Eliom_services.get_onload ~sp,
           Eliom_services.get_onunload ~sp
          ),
(*VVV Use another serialization format than XML for the page? *)
          Xhtml5compact.xhtml_list_print content)
      )


  let send ?(options = default_appl_service_options) ?charset ?code
      ?content_type ?headers ~sp content =
    let content_only = Eliom_sessions.get_sp_content_only ~sp in
    (if content_only &&
        (((Eliom_parameters.get_non_localized_get_parameters
             ~sp Eliom_mkforms.nl_internal_appl_form) = Some true) ||
            ((Eliom_parameters.get_non_localized_post_parameters
                ~sp Eliom_mkforms.nl_internal_appl_form) = Some true))
     then begin (* It was an internal form.
                   We want to change only the content.
                   But the browser is not doing an xhr.
                   We send 204 No Content 
                   and use the change_page_event to update the content. *)
       match (Eliom_sessions.get_volatile_session_data
                ~table:change_page_event_table ~sp ())
       with
         | Eliom_sessions.Data change_current_page ->
           get_eliom_page_content ~options sp content >>= fun data ->
           change_current_page data;
           Lwt.return (Ocsigen_http_frame.empty_result ())
         | Eliom_sessions.Data_session_expired
         | Eliom_sessions.No_data ->
(*VVV What to do here? *)
           Lwt.fail Eliom_process.Server_side_process_closed
     end
     else if content_only
(*VVV do not send container! *)
     then 
        get_eliom_page_content ~options sp content >>= Caml.send ~sp
     else begin
       let change_page_event, change_current_page =
         (* This event allows the server to ask the client to change 
            current page content *)
         React.E.create ()
       in
       Eliom_sessions.set_volatile_session_data
         ~table:change_page_event_table ~sp change_current_page;
       Eliom_sessions.set_cookie ~sp
         ~cookie_level:`Tab
         ~name:Eliom_common.appl_name_cookie_name
         ~value:Appl_params.application_name ();
       get_tab_cook sp >>= fun tab_cookies_to_send ->
(*VVV for now not possible to give other params for one page *)
       let page =
         create_page
           ~options ~sp Appl_params.params tab_cookies_to_send
           change_page_event content 
       in
       let options = Appl_params.params.ap_doctype in
       Xhtml_content.result_of_content ~options page
       >>= fun r ->
        Lwt.return
          {r with
            res_cookies= (Eliom_sessions.get_user_cookies ~sp);
            res_code= code_of_code_option code;
            res_charset= (match charset with
              | None -> Some (get_config_default_charset sp)
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
  include Xhtml5forms
  include MakeRegister(Eliom_appl_reg_
                         (Ocsigen_senders.Xhtml5compact_content)
                         (Appl_params))

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
module String_redirreg_ = struct

  type page = XHTML5.M.uri

  type options = [ `Temporary | `Permanent ]

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let do_appl_xhr = Eliom_services.XAlways
  (* actually, the service will decide itself *)

  let send ?(options = `Permanent) ?charset ?code
      ?content_type ?headers ~sp content =
    let uri = XHTML.M.string_of_uri content in
    let empty_result = Ocsigen_http_frame.empty_result () in
    let cookies = Eliom_sessions.get_user_cookies ~sp in
    let content_type = match content_type with
      | None -> empty_result.res_content_type
      | _ -> content_type
    in
    let headers = match headers with
      | None -> empty_result.res_headers
      | Some headers -> 
        Http_headers.with_defaults
          headers empty_result.res_headers
    in

    (* Now we decide the kind of redirection we do.
       If the request is an xhr done by a client side Eliom program,
       we do not send an HTTP redirection.
       In that case, we send a full xhr redirection.
       If the application to which belongs the destination service is the same,
       then it is ok, otherwise, there will be another redirection ...
    *)
    match Eliom_sessions.get_sp_appl_name ~sp with
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
            res_cookies= cookies;
            res_code= code;
            res_location = Some uri;
            res_content_type= content_type;
            res_headers= headers;
          }
      | _ ->
        Lwt.return
          {empty_result with
            res_cookies= cookies;
            res_content_type= content_type;
            res_headers= 
              Http_headers.add
                (Http_headers.name Eliom_common.full_xhr_redir_header)
                uri headers
          }


end


module String_redirection = MakeRegister(String_redirreg_)




module Redirreg_ = struct
  open Xhtmltypes

  type page = 
      (unit, unit, Eliom_services.get_service_kind,
       [ `WithoutSuffix ], 
       unit, unit, Eliom_services.registrable, Eliom_services.http)
        Eliom_services.service

  type options = [ `Temporary | `Permanent ]

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let do_appl_xhr = Eliom_services.XAlways
  (* actually, the service will decide itself *)

  let send ?(options = `Permanent) ?charset ?code
      ?content_type ?headers ~sp service =
    let uri = Xhtml.make_string_uri ~absolute:true ~sp ~service () in

    let empty_result = Ocsigen_http_frame.empty_result () in
    let cookies = Eliom_sessions.get_user_cookies ~sp in
    let content_type = match content_type with
      | None -> empty_result.res_content_type
      | _ -> content_type
    in
    let headers = match headers with
      | None -> empty_result.res_headers
      | Some headers -> 
        Http_headers.with_defaults
          headers empty_result.res_headers
    in

    (* Now we decide the kind of redirection we do.
       If the request is an xhr done by a client side Eliom program,
       we do not send an HTTP redirection.
       In that case, we send:
       - a full xhr redirection if the application to which belongs
       the destination service is the same (thus it will send back tab cookies)
       - a half xhr redirection otherwise
    *)
    match Eliom_sessions.get_sp_appl_name ~sp with
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
            res_cookies= cookies;
            res_code= code;
            res_location = Some uri;
            res_content_type= content_type;
            res_headers= headers; }

      | Some anr ->
          (* the browser asked application eliom data
             (content only) for the application called anr *)
        match Eliom_services.get_do_appl_xhr service with
            (* the appl name of the destination service *)
          | Eliom_services.XSame_appl an when (an = anr) ->
            (* Same appl, we do a full xhr redirection
               (not an http redirection, because we want to
               send back tab cookies) *)
            Lwt.return
              {empty_result with
                res_cookies= cookies;
                res_content_type= content_type;
                res_headers= 
                  Http_headers.add
                    (Http_headers.name Eliom_common.full_xhr_redir_header)
                    uri headers
              }
              
          | Eliom_services.XAlways ->
            (* It is probably an action, full xhr again *)
            Lwt.return
              {empty_result with
                res_cookies= cookies;
                res_content_type= content_type;
                res_headers= 
                  Http_headers.add
                    (Http_headers.name Eliom_common.full_xhr_redir_header)
                    uri headers
              }

          | _ -> (* No application, or another application.
                    We ask the browser to do an HTTP redirection. *)
            Lwt.return
              {empty_result with
                res_cookies= cookies;
                res_content_type= content_type;
                res_headers= 
                  Http_headers.add
                    (Http_headers.name Eliom_common.half_xhr_redir_header)
                    uri headers
              }


end


module Redirection = MakeRegister(Redirreg_)

(*****************************************************************************)






module Xhtml5reg_(Xhtml_content : Ocsigen_http_frame.HTTP_CONTENT
                  with type t = [ `Html ] XHTML5.M.elt
                  and type options = XHTML5.M.doctypes
) = struct
  open XHTML5.M
  open Xhtml5types

  type page = xhtml elt

  type options = XHTML5.M.doctypes

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let do_appl_xhr = Eliom_services.XNever

  module Xhtml_content = struct

    include Xhtml_content

    let add_css (a : 'a) : 'a =
      let css =
        XHTML5.M.toelt
          (XHTML5.M.style
             [XHTML5.M.pcdata "\n.";
             XHTML5.M.pcdata Eliom_common.inline_class_name;
             XHTML5.M.pcdata " {display: inline}\n.";
             XHTML5.M.pcdata Eliom_common.nodisplay_class_name;
             XHTML5.M.pcdata " {display: none}\n"])
      in
      let rec aux = function
        | { XML.elt = XML.Node ("head",al,el) } as e::l ->
            { e with XML.elt = XML.Node ("head",al,css::el) }::l
        | e::l -> e::(aux l)
        | [] -> []
      in
      XHTML5.M.tot
        (match XHTML5.M.toelt a with
           | { XML.elt = XML.Node ("html",al,el) } as e ->
               { e with XML.elt = XML.Node ("html",al,aux el) }
           | e -> e)

    let get_etag ?options c = get_etag (add_css c)

    let result_of_content ?options c = result_of_content ?options (add_css c)

  end

  let send ?(options = `XHTML_05_00) ?charset ?code
      ?content_type ?headers ~sp content =
    Xhtml_content.result_of_content ~options content >>= fun r ->
    Lwt.return
      {r with
         res_cookies= (Eliom_sessions.get_user_cookies ~sp);
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None -> Some (get_config_default_charset sp)
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

module Xhtml5reg = MakeRegister(Xhtml5reg_(Ocsigen_senders.Xhtml5_content))
module Xhtml5compactreg =
  MakeRegister(Xhtml5reg_(Ocsigen_senders.Xhtml5compact_content))

module Xhtml5prettyreg =
  MakeRegister(Xhtml5reg_(Ocsigen_senders.Xhtml5pretty_content))

module Xhtml5 = struct
  include Xhtml5forms
  include Xhtml5reg
end

module Xhtml5compact = struct
  include Xhtml5forms
  include Xhtml5compactreg
end

module Xhtml5pretty = struct
  include Xhtml5forms
  include Xhtml5prettyreg
end
