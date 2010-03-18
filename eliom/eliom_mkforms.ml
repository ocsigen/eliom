(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_mkforms
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
open Eliom_parameters
open Eliom_services
open Eliom_sessions


(*****************************************************************************)
(* Building href *)
let rec string_of_url_path' = function
  | [] -> ""
  | [a] when a = Eliom_common.eliom_suffix_internal_name -> ""
  | [a] -> Ocsigen_lib.encode ~plus:false a
  | a::b::l when b = Eliom_common.eliom_suffix_internal_name -> 
      string_of_url_path' (a::l)
  | a::l when a = Eliom_common.eliom_suffix_internal_name ->
      string_of_url_path' l
  | a::l -> (Ocsigen_lib.encode ~plus:false a)^"/"^(string_of_url_path' l)

let rec string_of_url_path_suff u = function
  | None -> string_of_url_path' u
  | Some suff -> 
      let pref = string_of_url_path' u in
      let suf = string_of_url_path' suff in
      if pref = "" then suf else String.concat "/" [pref; suf]

let reconstruct_absolute_url_path = string_of_url_path_suff

let reconstruct_relative_url_path current_url u =
  let rec drop cururl desturl = match cururl, desturl with
  | a::l, [b] -> l, desturl
  | [a], m -> [], m
  | a::l, b::m when a = b -> drop l m
  | a::l, m -> l, m
  | [], m -> [], m
  in let rec makedotdot = function
    | [] -> []
(*    | [a] -> "" *)
    | _::l -> ".."::(makedotdot l)
  in
  let aremonter, aaller = drop current_url u in 
  (makedotdot aremonter)@aaller

let reconstruct_relative_url_path_string current_url u suff =
  let relurl = reconstruct_relative_url_path current_url u in
  let s = string_of_url_path_suff relurl suff in
  if s = "" 
  then Eliom_common.defaultpagename 
  else if s.[0] = '/'
  then (* possible with optional parameters *) "./"^s
  else s

let rec relative_url_path_to_myself = function
  | []
  | [""] -> Eliom_common.defaultpagename
  | [a] -> a
  | a::l -> relative_url_path_to_myself l
(*****************************************************************************)




module type FORMCREATE =
  sig
    type form_content_elt
    type form_content_elt_list
    type form_elt
    type a_content_elt
    type a_content_elt_list
    type a_elt
    type a_elt_list
    type div_content_elt
    type div_content_elt_list
    type uri
    type link_elt
    type script_elt
    type textarea_elt
    type input_elt
    type pcdata_elt
    type select_elt
    type select_content_elt
    type select_content_elt_list
    type option_elt
    type option_elt_list
    type button_elt
    type button_content_elt
    type button_content_elt_list

    type a_attrib_t
    type form_attrib_t
    type input_attrib_t
    type textarea_attrib_t
    type select_attrib_t
    type link_attrib_t
    type script_attrib_t
    type optgroup_attrib_t
    type option_attrib_t
    type button_attrib_t


    type input_type_t
    type button_type_t




    val hidden : input_type_t
    val checkbox : input_type_t
    val radio : input_type_t
    val submit : input_type_t
    val file : input_type_t
    val image : input_type_t

    val buttonsubmit : button_type_t

    val empty_seq : form_content_elt_list
    val cons_form :
        form_content_elt -> form_content_elt_list -> form_content_elt_list
    val map_option :
        ('a -> option_elt) -> 'a list ->
          option_elt_list
    val map_optgroup :
        ('a -> select_content_elt) -> 'a -> 'a list ->
          (select_content_elt * select_content_elt_list)
    val select_content_of_option : option_elt -> select_content_elt

    val make_pcdata : string -> pcdata_elt
    val make_a : ?a:a_attrib_t -> href:string -> a_content_elt_list -> a_elt
    val make_get_form : ?a:form_attrib_t ->
      action:string ->
        form_content_elt -> form_content_elt_list -> form_elt
    val make_post_form : ?a:form_attrib_t ->
      action:string -> ?id:string -> ?inline:bool ->
        form_content_elt -> form_content_elt_list -> form_elt
    val make_hidden_field : input_elt option -> form_content_elt
    val remove_first :
        form_content_elt_list -> form_content_elt * form_content_elt_list
    val make_input : ?a:input_attrib_t -> ?checked:bool ->
      typ:input_type_t -> ?name:string -> ?src:uri ->
        ?value:string -> unit -> input_elt
    val make_button : ?a:button_attrib_t -> button_type:button_type_t ->
      ?name:string -> ?value:string ->
        button_content_elt_list -> button_elt
    val make_textarea :
        ?a:textarea_attrib_t ->
          name:string -> ?value:string -> rows:int -> cols:int ->
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
    val uri_of_string : string -> uri


    val make_css_link : ?a:link_attrib_t -> uri:uri -> unit -> link_elt

    val make_js_script : ?a:script_attrib_t -> uri:uri -> unit -> script_elt

  end

module type ELIOMFORMSIG =
(* pasted from mli *)
  sig


    type form_content_elt
    type form_content_elt_list
    type form_elt
    type a_content_elt
    type a_content_elt_list
    type a_elt
    type a_elt_list
    type div_content_elt
    type div_content_elt_list
    type uri
    type link_elt
    type script_elt
    type textarea_elt
    type input_elt
    type pcdata_elt
    type select_elt
    type select_content_elt
    type select_content_elt_list
    type option_elt
    type option_elt_list
    type button_elt
    type button_content_elt
    type button_content_elt_list

    type a_attrib_t
    type form_attrib_t
    type input_attrib_t
    type textarea_attrib_t
    type select_attrib_t
    type link_attrib_t
    type script_attrib_t
    type optgroup_attrib_t
    type option_attrib_t
    type button_attrib_t

    type input_type_t
    type button_type_t

(** {2 Links and forms} *)

    val make_string_uri :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      service:('get, unit, [< get_service_kind ],
               [< suff ], 'gn, unit,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params ->
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
      sp:Eliom_sessions.server_params -> 
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
      sp:Eliom_sessions.server_params -> 
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
      sp:Eliom_sessions.server_params ->
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
      ?a:a_attrib_t ->
      service:('get, unit, [< get_service_kind ],
               [< suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      a_content_elt_list -> 
      'get -> 
      a_elt
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

*)

    val css_link : ?a:link_attrib_t -> uri:uri -> unit -> link_elt
(** Creates a [<link>] tag for a Cascading StyleSheet (CSS). *)

    val js_script :
        ?a:script_attrib_t -> uri:uri -> unit -> script_elt
(** Creates a [<script>] tag to add a javascript file *)


    val get_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib_t ->
      service:('get, unit, [< get_service_kind ],
               [<suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ('gn -> form_content_elt_list) -> 
      form_elt
(** [get_form service sp formgen] creates a GET form to [service].
   The content of
   the form is generated by the function [formgen], that takes the names
   of the service parameters as parameters. *)

    val lwt_get_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib_t ->
      service:('get, unit, [< get_service_kind ],
               [<suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      sp:Eliom_sessions.server_params -> 
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ('gn -> form_content_elt_list Lwt.t) -> 
      form_elt Lwt.t
(** The same but taking a cooperative function. *)


    val post_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib_t ->
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
      ('pn -> form_content_elt_list) -> 
      'get -> 
      form_elt
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
      ?a:form_attrib_t ->
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
      ('pn -> form_content_elt_list Lwt.t) -> 
      'get -> 
      form_elt Lwt.t
(** The same but taking a cooperative function. *)


(** {2 Form widgets} *)


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
(** Creates an [<input>] tag for an integer *)

    val float_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:[< float setoneradio ] param_name ->
            ?value:float -> unit -> input_elt
(** Creates an [<input>] tag for a float *)

    val string_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
           ?name:[< string setoneradio ] param_name ->
             ?value:string -> unit -> input_elt
(** Creates an [<input>] tag for a string *)

    val user_type_input : ('a -> string) ->
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:[< 'a setoneradio ] param_name ->
            ?value:'a -> unit -> input_elt
(** Creates an [<input>] tag for a user type *)

    val raw_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:string -> ?value:string -> unit -> input_elt
(** Creates an untyped [<input>] tag. You may use the name you want
   (for example to use with {!Eliom_parameters.any}).
 *)

    val file_input :
        ?a:input_attrib_t ->
          name:[< Ocsigen_lib.file_info setoneradio ] param_name ->
            unit -> input_elt
(** Creates an [<input>] tag for sending a file *)

    val image_input :
        ?a:input_attrib_t ->
          name:[< coordinates oneradio ] param_name ->
          ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="...">] tag that sends the coordinates
   the user clicked on *)

    val int_image_input :
        ?a:input_attrib_t ->
          name:[< (int * coordinates) oneradio ] param_name -> value:int ->
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int *)

    val int32_image_input :
        ?a:input_attrib_t ->
          name:[< (int32 * coordinates) oneradio ] param_name -> value:int32 ->
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int32 *)

    val int64_image_input :
        ?a:input_attrib_t ->
          name:[< (int64 * coordinates) oneradio ] param_name -> value:int64 ->
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int64 *)

    val float_image_input :
        ?a:input_attrib_t ->
          name:[< (float * coordinates) oneradio ] param_name -> value:float ->
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
    the coordinates the user clicked on and a value of type float *)

    val string_image_input :
        ?a:input_attrib_t ->
          name:[< (string * coordinates) oneradio ] param_name -> value:string ->
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type string *)

    val user_type_image_input : ('a -> string) ->
        ?a:input_attrib_t ->
          name:[< ('a * coordinates) oneradio ] param_name -> value:'a ->
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of user defined type *)

    val raw_image_input :
        ?a:input_attrib_t ->
          name:string -> value:string -> ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and an untyped value *)


    val bool_checkbox :
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `One of bool ] param_name -> unit -> input_elt
(** Creates a checkbox [<input>] tag that will have a boolean value.
   The service must declare a [bool] parameter.
 *)


    val int_checkbox :
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `Set of int ] param_name -> value:int -> unit -> input_elt
(** Creates a checkbox [<input>] tag that will have an int value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val int32_checkbox :
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `Set of int32 ] param_name -> value:int32 -> unit -> input_elt
(** Creates a checkbox [<input>] tag that will have an int32 value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val int64_checkbox :
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `Set of int64 ] param_name -> value:int64 -> unit -> input_elt
(** Creates a checkbox [<input>] tag that will have an int64 value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val float_checkbox :
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `Set of float ] param_name -> value:float -> unit -> input_elt
(** Creates a checkbox [<input>] tag that will have a float value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val string_checkbox :
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `Set of string ] param_name -> value:string ->
            unit -> input_elt
(** Creates a checkbox [<input>] tag that will have a string value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val user_type_checkbox : ('a -> string) ->
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `Set of 'a ] param_name -> value:'a ->
            unit -> input_elt
(** Creates a checkbox [<input>] tag that will have a "user type" value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val raw_checkbox :
        ?a:input_attrib_t -> ?checked:bool ->
          name:string -> value:string -> unit -> input_elt
(** Creates a checkbox [<input>] tag with untyped content.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [any].
 *)


    val string_radio :
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `Radio of string ] param_name ->
            value:string -> unit -> input_elt
(** Creates a radio [<input>] tag with string content *)

    val int_radio :
        ?a:input_attrib_t -> ?checked:bool ->
           name:[ `Radio of int ] param_name ->
             value:int -> unit -> input_elt
(** Creates a radio [<input>] tag with int content *)

    val int32_radio :
        ?a:input_attrib_t -> ?checked:bool ->
           name:[ `Radio of int32 ] param_name ->
             value:int32 -> unit -> input_elt
(** Creates a radio [<input>] tag with int32 content *)

    val int64_radio :
        ?a:input_attrib_t -> ?checked:bool ->
           name:[ `Radio of int64 ] param_name ->
             value:int64 -> unit -> input_elt
(** Creates a radio [<input>] tag with int64 content *)

    val float_radio :
        ?a:input_attrib_t -> ?checked:bool ->
           name:[ `Radio of float ] param_name ->
             value:float -> unit -> input_elt
(** Creates a radio [<input>] tag with float content *)

    val user_type_radio : ('a -> string) ->
        ?a:input_attrib_t -> ?checked:bool ->
           name:[ `Radio of 'a ] param_name ->
             value:'a -> unit -> input_elt
(** Creates a radio [<input>] tag with user_type content *)

    val raw_radio :
        ?a:input_attrib_t -> ?checked:bool ->
          name:string -> value:string -> unit -> input_elt
(** Creates a radio [<input>] tag with untyped string content (low level) *)


    val string_button :
        ?a:button_attrib_t ->
          name:[< string setone ] param_name -> value:string ->
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with string content *)

    val int_button :
        ?a:button_attrib_t ->
          name:[< int setone ] param_name -> value:int ->
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with int content *)

    val int32_button :
        ?a:button_attrib_t ->
          name:[< int32 setone ] param_name -> value:int32 ->
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with int32 content *)

    val int64_button :
        ?a:button_attrib_t ->
          name:[< int64 setone ] param_name -> value:int64 ->
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with int64 content *)

    val float_button :
        ?a:button_attrib_t ->
          name:[< float setone ] param_name -> value:float ->
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with float content *)

    val user_type_button : ('a -> string) ->
        ?a:button_attrib_t ->
          name:[< 'a setone ] param_name -> value:'a ->
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with user_type content *)

    val raw_button :
        ?a:button_attrib_t ->
          button_type:button_type_t ->
            name:string -> value:string ->
              button_content_elt_list -> button_elt
(** Creates a [<button>] tag with untyped string content *)

    val button :
        ?a:button_attrib_t ->
          button_type:button_type_t ->
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with no value. No value is sent. *)




    val textarea :
        ?a:textarea_attrib_t ->
          name:[< string setoneradio ] param_name -> ?value:string ->
            rows:int -> cols:int -> unit -> textarea_elt
(** Creates a [<textarea>] tag *)

    val raw_textarea :
        ?a:textarea_attrib_t ->
          name:string -> ?value:string ->
            rows:int -> cols:int -> unit -> textarea_elt
(** Creates a [<textarea>] tag for untyped form *)

    type 'a soption =
        option_attrib_t
          * 'a (* Content (or value if the following is present) *)
          * pcdata_elt option (* if content different from value *)
          * bool (* selected *)

    type 'a select_opt =
      | Optgroup of
          optgroup_attrib_t
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

    val raw_select :
        ?a:select_attrib_t ->
          name:string ->
            string select_opt ->
              string select_opt list ->
                select_elt
(** Creates a [<select>] tag for any (untyped) value. *)

    val int_select :
        ?a:select_attrib_t ->
          name:[< `One of int ] param_name ->
            int select_opt ->
              int select_opt list ->
                select_elt
(** Creates a [<select>] tag for int values. *)

    val int32_select :
        ?a:select_attrib_t ->
          name:[< `One of int32 ] param_name ->
            int32 select_opt ->
              int32 select_opt list ->
                select_elt
(** Creates a [<select>] tag for int32 values. *)

    val int64_select :
        ?a:select_attrib_t ->
          name:[< `One of int64 ] param_name ->
            int64 select_opt ->
              int64 select_opt list ->
                select_elt
(** Creates a [<select>] tag for int64 values. *)

    val float_select :
        ?a:select_attrib_t ->
          name:[< `One of float ] param_name ->
            float select_opt ->
              float select_opt list ->
                select_elt
(** Creates a [<select>] tag for float values. *)

    val string_select :
        ?a:select_attrib_t ->
          name:[< `One of string ] param_name ->
            string select_opt ->
              string select_opt list ->
                select_elt
(** Creates a [<select>] tag for string values. *)

    val user_type_select : ('a -> string) ->
        ?a:select_attrib_t ->
          name:[< `One of 'a ] param_name ->
            'a select_opt ->
              'a select_opt list ->
                  select_elt
(** Creates a [<select>] tag for user type values. *)

    val raw_multiple_select :
        ?a:select_attrib_t ->
          name:string ->
            string select_opt ->
              string select_opt list ->
                select_elt
(** Creates a [<select>] tag for any (untyped) value. *)

    val int_multiple_select :
        ?a:select_attrib_t ->
          name:[< `Set of int ] param_name ->
            int select_opt ->
              int select_opt list ->
                select_elt
(** Creates a [<select>] tag for int values. *)

    val int32_multiple_select :
        ?a:select_attrib_t ->
          name:[< `Set of int32 ] param_name ->
            int32 select_opt ->
              int32 select_opt list ->
                select_elt
(** Creates a [<select>] tag for int32 values. *)

    val int64_multiple_select :
        ?a:select_attrib_t ->
          name:[< `Set of int64 ] param_name ->
            int64 select_opt ->
              int64 select_opt list ->
                select_elt
(** Creates a [<select>] tag for int64 values. *)

    val float_multiple_select :
        ?a:select_attrib_t ->
          name:[< `Set of float ] param_name ->
            float select_opt ->
              float select_opt list ->
                select_elt
(** Creates a [<select>] tag for float values. *)

    val string_multiple_select :
        ?a:select_attrib_t ->
          name:[< `Set of string ] param_name ->
            string select_opt ->
              string select_opt list ->
                select_elt
(** Creates a [<select>] tag for string values. *)

    val user_type_multiple_select :
      ('a -> string) ->
      ?a:select_attrib_t ->
      name:[< `Set of 'a ] param_name ->
      'a select_opt ->
      'a select_opt list ->
      select_elt
(** Creates a [<select>] tag for user type values. *)


end



let make_proto_prefix
    ~sp
    ?hostname
    ?port
    https
    : string =
  let ssl = Eliom_sessions.get_ssl ~sp in
  let host = match hostname with
    | None -> Eliom_sessions.get_hostname ~sp 
    | Some h -> h
  in
  let port = 
    match port with
      | Some p -> p
      | None ->
          if https = ssl
          then Eliom_sessions.get_server_port ~sp 
          else if https
          then Eliom_sessions.get_default_sslport ~sp
          else Eliom_sessions.get_default_port ~sp
  in
  Ocsigen_lib.make_absolute_url https host port "/"



let make_uri_components_ (* does not take into account getparams *)
    ?(absolute = false) (* used to force absolute link *)
    ?(absolute_path = false) (* used to force absolute link without protocol/server/port *)
    ?https
    ~service
    ~sp
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?(nl_params = Eliom_parameters.empty_nl_params_set) 
    () =

  let ssl = Eliom_sessions.get_ssl ~sp in
  let https = 
    (https = Some true) || 
      (Eliom_services.get_https service) ||
      (https = None && ssl)
  in
  let absolute = 
    if absolute || https <> ssl 
    then Some (make_proto_prefix ~sp ?hostname ?port https)
    else if absolute_path
    then Some "/"
    else None 
  in
(*VVV We trust current protocol? *) 



  let nl_params = Eliom_parameters.table_of_nl_params_set nl_params in
  let keep_nl_params = match keep_nl_params with
    | None -> Eliom_services.keep_nl_params service
    | Some b -> b
  in
  (* for preapplied non localized and not non localized: *)
  let preappnlp, preapplied_params = get_pre_applied_parameters_ service in
  let nlp =
    match keep_nl_params with
      | `All ->
          (* We replace current nl params by preapplied ones *)
          Ocsigen_lib.String_Table.fold
            (fun key v b -> Ocsigen_lib.String_Table.add key v b)
            preappnlp
            (Eliom_sessions.get_nl_get_params ~sp)
      | `Persistent ->
          (* We replace current nl params by preapplied ones *)
          Ocsigen_lib.String_Table.fold
            (fun key v b -> Ocsigen_lib.String_Table.add key v b)
            preappnlp
            (Eliom_sessions.get_persistent_nl_get_params ~sp)
      | `None -> preappnlp
  in
  let nlp =
    (* We replace current nl params by nl_params *)
    Ocsigen_lib.String_Table.fold
      (fun key v b -> Ocsigen_lib.String_Table.add key v b)
      nl_params
      nlp
  in

  (* remove in nlp the one present in the service parameters *)
  let getparamstype = get_get_params_type_ service in
  let nlp = Eliom_parameters.remove_from_nlp nlp getparamstype in
  let hiddenparams = 
    Ocsigen_lib.String_Table.fold
      (fun _ l beg -> l@beg)
      nlp preapplied_params
  in


  match get_kind_ service with
    | `Attached attser ->
        begin
          let uri =
            let suff= None in
            if (get_att_kind_ attser) = `External
            then
              (get_prefix_ attser)^
                "/"^  (* we add the "/" even if there is no prefix,
                         because we should do absolute links in that case *)
                (reconstruct_absolute_url_path (get_full_path_ attser) suff)
            else
              match absolute with
                | Some proto_prefix ->
                    proto_prefix^
                      reconstruct_absolute_url_path
                      (get_full_path_ attser) suff
                | None ->
                    reconstruct_relative_url_path_string
                      (get_original_full_path sp)
                      (get_full_path_ attser) suff
          in
          match get_get_name_ attser with
            | Eliom_common.SAtt_no ->
                (uri, hiddenparams, fragment)
            | Eliom_common.SAtt_anon s ->
                (uri, 
                 ((Eliom_common.get_numstate_param_name, s)::hiddenparams),
                 fragment)
            | Eliom_common.SAtt_named s ->
                (uri,
                 ((Eliom_common.get_state_param_name, s)::hiddenparams),
                 fragment)
            | Eliom_common.SAtt_csrf_safe csrf_info ->
                let s = 
                  Eliom_services.register_delayed_get_or_na_coservice
                    ~sp csrf_info
                in
                (uri, 
                 ((Eliom_common.get_numstate_param_name, s)::hiddenparams),
                 fragment)
                  

        end
    | `Nonattached naser ->
        let na_name = get_na_name_ naser in
        let params' =
          let current_get_params =
            (if na_name = Eliom_common.SNa_void_keep
             then (Eliom_sessions.get_si sp).Eliom_common.si_all_get_but_nl
             else Lazy.force 
               (Eliom_sessions.get_si sp).Eliom_common.si_all_get_but_na_nl)
          in
          (match na_name with
             | Eliom_common.SNa_void_keep
             | Eliom_common.SNa_void_dontkeep -> current_get_params
             | Eliom_common.SNa_get' n ->
                 (Eliom_common.naservice_num, n)::current_get_params
             | Eliom_common.SNa_get_ n ->
                 (Eliom_common.naservice_name, n)::current_get_params
             | Eliom_common.SNa_get_csrf_safe csrf_info ->
                 let n = 
                   Eliom_services.register_delayed_get_or_na_coservice
                     ~sp csrf_info
                 in
                 (Eliom_common.naservice_num, n)::current_get_params
             | _ -> assert false)
        in
        let params = params'@hiddenparams in
        let beg =
          match absolute with
            | Some proto_prefix ->
                proto_prefix^ get_original_full_path_string sp
            | None -> 
                relative_url_path_to_myself (get_original_full_path sp)
        in
        (beg, params, fragment)



let make_uri_components
    ?absolute
    ?absolute_path
    ?https
    ~service
    ~sp
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?nl_params
    getparams =
  let (uri, pregetparams, fragment) =
    make_uri_components_
      ?absolute
      ?absolute_path
      ?https
      ~service
      ~sp
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?nl_params
      ()
  in
  (* for getparams and non localized params: *)
  let suff, params =
    construct_params_list
      String_Table.empty
      (get_get_params_type_ service) getparams 
      (* if nl params were already present, they will be replaced
         by new values *)
  in
  let uri =
    match suff with
      | None -> uri
      | Some suff ->
          if uri = "./" 
          then 
            let r = string_of_url_path' suff in
            if String.length r = 0 then "./" else r
          else String.concat "/" [uri; string_of_url_path' suff]
  in
  let fragment = Ocsigen_lib.apply_option Ocsigen_lib.encode fragment in
  (uri, (params@pregetparams), fragment)



let make_string_uri_from_components (uri, params, fragment) =
  let s = Ocsigen_lib.concat_strings uri "?" (construct_params_string params) in
  match fragment with
    | None -> s
    | Some f -> String.concat "#" [s; f]


let make_string_uri
    ?absolute
    ?absolute_path
    ?https
    ~service
    ~sp
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?nl_params
    getparams : string =
  make_string_uri_from_components
    (make_uri_components
       ?absolute
       ?absolute_path
       ?https
       ~service
       ~sp
       ?hostname
       ?port
       ?fragment
       ?keep_nl_params
       ?nl_params
       getparams)




let make_post_uri_components_ (* do not take into account postparams *)
    ?(absolute = false)
    ?(absolute_path = false)
    ?https
    ~service
    ~sp
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?(nl_params = Eliom_parameters.empty_nl_params_set) 
    ?(keep_nl_params : [ `All | `Persistent | `None ] option)
    ?keep_get_na_params
    getparams 
    () =
  match get_kind_ service with
    | `Attached attser ->

        let (uri, getparams, fragment), getname =
          let getname = get_get_name_ attser in
          match getname with
            | Eliom_common.SAtt_csrf_safe csrf_info ->
                (* special case for post-coservices on get csrf safe services:
                   we must register the get service first *)
                let s = 
                  Eliom_common.SAtt_anon 
                    (Eliom_services.register_delayed_get_or_na_coservice
                       ~sp csrf_info)
                in
                (make_uri_components
                   ~absolute
                   ~absolute_path
                   ?https
                   ~service:(Eliom_services.change_get_num service attser s)
                   ~sp
                   ?hostname
                   ?port
                   ?fragment
                   ?keep_nl_params
                   ~nl_params
                   getparams,
                 s)
            | _ -> (make_uri_components
                      ~absolute
                      ~absolute_path
                      ?https
                      ~service
                      ~sp
                      ?hostname
                      ?port
                      ?fragment
                      ?keep_nl_params
                      ~nl_params
                      getparams,
                    getname)
        in


        let postparams =
          match get_post_name_ attser with
            | Eliom_common.SAtt_no -> []
            | Eliom_common.SAtt_anon s ->
                [(Eliom_common.post_numstate_param_name, s)]
            | Eliom_common.SAtt_named s ->
                [(Eliom_common.post_state_param_name, s)]
            | Eliom_common.SAtt_csrf_safe csrf_info ->
                let s =
                  Eliom_services.register_delayed_post_coservice
                    ~sp csrf_info getname
                in
                [(Eliom_common.post_numstate_param_name, s)]
        in
        (uri, getparams, fragment, postparams)


    | `Nonattached naser ->

            let nl_params = Eliom_parameters.table_of_nl_params_set nl_params in
            let keep_nl_params = match keep_nl_params with
              | None -> Eliom_services.keep_nl_params service
              | Some b -> b
            in
            let preappnlp, preapp = get_pre_applied_parameters_ service in
            let nlp =
              match keep_nl_params with
                | `All ->
                    (* We replace current nl params by preapplied ones *)
                    Ocsigen_lib.String_Table.fold
                      (fun key v b -> Ocsigen_lib.String_Table.add key v b)
                      preappnlp
                      (Eliom_sessions.get_nl_get_params ~sp)
                | `Persistent ->
                    (* We replace current nl params by preapplied ones *)
                    Ocsigen_lib.String_Table.fold
                      (fun key v b -> Ocsigen_lib.String_Table.add key v b)
                      preappnlp
                      (Eliom_sessions.get_persistent_nl_get_params ~sp)
                | `None -> preappnlp
            in
            let nlp =
              (* We replace current nl params by nl_params *)
              Ocsigen_lib.String_Table.fold
                (fun key v b -> Ocsigen_lib.String_Table.add key v b)
                nl_params
                nlp
            in


            (* for getparams and non localized params: *)
            let suff, params =
              construct_params_list
                nlp (get_get_params_type_ service) getparams 
                (* if nl params were already present, they will be replaced
                   by new values *)
                (* getparams can be something else than [] 
                   if we have added nl params to the service (?) *)
            in
            let params = params @ preapp in

            let keep_get_na_params =
              match keep_get_na_params with
                | Some b -> b
                | None ->
                    match get_na_kind_ naser with
                      | `Post b -> b
                      | _ -> assert false
            in
            let params =
              params @
              (if keep_get_na_params
               then
                 (Eliom_sessions.get_si sp).Eliom_common.si_all_get_but_nl
               else
                 (Lazy.force
                   (Eliom_sessions.get_si sp).Eliom_common.si_all_get_but_na_nl))
            in


            let ssl = Eliom_sessions.get_ssl ~sp in
            let https = 
              (https = Some true) || 
                (Eliom_services.get_https service) ||
                (https = None && ssl)
            in
            let absolute = 
              if absolute || https <> ssl 
              then Some (make_proto_prefix ~sp ?hostname ?port https)
              else if absolute_path
              then Some "/"
              else None
            in


            (* absolute URL does not work behind a reverse proxy! *)
            let uri =
              match absolute with
                | Some proto_prefix ->
                    proto_prefix^get_original_full_path_string sp
                | None ->
                    relative_url_path_to_myself (get_original_full_path sp)
            in

            let naservice_line =
              match get_na_name_ naser with
               | Eliom_common.SNa_post' n -> (Eliom_common.naservice_num, n)
               | Eliom_common.SNa_post_ n -> (Eliom_common.naservice_name, n)
               | Eliom_common.SNa_post_csrf_safe csrf_info ->
                 let n =
                   Eliom_services.register_delayed_get_or_na_coservice
                     ~sp csrf_info
                 in
                 (Eliom_common.naservice_num, n)
               | _ -> assert false
            in


            let fragment = None (* fragment is not sent to the server *) in

            let postparams = [naservice_line] in
            (uri, params, fragment, postparams)



let make_post_uri_components
    ?absolute
    ?absolute_path
    ?https
    ~service
    ~sp
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?nl_params
    ?keep_get_na_params
    getparams 
    postparams =

  let (uri, getparams, fragment, prepostparams) =
    make_post_uri_components_
      ?absolute
      ?absolute_path
      ?https
      ~service
      ~sp
      ?hostname
      ?port
      ?fragment
      ?keep_nl_params
      ?nl_params
      ?keep_get_na_params
      getparams 
      () 
  in
  let _, postparams =
    construct_params_list
      Ocsigen_lib.String_Table.empty
      (get_post_params_type_ service) 
      postparams
  in
  (uri, getparams, fragment, postparams@prepostparams)


module MakeForms = functor
  (Pages : FORMCREATE) ->
    (struct

      type form_content_elt = Pages.form_content_elt
      type form_content_elt_list = Pages.form_content_elt_list
      type form_elt = Pages.form_elt
      type a_content_elt = Pages.a_content_elt
      type a_content_elt_list = Pages.a_content_elt_list
      type a_elt = Pages.a_elt
      type a_elt_list = Pages.a_elt_list
      type div_content_elt = Pages.div_content_elt
      type div_content_elt_list = Pages.div_content_elt_list
      type uri = Pages.uri
      type link_elt = Pages.link_elt
      type script_elt = Pages.script_elt
      type textarea_elt = Pages.textarea_elt
      type input_elt = Pages.input_elt
      type pcdata_elt = Pages.pcdata_elt
      type select_elt = Pages.select_elt
      type select_content_elt = Pages.select_content_elt
      type select_content_elt_list = Pages.select_content_elt_list
      type button_elt = Pages.button_elt
      type button_content_elt = Pages.button_content_elt
      type button_content_elt_list = Pages.button_content_elt_list
      type option_elt = Pages.option_elt
      type option_elt_list = Pages.option_elt_list

      type a_attrib_t = Pages.a_attrib_t
      type form_attrib_t = Pages.form_attrib_t
      type input_attrib_t = Pages.input_attrib_t
      type textarea_attrib_t = Pages.textarea_attrib_t
      type select_attrib_t = Pages.select_attrib_t
      type link_attrib_t = Pages.link_attrib_t
      type script_attrib_t = Pages.script_attrib_t
      type optgroup_attrib_t = Pages.optgroup_attrib_t
      type option_attrib_t = Pages.option_attrib_t
      type button_attrib_t = Pages.button_attrib_t

      type input_type_t = Pages.input_type_t
      type button_type_t = Pages.button_type_t


(** Functions to construct web pages: *)


      let make_proto_prefix = make_proto_prefix

      let make_string_uri = make_string_uri

      let make_uri_components = make_uri_components

      let make_post_uri_components = make_post_uri_components

      let make_uri
          ?absolute
          ?absolute_path
          ?https ~service ~sp ?hostname ?port ?fragment
          ?keep_nl_params ?nl_params gp =
        Pages.uri_of_string (make_string_uri
                               ?absolute ?absolute_path
                               ?https ?fragment ~service ~sp
                               ?hostname ?port ?keep_nl_params ?nl_params gp)


      let a 
          ?absolute
          ?absolute_path
          ?https
          ?a
          ~service
          ~sp
          ?hostname
          ?port
          ?fragment
          ?keep_nl_params
          ?nl_params
          content
          getparams =
        let href = 
          make_string_uri
            ?absolute
            ?absolute_path
            ?keep_nl_params
            ?nl_params
            ?https ~service ~sp ?hostname ?port ?fragment getparams
        in
        Pages.make_a ?a ~href content

      let get_form_
          bind
          return
          ?absolute
          ?absolute_path
          ?https
          ?a
          ~service
          ~sp
          ?hostname
          ?port
          ?fragment
          ?nl_params
          ?keep_nl_params
          f =

        let (uri, hiddenparams, fragment) =
          make_uri_components_
            ?absolute
            ?absolute_path
            ?https
            ~service
            ~sp
            ?hostname
            ?port
            ?fragment
            ?nl_params
            ?keep_nl_params
            ()
        in

        let getparamstype = get_get_params_type_ service in
        let issuffix, paramnames = make_params_names getparamstype in

        let uri = 
          if issuffix 
          then if uri.[String.length uri - 1] = '/'
          then uri^Eliom_common.eliom_nosuffix_page
          else String.concat "/" [uri; Eliom_common.eliom_nosuffix_page]
          else uri
        in
        let uri = 
          match fragment with
            | None -> uri
            | Some f -> String.concat "#" [uri; Ocsigen_lib.encode f]
        in

        bind (f paramnames)
          (fun inside ->
             let inside =
               List.fold_left
                 (fun s (n,v) ->
                    Pages.cons_form
                      (Pages.make_hidden_field
                         (Some (Pages.make_input
                                  ~typ:Pages.hidden
                                  ~name:n ~value:v ())))
                      s
                 )
                 inside
                 hiddenparams
             in
             let i1, i = Pages.remove_first inside in
             return (Pages.make_get_form ?a ~action:uri i1 i))



      let get_form
          ?absolute ?absolute_path
          ?https ?a ~service ~sp ?hostname ?port ?fragment
          ?keep_nl_params ?nl_params f =
        get_form_ (fun x f -> f x) (fun x -> x) 
          ?absolute ?absolute_path
          ?https ?a ~service ~sp ?keep_nl_params
          ?nl_params ?hostname ?port ?fragment f

      let lwt_get_form
          ?absolute ?absolute_path
          ?https ?a ~service ~sp ?hostname ?port ?fragment
          ?keep_nl_params ?nl_params f =
        get_form_ 
          Lwt.bind Lwt.return
          ?absolute ?absolute_path
          ?https ?a ~service ~sp ?hostname ?port ?fragment
          ?nl_params ?keep_nl_params f


      let post_form_
          bind
          return
          ?absolute
          ?absolute_path
          ?https
          ?a
          ~service
          ~sp
          ?hostname
          ?port
          ?fragment
          ?nl_params
          ?(keep_nl_params : [ `All | `Persistent | `None ] option)
          ?keep_get_na_params
          f
          getparams =

        let getparamstype = get_post_params_type_ service in
        let _, paramnames = make_params_names getparamstype in

        let (uri, getparams, fragment, hiddenparams) = 
          make_post_uri_components_
            ?absolute
            ?absolute_path
            ?https
            ~service
            ~sp
            ?hostname
            ?port
            ?fragment
            ?keep_nl_params
            ?nl_params
            ?keep_get_na_params
            getparams
            ()
        in
        
        bind (f paramnames)
          (fun inside ->
             let inside =
               List.fold_left
                 (fun s (n,v) ->
                    Pages.cons_form
                      (Pages.make_hidden_field
                         (Some (Pages.make_input
                                  ~typ:Pages.hidden
                                  ~name:n ~value:v ())))
                      s
                 )
                 inside
                 hiddenparams
             in
             let i1, i = Pages.remove_first inside in
             let uri = 
               make_string_uri_from_components (uri, getparams, fragment) 
             in
             return (Pages.make_post_form ?a ~action:uri i1 i))



      let post_form
          ?absolute ?absolute_path
          ?https ?a ~service ~sp ?hostname ?port
          ?fragment ?keep_nl_params ?keep_get_na_params 
          ?nl_params f getparams =
        post_form_ (fun x f -> f x) (fun x -> x)
          ?absolute ?absolute_path
          ?https ?a ~service ~sp ?hostname ?port
          ?fragment ?keep_get_na_params 
          ?keep_nl_params ?nl_params f getparams

      let lwt_post_form
          ?absolute ?absolute_path
          ?https ?a ~service ~sp ?hostname ?port
          ?fragment ?keep_nl_params ?keep_get_na_params 
          ?nl_params f getparams =
        post_form_ Lwt.bind Lwt.return
          ?absolute ?absolute_path
          ?https ?a ~service ~sp ?hostname ?port
          ?fragment ?keep_get_na_params ?keep_nl_params ?nl_params f getparams





      let js_script = Pages.make_js_script
      let css_link = Pages.make_css_link


      let gen_input ?a ~(input_type : input_type_t)
          ?value ?src
          ?name (string_of : 'a -> string) =
        let name = match name with
        | None -> None
        | Some n -> Some (string_of_param_name n)
        in
        (match value with
        | None ->
            Pages.make_input ?a ~typ:input_type ?name ?src ()
        | Some v ->
            Pages.make_input
              ?a
              ~value:(string_of v)
              ~typ:input_type
              ?src
              ?name
              ())

      let int_input ?a ~input_type
          ?name ?value () =
        gen_input ?a ~input_type ?value ?name string_of_int

      let int32_input ?a ~input_type
          ?name ?value () =
        gen_input ?a ~input_type ?value ?name Int32.to_string

      let int64_input ?a ~input_type
          ?name ?value () =
        gen_input ?a ~input_type ?value ?name Int64.to_string

      let float_input ?a ~input_type
          ?name ?value () =
        gen_input ?a ~input_type ?value ?name string_of_float

      let string_input ?a ~input_type
          ?name ?value () =
        gen_input ?a ~input_type ?value ?name id

      let user_type_input string_of ?a ~input_type
          ?name ?value () =
        gen_input ?a ~input_type ?value ?name string_of

      let raw_input ?a ~input_type ?name ?value () =
        (match value with
        | None ->
            Pages.make_input ?a ~typ:input_type ?name ()
        | Some v ->
            Pages.make_input
              ?a
              ~value:v
              ~typ:input_type
              ?name
              ())

      let file_input ?a ~name () =
        Pages.make_input ?a ~typ:Pages.file ~name:(string_of_param_name name) ()
      (* value attribute not supported by browsers for security reasons *)

      let image_input ?a ~name ?src () =
        Pages.make_input
          ?a ~typ:Pages.image
          ~name:(string_of_param_name name) ?src ()
    (* The behaviour of <input type="image"> without name attribute
       depends on browsers *)

      let int_image_input ?a ~name ~value ?src () =
        gen_input ?a ~input_type:Pages.image ~name
          ~value ?src string_of_int

      let int32_image_input ?a ~name ~value ?src () =
        gen_input ?a ~input_type:Pages.image ~name
          ~value ?src Int32.to_string

      let int64_image_input ?a ~name ~value ?src () =
        gen_input ?a ~input_type:Pages.image ~name
          ~value ?src Int64.to_string

      let float_image_input ?a ~name ~value ?src () =
        gen_input ?a ~input_type:Pages.image ~name
          ~value ?src string_of_float

      let string_image_input ?a ~name ~value ?src () =
        gen_input ?a ~input_type:Pages.image ~name
          ~value ?src id

      let user_type_image_input string_of ?a ~name ~value ?src () =
        gen_input ?a ~input_type:Pages.image ~name
          ~value ?src string_of

      let raw_image_input ?a ~(name : string) ~value ?src () =
        Pages.make_input
          ?a
          ~value
          ~typ:Pages.image
          ?src
          ~name
          ()

      let bool_checkbox ?a ?checked ~name () =
        Pages.make_input ?a ?checked ~typ:Pages.checkbox
          ~name:(string_of_param_name name) ()

      let int_checkbox ?a ?checked ~name ~value () =
        Pages.make_input ?a ?checked ~typ:Pages.checkbox
          ~name:(string_of_param_name name) ~value:(string_of_int value) ()

      let int32_checkbox ?a ?checked ~name ~value () =
        Pages.make_input ?a ?checked ~typ:Pages.checkbox
          ~name:(string_of_param_name name) ~value:(Int32.to_string value) ()

      let int64_checkbox ?a ?checked ~name ~value () =
        Pages.make_input ?a ?checked ~typ:Pages.checkbox
          ~name:(string_of_param_name name) ~value:(Int64.to_string value) ()

      let float_checkbox ?a ?checked ~name ~value () =
        Pages.make_input ?a ?checked ~typ:Pages.checkbox
          ~name:(string_of_param_name name) ~value:(string_of_float value) ()

      let string_checkbox ?a ?checked ~name ~value () =
        Pages.make_input ?a ?checked ~typ:Pages.checkbox
          ~name:(string_of_param_name name) ~value ()

      let user_type_checkbox string_of ?a ?checked ~name ~value () =
        Pages.make_input ?a ?checked ~typ:Pages.checkbox
          ~name:(string_of_param_name name) ~value:(string_of value) ()

      let raw_checkbox ?a ?checked ~name ~value () =
        Pages.make_input ?a ?checked ~typ:Pages.checkbox
          ~name:name ~value ()


      let string_radio ?a ?checked ~name ~value () =
        Pages.make_input
          ?a ?checked ~typ:Pages.radio
          ~name:(string_of_param_name name) ~value ()

      let int_radio ?a ?checked ~name ~value () =
        Pages.make_input
          ?a ?checked ~typ:Pages.radio
          ~name:(string_of_param_name name) ~value:(string_of_int value) ()

      let int32_radio ?a ?checked ~name ~value () =
        Pages.make_input
          ?a ?checked ~typ:Pages.radio
          ~name:(string_of_param_name name) ~value:(Int32.to_string value) ()

      let int64_radio ?a ?checked ~name ~value () =
        Pages.make_input
          ?a ?checked ~typ:Pages.radio
          ~name:(string_of_param_name name) ~value:(Int64.to_string value) ()

      let float_radio ?a ?checked ~name ~value () =
        Pages.make_input
          ?a ?checked ~typ:Pages.radio
          ~name:(string_of_param_name name) ~value:(string_of_float value) ()

      let user_type_radio string_of ?a ?checked ~name ~value () =
        Pages.make_input
          ?a ?checked ~typ:Pages.radio
          ~name:(string_of_param_name name) ~value:(string_of value) ()

      let raw_radio ?a ?checked ~(name : string) ~value () =
        Pages.make_input
          ?a ?checked ~typ:Pages.radio
          ~name:name ~value:value ()

      let string_button ?a ~name ~value c =
        Pages.make_button ?a ~button_type:Pages.buttonsubmit
          ~name:(string_of_param_name name) ~value c

      let int_button ?a ~name ~value c =
        Pages.make_button ?a ~button_type:Pages.buttonsubmit
          ~name:(string_of_param_name name) ~value:(string_of_int value) c

      let int32_button ?a ~name ~value c =
        Pages.make_button ?a ~button_type:Pages.buttonsubmit
          ~name:(string_of_param_name name) ~value:(Int32.to_string value) c

      let int64_button ?a ~name ~value c =
        Pages.make_button ?a ~button_type:Pages.buttonsubmit
          ~name:(string_of_param_name name) ~value:(Int64.to_string value) c

      let float_button ?a ~name ~value c =
        Pages.make_button ?a ~button_type:Pages.buttonsubmit
          ~name:(string_of_param_name name) ~value:(string_of_float value) c

      let user_type_button string_of ?a ~name ~value c =
        Pages.make_button ?a ~button_type:Pages.buttonsubmit
          ~name:(string_of_param_name name) ~value:(string_of value) c

      let raw_button ?a ~button_type ~name ~value c =
        Pages.make_button ?a ~button_type ~name ~value c

      let button ?a ~button_type c =
        Pages.make_button ?a ~button_type c


      let textarea ?a ~name =
        Pages.make_textarea ?a ~name:(string_of_param_name name)

      let raw_textarea ?a ~name =
        Pages.make_textarea ?a ~name



      type 'a soption =
          option_attrib_t
            * 'a (* Content (or value if the following is present) *)
            * pcdata_elt option (* if content different from value *)
            * bool (* selected *)

      type 'a select_opt =
        | Optgroup of
            optgroup_attrib_t
              * string (* label *)
              * 'a soption
              * 'a soption list
        | Option of 'a soption

      let gen_select ?a ?(multiple=false) ~name
          (fl : 'a select_opt) (ol : 'a select_opt list) string_of =


        let normalize_selected l =
          (* We change the list of option to have exactly one selected item.
             We do this because the behaviour of browsers differs.
             We select the first one if nothing is selected.
             We select the first selected if several are selected.
             Thus all browsers will behave the same way.
           *)
          let aux1 trouve ((a, b, c, selected) as line) =
            if trouve
            then ((a, b, c, false), true)
            else if selected
            then (line, true)
            else (line, false)
          in
          let rec aux2 trouve = function
            | line::l ->
                let (line, trouve) = aux1 trouve line in
                let (l, trouve) = aux2 trouve l in
                (line::l, trouve)
            | [] -> ([], trouve)
          in
          let rec aux trouve = function
            | (Option line)::l ->
                let (line, trouve) = aux1 trouve line in
                let (l, trouve) = aux trouve l in
                ((Option line)::l, trouve)
            | (Optgroup (a, b, fl, ol))::l ->
                let (fl, trouve) = aux1 trouve fl in
                let (ol, trouve) = aux2 trouve ol in
                let (l, trouve) = aux trouve l in
                ((Optgroup (a, b, fl, ol))::l, trouve)
            | [] -> ([], trouve)
          in
          let select_first = function
            | Option (a, b, c, _) -> Option (a, b, c, true)
            | Optgroup (a, b, (c, d, e, _), ol) ->
                Optgroup (a, b, (c, d, e, true), ol)
          in
            let (newl, trouve) = aux false l in
            if trouve
            then ((List.hd newl), (List.tl newl))
            else
              let first = List.hd newl in
              (* We select the first one by default *)
              ((select_first first), (List.tl newl))
        in


        let (fl, ol) =
          if multiple
          then (fl, ol)
          else normalize_selected (fl::ol)
        in
        let make_opt (a, cv, co, sel) =
          (match co with
          | None -> Pages.make_option ~a ~selected:sel
                (Pages.make_pcdata (string_of cv))
          | Some c -> Pages.make_option ~a ~selected:sel
                ~value:(string_of cv) c)
        in
        let rec make_optg = function
          | Option o -> Pages.select_content_of_option (make_opt o)
          | Optgroup (a, label, og1, ogl) ->
              Pages.make_optgroup
                ~a ~label (make_opt og1) (Pages.map_option make_opt ogl)
        in
        let fl2,ol2 = Pages.map_optgroup make_optg fl ol in
        Pages.make_select ?a ~multiple ~name fl2 ol2

      let raw_select ?a ~(name : string)
          (fl : string select_opt) (ol : string select_opt list) =
        gen_select ?a ~multiple:false ~name fl ol id

      let int_select ?a ~name
          (fl : int select_opt) (ol : int select_opt list) =
        gen_select ?a ~multiple:false
          ~name:(string_of_param_name name) fl ol string_of_int

      let int32_select ?a ~name
          (fl : int32 select_opt) (ol : int32 select_opt list) =
        gen_select ?a ~multiple:false
          ~name:(string_of_param_name name) fl ol Int32.to_string

      let int64_select ?a ~name
          (fl : int64 select_opt) (ol : int64 select_opt list) =
        gen_select ?a ~multiple:false
          ~name:(string_of_param_name name) fl ol Int64.to_string

      let float_select ?a ~name
          (fl : float select_opt) (ol : float select_opt list) =
        gen_select ?a ~multiple:false
          ~name:(string_of_param_name name) fl ol string_of_float

      let string_select ?a ~name
          (fl : string select_opt) (ol : string select_opt list) =
        gen_select ?a ~multiple:false
          ~name:(string_of_param_name name) fl ol id

      let user_type_select string_of ?a ~name (fl : 'a select_opt)
          (ol : 'a select_opt list) =
        gen_select ?a ~multiple:false
          ~name:(string_of_param_name name) fl ol string_of



      let raw_multiple_select ?a ~(name : string)
          (fl : string select_opt) (ol : string select_opt list) =
        gen_select ?a ~multiple:true ~name fl ol id

      let int_multiple_select ?a ~name
          (fl : int select_opt) (ol : int select_opt list) =
        gen_select ?a ~multiple:true
          ~name:(string_of_param_name name) fl ol string_of_int

      let int32_multiple_select ?a ~name
          (fl : int32 select_opt) (ol : int32 select_opt list) =
        gen_select ?a ~multiple:true
          ~name:(string_of_param_name name) fl ol Int32.to_string

      let int64_multiple_select ?a ~name
          (fl : int64 select_opt) (ol : int64 select_opt list) =
        gen_select ?a ~multiple:true
          ~name:(string_of_param_name name) fl ol Int64.to_string

      let float_multiple_select ?a ~name
          (fl : float select_opt) (ol : float select_opt list) =
        gen_select ?a ~multiple:true
          ~name:(string_of_param_name name) fl ol string_of_float

      let string_multiple_select ?a ~name
          (fl : string select_opt) (ol : string select_opt list) =
        gen_select ?a ~multiple:true
          ~name:(string_of_param_name name) fl ol id

      let user_type_multiple_select string_of ?a
          ~name (fl : 'a select_opt)
          (ol : 'a select_opt list) =
        gen_select ?a ~multiple:true
          ~name:(string_of_param_name name) fl ol string_of

    end : ELIOMFORMSIG with
type form_content_elt = Pages.form_content_elt
and type form_content_elt_list = Pages.form_content_elt_list
and type form_elt = Pages.form_elt
and type a_content_elt = Pages.a_content_elt
and type a_content_elt_list = Pages.a_content_elt_list
and type a_elt = Pages.a_elt
and type a_elt_list = Pages.a_elt_list
and type div_content_elt = Pages.div_content_elt
and type div_content_elt_list = Pages.div_content_elt_list
and type uri = Pages.uri
and type link_elt = Pages.link_elt
and type script_elt = Pages.script_elt
and type textarea_elt = Pages.textarea_elt
and type input_elt = Pages.input_elt
and type pcdata_elt = Pages.pcdata_elt
and type select_elt = Pages.select_elt
and type select_content_elt = Pages.select_content_elt
and type select_content_elt_list = Pages.select_content_elt_list
and type button_elt = Pages.button_elt
and type button_content_elt = Pages.button_content_elt
and type button_content_elt_list = Pages.button_content_elt_list
and type option_elt = Pages.option_elt
and type option_elt_list = Pages.option_elt_list

and type a_attrib_t = Pages.a_attrib_t
and type form_attrib_t = Pages.form_attrib_t
and type input_attrib_t = Pages.input_attrib_t
and type textarea_attrib_t = Pages.textarea_attrib_t
and type select_attrib_t = Pages.select_attrib_t
and type link_attrib_t = Pages.link_attrib_t
and type script_attrib_t = Pages.script_attrib_t
and type optgroup_attrib_t = Pages.optgroup_attrib_t
and type option_attrib_t = Pages.option_attrib_t
and type button_attrib_t = Pages.button_attrib_t

and type input_type_t = Pages.input_type_t
and type button_type_t = Pages.button_type_t

    )

