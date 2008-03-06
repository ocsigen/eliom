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


(** This module defines the functor to use to creates modules
   generating form widgets for your own types of pages.
   It is used for example in {!Eliom_predefmod}.
 *)


open Lwt
open Extensions
open Eliom_parameters
open Eliom_services



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
    val make_hidden_field : input_elt -> form_content_elt
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
          name:string -> ?value:pcdata_elt -> rows:int -> cols:int ->
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

    val make_full_string_uri :
        service:('get, unit, [< get_service_kind ],
                 [< suff ], 'gn, unit, 
                 [< registrable ]) service ->
                   sp:Eliom_sessions.server_params -> 
                     ?fragment:string ->
                       'get -> string
(** Creates the string corresponding to the 
    full (absolute) URL of a service applied to its GET parameters.
 *)

    val make_string_uri :
        service:('get, unit, [< get_service_kind ],
                 [< suff ], 'gn, unit, 
                 [< registrable ]) service ->
                   sp:Eliom_sessions.server_params -> 
                     ?fragment:string ->
                       'get -> string
(** Creates the string corresponding to the relative URL of a service applied to
   its GET parameters.
 *)

    val make_uri :
        service:('get, unit, [< get_service_kind ],
         [< suff ], 'gn, unit, 
         [< registrable ]) service ->
          sp:Eliom_sessions.server_params -> ?fragment:string -> 'get -> uri
(** Creates the text of (relative) URL for a service. 
    Like the [a] function, it may take extra parameters. *)


    val a :
        ?a:a_attrib_t ->
          service:('get, unit, [< get_service_kind ], 
           [< suff ], 'gn, 'pn,
           [< registrable ]) service ->
            sp:Eliom_sessions.server_params -> 
              ?fragment:string ->
                a_content_elt_list -> 'get -> a_elt
(** [a service sp cont ()] creates a link to [service]. 
   The text of
   the link is [cont]. For example [cont] may be something like
   [\[pcdata "click here"\]]. 

   The last  parameter is for GET parameters.
   For example [a service sp cont (42,"hello")]

   The [~a] optional parameter is used for extra attributes.

   The [~fragment] optional parameter is used for the "fragment" part
   of the URL, that is, the part after character "#".
 *)

    val css_link : ?a:link_attrib_t -> uri:uri -> unit -> link_elt
(** Creates a [<link>] tag for a Cascading StyleSheet (CSS). *)

    val js_script :
        ?a:script_attrib_t -> uri:uri -> unit -> script_elt
(** Creates a [<script>] tag to add a javascript file *)


    val get_form :
        ?a:form_attrib_t ->
          service:('get, unit, [< get_service_kind ],
           [<suff ], 'gn, 'pn, 
           [< registrable ]) service ->
             sp:Eliom_sessions.server_params ->
               ?fragment:string ->
               ('gn -> form_content_elt_list) -> form_elt
(** [get_form service sp formgen] creates a GET form to [service]. 
   The content of
   the form is generated by the function [formgen], that takes the names
   of the service parameters as parameters. *)


    val post_form :
      ?a:form_attrib_t ->
      service:('get, 'post, [< post_service_kind ],
               [< suff ], 'gn, 'pn, 
               [< registrable ]) service ->
      sp:Eliom_sessions.server_params ->
      ?fragment:string ->
      ?keep_get_na_params:bool ->
      ('pn -> form_content_elt_list) -> 'get -> form_elt
(** [post_form service sp formgen] creates a POST form to [service]. 
    The last parameter is for GET parameters (as in the function [a]).
    If the optional parameter [~keep_get_na_params] is [true],
    and if it is a form towards a non-attached POST coservice,
    GET non-attached parameters will be kept in the URL (if any).
    If it is [false], they will be removed.
    Default is the default behaviour for this non-attached service
    (see {!Eliom_services.new_post_coservice'}).
    [~keep_get_na_params] has no effect on attached (co)services.
 *)

(** {2 Form widgets} *)

    val int_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:[< int setoneopt ] param_name ->
            ?value:int -> unit -> input_elt
(** Creates an [<input>] tag for an integer *)

    val float_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:[< float setoneopt ] param_name ->
            ?value:float -> unit -> input_elt
(** Creates an [<input>] tag for a float *)

    val string_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
           ?name:[< string setoneopt ] param_name -> 
             ?value:string -> unit -> input_elt
(** Creates an [<input>] tag for a string *)

    val user_type_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:[< 'a setoneopt ] param_name -> 
            ?value:'a -> ('a -> string) -> input_elt
(** Creates an [<input>] tag for a user type *)

    val raw_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:string -> ?value:string -> unit -> input_elt
(** Creates an untyped [<input>] tag. You may use the name you want
   (for example to use with {!Eliom_parameters.any}).
 *)

    val file_input :
        ?a:input_attrib_t -> 
          name:[< file_info setoneopt ] param_name -> 
            unit -> input_elt
(** Creates an [<input>] tag for sending a file *)

    val image_input :
        ?a:input_attrib_t -> 
          name:[< coordinates oneopt ] param_name -> 
          ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="...">] tag that sends the coordinates 
   the user clicked on *)

    val int_image_input :
        ?a:input_attrib_t -> 
          name:[< (int * coordinates) oneopt ] param_name -> value:int -> 
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int *)

    val float_image_input :
        ?a:input_attrib_t -> 
          name:[< (float * coordinates) oneopt ] param_name -> value:float -> 
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
    the coordinates the user clicked on and a value of type float *)

    val string_image_input :
        ?a:input_attrib_t -> 
          name:[< (string * coordinates) oneopt ] param_name -> value:string -> 
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type string *)

    val user_type_image_input :
        ?a:input_attrib_t -> 
          name:[< ('a * coordinates) oneopt ] param_name -> value:'a -> 
            ?src:uri -> ('a -> string) -> input_elt
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

    val user_type_checkbox :
        ?a:input_attrib_t -> ?checked:bool -> 
          name:[ `Set of 'a ] param_name -> value:'a -> 
            ('a -> string) -> input_elt
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
          name:[ `Opt of string ] param_name -> 
            value:string -> unit -> input_elt
(** Creates a radio [<input>] tag with string content *)

    val int_radio :
        ?a:input_attrib_t -> ?checked:bool -> 
           name:[ `Opt of int ] param_name -> 
             value:int -> unit -> input_elt
(** Creates a radio [<input>] tag with int content *)

    val float_radio :
        ?a:input_attrib_t -> ?checked:bool -> 
           name:[ `Opt of float ] param_name -> 
             value:float -> unit -> input_elt
(** Creates a radio [<input>] tag with float content *)

    val user_type_radio :
        ?a:input_attrib_t -> ?checked:bool ->
           name:[ `Opt of 'a ] param_name -> 
             value:'a -> ('a -> string) -> input_elt
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

    val float_button : 
        ?a:button_attrib_t ->
          name:[< float setone ] param_name -> value:float -> 
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with float content *)

    val user_type_button : 
        ?a:button_attrib_t ->
          name:[< 'a setone ] param_name -> value:'a -> ('a -> string) ->
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
          name:[< string setoneopt ] param_name -> ?value:pcdata_elt -> 
            rows:int -> cols:int -> unit -> textarea_elt
(** Creates a [<textarea>] tag *)

    val raw_textarea :
        ?a:textarea_attrib_t ->
          name:string -> ?value:pcdata_elt -> 
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

    val user_type_select :
        ?a:select_attrib_t ->
          name:[< `One of 'a ] param_name ->
            'a select_opt ->
              'a select_opt list ->
                ('a -> string) ->
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
        ?a:select_attrib_t ->
          name:[< `Set of 'a ] param_name ->
            'a select_opt ->
              'a select_opt list ->
                ('a -> string) ->
                  select_elt
(** Creates a [<select>] tag for user type values. *)


  end


module MakeForms : functor (Pages: FORMCREATE) -> ELIOMFORMSIG with 
type form_content_elt = Pages.form_content_elt
and type form_content_elt_list = Pages.form_content_elt_list
and type form_elt = Pages.form_elt
and type a_content_elt = Pages.a_content_elt
and type a_content_elt_list = Pages.a_content_elt_list
and type a_elt = Pages.a_elt
and type a_elt_list = Pages.a_elt_list
and type div_content_elt = Pages.div_content_elt
and type div_content_elt_list = Pages.div_content_elt_list
and type button_elt = Pages.button_elt
and type button_content_elt = Pages.button_content_elt
and type button_content_elt_list = Pages.button_content_elt_list
      
and type uri = Pages.uri
and type link_elt = Pages.link_elt
and type script_elt = Pages.script_elt
and type textarea_elt = Pages.textarea_elt
and type select_elt = Pages.select_elt
and type input_elt = Pages.input_elt
and type pcdata_elt = Pages.pcdata_elt
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


