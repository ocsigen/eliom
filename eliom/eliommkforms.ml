(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliommkforms
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
open Ocsimisc
open Extensions
open Eliomparameters
open Eliomservices
open Eliomsessions






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
        service:('get, unit, [< get_service_kind ],
                 [< suff ], 'gn, unit, 
                 [< registrable ]) service ->
                   sp:Eliomsessions.server_params -> 
                     ?fragment:string ->
                       'get -> string
(** Creates the string corresponding to the URL of a service applyed to
   its GET parameters.
 *)

    val make_uri :
        service:('get, unit, [< get_service_kind ],
         [< suff ], 'gn, unit, 
         [< registrable ]) service ->
          sp:server_params -> ?fragment:string -> 'get -> uri
(** Create the text of the service. Like the [a] function, it may take
   extra parameters. *)

    val a :
        ?a:a_attrib_t ->
          service:('get, unit, [< get_service_kind ], 
           [< suff ], 'gn, 'pn,
           [< registrable ]) service ->
            sp:server_params -> ?fragment:string ->
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
             sp:server_params -> ?fragment:string ->
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
      sp:server_params -> 
      ?fragment:string ->
      ?keep_get_na_params:bool ->
      ('pn -> form_content_elt_list) -> 'get -> form_elt
(** [post_form service sp formgen] creates a POST form to [service]. 
   The last parameter is for GET parameters (as in the function [a]).
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
   (for example to use with {!Eliomparameters.any}).
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
      type select_elt = Pages.select_elt
      type input_elt = Pages.input_elt
      type pcdata_elt = Pages.pcdata_elt
      type select_content_elt = Pages.select_content_elt
      type select_content_elt_list = Pages.select_content_elt_list
      type button_elt = Pages.button_elt
      type button_content_elt = Pages.button_content_elt
      type button_content_elt_list = Pages.button_content_elt_list
            
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

      let make_string_uri
          ~service
          ~sp
          ?(fragment = "")
          getparams : string =
        match get_kind_ service with
        | `Attached attser ->
            begin
              let suff, params_string = 
                construct_params (get_get_params_type_ service) getparams in
              let preapplied_params = 
                construct_params_string (get_pre_applied_parameters_ service) in
              let params_string =
                concat_strings preapplied_params "&" params_string in
              let uri = 
                (if (get_att_kind_ attser) = `External
                then
                  concat_strings
                    (get_prefix_ attser)
                    "/"
                    (reconstruct_absolute_url_path
                       (get_current_full_path sp) 
                       (get_full_path_ attser) suff)
                else (reconstruct_relative_url_path
                        (get_current_full_path sp) (get_full_path_ attser) suff))
              in
              match get_get_state_ attser with
              | None ->
                  add_to_string
                    (add_to_string uri "?" params_string)
                    "#"
                    (Netencoding.Url.encode fragment)
              | Some s -> 
                  add_to_string
                    (add_to_string (uri^"?"^Eliommod.get_state_param_name^"="^s)
                       "&" params_string)
                    "#"
                    (Netencoding.Url.encode fragment)
            end
        | `Nonattached naser ->
            let current_get_params =
              List.remove_assoc
                Eliommod.naservice_num
                (remove_prefixed_param 
                   Eliommod.na_co_param_prefix
                   (get_all_get_params sp))
            in
            let _, params_string = 
              construct_params (get_get_params_type_ service) getparams in
            let preapplied_params = 
              construct_params_string (get_pre_applied_parameters_ service) in
            let params_string =
              concat_strings preapplied_params "&" params_string in
            let naservice_param = 
              match get_na_name_ naser with
              | Eliommod.Na_get' n -> Eliommod.naservice_num^"="^n
              | Eliommod.Na_get_ n -> Eliommod.naservice_name^"="^n
              | _ -> assert false
            in
            let current_get_params_string = 
              construct_params_string current_get_params 
            in
            let cur = get_current_sub_path sp in
            ((* ("/"^(get_current_path_string sp)) --> absolute (wrong) *)
              (reconstruct_relative_url_path cur cur None)
              ^"?"^ 
             (concat_strings
                current_get_params_string
                "&"
                (concat_strings naservice_param "&" params_string))
            )


      let a ?a
          ~service
          ~sp
          ?(fragment = "")
          content
          getparams =
        match get_kind_ service with
        | `Attached attser ->
            (let suff, params_string = 
              construct_params (get_get_params_type_ service) getparams in
            let preapplied_params = 
              construct_params_string (get_pre_applied_parameters_ service) in
            let params_string =
              concat_strings preapplied_params "&" params_string in
            let uri = 
              (if (get_att_kind_ attser) = `External
              then 
                concat_strings
                  (get_prefix_ attser)
                  "/"
                  (reconstruct_absolute_url_path
                     (get_current_full_path sp) 
                     (get_full_path_ attser) suff)
              else 
                (reconstruct_relative_url_path
                   (get_current_full_path sp) (get_full_path_ attser) suff))
            in
            match get_get_state_ attser with
            | None ->
                Pages.make_a 
                  ?a
                  ~href:(add_to_string
                           (add_to_string uri "?" params_string)
                           "#"
                           (Netencoding.Url.encode fragment)
                        ) content
            | Some s -> 
                Pages.make_a ?a
                  ~href:
                  (add_to_string
                     (add_to_string 
                        (uri^"?"^Eliommod.get_state_param_name^"="^s)
                        "&" params_string)
                     "#"
                     (Netencoding.Url.encode fragment))
                  content)
        | `Nonattached naser ->
            let current_get_params =
              List.remove_assoc
                Eliommod.naservice_num
                (remove_prefixed_param
                   Eliommod.na_co_param_prefix (get_all_get_params sp))
            in
            let _, params_string = 
              construct_params (get_get_params_type_ service) getparams in
            let preapplied_params = 
              construct_params_string (get_pre_applied_parameters_ service) in
            let params_string =
              concat_strings preapplied_params "&" params_string in
            let naservice_param = 
              match get_na_name_ naser with
              | Eliommod.Na_get' n -> Eliommod.naservice_num^"="^n
              | Eliommod.Na_get_ n -> Eliommod.naservice_name^"="^n
              | _ -> assert false
            in
            let current_get_params_string = 
              construct_params_string current_get_params 
            in
            let cur = get_current_sub_path sp in
            Pages.make_a ?a
              ~href:( 
                (* "/"^(get_current_path_string sp) --> absolute (wrong) *)
                (reconstruct_relative_url_path cur cur None)^"?"^
                  (concat_strings
                     current_get_params_string
                     "&"
                     (concat_strings naservice_param "&" params_string))
              )
              content

      let get_form 
          ?a
          ~service
          ~sp
          ?(fragment = "")
          f =
        match get_kind_ service with
        | `Attached attser ->
            let urlname =
              (if (get_att_kind_ attser) = `External
              then 
                concat_strings
                  (get_prefix_ attser)
                  "/"
                  (reconstruct_absolute_url_path
                     (get_current_full_path sp)
                     (get_full_path_ attser) None)
              else (reconstruct_relative_url_path
                      (get_current_full_path sp) (get_full_path_ attser) None)) in
            let urlname =
              add_to_string urlname "#" (Netencoding.Url.encode fragment)
            in
            let state_param =
              (match get_get_state_ attser with
              | None -> None
              | Some s -> 
                  Some (Pages.make_input ~typ:Pages.hidden
                          ~name:Eliommod.get_state_param_name
                          ~value:s ()))
            in
            let inside = f (make_params_names (get_get_params_type_ service)) in
            let inside =
              List.fold_left
                (fun s (n,v) -> 
                  Pages.cons_form
                    (Pages.make_hidden_field
	               (Pages.make_input
                          ~typ:Pages.hidden
                          ~name:n ~value:v ()))
                    s
                )
                inside
                (get_pre_applied_parameters_ service)
            in
            let i1, i =
              match state_param, inside with
              | Some s, i -> (Pages.make_hidden_field s),i
              | None, i -> Pages.remove_first i
            in Pages.make_get_form ?a ~action:urlname i1 i
        | `Nonattached naser ->
            let cur = get_current_sub_path sp in
            let urlname = reconstruct_relative_url_path cur cur None in
            (* "/"^(get_current_path_string sp) --> absolute (wrong) *)
            let naservice_line = 
              match get_na_name_ naser with
              | Eliommod.Na_get' n ->
                    Pages.make_hidden_field
	            (Pages.make_input
	               ~typ:Pages.hidden 
                       ~name:Eliommod.naservice_num
                       ~value:n ())
              | Eliommod.Na_get_ n ->
                    Pages.make_hidden_field
	            (Pages.make_input
	               ~typ:Pages.hidden 
                       ~name:Eliommod.naservice_name
                       ~value:n ())
              | _ -> assert false
            in
            let current_get_params =
              List.remove_assoc
                Eliommod.naservice_num
                (remove_prefixed_param
                   Eliommod.na_co_param_prefix (get_all_get_params sp))
            in
            let inside = f (make_params_names (get_get_params_type_ service)) in
            let all_lines = 
              List.fold_left
                (fun s (n,v) -> 
                  Pages.cons_form
                    (Pages.make_hidden_field
	               (Pages.make_input
                          ~typ:Pages.hidden
                          ~name:n ~value:v ()))
                    s
                )
                inside
                current_get_params
            in
            let all_lines =
              List.fold_left
                (fun s (n,v) -> 
                  Pages.cons_form
                    (Pages.make_hidden_field
	               (Pages.make_input
                          ~typ:Pages.hidden
                          ~name:n
                          ~value:v ()))
                    s
                )
                all_lines
                (get_pre_applied_parameters_ service)
            in
            Pages.make_get_form ?a ~action:urlname naservice_line all_lines


      let post_form
          ?a
          ~service
          ~sp
          ?(fragment = "")
          ?keep_get_na_params
          f 
          getparams =
        match get_kind_ service with
        | `Attached attser ->
            let suff,params_string = 
              construct_params (get_get_params_type_ service) getparams in
            let preapplied_params = 
              construct_params_string (get_pre_applied_parameters_ service) in
            let params_string =
              concat_strings preapplied_params "&" params_string in
            let params_string =
              match get_get_state_ attser with
              | None -> params_string
              | Some s -> 
                  add_to_string
                    (Eliommod.get_state_param_name^"="^s)
                    "&"
                    params_string
            in
            let urlname = 
              (if (get_att_kind_ attser) = `External
              then 
                concat_strings
                  (get_prefix_ attser)
                  "/"
                  (reconstruct_absolute_url_path
                     (get_current_full_path sp)
                     (get_full_path_ attser) suff)
              else (reconstruct_relative_url_path
                      (get_current_full_path sp) (get_full_path_ attser) suff))
            in
            let urlname =
              add_to_string urlname "#" (Netencoding.Url.encode fragment)
            in
            let state_param =
              (match get_post_state_ attser with
              | None -> None
              | Some s -> 
                  Some (Pages.make_input ~typ:Pages.hidden
                          ~name:Eliommod.post_state_param_name
                          ~value:s ()))
            in
            let inside = f (make_params_names (get_post_params_type_ service)) in
            let i1, i =
              match state_param, inside with
              | Some s, i -> (Pages.make_hidden_field s),i
              | None, i -> Pages.remove_first i
            in Pages.make_post_form ?a
              ~action:(add_to_string urlname "?" params_string)
              i1 i
        | `Nonattached naser ->
            (* no GET params here for now *)
            let keep_get_na_params = 
              match keep_get_na_params with
                | Some b -> b
                | None ->
                    match get_na_kind_ naser with
                      | `Post b -> b
                      | _ -> assert false
            in
            let current_get_params =
              if keep_get_na_params then
                get_all_get_params sp
              else
                List.remove_assoc
                  Eliommod.naservice_num
                  (remove_prefixed_param
                     Eliommod.na_co_param_prefix (get_all_get_params sp))
            in
            let current_get_params_string = 
              construct_params_string current_get_params 
            in
            let cur = get_current_sub_path sp in
            (* absolute URL does not work behind a revproxy! *)
            let urlpath = reconstruct_relative_url_path cur cur None in
            let v = concat_strings urlpath "?" current_get_params_string in
            let naservice_line = 
              match get_na_name_ naser with
               | Eliommod.Na_post' n ->
	           Pages.make_input
	             ~typ:Pages.hidden
                     ~name:Eliommod.naservice_num
                     ~value:n () 
               | Eliommod.Na_post_ n ->
	           Pages.make_input
	             ~typ:Pages.hidden
                     ~name:Eliommod.naservice_name
                     ~value:n ()
               | _ -> assert false
            in

            let inside = 
              f (make_params_names (get_post_params_type_ service)) 
            in
            Pages.make_post_form ?a ~action:v
              (Pages.make_hidden_field naservice_line)
              inside

          



      let make_uri ~service ~sp ?fragment gp =
        Pages.uri_of_string (make_string_uri ?fragment ~service ~sp gp)
                  
          
          
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
      let float_input ?a ~input_type 
          ?name ?value () =
        gen_input ?a ~input_type ?value ?name string_of_float
      let string_input ?a ~input_type 
          ?name ?value () =
        gen_input ?a ~input_type ?value ?name id
      let user_type_input ?a ~input_type
          ?name ?value string_of = 
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
      let float_image_input ?a ~name ~value ?src () = 
        gen_input ?a ~input_type:Pages.image ~name
          ~value ?src string_of_float
      let string_image_input ?a ~name ~value ?src () = 
        gen_input ?a ~input_type:Pages.image ~name
          ~value ?src id
      let user_type_image_input ?a ~name ~value ?src string_of = 
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

      let float_checkbox ?a ?checked ~name ~value () =
        Pages.make_input ?a ?checked ~typ:Pages.checkbox
          ~name:(string_of_param_name name) ~value:(string_of_float value) ()

      let string_checkbox ?a ?checked ~name ~value () =
        Pages.make_input ?a ?checked ~typ:Pages.checkbox
          ~name:(string_of_param_name name) ~value ()

      let user_type_checkbox ?a ?checked ~name ~value string_of =
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
      let float_radio ?a ?checked ~name ~value () =
        Pages.make_input
          ?a ?checked ~typ:Pages.radio 
          ~name:(string_of_param_name name) ~value:(string_of_float value) ()
      let user_type_radio ?a ?checked ~name ~value string_of =
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

      let float_button ?a ~name ~value c =
        Pages.make_button ?a ~button_type:Pages.buttonsubmit
          ~name:(string_of_param_name name) ~value:(string_of_float value) c

      let user_type_button ?a ~name ~value string_of c =
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

      let float_select ?a ~name 
          (fl : float select_opt) (ol : float select_opt list) =
        gen_select ?a ~multiple:false 
          ~name:(string_of_param_name name) fl ol string_of_float

      let string_select ?a ~name 
          (fl : string select_opt) (ol : string select_opt list) =
        gen_select ?a ~multiple:false 
          ~name:(string_of_param_name name) fl ol id

      let user_type_select ?a ~name (fl : 'a select_opt) 
          (ol : 'a select_opt list) string_of =
        gen_select ?a ~multiple:false 
          ~name:(string_of_param_name name) fl ol string_of



      let raw_multiple_select ?a ~(name : string)
          (fl : string select_opt) (ol : string select_opt list) =
        gen_select ?a ~multiple:true ~name fl ol id

      let int_multiple_select ?a ~name 
          (fl : int select_opt) (ol : int select_opt list) =
        gen_select ?a ~multiple:true 
          ~name:(string_of_param_name name) fl ol string_of_int

      let float_multiple_select ?a ~name 
          (fl : float select_opt) (ol : float select_opt list) =
        gen_select ?a ~multiple:true 
          ~name:(string_of_param_name name) fl ol string_of_float

      let string_multiple_select ?a ~name 
          (fl : string select_opt) (ol : string select_opt list) =
        gen_select ?a ~multiple:true 
          ~name:(string_of_param_name name) fl ol id

      let user_type_multiple_select ?a
          ~name (fl : 'a select_opt) 
          (ol : 'a select_opt list) string_of =
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
    )

