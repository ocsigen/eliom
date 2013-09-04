(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2012 Vincent Balat, Benedikt Becker
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

(** This module provides the creation of valid XML content, i.e. XML, SVG,
    and (X)HTML5.
    XML tree manipulation within Eliom is based on the TyXML library
    but use a custom representation for XML values (see
    {!Xml}). Then, [Eliom_content] redefines the three high level
    interfaces ({!Svg}, {!Html5}) that are provided by
    TyXML for valid XML tree creation and printing.

    Modules {!Eliom_content.Html5}, {!Eliom_content.Svg} contain two
    implementing sub-modules: {!Eliom_content.Html5.F} and {!Eliom_content.Html5.D}.

    {5 Functional semantics}

    The [F] modules provide functions to create elements with {e f}unctional
    semantics: On the one hand side, those values do not have an identifier,
    which means utilizations of those values are independent of each other.
    On the other hand side, they cannot be referred to, neither by client code
    when created on the server, nor for usage in the functions of
    {% <<a_api subproject="client"|module Eliom_content.Html5.To_dom>> %} and
    {% <<a_api subproject="client"|module Eliom_content.Html5.Manip>> %}.

    {5 DOM semantics}

    The [D] modules provide functions to create elements with {e D}OM semantics:
    Firstly, they behave like DOM nodes, e.g. they can only be added once to the
    DOM tree even when appended several times.
    Secondly, those values have an identifier,
    which means they can be referred to
    on the client side (by [%variable]) or used with the functions in
    {% <<a_api subproject="client"|module Eliom_content.Html5.To_dom>> %} and
    {% <<a_api subproject="client"|module Eliom_content.Html5.Manip>> %}.

    In case of doubt, use the modules with DOM-like semantics {!Eliom_content.Html5.D}.
  *)

(** Abstract signature for links and forms creation functions. For
    concrete instance see {!Html5}, or {!Html_text}. *)
module type Forms = "sigs/eliom_forms.mli"

(** Low-level XML manipulation. *)
module Xml : module type of Eliom_content_core.Xml
    with type uri = Eliom_content_core.Xml.uri
    and type attrib = Eliom_content_core.Xml.attrib
    and type elt = Eliom_content_core.Xml.elt
    and type event_handler = Eliom_content_core.Xml.event_handler
    and type event_handler_table = Eliom_content_core.Xml.event_handler_table
    and type -'a caml_event_handler = 'a Eliom_content_core.Xml.caml_event_handler

(** Building and pretty-printing valid SVG tree. *)
module Svg : module type of Eliom_content_core.Svg
    with type uri = Eliom_content_core.Svg.uri
    and type 'a attrib = 'a Eliom_content_core.Svg.attrib
    and type +'a elt = 'a Eliom_content_core.Svg.elt

(** Building and printing valid (X)HTML5 tree. *)
module Html5 : sig

  (** See the Eliom manual for more information on {% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for HTML5 tree manipulated by client/server
      application. *)

  type +'a elt = 'a Eliom_content_core.Html5.elt
  type +'a attrib = 'a Eliom_content_core.Html5.attrib
  type uri = Eliom_content_core.Html5.uri

  (** Creation of {b F}unctional HTML5 content (copy-able but not referable, see also {% <<a_api|module Eliom_content>> %}). *)
  module F : sig
    (** {2 Content creation}
        See {% <<a_api project="tyxml" | module Html5_sigs.T >> %} *)
    open Pervasives
    include module type of Eliom_content_core.Html5.F
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   with type +'a elt = 'a elt
                   and type 'a attrib = 'a attrib
                   and type uri = uri

    include "sigs/eliom_html5_forms.mli"

    (** Creates an untyped form. *)
    val raw_form : ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) plus

    (** This is an alias to {% <<a_api|val Eliom_content.Html5.F.get_form>> %}
        to avoid the untyped [Eliom_content_core.Html5.F.form]. *)
    val form : ?absolute:bool -> ?absolute_path:bool -> ?https:bool -> ?a:Html5_types.form_attrib attrib list ->
      service:('get, unit, [< get_service_kind ], [<suff ], 'gn, 'pn, [< registrable ], [< non_caml_service ]) service ->
      ?hostname:string -> ?port:int -> ?fragment:string -> ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameter.nl_params_set -> ?xhr:bool ->
      ('gn -> Html5_types.form_content elt list) -> [> Html5_types.form ] elt

    (** This is an alias to {% <<a_api|val Eliom_content.Html5.F.string_input>>
        %} to avoid the untyped [Eliom_content_core.Html5.F.input]. *)
    val input : ?a:Html5_types.input_attrib attrib list -> input_type:[<
	| `Url | `Tel | `Text | `Time | `Search | `Password | `Checkbox | `Range | `Radio | `Submit | `Reset | `Number | `Hidden
	| `Month | `Week | `File | `Email | `Image | `Datetime_local | `Datetime | `Date | `Color | `Button]
      -> ?name:[< string setoneradio ] param_name -> ?value:string -> unit -> [> Html5_types.input ] elt

    (** This is an alias to
        {% <<a_api|val Eliom_content.Html5.F.string_select>> %}
        to avoid the untyped [Eliom_content_core.Html5.F.select]. *)
    val select : ?a:Html5_types.select_attrib attrib list -> name:[< `One of string ] param_name -> string select_opt -> string select_opt list -> [> Html5_types.select ] elt
  end

  (** Creation of HTML5 content with {b D}OM semantics (referable, see also {% <<a_api|module Eliom_content>> %}). *)
  module D : sig
    (** {2 Content creation}
        See {% <<a_api project="tyxml" | module Html5_sigs.T >> %} *)
    open Pervasives
    include module type of Eliom_content_core.Html5.D
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   with type +'a elt = 'a elt
                   and type 'a attrib = 'a attrib
                   and type uri = uri
    include "sigs/eliom_html5_forms.mli"

    (** Creates an untyped form. *)
    val raw_form : ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) plus

    (** This is an alias to {% <<a_api|val Eliom_content.Html5.D.get_form>> %}
        to avoid the untyped [Eliom_content_core.Html5.D.form]. *)
    val form : ?absolute:bool -> ?absolute_path:bool -> ?https:bool -> ?a:Html5_types.form_attrib attrib list ->
      service:('get, unit, [< get_service_kind ], [<suff ], 'gn, 'pn, [< registrable ], [< non_caml_service ]) service ->
      ?hostname:string -> ?port:int -> ?fragment:string -> ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameter.nl_params_set -> ?xhr:bool ->
      ('gn -> Html5_types.form_content elt list) -> [> Html5_types.form ] elt

    (** This is an alias to
        {% <<a_api|val Eliom_content.Html5.D.string_input>> %}
        to avoid the untyped [Eliom_content_core.Html5.D.input]. *)
    val input : ?a:Html5_types.input_attrib attrib list -> input_type:[<
	| `Url | `Tel | `Text | `Time | `Search | `Password | `Checkbox | `Range | `Radio | `Submit | `Reset | `Number | `Hidden
	| `Month | `Week | `File | `Email | `Image | `Datetime_local | `Datetime | `Date | `Color | `Button]
      -> ?name:[< string setoneradio ] param_name -> ?value:string -> unit -> [> Html5_types.input ] elt

    (** This is an alias to
        {% <<a_api|val Eliom_content.Html5.D.string_select>> %}
        to avoid the untyped [Eliom_content_core.Html5.D.select]. *)
    val select : ?a:Html5_types.select_attrib attrib list -> name:[< `One of string ] param_name -> string select_opt -> string select_opt list -> [> Html5_types.select ] elt
  end

  (** Node identifiers *)
  module Id : module type of Eliom_content_core.Html5.Id
                               with type +'a id = 'a Eliom_content_core.Html5.Id.id

  module Custom_data : module type of Eliom_content_core.Html5.Custom_data
                                        with type 'a t = 'a Eliom_content_core.Html5.Custom_data.t

  module Printer : module type of Eliom_content_core.Html5.Printer

end


module Html_text : sig
  include "sigs/eliom_forms.mli"
    subst type uri := string
    and type pcdata_elt := string

    and type form_elt := string
    and type form_content_elt := string
    and type form_content_elt_list := string
    and type form_attrib_t := string

    and type 'a a_elt := string
    and type 'a a_content_elt := string
    and type 'a a_content_elt_list := string
    and type a_attrib_t := string

    and type link_elt := string
    and type link_attrib_t := string

    and type script_elt := string
    and type script_attrib_t := string

    and type textarea_elt := string
    and type textarea_attrib_t := string

    and type input_elt := string
    and type input_attrib_t := string

    and type select_elt := string
    and type select_attrib_t := string

    and type button_elt := string
    and type button_content_elt := string
    and type button_content_elt_list := string
    and type button_attrib_t := string

    and type optgroup_attrib_t := string
    and type option_attrib_t := string

    and type input_type_t := string
    and type button_type_t := string
    and type for_attrib := string
end
