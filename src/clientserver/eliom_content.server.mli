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

open Eliom_service
open Eliom_parameter

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

    So if you want to generate typed HTML, you have to got to {!Eliom_content.Html5},
    if you want to handly write untyped html, got to {!Eliom_content.Html_text} and
    if you want to generate svg, go to {!Eliom_content.Svg}.
  *)

(** Abstract signature for links and forms creation functions, for
    concrete instance see {!Html5}, or {!Html_text}. *)
module type Forms = "sigs/eliom_forms.mli"

(** Low-level XML manipulation. *)
module Xml : sig

  (** {2 Base functions}
      Cf. {% <<a_api project="tyxml" | module Xml_sigs.Iterable >> %}. *)

  include Xml_sigs.Iterable with type 'a wrap = 'a
                             and type uri = Eliom_content_core.Xml.uri

  (** {2 Unique nodes } *)

  (** Unique nodes are XML nodes that are manipulated 'by reference'
      when sent to the client part of an Eliom-application: the
      created element is allocated only one time in each instance of
      an application. See {% <<a_manual chapter="clientserver-html"
      fragment="unique" |the eliom manual>>%} for more
      details. *)

  (** Event handlers *)

  (** Values of type ['a caml_event_handler] represents event handler
      build with the [{{ ... }}] syntax (see the Eliom manual for more
      information on {% <<a_manual chapter="clientserver-html"
      fragment="syntax"|syntax extension>>%}). Such values are expected
      by functions like {!Eliom_service.onload} or
      {!Eliom_content.Html5.a_onclick}. The type parameter is the
      type of the javascript event expected by the handler, for
      example {% <<a_api project="js_of_ocaml" | type
      Dom_html.mouseEvent>>%} or {% <<a_api project="js_of_ocaml" | type
      Dom_html.keyboardEvent >>%}. *)
  type -'a caml_event_handler constraint 'a = #Dom_html.event

  (**/**)

  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : elt -> elt

  val uri_of_fun: (unit -> string) -> uri

  (* Building ref tree. *)
  type event_handler_table = Eliom_content_core.Xml.event_handler_table
  (* Concrete on client-side only. *)
  type node_id
  val get_node_id : elt -> node_id
  val make_event_handler_table : elt -> event_handler_table

  val event_handler_of_string : string -> event_handler
  val string_of_event_handler : event_handler -> string
  val event_handler_of_service :
    ( [ `A | `Form_get | `Form_post ]
      * (bool * string list) option
      * string option) option Eliom_lazy.request -> event_handler

  val caml_event_handler : ((#Dom_html.event as 'a) Js.t -> unit) Eliom_lib.client_value -> 'a caml_event_handler
  val event_handler : (Dom_html.event Js.t -> unit) Eliom_lib.client_value -> event_handler

  type racontent =
    | RA of acontent
    | RAReact of acontent option React.signal
    | RACamlEventHandler of Dom_html.event caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
  val racontent : attrib -> racontent

  val lazy_node : ?a:(attrib list) -> ename -> elt list Eliom_lazy.request -> elt

  (**/**)
  (** [Eliom_content.Xml.wrap page v] is like [Eliom_wrap.wrap v] but
      it makes sure that all [elt]s in [v] which are included in
      [page] are sent with empty content. This is safe because such
      elements will be taken from the DOM on the client either
      ways. *)
  val wrap : elt -> 'a -> 'a Eliom_wrap.wrapped_value

end

(** Building and pretty-printing valid SVG tree.
Information about Svg api can be found at {% <<a_api project="tyxml" | module Svg_sigs.T >> %}*)
module Svg : sig

  (** See the Eliom manual for more information on{% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for SVG tree manipulated by client/server
      application. *)

  type +'a elt = 'a Eliom_content_core.Svg.elt
  type 'a wrap = 'a
  type 'a attrib = 'a Eliom_content_core.Svg.attrib
  type uri = Xml.uri

  (** Typed interface for building valid SVG tree (functional
      semantics). See {% <<a_api project="tyxml" | module type
      Svg_sigs.T >> %}. *)
  module F : sig

    (** Cf. {% <<a_api project="tyxml" | module type Html5_sigs.T >> %}. *)
    module Raw : Svg_sigs.T with type Xml.uri = Xml.uri
                             and type Xml.event_handler = Xml.event_handler
                             and type Xml.attrib = Xml.attrib
                             and type Xml.elt = Xml.elt
			     and type 'a elt = 'a elt
                             and type 'a Xml.wrap = 'a
                             and type 'a wrap = 'a
                             and type 'a attrib = 'a attrib
		             and type uri = uri

    include module type of Raw

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_svg_event_handler.mli"

  end

  (** Typed interface for building valid SVG tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type Svg_sigs.T >> %}. *)
  module D : sig

    (** Cf. {% <<a_api project="tyxml" | module type Html5_sigs.T >> %}. *)
    module Raw : Svg_sigs.T with type Xml.uri = Xml.uri
                             and type Xml.event_handler = Xml.event_handler
                             and type Xml.attrib = Xml.attrib
                             and type Xml.elt = Xml.elt
			     and type 'a elt = 'a elt
                             and type 'a Xml.wrap = 'a
                             and type 'a wrap = 'a
                             and type 'a attrib = 'a attrib
		             and type uri = uri

    include module type of Raw

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_svg_event_handler.mli"

  end


  (** Typed interface for building valid SVG tree (reactive DOM). See
      {% <<a_api project="tyxml" | module type Svg_sigs.T >> %}. *)
  module R : sig

    module Raw : Svg_sigs.T
      with type Xml.uri = Xml.uri
       and type Xml.event_handler = Xml.event_handler
       and type Xml.attrib = Xml.attrib
       and type Xml.elt = Xml.elt
       and type 'a elt = 'a elt
       and type 'a Xml.wrap = 'a React.signal Eliom_pervasives.client_value
       and type 'a wrap = 'a React.signal Eliom_pervasives.client_value
       and type 'a attrib = 'a attrib
       and type uri = uri

    include module type of Raw

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_svg_event_handler.mli"

  end

  (** Node identifiers. *)
  module Id : sig
    (** The type of global SVG element identifier. *)
    type +'a id

    (** The function [new_elt_id ()] creates a new HTML5 element
        identifier. (see the Eliom manual for more information on {%
        <<a_manual project="eliom" chapter="clientserver-html"
        fragment="global"|global element>>%}).*)
    val new_elt_id: ?global:bool -> unit -> 'a id

    (** The function [create_named_elt ~id elt] create a copy of the
        element [elt] that will be accessible through the name [id]. *)
    val create_named_elt: id:'a id -> 'a elt -> 'a elt

    (** The function [create_named_elt elt] is equivalent to
        [create_named_elt ~id:(new_elt_id ()) elt]. *)
    val create_global_elt: 'a elt -> 'a elt
  end

  (** SVG printer.
      See {% <<a_api project="tyxml" | module type Xml_sigs.Typed_simple_printer >> %}. *)
  module Printer : Xml_sigs.Typed_simple_printer with type 'a elt := 'a F.elt
                                          and type doc := F.doc

end





(** Building and printing valid HTML5 tree.
    Information about Html5 api can be found at {% <<a_api project="tyxml" | module Html5_sigs.T >> %} .*)
module Html5 : sig

  (** See the Eliom manual for more information on {% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for HTML5 tree manipulated by client/server
      application. *)

  type +'a elt = 'a Eliom_content_core.Html5.elt
  type +'a attrib = 'a Eliom_content_core.Html5.attrib
  type uri = Xml.uri

  (** Creation of {b F}unctional HTML5 content (copy-able but not referable, see also {% <<a_api|module Eliom_content>> %}). *)
  module F : sig
    (** {2 Content creation}
        See {% <<a_api project="tyxml" | module Html5_sigs.T >> %},
        If you want to create an untyped form,
        you will have to use {% <<a_api|module Eliom_content.Html5.F.Raw>> %}
        otherwise, use the form module.
        For more information,
        see {{:http://ocsigen.org/howto/forms/}"how to make forms"} *)
    open Pervasives

    (** Cf. {% <<a_api project="tyxml" | module type Html5_sigs.T >> %}. *)
    module Raw : Html5_sigs.T
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   and type 'a Xml.wrap = 'a
                   with module Svg := Svg.F.Raw
                   with type +'a elt = 'a elt
                   and type 'a wrap = 'a
                   and type 'a attrib = 'a attrib
                   and type uri = uri

    include module type of Raw (*BB TODO Hide untyped [input]. *)

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) lazy_plus

    (**/**)

    (** {2 Forms} *)
    include "sigs/eliom_html5_forms.mli"

    (** Creates an untyped form. *)
    val raw_form : ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) plus

    (** This is an alias to {% <<a_api|val Eliom_content.Html5.F.get_form>> %}
        to avoid the untyped [Eliom_content.Html5.F.form]. *)
    val form : ?absolute:bool -> ?absolute_path:bool -> ?https:bool -> ?a:Html5_types.form_attrib attrib list ->
      service:('get, unit, [< get_service_kind ], [<suff ], 'gn, 'pn, [< registrable ], [< non_ocaml_service ]) service ->
      ?hostname:string -> ?port:int -> ?fragment:string -> ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameter.nl_params_set -> ?xhr:bool ->
      ('gn -> Html5_types.form_content elt list) -> [> Html5_types.form ] elt

    (** This is an alias to {% <<a_api|val Eliom_content.Html5.F.string_input>>
        %} to avoid the untyped [Eliom_content.Html5.F.input]. *)
    val input : ?a:Html5_types.input_attrib attrib list -> input_type:[<
	| `Url | `Tel | `Text | `Time | `Search | `Password | `Checkbox | `Range | `Radio | `Submit | `Reset | `Number | `Hidden
	| `Month | `Week | `File | `Email | `Image | `Datetime_local | `Datetime | `Date | `Color | `Button]
      -> ?name:[< string setoneradio ] param_name -> ?value:string -> unit -> [> Html5_types.input ] elt

    (** This is an alias to
        {% <<a_api|val Eliom_content.Html5.F.string_select>> %}
        to avoid the untyped [Eliom_content.Html5.F.select]. *)
    val select : ?a:Html5_types.select_attrib attrib list -> name:[< `One of string ] param_name -> string select_opt -> string select_opt list -> [> Html5_types.select ] elt
  end

  (** Creation of HTML5 content with {b D}OM semantics
      (referable, see also {% <<a_api|module Eliom_content>> %}). *)
  module D : sig
    (** {2 Content creation}
        See {% <<a_api project="tyxml" | module Html5_sigs.T >> %},
        If you want to create an untyped form,
        you will have to use {% <<a_api|module Eliom_content.Html5.D.Raw>> %}
        otherwise, use the form module.
        For more information,
        see {{:http://ocsigen.org/howto/forms/}"how to make forms"} *)
    open Pervasives

    (** Cf. {% <<a_api project="tyxml" | module type Html5_sigs.T >> %}. *)
    module Raw : Html5_sigs.T
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   and type 'a Xml.wrap = 'a
                   with module Svg := Svg.D.Raw
                   with type +'a elt = 'a elt
                   and type 'a wrap = 'a
                   and type 'a attrib = 'a attrib
                   and type uri = uri
    include module type of Raw (*BB TODO Hide untyped [input]. *)

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) lazy_plus
    (**/**)


    (** {2 Forms} *)
    include "sigs/eliom_html5_forms.mli"

    (** Creates an untyped form. *)
    val raw_form : ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) plus

    (** This is an alias to {% <<a_api|val Eliom_content.Html5.D.get_form>> %}
        to avoid the untyped [Eliom_content.Html5.D.form]. *)
    val form : ?absolute:bool -> ?absolute_path:bool -> ?https:bool -> ?a:Html5_types.form_attrib attrib list ->
      service:('get, unit, [< get_service_kind ], [<suff ], 'gn, 'pn, [< registrable ], [< non_ocaml_service ]) service ->
      ?hostname:string -> ?port:int -> ?fragment:string -> ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameter.nl_params_set -> ?xhr:bool ->
      ('gn -> Html5_types.form_content elt list) -> [> Html5_types.form ] elt

    (** This is an alias to
        {% <<a_api|val Eliom_content.Html5.D.string_input>> %}
        to avoid the untyped [Eliom_content.Html5.D.input]. *)
    val input : ?a:Html5_types.input_attrib attrib list -> input_type:[<
	| `Url | `Tel | `Text | `Time | `Search | `Password | `Checkbox | `Range | `Radio | `Submit | `Reset | `Number | `Hidden
	| `Month | `Week | `File | `Email | `Image | `Datetime_local | `Datetime | `Date | `Color | `Button]
      -> ?name:[< string setoneradio ] param_name -> ?value:string -> unit -> [> Html5_types.input ] elt

    (** This is an alias to
        {% <<a_api|val Eliom_content.Html5.D.string_select>> %}
        to avoid the untyped [Eliom_content.Html5.D.select]. *)
    val select : ?a:Html5_types.select_attrib attrib list -> name:[< `One of string ] param_name -> string select_opt -> string select_opt list -> [> Html5_types.select ] elt
  end

  (** Creation of reactive HTML5 content.
      This nodes will react to client-side react signals *)
  module R : sig
    (** {2 Content creation}
        See {% <<a_api project="tyxml" | module Html5_sigs.T >> %},
        If you want to create an untyped form,
        you will have to use {% <<a_api|module Eliom_content.Html5.D.Raw>> %}
        otherwise, use the form module.
        For more information,
        see {{:http://ocsigen.org/howto/forms/}"how to make forms"} *)
    open Pervasives

    (** the function [node s] create an HTML5 [elt] from a signal [s].
    The resulting HTML5 [elt] can then be used like anyother HTML5 [elt] *)
    val node : 'a elt React.signal Eliom_pervasives.client_value -> 'a elt

    (** Cf. {% <<a_api project="tyxml" | module type Html5_sigs.T >> %}. *)
    module Raw : Html5_sigs.T
                   with type Xml.uri = Xml.uri
                    and type Xml.event_handler = Xml.event_handler
                    and type Xml.attrib = Xml.attrib
                    and type Xml.elt = Xml.elt
                    and type 'a Xml.wrap = 'a React.signal Eliom_pervasives.client_value
                   with module Svg := Svg.D.Raw
                   with type +'a elt = 'a elt
                    and type 'a wrap = 'a React.signal Eliom_pervasives.client_value
                    and type 'a attrib = 'a attrib
                    and type uri = uri
    include module type of Raw (*BB TODO Hide untyped [input]. *)

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) lazy_plus
    (**/**)


    (* (\** {2 Forms} *\) *)
    (* include "sigs/eliom_html5_forms.mli" *)

    (* (\** Creates an untyped form. *\) *)
    (* val raw_form : ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) plus *)

    (* (\** This is an alias to {% <<a_api|val Eliom_content.Html5.D.get_form>> %} *)
    (*     to avoid the untyped [Eliom_content.Html5.D.form]. *\) *)
    (* val form : ?absolute:bool -> ?absolute_path:bool -> ?https:bool -> ?a:Html5_types.form_attrib attrib list -> *)
    (*   service:('get, unit, [< get_service_kind ], [<suff ], 'gn, 'pn, [< registrable ], [< non_ocaml_service ]) service -> *)
    (*   ?hostname:string -> ?port:int -> ?fragment:string -> ?keep_nl_params:[ `All | `Persistent | `None ] -> *)
    (*   ?nl_params: Eliom_parameter.nl_params_set -> ?xhr:bool -> *)
    (*   ('gn -> Html5_types.form_content elt list) -> [> Html5_types.form ] elt *)

    (* (\** This is an alias to *)
    (*     {% <<a_api|val Eliom_content.Html5.D.string_input>> %} *)
    (*     to avoid the untyped [Eliom_content.Html5.D.input]. *\) *)
    (* val input : ?a:Html5_types.input_attrib attrib list -> input_type:[< *)
    (*     | `Url | `Tel | `Text | `Time | `Search | `Password | `Checkbox | `Range | `Radio | `Submit | `Reset | `Number | `Hidden *)
    (*     | `Month | `Week | `File | `Email | `Image | `Datetime_local | `Datetime | `Date | `Color | `Button] *)
    (*   -> ?name:[< string setoneradio ] param_name -> ?value:string -> unit -> [> Html5_types.input ] elt *)

    (* (\** This is an alias to *)
    (*     {% <<a_api|val Eliom_content.Html5.D.string_select>> %} *)
    (*     to avoid the untyped [Eliom_content.Html5.D.select]. *\) *)
    (* val select : ?a:Html5_types.select_attrib attrib list -> name:[< `One of string ] param_name -> string select_opt -> string select_opt list -> [> Html5_types.select ] elt *)
  end


  (** Node identifiers *)
  module Id : sig
    (** The type of global HTML5 element identifier. *)
    type +'a id

    (** The function [new_elt_id ()] creates a new global HTML5 element
        identifier (see the Eliom manual for more information on {%
        <<a_manual project="eliom" chapter="clientserver-html"
        fragment="global"|global element>>%}).*)
    val new_elt_id: ?global:bool -> unit -> 'a id

    (** The function [create_named_elt ~id elt] create a copy of the
        element [elt] that will be sent to client with the reference
        [id]. *)
    val create_named_elt: id:'a id -> 'a elt -> 'a elt

    (** The function [create_named_elt elt] is equivalent to
        [create_named_elt ~id:(new_elt_id ()) elt]. *)
    val create_global_elt: 'a elt -> 'a elt

    (**/**)
    val have_id: 'a id -> 'b elt -> bool
  end



  (** Type-safe custom data for HTML5.
      See the {% <<a_manual chapter="clientserver-html"
      fragment="custom_data"|examples in the manual>> %}. *)
  module Custom_data : sig

    (** Custom data with values of type ['a]. *)
    type 'a t

    (** Create a custom data field by providing string conversion functions.
        If the [default] is provided, calls to {% <<a_api project="eliom" subproject="client" |
        val Eliom_content.Html5.Custom_data.get_dom>> %} return that instead of throwing an
        exception [Not_found].  *)
    val create : name:string -> ?default:'a -> to_string:('a -> string) -> of_string:(string -> 'a) -> unit -> 'a t

    (** Create a custom data from a Json-deriving type.  *)
    val create_json : name:string -> ?default:'a -> 'a Deriving_Json.t -> 'a t

    (** [attrib my_data value ] creates a HTML5 attribute for the custom-data
        type [my_data] with value [value] for injecting it into an a HTML5 tree
        ({% <<a_api | type Eliom_content.Html5.elt >> %}). *)
    val attrib : 'a t -> 'a -> [> | `User_data ] attrib

  end

  (** {{:http://dev.w3.org/html5/html-xhtml-author-guide/}"Polyglot"} HTML5 printer.
     See {% <<a_api project="tyxml" | module type Xml_sigs.Typed_simple_printer >> %}. *)
  module Printer : Xml_sigs.Typed_simple_printer with type 'a elt := 'a F.elt
                                          and type doc := F.doc


end

(** Generate untyped html as text.*)
module Html_text : sig
  include "sigs/eliom_forms.mli"
    (** Have a look on  {% <<a_manual
      chapter="clientserver-html" fragment="text_html" | Client and Server side HTML>> %} for HTML tree manipulated by client/server
      application. *)
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
