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
open Eliom_lib

(** This module provides the creation of valid XML content, i.e. XML, SVG,
    and (X)HTML5.
    See {% <<a_api subproject="server" | module Eliom_content >> %} for an explication
    of the modules [F] and [D].
  *)

(** Low-level XML manipulation. *)
module Xml : module type of Eliom_content_core.Xml
    with type uri = Eliom_content_core.Xml.uri
    and type attrib = Eliom_content_core.Xml.attrib
    and type elt = Eliom_content_core.Xml.elt

(** Building valid SVG . *)
module Svg : sig

  (** See the Eliom manual for more information on {% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for HTML5 tree manipulated by client/server
      application. *)

  type +'a elt = 'a Eliom_content_core.Svg.elt
  type +'a attrib = 'a Eliom_content_core.Svg.attrib
  type uri = Eliom_content_core.Svg.uri

  (** Creation of {e f}unctional content (copy-able but not referable).

       See {% <<a_api project="tyxml" | module Svg_sigs.T >> %} *)
  module F : module type of Eliom_content_core.Svg.F
        with type Xml.uri = Xml.uri
        and type Xml.attrib = Xml.attrib
        and type Xml.elt = Xml.elt
        with type +'a elt = 'a elt
        and type 'a attrib = 'a attrib
        and type uri = uri


  (** Creation of content with {e D}OM semantics (referable

       See {% <<a_api project="tyxml" | module Svg_sigs.T >> %} *)
  module D : module type of Eliom_content_core.Svg.D
        with type Xml.uri = Xml.uri
        and type Xml.attrib = Xml.attrib
        and type Xml.elt = Xml.elt
        with type +'a elt = 'a elt
        and type 'a attrib = 'a attrib
        and type uri = uri

  (** Creation of reactive content *)
  module R : module type of Eliom_content_core.Svg.R
        with type Xml.uri = Xml.uri
        and type Xml.attrib = Xml.attrib
        and type Xml.elt = Xml.elt
        with type +'a elt = 'a elt
        and type 'a attrib = 'a attrib
        and type uri = uri

  (** Node identifiers *)
  module Id : sig
    include module type of Eliom_content_core.Svg.Id
                             with type +'a id = 'a Eliom_content_core.Svg.Id.id

    (** [get_element id] returns the HTML element in the DOM with the given [id].
        @raises Not_found if the [id] was no such element. *)
    val get_element : 'a id -> 'a elt
  end

  (** DOM-like manipulation functions.

      In this module, all the functions apply only to SVG element with
      {% <<a_manual chapter="clientserver-html" fragment="unique"|Dom semantics>>
      %}.
  *)
  module Manip : sig

    (** The function [appendChild e1 e2] inserts the element [e2] as last
        child of [e1]. If the optional parameter [~before:e3] is present
        and if [e3] is a child of [e1], then [e2] is inserted before [e3]
        in the list of [e1] children. *)
    val appendChild: ?before:'a elt -> 'b elt ->  'c elt -> unit

    (** The function [appendChildren e1 elts] inserts [elts] as last children
        of [e1]. If the optional parameter [~before:e3] is present and if
        [e3] is a child of [e1], then [elts] are inserted before [e3] in
        the list of [e1] children. *)
    val appendChildren: ?before:'a elt -> 'b elt ->  'c elt list -> unit

    (** [appendChildFirst p c] appends [c] as first child of [p] *)
    val appendChildFirst: 'b elt ->  'c elt -> unit

    (** [nth e n] returns the nth child of [e] (first is 0) *)
    val nth : 'a elt -> int -> 'b elt option

    (** [childLength e] returns the number of chilren of [e] *)
    val childLength : 'a elt -> int

    (** The function [removeChild e1 e2] removes for [e2] from the list of
        [e1] children. *)
    val removeChild: 'a elt -> 'b elt -> unit

    (** The function [replace e1 e2 e3] replaces for [e2] by [e3] in the
        list of [e1] children. *)
    val replaceChild: 'a elt -> 'b elt -> 'c elt -> unit

    (** The function [removeChildren e1] removes [e1] children. *)
    val removeChildren: 'a elt -> unit

    (** [removeSelf e] removes element e from the DOM. *)
    val removeSelf: 'a elt -> unit

    (** The function [replaceChildren e1 elts] replaces all the children of
        [e1] by [elt]. *)
    val replaceChildren: 'a elt -> 'b elt list -> unit

    (* (\** The function [addEventListener elt evt handler] attach the *)
        (* [handler] for the event [evt] on the element [elt]. See the *)
        (* Js_of_ocaml manual, for a list of {% <<a_api project="js_of_ocaml" *)
        (* text="available events"| module Dom_html.Event >>%}. *\) *)
    (* val addEventListener: *)
      (* ?capture:bool -> *)
      (* 'a elt -> *)
      (* (#Dom_html.event as 'b) Js.t Dom_html.Event.typ -> *)
      (* ('a elt -> 'b Js.t -> bool) -> *)
      (* Dom_html.event_listener_id *)

    (** Dom manipulation by element identifier. *)
    module Named: sig

      (** The module [Named] defines the same functions as
          [Eliom_dom]. They take as parameter an element identifier
          instead of an element with Dom semantics. Those functions only
          works if the element is available in the application (sent in
          the page or along the page). If the element is not available,
          those functions raise with [Not_found]. *)

      (** see [appendChild] *)
      val appendChild: ?before:'a elt -> 'b Id.id -> 'c elt -> unit
      (** see [appendChildren] *)
      val appendChildren: ?before:'a elt -> 'b Id.id ->  'c elt list -> unit
      (** see [removeChild] *)
      val removeChild: 'a Id.id -> 'b elt -> unit
      (** see [replaceChild] *)
      val replaceChild: 'a Id.id -> 'b elt -> 'c elt -> unit
      (** see [removeChildren] *)
      val removeChildren: 'a Id.id -> unit
      (** see [replaceChildren] *)
      val replaceChildren: 'a Id.id -> 'b elt list -> unit

      (* (\** see [addEventListener] *\) *)
      (* val addEventListener: *)
        (* ?capture:bool -> *)
        (* 'a Id.id -> *)
        (* (#Dom_html.event as 'b) Js.t Dom_html.Event.typ -> *)
        (* ('a elt -> 'b Js.t -> bool) -> *)
        (* Dom_html.event_listener_id *)

    end

    (**/**)
    val childNodes: 'a elt -> Dom.node Js.t list
    val childElements: 'a elt -> Dom.element Js.t list
    (**/**)

    module Class : sig
      val contain : 'a elt -> string -> bool
      val remove : 'a elt -> string -> unit
      val removes :'a elt -> string list -> unit
      val add :'a elt -> string -> unit
      val adds :'a elt -> string list -> unit
      val replace :  'a elt ->  string -> string -> unit
      val clear : 'a elt -> unit
      val toggle : 'a elt -> string -> unit
      val toggle2 : 'a elt -> string -> string -> unit
    end
  end

  (** Conversion from Svg [elt]s to Javascript DOM elements ([<:] {% <<a_api
      project="js_of_ocaml"| class Dom_html.element >> %}).
      One conversion function per source type (stressed by the [of_] prefix). *)
  module To_dom : sig

    val of_element : 'a elt -> Dom_html.element Js.t
    val of_node : 'a elt -> Dom.node Js.t

    val of_pcdata : [> `Pcdata] elt -> Dom.text Js.t

  end

  (** Conversion functions from DOM nodes ({% <<a_api project="js_of_ocaml"| type Dom_html.element>> %} {% <<a_api
      project="js_of_ocaml"| type Js.t>> %}) to Eliom nodes ({% <<a_api | type Eliom_content.Html5.elt>> %}). *)
  module Of_dom : module type of Eliom_content_core.Svg.Of_dom

end

(** Building valid (X)HTML5. *)
module Html5 : sig

  (** See the Eliom manual for more information on {% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for HTML5 tree manipulated by client/server
      application. *)

  type +'a elt = 'a Eliom_content_core.Html5.elt
  type +'a attrib = 'a Eliom_content_core.Html5.attrib
  type uri = Eliom_content_core.Html5.uri

  (** Creation of {e f}unctional HTML5 content (copy-able but not referable). *)
  module F : sig
    (** {2 Content creation}
        See {% <<a_api project="tyxml" | module Html5_sigs.T >> %} *)
    open Pervasives
    include module type of Eliom_content_core.Html5.F
        with type Xml.uri = Xml.uri
        and type Xml.attrib = Xml.attrib
        and type Xml.elt = Xml.elt
        with type +'a elt = 'a elt
        and type 'a attrib = 'a attrib
        and type uri = uri

    include "sigs/eliom_html5_forms.mli"

    (** Creates an untyped form. *)
    val raw_form : ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) plus

    (** This is an alias to
        {% <<a_api|val Eliom_content.Html5.F.get_form>> %}
        to avoid the untyped [Eliom_content_core.Html5.F.form]. *)
    val form : ?absolute:bool -> ?absolute_path:bool -> ?https:bool -> ?a:Html5_types.form_attrib attrib list ->
      service:('get, unit, [< get_service_kind ], [<suff ], 'gn, 'pn, [< registrable ], [< non_ocaml_service ]) service ->
      ?hostname:string -> ?port:int -> ?fragment:string -> ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameter.nl_params_set -> ?xhr:bool ->
      ('gn -> Html5_types.form_content elt list) -> [> Html5_types.form ] elt

    (** This is an alias to
        {% <<a_api|val Eliom_content.Html5.F.string_input>> %}
        to avoid the untyped [Eliom_content_core.Html5.F.input]. *)
    val input : ?a:Html5_types.input_attrib attrib list -> input_type:[<
	| `Url | `Tel | `Text | `Time | `Search | `Password | `Checkbox | `Range | `Radio | `Submit | `Reset | `Number | `Hidden
	| `Month | `Week | `File | `Email | `Image | `Datetime_local | `Datetime | `Date | `Color | `Button]
      -> ?name:[< string setoneradio ] param_name -> ?value:string -> unit -> [> Html5_types.input ] elt

    (** This is an alias to
        {% <<a_api|val Eliom_content.Html5.F.string_select>> %}
        to avoid the untyped [Eliom_content_core.Html5.F.select]. *)
    val select : ?a:Html5_types.select_attrib attrib list -> name:[< `One of string ] param_name -> string select_opt -> string select_opt list -> [> Html5_types.select ] elt
  end

  (** Creation of HTML5 content with {e D}OM semantics (referable) *)
  module D : sig
    (** {2 Content creation}
        See {% <<a_api project="tyxml" | module Html5_sigs.T >> %} *)
    open Pervasives
    include module type of Eliom_content_core.Html5.D
        with type Xml.uri = Xml.uri
        and type Xml.attrib = Xml.attrib
        and type Xml.elt = Xml.elt
        with type +'a elt = 'a elt
        and type 'a attrib = 'a attrib
        and type uri = uri
    include "sigs/eliom_html5_forms.mli"

    (** Creates an untyped form. *)
    val raw_form : ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) plus

    (** This is an alias to
        {% <<a_api|val Eliom_content.Html5.D.get_form>> %}
        to avoid the untyped [Eliom_content_core.Html5.D.form]. *)
    val form : ?absolute:bool -> ?absolute_path:bool -> ?https:bool -> ?a:Html5_types.form_attrib attrib list ->
      service:('get, unit, [< get_service_kind ], [<suff ], 'gn, 'pn, [< registrable ], [< non_ocaml_service ]) service ->
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


  (** Creation of HTML5 content from {{: http://erratique.ch/software/react} React } signals.
      HTML5's trees are automatically updated whenever
      corresponding signals change.  *)
  module R : sig
    (** {2 Content creation} *)

    (** See {% <<a_api project="tyxml" | module Html5_sigs.T >> %} and
        the Eliom manual for more detail on
        {% <<a_manual chapter="reactive-dom"| Reactive HTML5 content >>%}. *)
    include module type of Eliom_content_core.Html5.R
  end


  (** Node identifiers *)
  module Id : sig
    include module type of Eliom_content_core.Html5.Id
                             with type +'a id = 'a Eliom_content_core.Html5.Id.id

    (** [get_element id] returns the HTML element in the DOM with the given [id].
        @raises Not_found if the [id] was no such element. *)
    val get_element : 'a id -> 'a elt
  end

  module Custom_data : module type of Eliom_content_core.Html5.Custom_data
                                        with type 'a t = 'a Eliom_content_core.Html5.Custom_data.t

  (** Conversion from HTML5 [elt]s to Javascript DOM elements ([<:] {% <<a_api
      project="js_of_ocaml"| class Dom_html.element >> %}).
      One conversion function per source type (stressed by the [of_] prefix). *)
  module To_dom : sig

    val of_element : 'a elt -> Dom_html.element Js.t
    val of_node : 'a elt -> Dom.node Js.t

    val of_heading : Html5_types.heading elt -> Dom_html.headingElement Js.t

    val of_pcdata : [> `Pcdata] elt -> Dom.text Js.t

    val of_abbr : [> `Abbr] elt -> Dom_html.element Js.t
    val of_acronym : [> `Acronym] elt -> Dom_html.element Js.t
    val of_address : [> `Address] elt -> Dom_html.element Js.t
    val of_applet : [> `Applet] elt -> Dom_html.element Js.t
    val of_article : [> `Article] elt -> Dom_html.element Js.t
    val of_aside : [> `Aside] elt -> Dom_html.element Js.t
    val of_audio : [> `Audio of 'a ] elt -> Dom_html.element Js.t
    val of_b : [> `B] elt -> Dom_html.element Js.t
    val of_basefont : [> `basefont] elt -> Dom_html.element Js.t
    val of_bdi : [> `Bdi] elt -> Dom_html.element Js.t
    val of_bdo : [> `Bdo] elt -> Dom_html.element Js.t
    val of_big : [> `Big] elt -> Dom_html.element Js.t
    val of_center : [> `Center] elt -> Dom_html.element Js.t
    val of_cite : [> `Cite] elt -> Dom_html.element Js.t
    val of_code : [> `Code] elt -> Dom_html.element Js.t
    val of_colgroup : [> `Colgroup] elt -> Dom_html.element Js.t
    val of_command : [> `Command] elt -> Dom_html.element Js.t
    val of_datalist : [> `Datalist] elt -> Dom_html.element Js.t
    val of_dd : [> `Dd] elt -> Dom_html.element Js.t
    val of_del : [> `Del] elt -> Dom_html.element Js.t
    val of_details : [> `Details] elt -> Dom_html.element Js.t
    val of_dfn : [> `Dfn] elt -> Dom_html.element Js.t
    val of_dir : [> `Dir] elt -> Dom_html.element Js.t
    val of_dt : [> `Dt] elt -> Dom_html.element Js.t
    val of_em : [> `Em] elt -> Dom_html.element Js.t
    val of_embed : [> `Embed] elt -> Dom_html.element Js.t
    val of_figcaption : [> `Figcaption] elt -> Dom_html.element Js.t
    val of_figure : [> `Figure] elt -> Dom_html.element Js.t
    val of_font : [> `Font] elt -> Dom_html.element Js.t
    val of_footer : [> `Footer] elt -> Dom_html.element Js.t
    val of_frame : [> `Frame] elt -> Dom_html.element Js.t
    val of_frameset : [> `Frameset] elt -> Dom_html.element Js.t
    val of_h1 : Html5_types.heading elt -> Dom_html.headingElement Js.t
    val of_h2 : Html5_types.heading elt -> Dom_html.headingElement Js.t
    val of_h3 : Html5_types.heading elt -> Dom_html.headingElement Js.t
    val of_h4 : Html5_types.heading elt -> Dom_html.headingElement Js.t
    val of_h5 : Html5_types.heading elt -> Dom_html.headingElement Js.t
    val of_h6 : Html5_types.heading elt -> Dom_html.headingElement Js.t
    val of_header : [> `Header] elt -> Dom_html.element Js.t
    val of_hgroup : [> `Hgroup] elt -> Dom_html.element Js.t
    val of_i : [> `I] elt -> Dom_html.element Js.t
    val of_ins : [> `Ins] elt -> Dom_html.element Js.t
    val of_keygen : [> `Keygen] elt -> Dom_html.element Js.t
    val of_kbd : [> `Kbd] elt -> Dom_html.element Js.t
    val of_mark : [> `Mark] elt -> Dom_html.element Js.t
    val of_menu : [> `Menu] elt -> Dom_html.element Js.t
    val of_meter : [> `Meter] elt -> Dom_html.element Js.t
    val of_nav : [> `Nav] elt -> Dom_html.element Js.t
    val of_noframes : [> `Noframes] elt -> Dom_html.element Js.t
    val of_noscript : [> `Noscript] elt -> Dom_html.element Js.t
    val of_output : [> `Output] elt -> Dom_html.element Js.t
    val of_progress : [> `Progress] elt -> Dom_html.element Js.t
    val of_q : [> `Q] elt -> Dom_html.element Js.t
    val of_rp : [> `Rp] elt -> Dom_html.element Js.t
    val of_rt : [> `Rt] elt -> Dom_html.element Js.t
    val of_ruby : [> `Ruby] elt -> Dom_html.element Js.t
    val of_s : [> `S] elt -> Dom_html.element Js.t
    val of_samp : [> `Samp] elt -> Dom_html.element Js.t
    val of_section : [> `Section] elt -> Dom_html.element Js.t
    val of_small : [> `Small] elt -> Dom_html.element Js.t
    val of_source : [> `Source] elt -> Dom_html.element Js.t
    val of_span : [> `Span] elt -> Dom_html.element Js.t
    val of_strike : [> `Strike] elt -> Dom_html.element Js.t
    val of_strong : [> `Strong] elt -> Dom_html.element Js.t
    val of_sub : [> `Sub] elt -> Dom_html.element Js.t
    val of_summary : [> `Summary] elt -> Dom_html.element Js.t
    val of_sup : [> `Sup] elt -> Dom_html.element Js.t
    val of_th : [> `Th] elt -> Dom_html.element Js.t
    val of_time : [> `Time] elt -> Dom_html.element Js.t
    val of_track : [> `Track] elt -> Dom_html.element Js.t
    val of_tt : [> `Tt] elt -> Dom_html.element Js.t
    val of_u : [> `U] elt -> Dom_html.element Js.t
    val of_var : [> `Var] elt -> Dom_html.element Js.t
    val of_video : [> `Video] elt -> Dom_html.element Js.t
    val of_wbr : [> `Wbr] elt -> Dom_html.element Js.t

    val of_html : Html5_types.html elt -> Dom_html.htmlElement Js.t
    val of_head : Html5_types.head elt -> Dom_html.headElement Js.t
    val of_link : Html5_types.link elt -> Dom_html.linkElement Js.t
    val of_title : Html5_types.title elt -> Dom_html.titleElement Js.t
    val of_meta : Html5_types.meta elt -> Dom_html.metaElement Js.t
    val of_base : Html5_types.base elt -> Dom_html.baseElement Js.t
    val of_style : Html5_types.style elt -> Dom_html.styleElement Js.t
    val of_body : Html5_types.body elt -> Dom_html.bodyElement Js.t
    val of_form : Html5_types.form elt -> Dom_html.formElement Js.t
    val of_optgroup : Html5_types.optgroup elt -> Dom_html.optGroupElement Js.t
    val of_option : Html5_types.selectoption elt -> Dom_html.optionElement Js.t
    val of_select : Html5_types.select elt -> Dom_html.selectElement Js.t
    val of_input : Html5_types.input elt -> Dom_html.inputElement Js.t
    val of_textarea : Html5_types.textarea elt -> Dom_html.textAreaElement Js.t
    val of_button : Html5_types.button elt -> Dom_html.buttonElement Js.t
    val of_label : Html5_types.label elt -> Dom_html.labelElement Js.t
    val of_fieldset : Html5_types.fieldset elt -> Dom_html.fieldSetElement Js.t
    val of_legend : Html5_types.legend elt -> Dom_html.legendElement Js.t
    val of_ul : Html5_types.ul elt -> Dom_html.uListElement Js.t
    val of_ol : Html5_types.ol elt -> Dom_html.oListElement Js.t
    val of_dl : [`Dl] elt -> Dom_html.dListElement Js.t
    val of_li : Html5_types.li elt -> Dom_html.liElement Js.t
    val of_div : Html5_types.div elt -> Dom_html.divElement Js.t
    val of_p : Html5_types.p elt -> Dom_html.paragraphElement Js.t
    val of_blockquote : Html5_types.blockquote elt -> Dom_html.quoteElement Js.t
    val of_pre : Html5_types.pre elt -> Dom_html.preElement Js.t
    val of_br : Html5_types.br elt -> Dom_html.brElement Js.t
    val of_hr : Html5_types.hr elt -> Dom_html.hrElement Js.t
    val of_a : 'a Html5_types.a elt -> Dom_html.anchorElement Js.t
    val of_img : [`Img] elt -> Dom_html.imageElement Js.t
    val of_object : 'a Html5_types.object_ elt -> Dom_html.objectElement Js.t
    val of_param : Html5_types.param elt -> Dom_html.paramElement Js.t
    val of_area : Html5_types.area elt -> Dom_html.areaElement Js.t
    val of_map : 'a Html5_types.map elt -> Dom_html.mapElement Js.t
    val of_script : Html5_types.script elt -> Dom_html.scriptElement Js.t
    val of_td : [ Html5_types.td | Html5_types.td ] elt -> Dom_html.tableCellElement Js.t
    val of_tr : Html5_types.tr elt -> Dom_html.tableRowElement Js.t
    val of_col : Html5_types.col elt -> Dom_html.tableColElement Js.t
    val of_tfoot : Html5_types.tfoot elt -> Dom_html.tableSectionElement Js.t
    val of_thead : Html5_types.thead elt -> Dom_html.tableSectionElement Js.t
    val of_tbody : Html5_types.tbody elt -> Dom_html.tableSectionElement Js.t
    val of_caption : Html5_types.caption elt -> Dom_html.tableCaptionElement Js.t
    val of_table : Html5_types.table elt -> Dom_html.tableElement Js.t
    val of_canvas : 'a Html5_types.canvas elt -> Dom_html.canvasElement Js.t
    val of_iframe : Html5_types.iframe elt -> Dom_html.iFrameElement Js.t

  end

  (** DOM-like manipulation functions.

      In this module, all the functions apply only to HTML5 element with
      {% <<a_manual chapter="clientserver-html" fragment="unique"|Dom semantics>>
      %}.
  *)
  module Manip : sig

    (** The function [appendChild e1 e2] inserts the element [e2] as last
        child of [e1]. If the optional parameter [~before:e3] is present
        and if [e3] is a child of [e1], then [e2] is inserted before [e3]
        in the list of [e1] children. *)
    val appendChild: ?before:'a elt -> 'b elt ->  'c elt -> unit

    (** Append to the body of the document. *)
    val appendToBody: ?before:'a elt -> 'c elt -> unit

    (** The function [appendChildren e1 elts] inserts [elts] as last children
        of [e1]. If the optional parameter [~before:e3] is present and if
        [e3] is a child of [e1], then [elts] are inserted before [e3] in
        the list of [e1] children. *)
    val appendChildren: ?before:'a elt -> 'b elt ->  'c elt list -> unit

    (** [appendChildFirst p c] appends [c] as first child of [p] *)
    val appendChildFirst: 'b elt ->  'c elt -> unit

    (** [nth e n] returns the nth child of [e] (first is 0) *)
    val nth : 'a elt -> int -> 'b elt option

    (** [childLength e] returns the number of chilren of [e] *)
    val childLength : 'a elt -> int

    (** The function [removeChild e1 e2] removes for [e2] from the list of
        [e1] children. *)
    val removeChild: 'a elt -> 'b elt -> unit

    (** The function [replace e1 e2 e3] replaces for [e2] by [e3] in the
        list of [e1] children. *)
    val replaceChild: 'a elt -> 'b elt -> 'c elt -> unit

    (** The function [removeChildren e1] removes [e1] children. *)
    val removeChildren: 'a elt -> unit

    (** [removeSelf e] removes element e from the DOM. *)
    val removeSelf: 'a elt -> unit

    (** The function [replaceChildren e1 elts] replaces all the children of
        [e1] by [elt]. *)
    val replaceChildren: 'a elt -> 'b elt list -> unit

    (** The function [addEventListener elt evt handler] attach the
        [handler] for the event [evt] on the element [elt]. See the
        Js_of_ocaml manual, for a list of {% <<a_api project="js_of_ocaml"
        text="available events"| module Dom_html.Event >>%}. *)
    val addEventListener:
      ?capture:bool ->
      'a elt ->
      (#Dom_html.event as 'b) Js.t Dom_html.Event.typ ->
      ('a elt -> 'b Js.t -> bool) ->
      Dom_html.event_listener_id

    (** Dom manipulation by element identifier. *)
    module Named: sig

      (** The module [Named] defines the same functions as
          [Eliom_dom]. They take as parameter an element identifier
          instead of an element with Dom semantics. Those functions only
          works if the element is available in the application (sent in
          the page or along the page). If the element is not available,
          those functions raise with [Not_found]. *)

      (** see [appendChild] *)
      val appendChild: ?before:'a elt -> 'b Id.id -> 'c elt -> unit
      (** see [appendChildren] *)
      val appendChildren: ?before:'a elt -> 'b Id.id ->  'c elt list -> unit
      (** see [removeChild] *)
      val removeChild: 'a Id.id -> 'b elt -> unit
      (** see [replaceChild] *)
      val replaceChild: 'a Id.id -> 'b elt -> 'c elt -> unit
      (** see [removeChildren] *)
      val removeChildren: 'a Id.id -> unit
      (** see [replaceChildren] *)
      val replaceChildren: 'a Id.id -> 'b elt list -> unit

      (** see [addEventListener] *)
      val addEventListener:
        ?capture:bool ->
        'a Id.id ->
        (#Dom_html.event as 'b) Js.t Dom_html.Event.typ ->
        ('a elt -> 'b Js.t -> bool) ->
        Dom_html.event_listener_id

    end

    (** The function [scrollIntoView elt] scroll the page to a position
        where [elt] is displayed at the top of the window. If the optional
        parameter [~bottom:true] is present, the page is scrolled to a
        position where [elt] is displayed at the bottom of the window. *)
    val scrollIntoView: ?bottom:bool -> 'a elt -> unit

    (**/**)
    val childNodes: 'a elt -> Dom.node Js.t list
    val childElements: 'a elt -> Dom.element Js.t list
    (**/**)

(*
    val get_custom_data : _ elt -> 'a Custom_data.t -> 'a
    val set_custom_data : _ elt -> 'a Custom_data.t -> 'a -> unit
 *)

    module Class : sig
      val contain : 'a elt -> string -> bool
      val remove : 'a elt -> string -> unit
      val removes :'a elt -> string list -> unit
      val add :'a elt -> string -> unit
      val adds :'a elt -> string list -> unit
      val replace :  'a elt ->  string -> string -> unit
      val clear : 'a elt -> unit
      val toggle : 'a elt -> string -> unit
      val toggle2 : 'a elt -> string -> string -> unit
    end

    module Elt : sig
      val body : [`Body] elt
    end

    module Ev : sig
      type ('a,'b) ev = 'a elt -> ('b Js.t -> bool) -> unit
      type ('a,'b) ev_unit = 'a elt -> ('b Js.t -> unit) -> unit
      val onkeyup : ('a,Dom_html.keyboardEvent) ev
      val onkeydown : ('a,Dom_html.keyboardEvent) ev
      val onmouseup : ('a,Dom_html.mouseEvent) ev
      val onmousedown : ('a,Dom_html.mouseEvent) ev
      val onmouseout : ('a,Dom_html.mouseEvent) ev
      val onmouseover : ('a,Dom_html.mouseEvent) ev
      val onclick : ('a,Dom_html.mouseEvent) ev
      val ondblclick : ('a,Dom_html.mouseEvent) ev
      val onload : ('a,Dom_html.event) ev
      val onerror : ('a,Dom_html.event) ev
      val onabort : ('a,Dom_html.event) ev
      val onfocus : ('a,Dom_html.event) ev
      val onblur : ('a,Dom_html.event) ev
      val onfocus_textarea : ('a,Dom_html.event) ev
      val onblur_textarea : ('a,Dom_html.event) ev
      val onscroll : ('a,Dom_html.event) ev
      val onreturn : ('a,Dom_html.keyboardEvent) ev_unit
      val onchange : ('a,Dom_html.event) ev
      val onchange_select : ('a,Dom_html.event) ev
    end

    module Attr : sig
      val clientWidth : 'a elt -> int
      val clientHeight : 'a elt -> int
      val offsetWidth : 'a elt -> int
      val offsetHeight : 'a elt -> int
      val clientLeft : 'a elt -> int
      val clientTop : 'a elt -> int
    end

    (** Read the CSS properties of DOM elements. *)
    module Css : sig
      val background: 'a elt -> string
      val backgroundAttachment: 'a elt -> string
      val backgroundColor: 'a elt -> string
      val backgroundImage: 'a elt -> string
      val backgroundPosition: 'a elt -> string
      val backgroundRepeat: 'a elt -> string
      val border: 'a elt -> string
      val borderBottom: 'a elt -> string
      val borderBottomColor: 'a elt -> string
      val borderBottomStyle: 'a elt -> string
      val borderBottomWidth: 'a elt -> string
      val borderBottomWidthPx : 'a elt -> int
      val borderCollapse: 'a elt -> string
      val borderColor: 'a elt -> string
      val borderLeft: 'a elt -> string
      val borderLeftColor: 'a elt -> string
      val borderLeftStyle: 'a elt -> string
      val borderLeftWidth: 'a elt -> string
      val borderLeftWidthPx : 'a elt -> int
      val borderRight: 'a elt -> string
      val borderRightColor: 'a elt -> string
      val borderRightStyle: 'a elt -> string
      val borderRightWidth: 'a elt -> string
      val borderRightWidthPx : 'a elt -> int
      val borderSpacing: 'a elt -> string
      val borderStyle: 'a elt -> string
      val borderTop: 'a elt -> string
      val borderTopColor: 'a elt -> string
      val borderTopStyle: 'a elt -> string
      val borderTopWidth: 'a elt -> string
      val borderTopWidthPx : 'a elt -> int
      val borderWidth: 'a elt -> string
      val bottom: 'a elt -> string
      val captionSide: 'a elt -> string
      val clear: 'a elt -> string
      val clip: 'a elt -> string
      val color: 'a elt -> string
      val content: 'a elt -> string
      val counterIncrement: 'a elt -> string
      val counterReset: 'a elt -> string
      val cssFloat: 'a elt -> string
      val cssText: 'a elt -> string
      val cursor: 'a elt -> string
      val direction: 'a elt -> string
      val display: 'a elt -> string
      val emptyCells: 'a elt -> string
      val font: 'a elt -> string
      val fontFamily: 'a elt -> string
      val fontSize: 'a elt -> string
      val fontStyle: 'a elt -> string
      val fontVariant: 'a elt -> string
      val fontWeight: 'a elt -> string
      val height: 'a elt -> string
      val heightPx : 'a elt -> int
      val left: 'a elt -> string
      val leftPx : 'a elt -> int
      val letterSpacing: 'a elt -> string
      val lineHeight: 'a elt -> string
      val listStyle: 'a elt -> string
      val listStyleImage: 'a elt -> string
      val listStylePosition: 'a elt -> string
      val listStyleType: 'a elt -> string
      val margin: 'a elt -> string
      val marginBottom: 'a elt -> string
      val marginBottomPx : 'a elt -> int
      val marginLeft: 'a elt -> string
      val marginLeftPx : 'a elt -> int
      val marginRight: 'a elt -> string
      val marginRightPx : 'a elt -> int
      val marginTop: 'a elt -> string
      val marginTopPx : 'a elt -> int
      val maxHeight: 'a elt -> string
      val maxHeightPx : 'a elt -> int
      val maxWidth: 'a elt -> string
      val maxWidthPx : 'a elt -> int
      val minHeight: 'a elt -> string
      val minHeightPx : 'a elt -> int
      val minWidth: 'a elt -> string
      val minWidthPx : 'a elt -> int
      val opacity: 'a elt -> string option
      val outline: 'a elt -> string
      val outlineColor: 'a elt -> string
      val outlineOffset: 'a elt -> string
      val outlineStyle: 'a elt -> string
      val outlineWidth: 'a elt -> string
      val overflow: 'a elt -> string
      val overflowX: 'a elt -> string
      val overflowY: 'a elt -> string
      val padding: 'a elt -> string
      val paddingBottom: 'a elt -> string
      val paddingBottomPx : 'a elt -> int
      val paddingLeft: 'a elt -> string
      val paddingLeftPx : 'a elt -> int
      val paddingRight: 'a elt -> string
      val paddingRightPx : 'a elt -> int
      val paddingTop: 'a elt -> string
      val paddingTopPx : 'a elt -> int
      val pageBreakAfter: 'a elt -> string
      val pageBreakBefore: 'a elt -> string
      val position: 'a elt -> string
      val right: 'a elt -> string
      val rightPx : 'a elt -> int
      val tableLayout: 'a elt -> string
      val textAlign: 'a elt -> string
      val textDecoration: 'a elt -> string
      val textIndent: 'a elt -> string
      val textTransform: 'a elt -> string
      val top: 'a elt -> string
      val topPx : 'a elt -> int
      val verticalAlign: 'a elt -> string
      val visibility: 'a elt -> string
      val whiteSpace: 'a elt -> string
      val width: 'a elt -> string
      val widthPx : 'a elt -> int
      val wordSpacing: 'a elt -> string
      val zIndex: 'a elt -> string
    end

    (** Modify the CSS properties of DOM elements. *)
    module SetCss : sig
      val background: 'a elt -> string -> unit
      val backgroundAttachment: 'a elt -> string -> unit
      val backgroundColor: 'a elt -> string -> unit
      val backgroundImage: 'a elt -> string -> unit
      val backgroundPosition: 'a elt -> string -> unit
      val backgroundRepeat: 'a elt -> string -> unit
      val border: 'a elt -> string -> unit
      val borderBottom: 'a elt -> string -> unit
      val borderBottomColor: 'a elt -> string -> unit
      val borderBottomStyle: 'a elt -> string -> unit
      val borderBottomWidth: 'a elt -> string -> unit
      val borderBottomWidthPx : 'a elt -> int -> unit
      val borderCollapse: 'a elt -> string -> unit
      val borderColor: 'a elt -> string -> unit
      val borderLeft: 'a elt -> string -> unit
      val borderLeftColor: 'a elt -> string -> unit
      val borderLeftStyle: 'a elt -> string -> unit
      val borderLeftWidth: 'a elt -> string -> unit
      val borderLeftWidthPx : 'a elt -> int -> unit
      val borderRight: 'a elt -> string -> unit
      val borderRightColor: 'a elt -> string -> unit
      val borderRightStyle: 'a elt -> string -> unit
      val borderRightWidth: 'a elt -> string -> unit
      val borderRightWidthPx : 'a elt -> int -> unit
      val borderSpacing: 'a elt -> string -> unit
      val borderStyle: 'a elt -> string -> unit
      val borderTop: 'a elt -> string -> unit
      val borderTopColor: 'a elt -> string -> unit
      val borderTopStyle: 'a elt -> string -> unit
      val borderTopWidth: 'a elt -> string -> unit
      val borderTopWidthPx : 'a elt -> int -> unit
      val borderWidth: 'a elt -> string -> unit
      val bottom: 'a elt -> string -> unit
      val bottomPx : 'a elt -> int -> unit
      val captionSide: 'a elt -> string -> unit
      val clear: 'a elt -> string -> unit
      val clip: 'a elt -> string -> unit
      val color: 'a elt -> string -> unit
      val content: 'a elt -> string -> unit
      val counterIncrement: 'a elt -> string -> unit
      val counterReset: 'a elt -> string -> unit
      val cssFloat: 'a elt -> string -> unit
      val cssText: 'a elt -> string -> unit
      val cursor: 'a elt -> string -> unit
      val direction: 'a elt -> string -> unit
      val display: 'a elt -> string -> unit
      val emptyCells: 'a elt -> string -> unit
      val font: 'a elt -> string -> unit
      val fontFamily: 'a elt -> string -> unit
      val fontSize: 'a elt -> string -> unit
      val fontStyle: 'a elt -> string -> unit
      val fontVariant: 'a elt -> string -> unit
      val fontWeight: 'a elt -> string -> unit
      val height: 'a elt -> string -> unit
      val heightPx : 'a elt -> int -> unit
      val left: 'a elt -> string -> unit
      val leftPx : 'a elt -> int -> unit
      val letterSpacing: 'a elt -> string -> unit
      val lineHeight: 'a elt -> string -> unit
      val listStyle: 'a elt -> string -> unit
      val listStyleImage: 'a elt -> string -> unit
      val listStylePosition: 'a elt -> string -> unit
      val listStyleType: 'a elt -> string -> unit
      val margin: 'a elt -> string -> unit
      val marginBottom: 'a elt -> string -> unit
      val marginBottomPx : 'a elt -> int -> unit
      val marginLeft: 'a elt -> string -> unit
      val marginLeftPx : 'a elt -> int -> unit
      val marginRight: 'a elt -> string -> unit
      val marginRightPx : 'a elt -> int -> unit
      val marginTop: 'a elt -> string -> unit
      val marginTopPx : 'a elt -> int -> unit
      val maxHeight: 'a elt -> string -> unit
      val maxHeightPx : 'a elt -> int -> unit
      val maxWidth: 'a elt -> string -> unit
      val maxWidthPx : 'a elt -> int -> unit
      val minHeight: 'a elt -> string -> unit
      val minHeightPx : 'a elt -> int -> unit
      val minWidth: 'a elt -> string -> unit
      val minWidthPx : 'a elt -> int -> unit
      val opacity: 'a elt -> string option -> unit
      val outline: 'a elt -> string -> unit
      val outlineColor: 'a elt -> string -> unit
      val outlineOffset: 'a elt -> string -> unit
      val outlineStyle: 'a elt -> string -> unit
      val outlineWidth: 'a elt -> string -> unit
      val overflow: 'a elt -> string -> unit
      val overflowX: 'a elt -> string -> unit
      val overflowY: 'a elt -> string -> unit
      val padding: 'a elt -> string -> unit
      val paddingBottom: 'a elt -> string -> unit
      val paddingBottomPx : 'a elt -> int -> unit
      val paddingLeft: 'a elt -> string -> unit
      val paddingLeftPx : 'a elt -> int -> unit
      val paddingRight: 'a elt -> string -> unit
      val paddingRightPx : 'a elt -> int -> unit
      val paddingTop: 'a elt -> string -> unit
      val paddingTopPx : 'a elt -> int -> unit
      val pageBreakAfter: 'a elt -> string -> unit
      val pageBreakBefore: 'a elt -> string -> unit
      val position: 'a elt -> string -> unit
      val right: 'a elt -> string -> unit
      val rightPx : 'a elt -> int -> unit
      val tableLayout: 'a elt -> string -> unit
      val textAlign: 'a elt -> string -> unit
      val textDecoration: 'a elt -> string -> unit
      val textIndent: 'a elt -> string -> unit
      val textTransform: 'a elt -> string -> unit
      val top: 'a elt -> string -> unit
      val topPx : 'a elt -> int -> unit
      val verticalAlign: 'a elt -> string -> unit
      val visibility: 'a elt -> string -> unit
      val whiteSpace: 'a elt -> string -> unit
      val width: 'a elt -> string -> unit
      val widthPx : 'a elt -> int -> unit
      val wordSpacing: 'a elt -> string -> unit
      val zIndex: 'a elt -> string -> unit
    end
  end

  (** Conversion functions from DOM nodes ({% <<a_api project="js_of_ocaml"| type Dom_html.element>> %} {% <<a_api
      project="js_of_ocaml"| type Js.t>> %}) to Eliom nodes ({% <<a_api | type Eliom_content.Html5.elt>> %}). *)
  module Of_dom : module type of Eliom_content_core.Html5.Of_dom

end

val force_link : unit
